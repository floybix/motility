(ns org.nfrac.motility.tcppn
  (:require [org.nfrac.gpai.evolution :as evo]
            [org.nfrac.gpai.coevolution :as coevo]
            [org.nfrac.gpai.utils :as gpai-utils]
            [org.nfrac.cppn :as cppn]
            [org.nfrac.cppn.compile :as cc]
            [org.nfrac.cppn.util :as util]
            [org.nfrac.bok.cljplayer :as cljplayer]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.test.check.random :as random]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pp])
  (:import [org.zeromq ZMQ]))

(def ident {:creature-type :bipoid
            :name "t-CPPN bipoid process"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

(def output-joints
  [:leg-a1 :leg-a2 :wheel-a
   :leg-b1 :leg-b2 :wheel-b])

(def joint-origins
  {:leg-a1 [-0.2 -0.2]
   :leg-b1 [ 0.2 -0.2]
   :leg-a2 [-0.6 0.5]
   :leg-b2 [ 0.6 0.5]
   :wheel-a [-0.9 0.9]
   :wheel-b [ 0.9 0.9]})

(def max-torque 200.0)

(def max-motor-speed 12.0) ;; radians per second

(def win-fitness 2.0)
(def loss-fitness -1.0)
(def dist-to-edge-scale 7.0)
(def dist-to-other-scale 10.0)
(def height-scale 4.0)
(def time-scale 1.0)

;; CPPN is evaluated on each joint

(def sensory-inputs
  #{:head-height :d-target :d-edge})

(def cppn-inputs (into #{:bias :x0 :y0 :d0 :tx1 :tx2 :tx4}
                       sensory-inputs))

(def cppn-outputs #{:motor-speed}) ;:max-torque

(defn x-val [[x y]] x)
(defn y-val [[x y]] y)

(defn its-xy
  [component]
  (:position (first (:points component))))

(defn clamp [x lo hi] (-> (double x) (min hi) (max lo)))

(defn sign [x] (if (neg? x) -1.0 1.0))

(defn my-distance-to-edge
  [state]
  (let [{:keys [entities my-key]} (:current state)
        my-x (->> entities my-key :components :head :points first :position x-val)]
    (->>
     (for [point (->> entities :ground :components :it :points)
           :let [x (x-val (:position point))]]
       (cc/abs (- x my-x)))
     (apply min))))

(defn extract-invals
  [state]
  (let [{:keys [entities my-key other-players raycast time]} (:current state)
        me (get entities my-key)
        other (get entities (first other-players))
        head (:head (:components me))
        other-head (:head (:components other))
        eye (first (:points head))
        o-eye (first (:points other-head))
        o-offset (- (or (x-val (:position o-eye)) 100) ;; nil if occluded
                    (x-val (:position eye)))
        target-direction (sign o-offset)
        height (y-val (:position eye))
        tx1 (-> (/ time (* 1 time-scale)) (mod 1.0))
        tx2 (-> (/ time (* 2 time-scale)) (mod 1.0))
        tx4 (-> (/ time (* 4 time-scale)) (mod 1.0))
        joint-invals (reduce (fn [m joint-id]
                               (let [[x0* y0] (get joint-origins joint-id)
                                     x0 (* target-direction x0*)
                                     d0 (cc/xy->d x0 y0)]
                                 (assoc m joint-id
                                        {:x0 x0
                                         :y0 y0
                                         :d0 d0})))
                             {}
                             output-joints)
        globals {:bias 1.0
                 :head-height (-> (/ height height-scale) (clamp -1.0 1.0))
                 :d-target (-> (/ o-offset dist-to-other-scale) (cc/abs) (clamp 0.0 1.0))
                 :d-edge (-> (/ (my-distance-to-edge state) dist-to-edge-scale) (clamp -1.0 1.0))
                 :target-direction target-direction
                 :debug [my-key (:position eye) (:position o-eye)]
                 :tx1 tx1
                 :tx2 tx2
                 :tx4 tx4}]
    (reduce-kv (fn [m joint-id joint-vals]
                 (assoc m joint-id (merge globals joint-vals)))
               {}
               joint-invals)))

(def debug-counter (atom 0))

(defn action-fn
  [cppn-fn in-order]
  (fn [info]
    (let [all-invals (extract-invals info)
          per-joint (reduce-kv (fn [m joint-id invals]
                                 (let [outs (apply cppn-fn (map invals in-order))]
                                   (assoc m joint-id
                                          [(* (:motor-speed outs) max-motor-speed
                                              (:target-direction invals))
                                           max-torque
                                           ;(* (cc/abs (:max-torque outs)) max-torque)
                                           ])))
                               {}
                               all-invals)]
      (when false ;(zero? (mod (swap! debug-counter inc) (* 30 3)))
        (println (:leg-a1 all-invals)))
      (assoc info :actions
             {:joint-motors per-joint})
      )))

(defn bok-result
  "Takes a pair of CPPN genomes and returns the result as a pair
  of numbers (:win 1, :loss -1, :draw 0) from a bok game."
  [[gm1 gm2] socket1 socket2 peek-ref]
  (let [f1 (cc/build-cppn-fn gm1)
        f2 (cc/build-cppn-fn gm2)
        in-order (sort (:inputs gm1))
        action-fn1 (action-fn f1 in-order)
        action-fn2 (action-fn f2 in-order)
        foo1 (future (cljplayer/run-one-bout socket1 ident action-fn1 peek-ref))
        foo2 (future (cljplayer/run-one-bout socket2 ident action-fn2 (atom {})))]
    (case (:my-result @foo1)
      :win [win-fitness loss-fitness]
      :loss [loss-fitness win-fitness]
      :draw (let [dist1 (my-distance-to-edge (:last-scene @foo1))
                  dist2 (my-distance-to-edge (:last-scene @foo2))]
              (if (> dist1 dist2)
                ;; 2 is closer to edge; 1 wins
                [(-> (- 1.0 (/ dist2 dist-to-edge-scale))
                     (clamp 0.0 1.0))
                 0.0]
                ;; 1 is closer to edge; 2 wins
                [0.0
                 (-> (- 1.0 (/ dist1 dist-to-edge-scale))
                     (clamp 0.0 1.0))]
                )))))

(defn print-fitness-ranges-fn
  "Returns fn to plot fitness with a text line mapped to range [fit-min fit-max]."
  [fit-min fit-max]
  (fn [i xs h ]
    (let [strata (group-by coevo/get-popn xs)
          width 80
          fit-range (- fit-max fit-min)
          line-scale (fn [fit] (* width (/ (- fit fit-min) fit-range)))
          i-0 (line-scale 0)
          prefix (format "%4d |" i)
          postfix "|=best"]
      (doseq [[id sxs] (sort strata)]
        (let [me (if (= :a id) "a" "B")
              fso (sort (map evo/get-fitness-0 sxs))
              fit-lo (first fso)
              fit-hi (last fso)
              i-lo (line-scale fit-lo)
              i-hi (line-scale fit-hi)
              my-line (take width (concat (repeat i-lo \space)
                                          (repeat (- i-hi i-lo) me)
                                          (repeat \space)))
              my-line (assoc (vec my-line) i-0 "0")]
          (println (str prefix
                        (apply str my-line)
                        postfix)))))))


(def cppn-mutation-parameters
  (assoc cppn/parameter-defaults
         :weight-perturbation 0.5))

(defn mutate
  [cppn]
  (let [rng (random/make-random)]
    (cppn/mutate-with-perturbation cppn rng cppn-mutation-parameters)))

(def seed 42)
(def select-n 8)
(def subpop-n 20)
(def elitism 1)
(def vs-curr-n 5)
(def vs-hist-n 5)

(defn run-evo
  [init-popn1 init-popn2 history socket1 socket2 peek-ref n-gens]
  (let [fitness (fn [gm1 gm2]
                  ;; want behaviour to work from each side of world.
                  ;; so evaluate twice with switched sides; aggregate results.
                  (let [[a1 b1] (bok-result [gm1 gm2] socket1 socket2 peek-ref)
                        [a2 b2] (bok-result [gm1 gm2] socket2 socket1 peek-ref)]
                    (println (format "fitness [%.1f %.1f] [%.1f %.1f]"
                                     (double a1) (double a2)
                                     (double b1) (double b2)))
                    [(/ (+ a1 a2) 2.0)
                     (/ (+ b1 b2) 2.0)]))
        regen (evo/negative-selection-fn select-n mutate
                                         nil ;; no crossover
                                         :elitism elitism)
        progress (juxt
                  (gpai-utils/snapshot-to-file-fn (str "tcppn-snap-" seed ".edn") 2.0)
                  (print-fitness-ranges-fn -10 10))
        soln (time (coevo/coevolve init-popn-1 init-popn-2
                                   fitness
                                   (coevo/history-peaks-parasites-fn vs-curr-n vs-hist-n)
                                   regen
                                   {:n-gens n-gens
                                    :progress! progress
                                    :progress-every 1}))]
    (let [achamps (mapv #(get-in % [:a :best]) (:history soln))
          bchamps (mapv #(get-in % [:b :best]) (:history soln))
          apeaks (gpai-utils/ts-peaks (map evo/get-fitness achamps))
          bpeaks (gpai-utils/ts-peaks (map evo/get-fitness bchamps))
          asel (map (fn [p] (nth achamps (:end p))) apeaks)
          bsel (map (fn [p] (nth bchamps (:end p))) bpeaks)
          ]
      (println)
      (println "a subpop fitness peaks:")
      (dorun (map println apeaks))
      (println)
      (println "b subpop fitness peaks:")
      (dorun (map println bpeaks))
      (println)
      (let [final (last (:history soln))
            a-gm (:best (:a final))
            b-gm (:best (:b final))
            ]
        (println "a final genome:")
        (prn a-gm)
        (println)
        (binding [pp/*print-suppress-namespaces* true]
          (pp/pprint a-gm))
        (println)
        (println "b final genome:")
        (prn b-gm)
        (println)
        (binding [pp/*print-suppress-namespaces* true]
          (pp/pprint b-gm))
        a-gm))))

(defn run-evo
  [socket1 socket2 peek-ref n-gens]
  (let [[rng r2 r3] (-> (random/make-random seed) (random/split-n 3))
        init-popn-1 (for [r (random/split-n r2 subpop-n)]
                      (cppn/rand-cppn cppn-inputs cppn-outputs r))
        init-popn-2 (for [r (random/split-n r3 subpop-n)]
                      (cppn/rand-cppn cppn-inputs cppn-outputs r))
        ]
    (continue-evo socket1 socket2 peek-ref n-gens)
    )
  )

(defn to-file
  [file form]
  (with-open [w (io/writer file)]
    (print-dup form w)))

(defn from-file
  [file]
  (with-open [r (java.io.PushbackReader. (io/reader file))]
    (edn/read r)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port1 port2 peek-ref n-gens & [continue-file]]
  (let [ctx (ZMQ/context 1)
        addr1 (str "tcp://*:" port1)
        addr2 (str "tcp://*:" port2)
        continue (when-let [f continue-file]
                   (from-file f))]
    (println "starting server on TCP port " port1 " and " port2)
    (try
      (with-open [socket1 (.socket ctx ZMQ/REP)
                  socket2 (.socket ctx ZMQ/REP)]
        (doto socket1
          (.bind addr1)
          (.setSendTimeOut 2000))
        (doto socket2
          (.bind addr2)
          (.setSendTimeOut 2000))
        (binding [*out* *err*]
          (run-evo socket1 socket2 peek-ref n-gens)))
      (finally
        (println "closing ZMQ context")
        (.term ctx)))))

(defn -main
  [& [port1 port2 more-args]]
  (main port1 port2 (atom {})))
