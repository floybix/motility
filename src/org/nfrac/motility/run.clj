(ns org.nfrac.motility.run
  (:require (org.nfrac.gpai [lang-float :as langf]
                            [cgp :as cgp]
                            [evolution :as evo])
            [clojure.data.generators :as gen]
            [org.nfrac.bok.cljplayer :refer [send-msg recv-msg patch]]
            [clojure.pprint :as pp])
  (:import [org.zeromq ZMQ]))

(def ident {:creature-type :bipoid
            :name "cgp bipoid process"
            :author "Felix Andrews <felix@nfrac.org>"
            :version "0.1.0-SNAPSHOT"
            :bok-version [0 1 0]})

;; max torque
(def MT 100.0)

(def raycast-syms '[e-dist w-dist se-dist sw-dist])
;; corresponding to
(def raycast-angles (map #(* % Math/PI) [0 -1 -0.25 -0.75]))

(def input-symbols
  (into
   '[
     ;;height
     a1-ang a2-ang
     b1-ang b2-ang
     aw-yo aw-xo
     bw-yo bw-xo
     a1-speed a2-speed
     b1-speed b2-speed
     a2-norm-imp aw-norm-imp 
     b2-norm-imp bw-norm-imp
     t-sin t-cos
     ;; TODO time signals - sine waves?
     ]
   raycast-syms))

(def output-joints
  [:leg-a1 :leg-a2 :wheel-a
   :leg-b1 :leg-b2 :wheel-b])

(defn x-val [[x y]] x)
(defn y-val [[x y]] y)

(defn height
  [component]
  (y-val (:position (first (:points component)))))

(defn x-offset
  [component]
  (x-val (:position (first (:points component)))))

(defn norm-imp
  [component]
  (->> (:contacts component)
       (mapcat :normal-impulses)
       (apply max -1)))

(defn extract-invals
  [state]
  (let [{:keys [entities my-key raycast time]} (:current state)
        me (get entities my-key)
        {:keys [head leg-a1 leg-a2 leg-b1 leg-b2 wheel-a wheel-b]} (:components me)
        joints (:joints me)
        rcs (zipmap raycast-syms (map :distance raycast))
        ;eye (first (:points head))
        ]
    (for [sym input-symbols]
     (case sym
       a1-ang (:angle leg-a1)
       a2-ang (:angle leg-a2)
       b1-ang (:angle leg-b1)
       b2-ang (:angle leg-b2)
       ;; note lower legs have the POI, not the wheel:
       aw-yo (- (height leg-a2) (height head))
       bw-yo (- (height leg-b2) (height head))
       aw-xo (- (x-offset leg-a2) (x-offset head))
       bw-xo (- (x-offset leg-b2) (x-offset head))
       a1-speed (:joint-speed (:leg-a1 joints))
       b1-speed (:joint-speed (:leg-b1 joints))
       a2-speed (:joint-speed (:leg-a2 joints))
       b2-speed (:joint-speed (:leg-b2 joints))
       a2-norm-imp (norm-imp leg-a2)
       b2-norm-imp (norm-imp leg-b2)
       aw-norm-imp (norm-imp wheel-a)
       bw-norm-imp (norm-imp wheel-b)
       w-dist (or (get rcs 'w-dist) 100.0)
       e-dist (or (get rcs 'e-dist) 100.0)
       sw-dist (or (get rcs 'sw-dist) 100.0)
       se-dist (or (get rcs 'se-dist) 100.0)
       t-sin (Math/sin time)
       t-cos (Math/cos time)
       ))))

(defn run-one-bout
  "Returns the final result of the bout."
  [socket action-fn peek-ref]
  (loop []
    (let [msg (recv-msg socket)
          bouts peek-ref]
      (case (:type msg)
        :identify (send-msg socket {:type :ident
                                    :data ident})
        :invite (let [bout-id (:bout-id msg)]
                  (send-msg socket {:type :join
                                    :bout-id bout-id})
                  (swap! bouts assoc bout-id {}))
        :react (let [bout-id (:bout-id msg)
                     last-state (@bouts bout-id)
                     state (-> last-state
                               (update-in [:current]
                                          patch (:data msg))
                               (action-fn))
                     actions (:actions state)]
                 (send-msg socket {:type :actions
                                   :data actions})
                 (swap! bouts assoc bout-id state))
        :final-result (let [bout-id (:bout-id msg)]
                        (send-msg socket {:type :bye})
                        (swap! bouts dissoc bout-id))
        (println "Unrecognised message type:" msg))
      (if (= :final-result (:type msg))
        (:data msg)
        (recur)))))


(defn bok-alti-fitness
  "Take a CGP genome and return height achieved in bok mayan altitude game."
  [gm socket peek-ref]
  (let [f (cgp/function gm)
        action-fn (fn [info]
                    (let [invals (extract-invals info)
                          outvals (apply f invals)]
                      (assoc info :actions
                             {:joint-torques
                              (zipmap output-joints outvals)
                              :raycast raycast-angles})))]
    (-> (run-one-bout socket action-fn peek-ref)
        :heights
        :player-a
        (or -100))))

(defn run-evo
  [socket peek-ref]
  (let [lang (conj langf/lang [0.0])
        inm (mapv str input-symbols)
        opts {:data-type 'double
              :erc-prob 0.25
              :erc-gen #(* (gen/double) 4.0)}
        fitness (fn [gm]
                  (bok-alti-fitness gm socket peek-ref))
        regen (evo/negative-selection-fn 1 cgp/mutate
                                         nil ;; no crossover
                                         :elitism 1)
        init-popn (repeatedly 20 #(cgp/rand-genome inm 200 (count output-joints)
                                                   lang opts))
        soln (time (evo/simple-evolve init-popn
                                      fitness
                                      regen
                                      {:target 10.0
                                       :n-gens 1000
                                       :progress-every 1}))]
    ;; print out grid of hits/misses
    (let [gm (:best (last (:history soln)))]
      (println "Genome expression:")
      (binding [pp/*print-suppress-namespaces* true]
        (pp/pprint (cgp/genome->expr gm)))
      gm)))

(defn main
  "For REPL use. Pass an `(atom {})` for peeking at the state."
  [port peek-ref]
  (let [ctx (ZMQ/context 1)
        addr (str "tcp://*:" port)]
    (println "starting server on TCP port " port)
    (try
      (with-open [socket (.socket ctx ZMQ/REP)]
        (doto socket
          (.bind addr)
          (.setSendTimeOut 2000))
        (binding [*out* *err*]
          (run-evo socket peek-ref)))
      (finally
        (println "closing ZMQ context")
        (.term ctx)))))

(defn -main
  [& [port more-args]]
  (main port (atom {})))
