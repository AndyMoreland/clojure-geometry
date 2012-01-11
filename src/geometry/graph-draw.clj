(ns geometry.graph-draw
  (:import java.lang.Thread [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit] [java.awt Color])
  (:require [geometry.graphics :as graph]))


(def radius 40)

(def graph-relations {:A1 [:B1 :A2]
                      :B1 [:A1 :B2 :C1]
                      :C1 [:B1 :C2 :D1]
                      :D1 [:C1 :D2]
                      :A2 [:B2 :A1 :A3]
                      :B2 [:B1 :A2 :C2 :B3]
                      :C2 [:C1 :B2 :D2 :C3]
                      :D2 [:D1 :C2 :D3]
                      :A3 [:B3 :A2]
                      :B3 [:A3 :B2 :C3]
                      :C3 [:C2 :B3 :D3]
                      :D3 [:D2 :C3 :A]
                      :A [:D3 :B :D :E]
                      :B [:A :C :H]
                      :C [:B :D :G]
                      :D [:A :C :F]
                      :E [:A :H :F]
                      :F [:G :E :D]
                      :G [:F :C :H]
                      :H [:E :G :B]})

;; (def graph-relations {:A [:B :D :E]
;;                       :B [:A :C :H]
;;                       :C [:B :D :G]
;;                       :D [:A :C :F]
;;                       :E [:A :H :F]
;;                       :F [:G :E :D]
;;                       :G [:F :C :H]
;;                       :H [:E :G :B]})

;; (def graph-relations {:A [:B]
;;                       :B [:A :C]
;;                       :C [:B :D]
;;                       :D [:C :E]
;;                       :E [:D :F]
;;                       :F [:E :G]
;;                       :G [:F :H]
;;                       :H [:G :A]})

;; (def graph-relations {:A [:B :C]
;;                       :B [:A :D :E]
;;                       :D [:B :H :I]
;;                       :E [:B :J :K]
;;                       :J [:E ]
;;                       :K [:E ]
;;                       :H [:D ]
;;                       :I [:D ]
;;                       :C [:A :F :G]
;;                       :F [:C :X]
;;                       :X [:F :Y :Z]
;;                       :Y [:X]
;;                       :Z [:X]
;;                       :G [:C]})

(defn distance-nodes "Calculate the distance between an ordered pair"
  [[{x1 :x y1 :y} {x2 :x y2 :y}]] 
  (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))))

(defn angle-nodes "Calculate the angle between an ordered pair"
  [[{x1 :x y1 :y} {x2 :x y2 :y}]] 
  (Math/atan2 (- y1 y2) (- x1 x2)))

(defn add-forces "Add two force hashes"
  [{x1 :x y1 :y} {x2 :x y2 :y}] 
  (identity {:x (+ x1 x2) :y (+ y1 y2)}))

(defn assoc-concat "If the key exists, concat, otherwise, assoc"
  [hash key value] 
  (let [value-in-hash (key hash)]
    (if value-in-hash
      (assoc hash key (conj value-in-hash value))
      (assoc hash key (vector value)))))

(defn connections "Return all ordered pairings of connected nodes"
  [graph-relations] 
  (apply concat (map (fn [[root nodes]] (map #(identity [root %]) nodes)) graph-relations)))

(defn edges "Return pairs of positions of nodes in order to draw lines"
  [graph-relations positions] 
  (map (fn [[a b]] (identity [(a positions) (b positions)]))
       (map seq (distinct (map set (connections graph-relations))))))

(defn node-pairs "Retrun all ordered pairings of nodes"
  [graph-relations] 
  (let [nodes (set (keys graph-relations))]
    (apply concat (map (fn [a] (map #(identity [a %]) (disj nodes a))) nodes))))

(defn force-fn "Generic function that takes a function to calculate magnitude of force vector given distance and split it over x and y parts"
  [{init-x :x init-y :y} pair mag-calc direction] 
    (let [distance (distance-nodes pair)
        mag (mag-calc distance)
        angle (angle-nodes pair)]
    (identity {:x (+ init-x (* direction mag (Math/cos angle)))
               :y (+ init-y (* direction mag (Math/sin angle)))})))

(defn calc-cumulative-forces "Generic function to symmetrically apply force to a bunch of pairings"
  [node-pairs positions mag-function direction] 
  (reduce (fn [hash [label pairs]] (assoc hash label (reduce (fn [init pair] (force-fn init pair mag-function direction)) {:x 0 :y 0} pairs)))
          {} (reduce (fn [hash [a b]] (assoc-concat hash a (identity [(a positions) (b positions)]))) 
                     {} (map seq node-pairs))))

(defn update-points "Called repeatedly from the draw function to re-calculate dot positions"
  [positions] 
  (let [repulsive-forces
        (calc-cumulative-forces (node-pairs graph-relations) @positions #(/ 10 %) 1)
        attractive-forces
        (calc-cumulative-forces (connections graph-relations) @positions #(* 0.001 (Math/pow % 2)) -1)
        forces (merge-with
                (fn [{x1 :x y1 :y} {x2 :x y2 :y}] (hash-map :x (+ x1 x2) :y (+ y1 y2))) repulsive-forces attractive-forces)]
    (reset! positions (reduce (fn [hash [label position]]
                             (assoc hash label (add-forces position (label forces))))
                           {} @positions))))

(defn allocate-graph-points "Function called to allocate the initial circular graph layout"
  [graph-relations] 
  (let [size (count graph-relations)
        spacing (/ (* 2 (Math/PI)) size)
        nodes (seq (zipmap (keys graph-relations) (range 0 size)))]
    (reduce (fn [positions [node order]]
              (assoc positions node
                     {:x (+ 300 (* 40 (Math/cos (* order spacing))))
                      :y (+ 300 (* 40 (Math/sin (* order spacing))))}))
            {} nodes)))

(def positions (atom (allocate-graph-points graph-relations)))

(defn draw [g]
  (doall (repeatedly 1 #(update-points positions)))
  (graph/draw-lines g (edges graph-relations @positions))
  (graph/draw-points g (vals @positions)))

(defn redraw [g]
  (draw g))

(def panel (atom (graph/panel)))
(def executor (atom (ScheduledThreadPoolExecutor. 1)))

(defn stop []
  (.shutdown @executor))

(defn restart []
  (.shutdown @executor)
  (reset! executor (ScheduledThreadPoolExecutor. 1))
  (reset! positions (allocate-graph-points graph-relations))
  (doto @executor
    (.scheduleWithFixedDelay #(graph/update-drawing @panel
                                                    (fn [g] (redraw g)))
                             0 10 TimeUnit/MILLISECONDS)))

(defn -main []
  (graph/create-new-pane @panel 600 600)
  (doto @executor
    (.scheduleWithFixedDelay #(graph/update-drawing @panel
                                                    (fn [g] (redraw g)))
                             0 10 TimeUnit/MILLISECONDS)))

