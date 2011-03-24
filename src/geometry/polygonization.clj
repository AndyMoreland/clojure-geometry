(ns geometry.polygonization (:require [geometry.graphics :as graph]))


(defn sort-points-by-angle [ref-point points]
  (sort-by (fn [{x :x y :y}] (Math/atan (float (/ (- y (:y ref-point)) (- x (:x ref-point)))))) points))

(defn polygonalize-points [points]
  (let [points-by-x (reverse (sort-by :x points))
        rightmost-point (first points-by-x)
        rightmost-x (:x rightmost-point)
        rightmost-y (:y rightmost-point)
        points-by-angle [rightmost-point (sort-points-by-angle {:x rightmost-x :y rightmost-y} (rest points-by-x)) rightmost-point]]
    (partition 2 1 (flatten points-by-angle))))


(defn -main []
  (let [my-panel (graph/panel)
        points (take 15 (graph/random-points))]
    (graph/create-new-pane my-panel)
    (graph/update-drawing my-panel (fn [g]
                               (graph/draw-lines g (polygonalize-points points))
                               (graph/draw-points g points)))))
