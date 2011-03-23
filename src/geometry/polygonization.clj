(ns geometry.polygonization (:require [geometry.graphics :as graph]))


(defn sort-points-by-angle [ref-point points]
  (sort-by (fn [[x y]] (Math/atan (float (/ (- y (:y ref-point)) (- x (:x ref-point)))))) points))

(defn polygonalize-points [points]
  (let [points-by-x (reverse (sort-by :x points))
        rightmost-point (first points-by-x)
        rightmost-x (:x rightmost-point)
        rightmost-y (:y rightmost-point)
        points-by-angle (cons (cons
                               rightmost-point
                               (sort-points-by-angle {:x rightmost-x :y rightmost-y} (rest points)))
                              rightmost-point)]
    (partition 4 2 (flatten points-by-angle))))


(defn -main []
  (let [my-panel (graph/panel)
        points [[10 10] [30 20] [100 120] [90 60] [95 19 ] [90 90] [25 195] [144 144]]]
    (graph/create-new-pane my-panel)
    (graph/update-drawing my-panel (fn [g]
                               (graph/draw-lines g (polygonalize-points points))
                               (graph/draw-points g points)))))
