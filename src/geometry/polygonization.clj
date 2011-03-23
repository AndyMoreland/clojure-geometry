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
        points [{:x 10 :y 10} {:x 30 :y 20} {:x 100 :y 120} {:x 90 :y 60} {:x 95 :y 19 } {:x 90 :y 90} {:x 25 :y 195} {:x 144 :y 144}]]
    (graph/create-new-pane my-panel)
    (graph/update-drawing my-panel (fn [g]
                               (graph/draw-lines g (polygonalize-points points))
                               (graph/draw-points g points)))))
