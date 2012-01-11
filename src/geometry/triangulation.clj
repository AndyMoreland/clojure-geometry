(ns geometry.triangulation (:import java.lang.Thread [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit] [java.awt Color]) (:require [geometry.polygonization :as poly] [geometry.graphics :as graph]))

(defn slope [[{x1 :x y1 :y} {x2 :x y2 :y}]]
  (if (not (= 0 (- x2 x1)))
    (/ (- y2 y1) (- x2 x1))
    10000000000))

(defn collinear? [point-pair1 point-pair2]
  (if (= (slope point-pair1) (slope point-pair2) (slope [(first point-pair1) (first point-pair2)]))
    true false))

(defn triangulate-points [points]
  (let [points-by-y (reverse (sort-by :y points))
        polygonized-points (poly/polygonalize-points points)
        top-point (first points-by-y)
        lower-points (rest points-by-y)]
    
    (partition 2 1 (flatten lower-points))))

(defn draw-function [g]
  (let [points (take 15 (graph/random-points))]
    (println "Hello")
    (.setColor g (Color. 0 0 255))
    (graph/draw-lines g (triangulate-points points))
    (graph/draw-points g points)
    (.setColor g (Color. 0 0 0))
    (graph/draw-lines g (poly/polygonalize-points points))))

(defn redraw [g]
  (draw-function g))

(defn -main [my-panel]
  (doto (ScheduledThreadPoolExecutor. 1)
    (.scheduleWithFixedDelay #(graph/update-drawing my-panel (fn [g] (redraw g))) 0 5000 TimeUnit/MILLISECONDS)))
