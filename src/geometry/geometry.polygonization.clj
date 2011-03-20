(ns geometry.polygonization (:import (javax.swing JFrame JPanel)))

(defn panel []
     (proxy [JPanel] [] (paintComponent [g] (proxy-super paintComponent g))))

(defn create-new-pane [content-panel]
  (doto (JFrame.) (.setContentPane content-panel) (.setSize 200 200) (.show)))

(defn update-drawing [content-panel draw-fn]
  (update-proxy content-panel {"paintComponent" (fn [t g] (.clearRect g 0 0 (.getWidth content-panel) (.getHeight content-panel)) (draw-fn g))})
  (.repaint content-panel))

(defn offset-circle [x y r]
  (let [offset (/ r 2)
        adjust-x (- x offset)
        adjust-y (- y offset)]
    [adjust-x adjust-y r r]))

(defn point [x y]
  (offset-circle x y 4))

(defn draw-point [g x y]
  (apply (memfn drawOval x y w h) g (point x y)))

(defn draw-points [g points]
  (doseq [point points] (apply draw-point g point)))

(defn draw-lines [g coord-pairs]
  (doseq [[x1 y1 x2 y2] coord-pairs]
    (.drawLine g x1 y1 x2 y2)))

(defn polygonalize-points [points]
  (let [points-by-x (reverse (sort-by first points))
        rightmost-point (first points-by-x)
        rightmost-x (first rightmost-point)
        rightmost-y (rightmost-point 1)
        points-by-angle (cons (cons
                               rightmost-point
                               (sort-by (fn [[x y]] (Math/atan (float (/ (- y rightmost-y) (- x rightmost-x)))))
                                        (rest points-by-x)))
                              rightmost-point)]
    (partition 4 2 (flatten points-by-angle))))


(defn -main []
  (let [my-panel (panel)
        points [[10 10] [30 20] [100 120] [90 60] [95 19 ] [90 90] [25 195] [144 144]]]
    (create-new-pane my-panel)
    (update-drawing my-panel (fn [g]
                               (draw-lines g (polygonalize-points points))
                               (draw-points g points)))))
