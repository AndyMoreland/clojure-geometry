(ns geometry.graphics (:import (javax.swing JFrame JPanel)))

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