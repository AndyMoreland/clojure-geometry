(ns geometry.graphics (:import (javax.swing JFrame JPanel)))

(defn panel []
     (proxy [JPanel] [] (paintComponent [g] (proxy-super paintComponent g))))

(defn create-new-pane [content-panel x y]
  (doto (JFrame.) (.setContentPane content-panel) (.setSize x y) (.show)))

(defn update-drawing [content-panel draw-fn]
  (update-proxy content-panel {"paintComponent" (fn [t g] (.clearRect g 0 0 (.getWidth content-panel) (.getHeight content-panel)) (draw-fn g))})
  (.repaint content-panel))

(defn offset-circle [x y r]
  (let [offset (/ r 2)
        adjust-x (- x offset)
        adjust-y (- y offset)]
    [adjust-x adjust-y r r]))

(defn random-points [x y]
  (repeatedly #(identity {:x (rand-int x) :y (rand-int y)})))

(defn point [x y]
  (offset-circle x y 4))

(defn draw-point [g {x :x y :y}]
  (apply (memfn drawOval x y w h) g (point x y)))

(defn draw-points [g points]
  (doseq [point points] (draw-point g point)))

(defn draw-lines [g coord-pairs]
  (doseq [[{x1 :x y1 :y} {x2 :x y2 :y}] coord-pairs]
    (.drawLine g x1 y1 x2 y2)))
