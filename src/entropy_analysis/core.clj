(ns entropy-analysis.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(refer 'entropy-analysis.markov)
(refer 'entropy-analysis.entropy)
(refer 'entropy-analysis.input)
(refer 'entropy-analysis.pprecision)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 60)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :rgb)
  ; setup function returns initial state. It contains
  ; circle color and position.
  )

(defn update-state [state]
  state)

(defn key-pressed
  [old-state event]
  (println event)
  (if (= (:key-code event) 32)
    (do
      )
    (if (= (:key-code event) 10)
      (do
        (assoc old-state :lines
               (loop [lines (:lines old-state) newlines []]
                 (if (> (count lines) 0)
                   (recur (rest lines) (conj newlines (reduce-precision (first lines) (int (/ (q/width) 10)))))
                   newlines)))
        )
      old-state))
  )

(defn mouse-pressed
  [old-state event]
  old-state)

; Doesn't square root to make it faster. Use for comparing lengths
(defn opti-length
  ([p1 p2]
   (opti-length (:x p1) (:y p1) (:x p2) (:y p2)))
  ([x y x1 y1]
   (+ (* (- x x1) (- x x1)) (* (- y y1) (- y y1)))))

(defn mouse-dragged
  [old-state event]
  ; If the previous point exists
  (if (:p-point old-state)
    ; Filter the data to avoid too many points
    ;(if (> (opti-length event (:p-point old-state)) 100)
      ; (not (< (- (q/frame-count) (:frame (:p-point old-state))) 1))
      (do
        ; Add a line connecting the last important point and this point to the array to render
        (assoc
          (assoc old-state :p-point
                      {:frame (q/frame-count)
                       :x (:x event)
                       :y (:y event)})
          :lines
          (conj (:lines old-state)
                {:x (:x event)
                 :y (:y event)
                 :p-x (:x (:p-point old-state))
                 :p-y (:y (:p-point old-state))})))
          ; Make this the last important point
      ; Else don't change anything
      ;old-state)
    ; This is the first point, record it as important
    (do
      (assoc old-state :p-point
           {:frame (q/frame-count)
            :x (:x event)
            :y (:y event)}))))

(defn mouse-released
  [old-state event]
  (assoc old-state :p-point nil))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 240)
    ; Set circle color.
    (q/fill 255 255 255)
    (loop [lstate (:lines state)]
      (when (> (count lstate) 0)
        (let [cstate (first lstate)]
          (q/line (:p-x cstate) (:p-y cstate) (:x cstate) (:y cstate)))
        (recur (rest lstate)))))

(q/defsketch drawing-program
  :title "Draw"
  :size [500 500]
  ; setup function called only once, during sketch initialization.
  :setup setup
  :mouse-dragged mouse-dragged
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :key-pressed key-pressed
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
