(ns entropy-analysis.entropy)
; Data represents a list of any type of data
; Depth represents how the data is analysized in relation to nearby points
(defmacro !=
  [left right]
  (not (= left right)))

; Counts the amount of times a data point shows up in a data set
; Depth is the amount of points per a pair of data evaluated
(defn data-counts
  [data depth]
  (loop [m-data data counts {}]
    (if (> (count m-data) (dec depth))
      (if (get counts (take depth m-data))
        (recur (rest m-data) (assoc counts (take depth m-data) (inc (get counts (take depth m-data)))))
        (recur (rest m-data) (assoc counts (take depth m-data) 1)))
      counts)))

; Assigns a percentage to each data point based on its what percent it accounts for of the total data
; Depth = the amount of points per a pair of data evaluated
(defn data-percents
  [data depth]
  (let [dcount (data-counts data depth)]
    (let [dsum (reduce + (vals dcount))]
      (loop [counts dcount percents {}]
        (if (> (count counts) 0)
          (recur (rest counts) (assoc percents (key (first counts)) (/ (val (first counts)) dsum)))
          percents)))))

; Assigns a percentage to the data points within data points to form the basis for a markov chain
(defn markov-counts
  [data]
  (let [counts (data-counts data 2)]
    ; Insert all of the links and counts into the states
    (loop [states (keys counts) 
           machine {}]
      (if (< 1 (count states))
        (let [k (first (first states))
                              ; Connection
              added-val (conj (conj [] (first (rest (first states)))) 
                              ; Count
                              (get counts (first states)))]
          ; If the state already exists, just add the connection. 
          ; Otherwise, add the state.
          (if (get machine k)
            (recur (rest states)
                   (assoc machine k (conj (get machine k) added-val)))
            (recur (rest states)
                   (assoc machine k (conj [] added-val)))))
        machine))))

; Count the amount of counts in each state, create a percentage for each count
(defn markov-percents
  [data]
  ; ks is an array of the different possible states
  ; mk counts is an array like this:
  ;{:state1 [[:state2 10] [:state3 2]] 
  ; :state2 [[:state1 20] [:state3 5]] 
  ; :state3 [[:state1 15] [:state2 1]]})
  (let [mkcounts (markov-counts data)
        ks (keys mkcounts)]
    (loop [mkpercents {} ks ks]
      (if (< 0 (count ks))
        ; Divide the count by the total and shift it by the last percent
        (let [total (reduce #(+ %1 (first (rest %2))) 0 (get mkcounts (first ks)))
              ; Counts is format [[:state1 10] [:state2 7]]
              counts (get mkcounts (first ks)) 
              percents (loop [counts counts percs [] rangeStart 0]
                         (if (< 0 (count counts))
                           (let [percent (* (/ (first (rest (first counts))) total) 100)]
                             (recur (rest counts) (conj percs (conj [] rangeStart (+ percent rangeStart) (first (first counts)))) (+ percent rangeStart)))
                           percs))]
          (recur (assoc mkpercents (first ks) percents) (rest ks)))
        mkpercents))))


; Examples
; (markov-counts '(1 2 3 3 2 1 4 5 3 2 4 5))
; (def mp (markov-percents '(1 2 3 3 2 1 4 5 3 2 4 5)))
; (next-state mp 2 80)
