(def markov-chain {:state1 [[0 25 :state2] [25 50 :state3]] 
                   :state2 [[50 100 :state1] [0 25 :state3]] 
                   :state3 [[0 25 :state1] [25 50 :state2]]})

; Rand-val = 0-99 (inclusive on both 0 and 99
(defn next-state
  [machine cstate-key rand-val]
  ; Get all possible next states
  (let [next-states (get machine cstate-key)]
    (loop [possible-states next-states]
      (if (> (count possible-states) 0)
        (let [state (first possible-states)]
          ; Find a value that fits the range
          (if (and (> rand-val (first state)) (< rand-val (first (rest state))))
            (last state)
            (recur (rest possible-states))))
        ; If there was no range that fit the value, return the original key
        cstate-key))))

(defn iter-state
  [machine cstate-key rand-vals]
  (loop [ckey cstate-key ivals rand-vals log []]
    (if (> (count ivals) 0)
     (recur (next-state machine ckey (first ivals)) (rest ivals) (conj log ckey))
     (conj log ckey))))

; Examples
; (iter-state markov-chain :state1 (take 10 (repeatedly #(rand-int 100))))
; (next-state markov-chain :state1 72)
