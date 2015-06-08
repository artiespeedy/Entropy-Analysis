(ns entropy-analysis.input
  (:require [clojure.string :as s]))

(defn load-to-list "Splits by spaces and new lines"
  [filename]
  (let [txt (slurp filename)]
    (s/split txt #"\s")))

(spit "hi" "hello you")
(def sula (load-to-list "SampleEssay1"))
(def sulamac (markov-percents sula))
(iter-state sulamac "Sula" (take 50 (repeatedly #(rand-int 100))))

