(ns entropy-analysis.input
  (:require [clojure.string :as s]))

(refer 'entropy-analysis.markov)
(refer 'entropy-analysis.entropy)

(defn load-to-list "Splits by spaces and new lines"
  [filename]
  (let [txt (slurp filename)]
    (s/split txt #"\s")))

(def sula (load-to-list "SampleEssay1"))
(def sulamac (markov-percents sula))
(reduce #(str %1 " " %2) (iter-state sulamac "Sula" (take 50 (repeatedly #(rand-int 100)))))

