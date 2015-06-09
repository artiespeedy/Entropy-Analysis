(ns entropy-analysis.pprecision)

(defn round [n]
  (if (< n 0.5)
    (Math/floor n)
    (Math/ceil n)))

(defn reduce-precision
  [point grid-unit-size]
  {:x (int (round (/ (:x point) grid-unit-size))) 
   :y (int (round (/ (:y point) grid-unit-size)))
   :p-x (int (round (/ (:p-x point) grid-unit-size)))
   :p-y (int (round (/ (:p-y point) grid-unit-size)))
   })

; Find the min and max x and y using an array of points
(defn find-min-max-xy
  [points]
  (let [unzipped (map vector points)]
    {:max-x (last sort (first unzipped))
     :max-y (last sort (last unzipped))
     :min-x (first sort (first unzipped))
     :min-y (first sort (last unzipped))}))
