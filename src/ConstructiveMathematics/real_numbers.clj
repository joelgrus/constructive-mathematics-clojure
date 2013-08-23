(ns ConstructiveMathematics.real-numbers)

(require '[ConstructiveMathematics.natural-numbers :as natural-numbers])
(require '[ConstructiveMathematics.integers :as integers])
(require '[ConstructiveMathematics.rational-numbers :as rational-numbers])

(defn real [f g] {:f f, :g g})

(defn constantly [q] (fn [_] q))
(defn always-integer-one [_] integers/one)

(defn from-rational [q] (real (constantly q) always-integer-one))

(def zero (from-rational rational-numbers/zero))
(def one (from-rational rational-numbers/one))

(def e 
  (let [partial-sum-to (fn [n]
                         (->> (integers/range integers/zero n)
                           (map #(-> % integers/factorial 
                                       rational-numbers/from-integer
                                       rational-numbers/invert))
                           (reduce rational-numbers/add)))]
    (real partial-sum-to (fn [n] n))))

