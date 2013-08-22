(ns ConstructiveMathematics.test-helpers)

(defn unique-up-to-i [vec i]
  (let [vec-i (vec i)]
    (= 0 (count (->> vec (take i) (filter #(= % vec-i)))))))
