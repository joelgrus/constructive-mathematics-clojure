(ns ConstructiveMathematics.natural-numbers)

(defn successor-of [n] {:predecessor n})

(def one (successor-of nil))
(def two (successor-of one))
(def three (successor-of two))
(def four (successor-of three))
; and so on

(defn compare [n1 n2]
  (let [n1' (:predecessor n1)
        n2' (:predecessor n2)]
  (cond
    (and (nil? n1') (nil? n2')) :equal
    (nil? n1') :less-than
    (nil? n2') :greater-than
    :else (compare n1' n2'))))


; convenience functions
(defn equal-to [n1 n2] (= :equal (compare n1 n2)))
(defn less-than [n1 n2] (= :less-than (compare n1 n2)))
(defn less-than-or-equal-to [n1 n2] (not= :greater-than (compare n1 n2)))

(defn add [n1 n2]
  (let [n1' (:predecessor n1)]
    (if (nil? n1') ; n1 is one
      (successor-of n2)
      (add n1' (successor-of n2)))))


(defn multiply [n1 n2]
  (let [n1' (:predecessor n1)]
    (if (nil? n1') ; n1 is one
      n2
      (add n2 (multiply n1' n2)))))

(defn all-naturals-from [n]
  (lazy-cat n (all-naturals-from (successor-of n))))

(def all-naturals (all-naturals-from one))

(defn try-subtract [n1 n2]
  (let [n1' (:predecessor n1)
        n2' (:predecessor n2)]
    (cond
     (and n1' (nil? n2')) n1'
     (and n1' n2') (try-subtract n1' n2'))))

(defn subtract [n1 n2]
  (let [ts (try-subtract n1 n2)]
    (if (nil? ts) (throw (Exception. "impossible subtraction!")) ts)))