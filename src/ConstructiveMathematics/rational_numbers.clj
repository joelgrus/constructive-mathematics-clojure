(ns ConstructiveMathematics.rational-numbers)

(require '[ConstructiveMathematics.natural-numbers :as natural-numbers])
(require '[ConstructiveMathematics.integers :as integers])

(defn rational [numerator denominator]
	  (let [gcd (if (integers/equal-to integers/zero denominator)
	              (throw (Exception. "cannot have a zero denominator!"))
	              (integers/gcd numerator denominator))
	        re-sign (if (integers/less-than denominator integers/zero)
	                  integers/negate
	                  (fn [i] i))]
	    {:numerator (integers/divide (re-sign numerator) gcd),
	     :denominator (integers/divide (re-sign denominator) gcd)}))

(defn from-integer [i] (rational i integers/one))

(def zero (from-integer integers/zero))
(def one (from-integer integers/one))

(defn negate [r]
  (rational (integers/negate (:numerator r)) (:denominator r)))

(def minus-one (negate one))

(defn invert [r]
  (rational (:denominator r) (:numerator r)))

(defn in-same-terms [r1 r2]
  (let [{n1 :numerator, d1 :denominator} r1
        {n2 :numerator, d2 :denominator} r2
        common-denominator (integers/multiply d1 d2)
        new-numerator1 (integers/multiply n1 d2)
        new-numerator2 (integers/multiply n2 n1)
        transform (if (integers/less-than integers/zero common-denominator)
                    (fn [i] i)
                    integers/negate)]
    [(transform new-numerator1) 
     (transform new-numerator2)
     (transform common-denominator)]))

(defn equal-to [r1 r2]
  (let [[nn1 nn2 _] (in-same-terms r1 r2)]
    (integers/equal-to nn1 nn2)))

(defn less-than [r1 r2]
  (let [[nn1 nn2 _] (in-same-terms r1 r2)]
    (integers/less-than nn1 nn2)))

(defn max [r1 r2] (if (less-than r1 r2) r2 r1))
(defn min [r1 r2] (if (less-than r1 r2) r1 r2))

(defn absolute-value [r]
  (if (less-than r zero)
    (negate r)
    r))

(defn add [r1 r2]
  (let [[nn1 nn2 cd] (in-same-terms r1 r2)]
    (rational (integers/add nn1 nn2) cd)))

(defn subtract [r1 r2] (add r1 (negate r2)))

(defn distance [r1 r2] (absolute-value (subtract r1 r2)))

(defn multiply [r1 r2]
    (let [{n1 :numerator, d1 :denominator} r1
          {n2 :numerator, d2 :denominator} r2]
      (rational (integers/multiply n1 n2) (integers/multiply d1 d2))))

(defn divide [r1 r2]
  (multiply r1 (invert r2)))

(defn power [r i]
  (case (:sign i)
    :zero one
    :negative (power (invert r) (integers/negate i))
    :positive (multiply r (power r (integers/predecessor-of i)))))

(defn try-to-integer [r] 
  (integers/try-divide (:numerator r) (:denominator r)))

(defn to-integer [r]
  (let [ti (try-to-integer r)]
    (or ti (throw (Exception. "not a rational representation of an integer")))))

(declare round-down)
(defn round-up [r]
  (if (less-than r zero)
    (integers/negate (round-down (negate r)))
    (or (try-to-integer r) (integers/add integers/one (round-down r)))))
(defn round-down [r]
  (if (less-than r zero)
    (integers/negate (round-up (negate r)))
    (or
      (try-to-integer r)
      (let 
        [n (:numerator r)
         d (:denominator r)
         m (integers/modulo n d)]
        (integers/divide (integers/subtract n m) d)))))

(defn all-for-total [n]
  (let [total (integers/positive n)
        pre-total (integers/predecessor-of total)]
    (->> (integers/range integers/one pre-total)
      (map (fn [i] [i (integers/subtract total i)]))
      (filter (fn [[i j]] (integers/equal-to integers/one (integers/gcd i j))))
      (mapcat (fn [[i j]] [(rational i j) (rational (integers/negate i) j)])))))


(defn all-rationals-from [n]
  (concat (all-for-total n) (lazy-seq (all-rationals-from (natural-numbers/successor-of n)))))

(def all-rationals
  (cons zero (all-rationals-from natural-numbers/one)))
         
          