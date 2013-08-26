(ns ConstructiveMathematics.real-numbers)

(require '[ConstructiveMathematics.natural-numbers :as natural-numbers])
(require '[ConstructiveMathematics.integers :as integers])
(require '[ConstructiveMathematics.rational-numbers :as rational-numbers])

(defn real [f g] {:f f, :g g})
(defn f-g [r] [(:f r) (:g r)])

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

(def TOLERANCE 
  (let [ten (integers/add integers/one (integers/multiply integers/three integers/three))
        one-half (rational-numbers/rational integers/one integers/two)]
    (rational-numbers/power one-half ten)))

(defn compare-with-tolerance [tolerance r1 r2]
  (let [[f1 g1] (f-g r1)
        [f2 g2] (f-g r2)
        ns (->> natural-numbers/all-naturals
             (filter #(rational-numbers/less-than 
                        (rational-numbers/multiply tolerance %) 
                        rational-numbers/one))
             (map #([% (rational-numbers/rational integers/one %)])))
        [n n-inverse] (first ns)
        n1 (g1 n) ; if n' and n'' >= N1 then | (f1 n) - (f1 n') | < tolerance
        n2 (g2 n) ; if n' and n'' >= N2 then | (f2 n) - (f2 n') | < tolerance
        q1 (f1 n1) ; q1 is approximately r1
        q2 (f2 n2) ; q2 is approximately r2
        max1 (rational-numbers/add q1 tolerance)
        min1 (rational-numbers/subtract q1 tolerance)
        max2 (rational-numbers/add q2 tolerance)
        min2 (rational-numbers/subtract q2 tolerance)]
    (cond
      (rational-numbers/less-than tolerance (rational-numbers/subtract min2 max1)) :less-than
      (rational-numbers/less-than tolerance (rational-numbers/subtract min1 max2)) :greater-than
      :else :equal)))

(defn compare [r1 r2] (compare-with-tolerance TOLERANCE r1 r2))

(defn equal-to [r1 r2] (= :equal (compare [r1 r2])))

(defn negate [r]
  (let [[f g] (f-g r)
        negative-f (fn [n] (rational-numbers/negate (f n)))]
    (real negative-f g)))

(defn add [r1 r2]
  (let [[f1 g1] (f-g r1)
        [f2 g2] (f-g r2)
        f (fn [n] (rational-numbers/add (f1 n) (f2 n)))
        g (fn [n]
            (let [two-n (integers/multiply integers/two n)]
              (integers/max (g1 two-n) (g2 two-n))))]
    (real f g)))

(defn subtract [r1 r2]
  (add r1 (negate r2)))

(defn multiply [r1 r2]
  (let [[f1 g1] (f-g r1)
        [f2 g2] (f-g r2)
        f (fn [n] (rational-numbers/multiply (f1 n) (f2 n)))
        g1-of-1 (g1 integers/one)
        g2-of-1 (g2 integers/one)
        g-of-1 (integers/max g1-of-1 g2-of-1)
        f1-bound (rational-numbers/add (rational-numbers/absolute-value (f1 g1-of-1)) rational-numbers/one)
        f2-bound (rational-numbers/add (rational-numbers/absolute-value (f1 g2-of-1)) rational-numbers/one)
        k (rational-numbers/round-up (rational-numbers/max f1-bound f2-bound))
        g (fn [n]
            (let [N (reduce integers/multiply [integers/two n k])
                  g1-of-N (g1 N)
                  g2-of-N (g2 N)]
              (reduce integers/max [g1-of-1 g2-of-1 g1-of-N g2-of-N])))]
    (real f g)))

(defn try-invert-with-tolerance [tolerance r]
  (if (not= :equal (compare-with-tolerance tolerance r zero))
    (let [[f g] (f-g r)
          n1 (-> tolerance rational-numbers/invert rational-numbers/round-up integers/square g) 
          f-inverted (fn [n]
                       (let [f-of-n (f n)]
                         (if (rational-numbers/equal-to rational-numbers/zero f-of-n)
                           rational-numbers/one
                           (rational-numbers/invert f-of-n))))
          g-inverted (fn [n] (integers/max (g n) n1))]
      (real f-inverted g-inverted))))

(defn try-invert [r] (try-invert-with-tolerance TOLERANCE r))

(defn invert [r] (or (try-invert r) (throw (Exception. "unable to invert a number so close to zero"))))

(defn try-divide [r1 r2]
  (let [r2-inverted (try-invert r2)]
    (and r2-inverted (multiply (r1 r2-inverted)))))

(defn divide [r1 r2] (or (try-divide [r1 r2]) (throw (Exception. "unable to divide by a number so close to zero"))))

(def two (from-rational (rational-numbers/rational integers/two integers/one)))

(def square-root-of-two 
  ; use newton's method
  (let [two (rational-numbers/from-integer integers/two)
        sq (fn [x] (rational-numbers/subtract (rational-numbers/multiply x x) two))
        sq' (fn [x] (rational-numbers/add x x))
        iterate (fn [guess] (rational-numbers/subtract guess (rational-numbers/divide (sq guess) (sq' guess))))
        f (fn f [n] (if (integers/equal-to integers/one n) 
                      rational-numbers/one
                      (iterate (f (integers/predecessor-of n)))))
        g (fn [n] (rational-numbers/rational integers/one n))]
    (real f g)))


                    
                    