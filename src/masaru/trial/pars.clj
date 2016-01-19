(ns masaru.trial.pars
  (:use masaru.core))

(def STATES
  (let [P [0
           [:S]
           [:S :S \( :S \)]]]
    {0 {\( (P 1)
        \) (P 1)
        :$ (P 1)
        :S 1}
     1 {\( 2
        :$ (P 0)}
     2 {\( (P 1)
        \) (P 1)
        :$ (P 1)
        :S 3}
     3 {\( 2
        \) 4}
     4 {\( (P 2)
        \) (P 2)
        :$ (P 2)}}))

(def STATES'
  (let [P [0
           [:S]
           [:S \( :S \) :S]]]
    {0 {\( 2
        \) (P 1)
        :$ (P 1)
        :S 1}
     1 {:$ (P 0)}
     2 {\( 2
        \) (P 1)
        :$ (P 1)
        :S 3}
     3 {\) 4}
     4 {\( 2
        \) (P 1)
        :$ (P 1)
        :S 5}
     5 {\) (P 2)
        :$ (P 2)}}))

(defn pairs
  "Generates all possible matchings of n pairs of parentheses."
  [n]
  (if (zero? n)
    [""]
    (for [m (range n)
          p (pairs m)
          q (pairs (dec (- n m)))]
      (str \( p \) q))))

(map #(count (pairs %)) (range 11))
;; (1 1 2 5 14 42 132 429 1430 4862 16796)
;; https://en.wikipedia.org/wiki/Catalan_number

(-> (partial number-of-parses STATES)
    (map (pairs 8))
    set)                                ; #{1}
(-> (partial number-of-parses STATES')
    (map (pairs 8))
    set)

(defn count-pairs
  ([s] 1/2)
  ([s vs]
   (if (empty? vs) 0      ; this is when the ϵ-rule (P 1) gets applied
       (transduce (comp (map meta) (map :res)) + vs))))

(map (partial parse-for-result STATES count-pairs)
     (map #(apply str (repeat % "()")) (range 11)))
;; (0 1N 2N 3N 4N 5N 6N 7N 8N 9N 10N)
(map (partial parse-for-result STATES' count-pairs)
     (map #(apply str (repeat % "()")) (range 11)))



(defn pairs*3
  "Generates all possible matchings from n pairs of any of the three
  types of brackets: round, square, or curly."
  [n]
  (if (zero? n)
    [""]
    (->> [(str p \( q \))
          (str p \[ q \])
          (str p \{ q \})]
         (for [m (range n)
               p (pairs*3 m)
               q (pairs*3 (dec (- n m)))])
         (apply concat))))

(def STATES*3
  (let [P {1 [:S]
           2 [:S :S \( :S \)]
           3 [:S :S \[ :S \]]
           4 [:S :S \{ :S \}]}
        V #{\( \) \[ \] \{ \} :$}
        opening-s {\( 1 \[ 2 \{ 3}
        all-reduce-by-rule (fn [n] (reduce #(assoc %1 %2 (P n)) {} V))
        epsilon-r (all-reduce-by-rule 1)]
    [(assoc epsilon-r :S 10)
     (assoc epsilon-r :S 7)
     (assoc epsilon-r :S 8)
     (assoc epsilon-r :S 9)
     (all-reduce-by-rule 2)
     (all-reduce-by-rule 3)
     (all-reduce-by-rule 4)
     (assoc opening-s \) 4)
     (assoc opening-s \] 5)
     (assoc opening-s \} 6)
     (assoc opening-s :$ 0)]))

(map #(count (pairs*3 %)) (range 8))
;; (1 3 18 135 1134 10206 96228 938223)
;; https://oeis.org/

;; TODO: function for counting different kinds of brackets
