(ns masaru.trial.pars
  (:use masaru.core))

(def P
  [ 0
   [:S]
   [:S :S \( :S \)]])

(def STATES
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
      :$ (P 2)}})

(def P'
  [ 0
   [:S]
   [:S \( :S \) :S]])

(def STATES'
  {0 {\( 2
      \) (P' 1)
      :$ (P' 1)
      :S 1}
   1 {:$ (P' 0)}
   2 {\( 2
      \) (P' 1)
      :$ (P' 1)
      :S 3}
   3 {\) 4}
   4 {\( 2
      \) (P' 1)
      :$ (P' 1)
      :S 5}
   5 {\) (P' 2)
      :$ (P' 2)}})

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
