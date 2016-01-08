(ns masaru.trial-111
  (:use masaru.core))

(def P
  [ 0
   [:S :S :S]
   [:S 1]])

(def STATES
  [{ 1 1
    :S 2}
   { 1 (P 2)
    :$ (P 2)}
   { 1 1
    :$ (P 0)
    :S 3}
   { 1 #{1 (P 1)}
    :$ (P 1)
    :S 3}])

(defn sum-into
  ([s] #{s})
  ([s vs]
   (->> vs (map meta) (map :res)
        (reduce #(set (for [n %1 n' %2] (+ n n')))))))

(map #(parse-for-result STATES sum-into (repeat % 1)) (range 11))
;; (nil #{1} #{2} #{3} #{4} #{5} #{6} #{7} #{8} #{9} #{10})

(map #(number-of-parses STATES (repeat % 1)) (range 11))
;; (0 1 1 2 5 14 42 132 429 1430 4862)

(parse-forest-as-sexp STATES [1 1 1])
;; [(:S (:S (:S 1) (:S 1)) (:S 1))
;;  (:S (:S 1) (:S (:S 1) (:S 1)))]

(parse-forest-as-sexp STATES [1 1 1 1])

[(:S (:S 1) (:S (:S 1) (:S (:S 1) (:S 1))))
 (:S (:S (:S 1) (:S 1)) (:S [(:S 1)
                             (:S (:S 1) (:S 1))] (:S 1)))
 (:S [(:S (:S (:S 1) (:S 1)) (:S 1))
      (:S (:S 1) (:S (:S 1) (:S 1)))] (:S 1))]

;; [(:S (:S 1) (:S (:S 1) (:S (:S 1) (:S 1))))
;;  (:S (:S (:S 1) (:S 1)) (:S (:S 1) (:S 1)))
;;  (:S [(:S (:S (:S 1) (:S 1)) (:S 1))
;;       (:S (:S 1) (:S (:S 1) (:S 1)))] (:S 1))
;;  (:S (:S 1) (:S (:S (:S 1) (:S 1)) (:S 1)))]

(time (number-of-parses STATES (repeat 11 1)))



(use 'masaru.core :reload-all)
(parse-for-result STATES sum-into [1 1 1 1])

(consume STATES 1 (consume STATES 1 (consume STATES 1 {0 nil})))
{1 #{{3 {2 {0 nil}}}
     {2 {0 nil}}}} ; correct
(consume STATES 1 (consume STATES 1 (consume STATES 1 (consume STATES 1 {0 nil}))))
{1 #{{3 {3 {2 {0 nil}}}} ; incorrect
     {3 {2 {0 nil}}}
     {2 {0 nil}}}}
(consume STATES 1 (consume STATES 1 (consume STATES 1 (consume STATES 1 (consume STATES 1 {0 nil})))))
{1 #{{3 {3 {2 {0 nil}}}} ; incorrect
     {3 {3 {3 {2 {0 nil}}}}} ; incorrect
     {3 {2 {0 nil}}} ; incorrect
     {2 {0 nil}}}}
