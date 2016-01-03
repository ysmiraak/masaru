(ns masaru.trial-111
  (:use masaru.core))

(def P
  [ 0
   [:S :S :S]
   [:S 1]])

(def STATES
  [{ 1 #{1}
    :S 2}
   { 1 #{(P 2)}
    :$ #{(P 2)}}
   { 1 #{1}
    :$ #{(P 0)}
    :S 3}
   { 1 #{1 (P 1)}
    :$ #{(P 1)}
    :S 3}])

(defn sum-into
  ([s] #{s})
  ([s vl]
   (-> #(set (for [n %1 n' %2] (+ n n')))
       (reduce (map #(-> % meta :res) vl)))))

(use 'masaru.core :reload-all)

(parse-forest-as-sexp STATES [1 1 1])
;; [(:S (:S (:S 1) (:S 1)) (:S 1))
;;  (:S (:S 1) (:S (:S 1) (:S 1)))]

(parse-for-result STATES sum-into [1])       ; #{1}
(parse-for-result STATES sum-into [1 1])     ; #{2}
(parse-for-result STATES sum-into [1 1 1])   ; #{3}
(parse-for-result STATES sum-into [1 1 1 1]) ; #{4 5} ???

(parse-forest-as-sexp STATES [1 1 1 1]) ; ???
;; [(:S [(:S (:S (:S 1) (:S 1)) (:S 1))
;;       (:S (:S 1) (:S (:S 1) (:S 1)))] (:S 1))
;;  (:S (:S (:S 1) (:S 1)) (:S [(:S 1)
;;                              (:S (:S 1) (:S 1))] (:S 1)))
;;  (:S (:S 1) (:S (:S 1) (:S (:S 1) (:S 1))))]

(map #(number-of-parses STATES (repeat % 1)) (range 11))
;; (0 1 1 2 5 18 103 658 5429 58150 937895)
