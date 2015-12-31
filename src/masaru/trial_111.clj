(ns masaru.trial-111
  (:use masaru.core :reload-all))

(def P
  [ 0
   [:S :S :S]
   [:S 1]])

(def STATES
  [{ 1 #{1}
    :S #{2}}
   { 1 #{(P 2)}
    :$ #{(P 2)}}
   { 1 #{1}
    :$ #{(P 0)}
    :S #{3}}
   { 1 #{1 (P 1)}
    :$ #{(P 1)}
    :S #{3}}])

(defn sum-into [vs]
  (letfn [(get-numbers [v] (if (empty? (:datas v)) #{(:symbol v)} (:datas v)))
          (sum-up [ns ns'] (set (for [n ns n' ns'] (+ n n'))))]
    (reduce sum-up (map get-numbers vs))))

(draw-forest-as-sexp (parse-forest STATES [1 1 1]))
