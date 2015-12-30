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

(defn sum [L]
  (letfn [(get-numbers [v]
            (if (number? (:symbol v))
              #{(:symbol v)}
              (:datas v)))
          (add-up [ns ns']
            (set (for [n ns n' ns'] (+ n n'))))
          (sum-up [vs]
            (reduce add-up (map get-numbers vs)))]
    (set (mapcat sum-up L))))

(draw-forest-as-sexp (parse STATES set [1 1 1]))

(parse STATES sum [1 1 1])
