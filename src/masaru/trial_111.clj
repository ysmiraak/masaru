(ns masaru.trial-111
  (:use masaru.core :reload-all))

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

(defn sum-into [vs]
  (letfn [(get-numbers [v] (if (empty? (:datas v)) #{(:symbol v)} (:datas v)))
          (sum-up [ns ns'] (set (for [n ns n' ns'] (+ n n'))))]
    (reduce sum-up (map get-numbers vs))))

;; (draw-forest-as-sexp (parse-forest STATES [1 1 1]))

(def consume' #(consume STATES %1 %2 (fn [dv] #{dv})))
(def one1 (consume' 1 (->Vertex nil nil {0 #{}})))
(def one2 (consume' 1 one1))
(def one3 (consume' 1 one2))
(def one4 (consume' 1 one3))
(def one$ (consume' :$ one4))
;; (clojure.pprint/pprint one$)
(draw-forest-as-sexp one$)

[(:S (:S (:S 1) (:S 1)) (:S (:S 1) (:S 1)))
 (:S (:S 1) (:S (:S (:S 1) (:S 1)) (:S 1)))
 (:S (:S (:S 1) (:S (:S 1) (:S 1))) (:S 1))
 (:S (:S 1) (:S (:S 1) (:S (:S 1) (:S 1))))
 (:S (:S (:S (:S 1) (:S 1)) (:S 1)) (:S 1))]

[(:S (:S 1) (:S (:S 1) (:S 1)))
 (:S (:S (:S 1) (:S 1)) (:S 1))]
