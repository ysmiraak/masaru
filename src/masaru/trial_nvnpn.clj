(ns masaru.trial-nvnpn
  (:use masaru.core :reload-all))

(def P
  [ 0         ; accept
   [:S :N :V] ; 1. S -> N V
   [:V :V :P] ; 2. V -> V P
   [:V \v :N] ; 3. V -> v N
   [:N :N :P] ; 4. N -> N P
   [:N \n]    ; 5. N -> n
   [:P \p :N] ; 6. P -> p N
   ])

(def STATES
  [ ;;0
   {\n #{2}
    :S 1
    :N 3}
   ;; 1
   {:$ #{(P 0)}}
   ;; 2
   {\v #{(P 5)}
    \p #{(P 5)}
    :$ #{(P 5)}}
   ;; 3
   {\v #{4}
    \p #{5}
    :V 7
    :P 6}
   ;; 4
   {\n #{2}
    :N 8}
   ;; 5
   {\n #{2}
    :N 9}
   ;; 6
   {\v #{(P 4)}
    \p #{(P 4)}
    :$ #{(P 4)}}
   ;; 7
   {\p #{5}
    :$ #{(P 1)}
    :P 10}
   ;; 8
   {\p #{5 (P 3)}
    :$ #{(P 3)}
    :P 6}
   ;; 9
   {\v #{(P 6)}
    \p #{5 (P 6)}
    :$ #{(P 6)}
    :P 6}
   ;; 10
   {\p #{(P 2)}
    :$ #{(P 2)}}])

(parse-forest STATES "nvnpnpn")

{0 #{{1 #{{0 nil}}, :res [(:S (:N \n) (:V \v (:N (:N \n) (:P [\p \p] (:N (:N \n) (:P [\p \p \p \p] (:N \n)))))))]}
     {1 #{{0 nil}}, :res [(:S (:N \n) (:V (:V \v (:N \n)) (:P [\p \p] (:N (:N \n) (:P [\p \p \p \p] (:N \n))))))]}
     {1 #{{0 nil}}, :res [(:S (:N \n) (:V (:V (:V \v (:N \n)) (:P [\p \p] (:N \n))) (:P [\p \p \p \p] (:N \n))))]}
     {1 #{{0 nil}}, :res [(:S (:N \n) (:V \v (:N (:N (:N \n) (:P [\p \p] (:N \n))) (:P [\p \p \p \p] (:N \n)))))]}
     {1 #{{0 nil}}, :res [(:S (:N \n) (:V (:V \v (:N (:N \n) (:P [\p \p] (:N \n)))) (:P [\p \p \p \p] (:N \n))))]}}, :res [:$ :$ :$ :$ :$]}
