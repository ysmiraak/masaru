(ns masaru.trial.pars
  (:use masaru.core))

(def P
  [ 0
   [:S \( \)]
   [:S :S \( \)]
   [:S \( :S \)]
   [:S \( \) :S]
   [:S :S :S]])

(def STATES
  {0 {\( 3
      :S 1}
   1 {\( 4
      :$ (P 0)
      :S 2}
   2 {\( #{4 (P 5)}
      \) (P 5)
      :$ (P 5)
      :S 2}
   3 {\( 3
      \) 6
      :S 5}
   4 {\( 3
      \) 7
      :S 5}
   5 {\( 4
      \) 9
      :S 2}
   6 {\( #{3 (P 1)}
      \) (P 1)
      :$ (P 1)
      :S 8}
   7 {\( #{3 (P 1) (P 2)}
      \) #{(P 1) (P 2)}
      :$ #{(P 1) (P 2)}
      :S 8}
   8 {\( #{4 (P 4)}
      \) (P 4)
      :$ (P 4)
      :S 2}
   9 {\( (P 3)
      \) (P 3)
      :$ (P 3)}})

(defn par [n]
  (letfn [(insert [v i] (into (into (subvec v 0 i) [\( \)]) (subvec v i)))
          (insertall [v] (for [i (range (inc (count v)))] (insert v i)))]
    (->> (loop [i 0 v [[]]]
           (if (= i n) v
               (recur (inc i) (set (mapcat insertall v)))))
         (map #(apply str %)))))

(filter false? (map (partial parsable? STATES) (par 8))) ; ()

(map #(number-of-parses STATES %) (par 3))
;; (6 60 2 2 1) ??????

(parse-forest-as-sexp STATES "()()")
;; [(:S (:S \( \)) \( \))
;;  (:S (:S \( \)) \( \))
;;  (:S (:S \( \)) (:S \( \)))
;;  (:S (:S \( \)) (:S \( \)))
;;  (:S \( \) (:S \( \)))
;;  (:S \( \) (:S \( \)))] ??????



;; TODO:
;; 1. fix the problem
;; 2. function for counting matching pars
