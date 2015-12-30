(ns masaru.core)

(defrecord Vertex [predecs symbol datas])
(defrecord Predec [state vertex])

(defn consume
  "Automaton A consumes symbol S at vertex V with method M."
  [A S V M]
  (letfn [(process [v] (->> v (get-actions S) (map (partial act v)) join))
          (get-actions [s v] ; get all possible actions at vertex v with symbol s
            (->> v :predecs (map #((A (:state %)) s)) (reduce into)))
          (act [v a] ; perform action a (either a number or a vector) at vertex v
            (if (number? a) ; a number represents the state for a shift operation
              (->Vertex #{(->Predec a v)} S #{}) ; a vector is a reduction rule
              (loop [a (pop a) s (peek a) ps (:predecs v) ds [(list v)]]
                (if (= 1 (count a))
                  (-> #(->> %2 :vertex
                            (get-actions s)
                            (map (fn [goto] (->Predec goto (:vertex %2))))
                            (into %1))
                      (reduce #{} ps)
                      (->Vertex s (M ds))
                      process)
                  (let [vs (filter #(= s (:symbol %)) (map :vertex ps)) a (pop a)]
                    (recur a (peek a)
                           (reduce into (map :predecs vs))
                           (for [v vs d ds] (conj d v))))))))
          (join [vs] ; merge vertices created through all courses of actions
            (case (count vs) 0 nil 1 (first vs)
                  (->Vertex (reduce into (map :predecs vs))
                            S (reduce into (map :datas vs)))))]
    (process V)))

;; NOTE on M: It must accept a list of lists of vertices as argument,
;; and returns a set, such as (fn [L] #{})

(defn parse
  "Let states consume string with method. Returns the final vertex
  whose predecs are the possible parses, or nil if no parse found."
  [states method string]
  (loop [v (->Vertex #{(->Predec 0 nil)} nil #{}) string string]
    (when-not (nil? v)
      (if (empty? string)
        (consume states :$ v method)
        (recur (consume states (first string) v method) (rest string))))))

(defn parse-forest
  "Let states consumes string for building a parse forest."
  [states string]
  (parse states set string))

(defn draw-forest-as-sexp
  "Draw forest from vertex v as s-expression, where the or-nodes are
  represented as vectos."
  [v]
  (if (= :$ (:symbol v))
    (->> v :predecs (map :vertex) (map draw-forest-as-sexp))
    (case (count (:datas v))
      0 (:symbol v)
      1 (conj (apply map draw-forest-as-sexp (:datas v)) (:symbol v))
      (cons (:symbol v) (mapv #(map draw-forest-as-sexp %) (:datas v))))))

(defn draw-forest-as-graph
  "todo"
  [v])

(defn print-in-dot
  "http://sandbox.kidstrythisathome.com/erdos/"
  [DAG]
  (println "digraph g {")
  (doseq [[fro tos] DAG to tos]
    (println (str \" fro \") "->" (str \" to \" \;)))
  (println \}))
