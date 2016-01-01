(ns masaru.core)

(defrecord Vertex [symbol data? prevmap])

(defn consume
  "Automaton A consumes symbol S at vertex V with method M."
  [A S V M]
  (letfn [(process [s v]
            (->> v :prevmap keys
                 (map #((A %) s))
                 (reduce into)
                 ;; returns the set of all possible actions at vertex v with symbol s
                 (map (partial act v))
                 join))
          
          (get-predecs [v] (reduce into (vals (:prevmap v))))

          (act [v a]
            ;; perform action a (either a number or a vector) at vertex v
            (if (number? a) ; a number represents the state for a shift operation
              (->Vertex S nil {a #{v}}) ; a vector is a reduction rule
              (loop [a (pop a) dvs [(list v)]]
                (if (= 1 (count a))
                  (->> dvs
                       (mapcat
                        (fn [dv]
                          (->> (first dv)
                               get-predecs
                               (map (fn [v]
                                      (->> v :prevmap keys
                                           (map #((A %) (peek a)))
                                           (reduce #(assoc %1 %2 #{v}) {})
                                           (->Vertex (peek a) (M dv))))))))
                       (map (partial process S))
                       join)
                  (recur (pop a)
                         (for [dv dvs p (get-predecs (first dv))]
                           (conj dv p)))))))
          (join [vs] ; merge vertices created through all courses of actions
            (case (count vs) 0 nil 1 (first vs)
                  (->Vertex (:symbol (first vs))
                            (reduce into (map :datas vs))
                            (apply merge-with into (map :prevmap vs)))))]
    (process S V)))

;; NOTE on M: It must accept a list of vertices as arguments,
;; and return a set. E.g. (fn do-nothing [dv] #{})

(defn parse
  "Let states consume string with method. Returns the final vertex
  whose predecs are the possible parses, or nil if no parse found."
  [states method string]
  (loop [v (->Vertex nil nil {0 #{}}) string string]
    (when-not (nil? v)
      (if (empty? string)
        (consume states :$ v method)
        (recur (consume states (first string) v method) (rest string))))))

(defn parse-forest
  "Let states consumes string for building a parse forest."
  [states string]
  (parse states (fn [dv] #{dv}) string))

(defn draw-forest-as-sexp
  "Draw forest from vertex v as s-expression, where the or-nodes are
  represented as vectos."
  [v]
  (if (= :$ (:symbol v))
    (->> v :prevmap vals (reduce into) (mapv draw-forest-as-sexp))
    (case (count (:data? v))
      0 (:symbol v)
      1 (conj (apply map draw-forest-as-sexp (:data? v)) (:symbol v))
      (mapv #(conj (map draw-forest-as-sexp %) (:symbol v)) (:data? v)))))

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
