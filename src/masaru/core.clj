(ns masaru.core)

(defrecord Vertex [symbol data? prevmap])

(defn consume
  "Automaton A consumes symbol S at vertex V, disposing by D."
  ([A S V] (consume A S V nil))
  ([A S V D]
   (letfn [(process [s v]
             (->> (dissoc v :res) keys
                  (map #((A %) s))
                  (reduce into)
                  ;; returns the set of all possible actions at vertex v with symbol s
                  (map (partial act v))
                  join))
           
           (get-predecs [v] (reduce into (vals (dissoc v :res))))

           (act [v a]
             ;; perform action a (either a number or a vector) at vertex v
             (if (number? a) ; a number represents the state for a shift operation
               {a #{v} :res (D S)}      ; a vector is a reduction rule
               (loop [r (pop a) dvs [(list v)]]
                 (if (= 1 (count r))
                   (->> dvs
                        (mapcat
                         (fn [dv]
                           (->> (first dv)
                                get-predecs
                                (map (fn [v]
                                       (assoc
                                        (->> (dissoc v :res) keys
                                             (map #((A %) (peek r)))
                                             (reduce #(assoc %1 %2 #{v}) {}))
                                        :res (D (peek r) dv)))))))
                        (map (partial process S))
                        join
                        ;; (map (fn [v]
                        ;;        (let [as (->> (dissoc v :res) keys
                        ;;                      (map #((A %) S))
                        ;;                      (reduce into)
                        ;;                      (filter vector?))]
                        ;;          (if (empty? as)
                        ;;            v
                        ;;            (join (map (partial act v) as))))))
                        )
                   (recur (pop r)
                          (for [dv dvs p (get-predecs (first dv))]
                            (conj dv p)))))))
           (join [ms] ; merge vertices creaed through all courses of actions
             (case (count ms) 0 nil 1 (first ms)
                   (apply merge-with into ms)))]
     (process S V))))

;; NOTE on M: It must accept a list of vertices as arguments,
;; and return a set. E.g. (fn do-nothing [dv] #{})

(defn parse
  "Let states consume string with method. Returns the final vertex
  whose predecs are the possible parses, or nil if no parse found."
  [states disposition string]
  (loop [v {0 nil} string string]
    (when-not (nil? v)
      (if (empty? string)
        (consume states :$ v disposition)
        (recur (consume states (first string) v disposition)
               (rest string))))))

(defn parse-forest
  "Let states consumes string for building a parse forest."
  [states string]
  (letfn [(splice [v] (if (= 1 (count v)) (first v) v))
          (as-sexp
            ([s] [s])
            ([s vs]
             [(conj (map splice (map :res vs)) s)]))]
    (parse states as-sexp string)))

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
