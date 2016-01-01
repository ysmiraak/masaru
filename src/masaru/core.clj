(ns masaru.core)

(defn consume
  "Automaton A consumes symbol S at vertex V, disposing by D.

  A must be a vector or a map with state numbers as keys and maps from
  symbols to operations as values, operations must be a single
  number (the goto state) when the keys are non-terminal symbols, and
  when the keys are the terminals, they can be either a number (the
  shift state) or a vector (the reduction rule) or a set of these
  things (any number of vectors, at most one number, and can be empty
  as well).

  S is a terminal symbol. When it is successfully consumed, the vertex
  returned represents S, and can be used as V in the next application.
  If S cannot be parsed, then nil is returned.

  V is a map from states to sets of predecessor vertices. The starting
  pseudo-vertex for example can be {0 nil} or {0 #{}}. When a D is
  supplied, the results of reductions are stored as metadata with :res
  as the key.

  D must be (when supplied) a function that can take a terminal
  symbol, or a non-terminal symbol and a list of vertices. The unary
  case will be applied during shift operation, and the binary case
  will be applied during reduction operation, when the list of
  vertices are to be reduce to the non-terminal symbol. The return
  value must be wrapped in a collection, so that when ambiguities
  occur, the 'into function can be applied to these collections."
  ([A S V] (consume A S V nil))
  ([A S V D]
   (letfn [(process [s v]
             (->> (stage s v)
                  (reduce #(if (set? %2) (into %1 %2) (conj %1 %2)) #{})
                  (map (partial act v))
                  join))
           (act [v a]
             (if (number? a)
               (if (nil? D) {a #{v}} ^{:res (D S)} {a #{v}})
               (loop [a (pop a) vls [(list v)]]
                 (if (= 1 (count a))
                   (->> vls
                        (mapcat (partial redus (peek a)))
                        (map (partial process S))
                        join)
                   (recur (pop a)
                          (for [vl vls p (->> vl first vals (reduce into))]
                            (conj vl p)))))))
           (redus [s vl]
             (->> vl first vals
                  (reduce into)
                  (map (if (nil? D)
                         #(goto s %)
                         #(with-meta (goto s %) {:res (D s vl)})))))
           (join [ms]
             (case (count ms) 0 nil 1 (first ms)
                   (apply merge-with (if (nil? D) into fuse) ms)))
           (fuse [s s']
             (-> (fn [s v]
                   (let [v' (some #(when (= % v) %) s)]
                     (if (nil? v')
                       (conj s v)
                       (->> (merge-with into (meta v) (meta v'))
                            (with-meta v)
                            (conj (disj s v'))))))
                 (reduce s s')))
           (goto [s v] (reduce #(assoc %1 %2 #{v}) {} (stage s v)))
           (stage [s v] (->> v keys (map #((A %) s)) (remove nil?)))]
     (process S V))))

(defn parse
  "Let states consume string by disposition. Returns the final vertex
  whose predecs are the possible parses, or nil if no parse found."
  [states disposition string]
  (loop [v {0 nil} string string]
    (when-not (nil? v)
      (if (empty? string)
        (consume states :$ v disposition)
        (recur (consume states (first string) v disposition)
               (rest string))))))

(defn parse-for-result
  "Let states consume string and return the final result produced by
  disposition, or nil if parse fails."
  [states disposition string]
  (when-let [v (parse states disposition string)]
    (->> (v 0) (map meta) (map :res) (reduce into))))

(defn parse-forest-as-sexp
  "Returns the parse forest in s-expression, where the or-nodes are
  represented as vectors."
  [states string]
  (letfn [(splice [v] (case (count v) 0 nil 1 (first v) v))
          (as-sexp ([s] [s])
            ([s vl] [(conj (map #(-> % meta :res splice) vl) s)]))]
    (splice (parse-for-result states as-sexp string))))

(defn number-of-parses
  "Return the number of different possible parses."
  [states string]
  (letfn [(nop ([s] [1])
            ([s vl] [(reduce * (map #(->> % meta :res (reduce +)) vl))]))]
    (reduce + (parse-for-result states nop string))))
