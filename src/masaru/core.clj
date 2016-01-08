(ns masaru.core)

(defn- collect
  "Collects items into coll. A form can either be a single item or a
  set of items. A single item must not be a set."
  ([coll form] ((if (set? form) into conj) coll form)))

(defn- converge
  "A number of state-vertex pairs converge as one vertex with result."
  [state-vertex-pairs result]
  (letfn [(fuse [v [a p]]
            (condp #(%1 %2) (v a)
              nil? (assoc v a p)
              set? (update v a join p)
              (update v a merge-or-group p)))
          (join [vs v]
            (let [v' (vs v)]
              (if (nil? v')
                (conj vs v)
                (conj (disj vs v')
                      (merge-meta v v')))))
          (merge-or-group [v v']
            (if (= v v') (merge-meta v v') #{v v'}))
          (merge-meta [v v']
            (->> v' meta :res
                 (update (meta v) :res into)
                 (with-meta v)))])
  (reduce fuse ^{:res result} {} state-vertex-pairs))

(defn consume
  "Automaton A consumes symbol S at vertex V, disposing by D.

  A must function this way: ((A state-number) symbol) => operation,
  where the operation is a single number (goto state) with a
  non-terminal symbol, and with a terminal symbol, the operation can
  be either a number (shift state), or a vector (reduction rule), or a
  set of these things (specifically, a set of any number of reduction
  rules but at most one shift state).

  S is a terminal symbol. When it is successfully consumed, the vertex
  returned represents S, and can be used as V in the next application.
  If S cannot be parsed, then an empty vertex is returned.

  V is a map from state numbers to predecessor vertices or sets of
  predecessors. The starting pseudo-vertex can be {0 nil}. The results
  of reductions produced by D are stored with key :res in the metadata
  of the vertices.

  D must be a function that takes a terminal symbol, or a non-terminal
  symbol and a list of vertices. The unary case will be applied during
  shift operation, and the binary case will be applied during reduce
  operation, when the list of vertices are to be reduce to a vertex
  representing the non-terminal symbol. The return value must be
  wrapped in a collection, so that when ambiguities occur, the 'into
  function can be applied to these collections. When D is omitted, the
  result in a vertex lists the symbol it represents and its children
  nodes (if any) in the parse tree, which would prevent some reduced
  and useless vertices from being garbage collected."
  ([A S V] (consume A S V (fn ([s] [s]) ([s vs] [(conj vs s)]))))
  ([A S V D]
   (letfn [(process [v] (mapcat (partial act v) (stage S v)))
           (stage [s v]
             (transduce (comp (map #((A %) s)) (remove nil?))
                        (completing collect) #{} (keys v)))
           (act [v a]
             (if (number? a)      [[a v]]
                 (redus (pop a) (list v))))
           (redus [r vs]
             (let [ps (->> vs first vals (reduce collect []))]
               (condp = 1
                 (count r) (-> (for [p ps g (stage (peek r) p)] [g p])
                               (converge (D (peek r) vs))
                               process)
                 (count ps) (recur (pop r) (apply conj vs ps))
                 (mapcat   #(redus (pop r) (conj vs %)) ps))))]
     (-> V process (converge (D S))))))

(defn parse
  "Let states consume string and dispose. Returns the final vertex
  representing the end symbol :$, whose predecessor at state 0
  represents the start symbol :S. Returns nil if no parse found."
  [states dispose string]
  (loop [v {0 nil} string string]
    (when-not (empty? v)
      (if (empty? string)
        (consume states :$ v dispose)
        (recur (consume states (first string) v dispose)
               (rest string))))))

(defn parse-for-result
  "Let states consume string and dispose. Returns the final result, or
  nil if parse fails."
  [states dispose string]
  (when-let [v (parse states dispose string)]
    (-> 0 v meta :res)))

(defn parsable?
  "Whether any parse exists for the given string."
  [states string]
  ((complement empty?) (parse states (fn [& args] nil) string)))

(defn parse-forest-as-sexp
  "Returns the parse forest in s-expression, where the or-nodes are
  represented as vectors."
  [states string]
  (letfn [(splice [v] (case (count v) 0 nil 1 (first v) v))
          (as-sexp ([s] [s])
            ([s vs] [(conj (map #(-> % meta :res splice) vs) s)]))]
    (splice (parse-for-result states as-sexp string))))

(defn number-of-parses
  "Returns the number of different possible parses."
  [states string]
  (letfn [(nop ([s] [1])
            ([s vs] [(reduce * (map #(->> % meta :res (reduce +)) vs))]))]
    (reduce + (parse-for-result states nop string))))
