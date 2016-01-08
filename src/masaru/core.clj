(ns masaru.core)

(defn- collect
  "Collects all the entities in args into collection in. Each form in
  args can either be a single entity or a set of entities."
  [in & args]
  (reduce #((if (set? %2) into conj) %1 %2) in args))

(defn- fuse
  "Adds the state-vertex pair [a p] (a predecessor) to the current
  vertex v aka the Tomita stack top."
  [v [a p]]
  (letfn [(up-meta [v v']
            (->> v' meta :res
                 (update (meta v) :res into)
                 (with-meta v)))
          (join [vs v]
            (let [v' (vs v)]
              (if (nil? v')        (conj vs v)
                  (-> vs (disj v') (conj (up-meta v v'))))))
          (merge-or-group [v v']
            (if (= v v') (up-meta v v') #{v v'}))]
    (condp #(%1 %2) (v a)
      nil? (assoc v a p)
      set? (update v a join p)
      (update v a merge-or-group p))))

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
  If S cannot be parsed, then nil is returned.

  V is a map from state numbers to predecessor vertices or sets of
  predecessors. The starting pseudo-vertex can be {0 nil}. The results
  of reductions produced by D are stored with key :res in the metadata
  of the vertices.

  D must be a function that takes a terminal symbol, or a non-terminal
  symbol and a list of vertices. The unary case will be applied during
  shift operation, and the binary case will be applied during
  reduction operation, when the list of vertices are to be reduce to
  the non-terminal symbol. The return value must be wrapped in a
  collection, so that when ambiguities occur, the 'into function can
  be applied to these collections. When D is omitted, the result in a
  vertex lists the symbol it represents and its children nodes (if
  any) in the parse tree, which would prevent some reduced and useless
  vertices from being garbage collected."
  ([A S V] (consume A S V (fn ([s] [s]) ([s vs] [(conj vs s)]))))
  ([A S V D]
   (letfn [(process [v] (map (partial act v) (stage S v)))
           (act [v a]
             (if (number? a)
               [a v]
               (redus (pop a) (list v))))
           (redus [r vs]
             (let [ps (->> vs first vals (apply collect []))]
               (condp = 1
                 (count r) (map (partial goto (peek r) vs) ps)
                 (count ps) (recur (pop r) (apply conj vs ps))
                 (map #(redus (pop r) (conj vs %)) ps))))
           (goto [s vs p]
             (as-> (stage s p) $
               (interleave $ (repeat p))
               (apply hash-map $)
               (with-meta $ {:res (D s vs)}) ; this step is wrong
               (process $)))
           (stage [s v]
             (->> v keys ; replace by transducer
                  (map #((A %) s)) (remove nil?) (apply collect #{})))]
     (let [vs (flatten (process V))
           xf (comp (remove nil?) (partition-all 2))]
       (transduce xf (completing fuse) ^{:res (D S)} {} vs)))))

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
  (if (parse states (fn [& args] nil) string) true false))

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
