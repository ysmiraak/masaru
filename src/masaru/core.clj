(ns masaru.core)

(defn- fuse
  "Returns the union of two sets of objects"
  [ms ms']
  (letfn [(join [ms m]
            (let [m' (ms m)]
              (if (nil? m')
                (conj ms m)
                (->> (merge-with into (meta m) (meta m'))
                     (with-meta m)
                     (conj (disj ms m'))))))]
    (reduce join ms ms')))

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
  supplied, the results of reductions are stored with key :res in the
  vertex returned.

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
  ([A S V] (consume A S V (fn [& args] args)))
  ([A S V D]
   (letfn [(process [s v]
             (->> (stage s v)
                  (reduce #(if (set? %2) (into %1 %2) (conj %1 %2)) #{})
                  (mapcat (partial act v))))
           (act [v a]
             (if (number? a)
               [{a #{v}}]
               (redus (pop a) (list v))))
           (redus [r vs]
             (let [ps (->> vs first vals (reduce into))]
               (if (= 1 (count r))
                 (mapcat (partial goto (peek r) vs) ps)
                 (if (= 1 (count ps))
                   (recur (pop r) (conj vs (first ps)))
                   (mapcat #(redus (pop r) (conj vs %)) ps)))))
           (goto [s vs v]
             (->> (stage s v)
                  (reduce #(assoc %1 %2 #{v}) ^{:res (D s vs)} {})
                  (process S)))
           (stage [s v]
             (->> v keys (map #((A %) s)) (remove nil?)))]
     (when-let [v (apply merge-with fuse (process S V))]
       (with-meta v {:res (D S)})))))

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
  "Return the number of different possible parses."
  [states string]
  (letfn [(nop ([s] [1])
            ([s vs] [(reduce * (map #(->> % meta :res (reduce +)) vs))]))]
    (reduce + (parse-for-result states nop string))))
