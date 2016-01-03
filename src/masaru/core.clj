(ns masaru.core)

(defn- fuse
  [ms ms']
  (letfn [(join [ms m]
            (let [m' (ms m)]
              (if (nil? m')
                (conj ms m)
                (->> (update (meta m) :res into (:res (meta m')))
                     (with-meta m)
                     (conj (disj ms m'))))))]
    (reduce join ms ms')))

(defn- fuse'
  [ms ms']
  (letfn [(join [ms m]
            (let [m' (some #(when (= (dissoc % :res) (dissoc m :res)) %) ms)]
              (if (nil? m')
                (conj ms m)
                (conj (disj ms m')
                      (update m :res into (:res m'))))))]
    (reduce join ms ms')))

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

  V is a map from state numbers to sets of predecessor vertices. The
  starting pseudo-vertex for example can be {0 nil} or {0 #{}}. The
  results of reductions produced by D are stored with key :res in the
  metadata of the vertices.

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
   (letfn [(process [v]
             (->> (stage S v)
                  (reduce #(if (set? %2) (into %1 %2) (conj %1 %2)) #{})
                  (mapcat (partial act v))))
           (act [v a]
             (if (number? a)
               [{a #{v}}]
               (redus (pop a) (list v))))
           (redus [r vs]
             (let [ps (->> vs first vals (reduce into)
                           (filter map?) ;; fix
                           )]
               (condp = 1
                 (count r) (mapcat (partial goto (peek r) vs) ps)
                 (count ps) (recur (pop r) (apply conj vs ps))
                 (mapcat #(redus (pop r) (conj vs %)) ps))))
           (goto [s vs v]
             (as-> (stage s v) $
               (interleave $ (repeat #{v}))
               (apply hash-map $)
               ;; (with-meta $ {:res (D s vs)})
               (assoc $ :res (D s vs)) ; fix
               (process $)))
           (stage [s v]
             (->> (dissoc v :res) ; fix
                  keys (map #((A %) s)) (remove nil?)))]
     (when-let [v (apply merge-with fuse' (process V))]
       ;; (with-meta v {:res (D S)})
       (assoc v :res (D S)) ; fix
       ))))

(defn parse
  "Let states consume string and dispose. Returns the final vertex
  representing the end symbol :$, whose predecessor at state 0
  represents the start symbol :S. Returns nil if no parse found."
  [states dispose string]
  (loop [v {0 nil} string string]
    (when-not (nil? v)
      (if (empty? string)
        (consume states :$ v dispose)
        (recur (consume states (first string) v dispose)
               (rest string))))))

(defn parse-for-result
  "Let states consume string and dispose. Returns the final result, or
  nil if parse fails."
  [states dispose string]
  (when-let [v (parse states dispose string)]
    (->> 0 v
         ;; (apply meta) :res
         (apply :res) ; fix
         )))

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
