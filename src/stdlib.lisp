; already defined - def, defmacro, fn, error, cons, first, rest

(defmacro let [args & body]
    `((fn ~(map first args) ~body) ~(map second args)))

(defmacro let* [args & body]
    (cond
        [(empty? args) body]
        [true `(let [~(first args)]
                    ~(list 'let* ~(rest args) body))]))

(defmacro letfn [func-name func-args func-body & action]
    (cons 'let [~func-name ~(cons 'fn func-body)]
        `(~func-name)))

(defmacro defn [func-name func-args & func-body]
    `(def ~func-name ~(cons 'fn func-body)))

(defmacro if [predicate then else]
    `(cond
        [~predicate ~then]
        [true ~else]))

(defn empty? [x]
    (or (= x '())
        (= x [])
        (= x "")))

(defn second [lst]
    (first (rest lst)))

(defn length [lst]
    (letfn go [acc 0, lst lst]
        (cond
            [(empty? lst) acc]
            [true (go (+ 1 acc) (rest lst))])))

(defn init [lst]
    (cond
        [(empty? lst) (error "Empty List")]
        [(empty? (rest lst)) '()]
        [true (cons (first lst) (init (rest lst)))]))

(defn last [lst]
    (cond
        [(empty? lst) (error "Empty List")]
        [(empty? (rest lst)) (first lst)]
        [true (last (rest lst)])))

(defn reduce [f acc lst]
    (cond
        [(empty? lst) acc]
        []))

(defn concat [& lists]
    (let [concat-2-lists (fn [xs ys]
                            (cond
                                [(empty? xs) ys]
                                [(empty? (rest ys)) (cons (first xs) ys)]))]
        (reduce concat-2-lists '() lists)))

(defn list [& args]
    (concat (init args) (last args)))