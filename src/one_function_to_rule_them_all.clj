(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (apply reduce concat a-seq []))


(defn str-cat [a-seq]
  (if (empty? a-seq) "" (reduce (fn [s1 s2] (str s1 " " s2)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq) '() (reduce (fn [a b] (conj a x b)) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [c _] (inc c)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [s a] (cons a s)) '() a-seq))


(defn min-max-element [a-seq]
  (if (empty? a-seq) []
    (reduce (fn [[m M] v](if (< v m) [v M] (if (> v M) [m v] [m M])))  [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (cond
    (empty? sorted-seq) (cons n sorted-seq)
    (> n (first sorted-seq)) (cons (first sorted-seq) (insert (rest sorted-seq) n))
    :else (cons n sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert '() a-seq))


(defn parity [a-seq]
  (reduce (fn [s e] (if (contains? s e ) (disj s e) (conj s e))) #{} a-seq))

(defn minus
  ([n] (- n))
  ([n1 n2] (- n1 n2)))

(defn count-params [& p]
  (count p))


(defn my-* [& x]
  (reduce (fn [r v] (* r v)) 1 x))


(defn pred-and [& p]
  (fn [x] (reduce (fn [b p1] (and b (p1 x))) true p)))


(defn my-map [f & a-seq]
  (if (empty? (first a-seq)) nil
    (cons (apply f (map first a-seq)) (apply my-map f (map rest a-seq)))))


