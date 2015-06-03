(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
    false
   (= elem (first a-seq))
   true
   :else
   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
    a-seq
   (pred? (first a-seq))
   (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
   '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
    a-seq
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else
   a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not= (count a-seq) (count b-seq)) false
   (and (= (count a-seq) 0) (= (count b-seq) 0)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else
   false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (nil? (first seq-1)) (nil? (first seq-2))) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) ()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons [] a-seq)
   :else (cons (vec a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
   (empty? a-seq) (cons [] a-seq)
   :else (cons a-seq (inits (vec (reverse (rest (reverse a-seq))))))))

(defn rotations-helper [a-seq counter]
  (if (= counter 0)
    []
    (let [new-seq (concat (rest a-seq) [(first a-seq)])]
      (cons new-seq (rotations-helper new-seq (dec counter))))))

(defn rotations [a-seq]
    (if (empty? a-seq)
      (list ())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [value (first a-seq)
          sum (if (contains? freqs value) (inc (get freqs value)) 1)]
      (my-frequencies-helper (assoc freqs value sum) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[string times] (first a-map)]
      (concat (repeat times string) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
     coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [elements (count a-seq)
        half (int (/ elements 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if (> (first a-seq) (first b-seq))
           (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
           (cons (first a-seq) (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (let [[first-half, second-half] (halve a-seq)
        elements (count a-seq)]
      (if (or (= (count a-seq) 0) (= (count a-seq) 1))
    a-seq
    (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

