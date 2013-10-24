(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [a-seq seq1
         b-seq seq2]
    (cond
     (and (empty? a-seq)
          (empty? b-seq)) true
     (or (empty? a-seq)
         (empty? b-seq)) false
     (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         b-seq a-seq]
    (cond
     (empty? b-seq) nil
     (pred (first b-seq)) acc
     :else (recur (inc acc) (rest b-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         b-seq a-seq]
    (if (empty? b-seq)
      (/ sum n)
      (recur (+ sum (first b-seq)) (inc n) (rest b-seq)))))

(defn parity [a-seq]
  (loop [b-set #{}
         b-seq a-seq]
    (if (empty? b-seq)
      b-set
      (let [toggle (fn [a-set elem]
                     (if (contains? a-set elem)
                       (disj a-set elem)
                       (conj a-set elem)))]
        (recur (toggle b-set (first b-seq)) (rest b-seq))))))

(defn fast-fibo [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else
   (loop [i 2
          prev 1
          prev-prev 0]
     (if (== i n)
       (+ prev prev-prev)
       (recur (inc i) (+ prev prev-prev) prev)))))

(defn cut-at-repetition [a-seq]
  (loop [vect []
         b-seq a-seq]
   (cond
    (empty? b-seq) vect
    (some #{(first b-seq)} vect) vect
    :else (recur (conj vect (first b-seq))
                 (rest b-seq)))))



