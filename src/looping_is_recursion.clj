(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [n k acc]
                 (if (zero? k)
                   acc
                   (recur n (dec k) (* acc n))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (let [helper (fn [tmp sq]
                 (if (empty? sq)
                   tmp
                   (recur (first sq) (rest sq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                  (and (empty? s1) (empty? s2))
                    true
                  (empty? s1)
                    false
                  (empty? s2)
                    false
                  (= (first s1) (first s2))
                    (recur (rest s1) (rest s2))
                  :else
                    false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [skv a-seq
         index 0]
    (cond
     (empty? skv)
       nil
     (pred (first skv))
      index
     :else
      (recur (rest skv) (inc index)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         skv a-seq]
    (if (empty? skv)
      (if (= n 0)
        sum
        (/ sum n))
      (recur (+ (first skv) sum) (inc n) (rest skv)))))

(defn parity [a-seq]
  (loop [even-set #{}
         odd-set #{}
         skv a-seq]
    (cond
     (empty? skv)
       odd-set
     (contains? odd-set (first skv))
       (recur (conj even-set (first skv)) (disj odd-set (first skv)) (rest skv))
     (contains? even-set (first skv))
       (recur (disj even-set (first skv)) (conj odd-set (first skv)) (rest skv))
     :else
       (recur even-set (conj odd-set (first skv)) (rest skv)))))

(defn fast-fibo [n]
  (loop [iter 0
         f-2 0
         f-1 1]
    (if (= iter n)
      (cond
       (= n 0)
         0
       (= n 1)
         1
       :else
        f-2)
      (recur (inc iter) f-1 (+ f-1 f-2)))))


(defn cut-at-repetition [a-seq]
  (loop [skv a-seq
         out-vector []
         bag #{}]
    (cond
     (empty? skv)
       out-vector
     (contains? bag (first skv))
       out-vector
     :else
       (recur (rest skv) (conj out-vector (first skv)) (conj bag (first skv))))))

