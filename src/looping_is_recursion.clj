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
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

