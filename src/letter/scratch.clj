(ns letter.scratch
  (:require [common.utils :as u]))

(defn handler [in]
  in)

(comment
  (def transform
    (-> handler
        (assoc :hair-color :gray)
        (update :age inc))))

(comment
  (defn x-1 []
    (transform {:age 0})))

(def switchvar (atom 1))
(def items [1 2 3])

(defn listitems [items]
  [:ul
   (for [item items
         idx (range (count items))]
     ^{:key item} [:li {:class
                        (if (and (odd? idx) (odd? @switchvar))
                          "active"
                          "inactive")
                        } "Item " item])])

(defn x-10 []
  (listitems items))

(def stars [1 2 3 4 5])
(def planets [:a :b :c :d :e :f :h :i :j :k :l :m])

(defn gen-universe []
  (let [max-num-planets 3
        s-picked (mapcat #(repeat (inc (rand-int max-num-planets)) %) stars)]
    (map #(list %1 %2) s-picked planets)))

(def input [4 5 1 6 1 2 4 6 2 3 6 4 6 5 4 3 1 2 1 2 3])

(defn decreasing-subsequences [xs]
  (lazy-seq
    (cond (empty? xs) []
          (not (next xs)) (list xs)
          :else (let [[x & [y :as more]] xs
                      remainder (decreasing-subsequences more)]
                  (if (> y x)
                    (cons [x] remainder)
                    (cons (cons x (first remainder)) (rest remainder)))))))

(defn increasing-subsequences [xs]
  (reduce
    (fn [[longest current] x]
      (let [tail (last current)
            next-seq (if (or (not tail) (> x tail))
                       (conj current x)
                       [x])
            new-longest (if (> (count next-seq) (count longest))
                          next-seq
                          longest)]
        [new-longest next-seq]))
    [[] []]
    xs))

(defn x-1 []
  (apply max-key count (decreasing-subsequences input)))

(defn iterate-fn [xs]
  ())

(defn x-2 []
  (->> input
       (partition-all 2 1)
       (take-while >)))

(defn x-3 []
  (take-while (fn [[x y]]
                (< x y)) (partition 2 1 input)))

(defn iteree [xs]
  (take-while > (take 2 xs)))

(defn first-seconds [[h & t]]
  (into (vec h) (map second t)))

(def in [3 2 1 0 -1 2 7 6 7 6 5 4 3 2])
(defn descending-sequences [xs]
  (->> xs
       (partition 2 1)
       (map (juxt (fn [[x y]] (> x y)) identity))
       (partition-by first)
       (filter ffirst)
       (map #(let [xs' (mapcat second %)]
              (take-nth 2 (cons (first xs') xs'))))
       #_(reduce #(if (> (count %2) (count %1)) %2 %1))
       (apply max-key count)
       ))

(defn x-1 []
  (descending-sequences in))

(defn combine-them [xs']
  (let [xs (mapcat second xs')]
    (take-nth 2 (cons (first xs) xs))))

(defn x-2 []
  (combine-them '([true (7 6)] [true (6 5)] [true (5 4)] [true (4 3)] [true (3 2)])))

