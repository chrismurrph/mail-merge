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
(defn greatest-continuous [op xs]
  (let [op-pair? (fn [[x y]] (op x y))
        take-every-second #(take-nth 2 (cons (first %) %))
        make-canonical #(take-every-second (apply concat %))]
    (->> xs
         (partition 2 1)
         (partition-by op-pair?)
         (filter (comp op-pair? first))
         (map make-canonical)
         (apply max-key count))))

(defn x-1 []
  (greatest-continuous > in))

(defn step-state-hof [op]
  (fn [{:keys [unprocessed current answer]}]
    (let [[x y & more] unprocessed]
      (let [next-current (if (op x y)
                           (conj current y)
                           [y])
            next-answer (if (> (count next-current) (count answer))
                          next-current
                          answer)]
        {:unprocessed (cons y more)
         :current     next-current
         :answer      next-answer}))))

(defn x-2 []
  (let [iter (step-state-hof >)]
    (iter (iter {:unprocessed (rest in)
                 :current     (vec (take 1 in))}))))

(defn x-3 []
  (let [step-state (step-state-hof >)]
    (->> (iterate step-state {:unprocessed (rest in)
                              :current     (vec (take 1 in))})
         (drop (- (count in) 2))
         first
         :answer)))
