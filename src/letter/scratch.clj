(ns letter.scratch)

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

