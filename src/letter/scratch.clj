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
