(ns letter.test-core
  (:require [letter.core :as c]
            [common.utils :as u]))

(defn x-1 []
  (->> c/addresses-file-name
       u/file-name->lines
       (partition-by #(= "" %))
       (remove #(= ["" ""] %))
       (map c/make-address)
       (drop 5)
       (take 3)
       u/pp))

(defn x-2 []
  (c/split-by-commas "PO Box 6237, Halifax Street, Adelaide, SA 5000"))
