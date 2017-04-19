(ns letter.test-core
  (:require [letter.core :as c]
            [common.utils :as u]
            [common.common :as cc]))

(defn x-1 []
  (->> c/addresses-file-name
       u/file-name->lines
       (partition-by #(= "" %))
       (remove #(= ["" ""] %))
       (map cc/make-address)
       (drop 5)
       (take 3)
       u/pp))

(defn x-2 []
  (cc/split-by-commas "PO Box 6237, Halifax Street, Adelaide, SA 5000"))
