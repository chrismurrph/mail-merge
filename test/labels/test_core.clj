(ns labels.test-core
  (:require [labels.core :as c]
            [common.utils :as u]
            [clojure.java.io :as io])
  (:import (cljpdf.text Phrase Document)
           (cljpdf.text.pdf PdfWriter)))

(def example-labels [["hello doggy world a" "hello doggy world b"]
                     ["hello doggy world 5" "hello doggy world 6"]
                     ["hello doggy world 7" "hello doggy world 8"]
                     ["hello doggy world 9" "hello doggy world 10"]])
(def file-name "example-labels.pdf")

(defn x-1 []
  (let [texts [{:grid-pos [0 0] :label ["hello doggy world 3" "hello doggy world 4"]}
               {:grid-pos [1 0] :label ["hello doggy world 5" "hello doggy world 6"]}
               {:grid-pos [2 0] :label ["hello doggy world 7" "hello doggy world 8"]}]]
    ((c/print-to-file file-name) texts)))

(defn x-2 []
  (c/cms->pts [1 1]))

(defn x-3 []
  (let [llabels (partition-all 24 example-labels)]
    (map-indexed vector llabels)))

(defn x-4 []
  (c/gen-label-files! example-labels))

