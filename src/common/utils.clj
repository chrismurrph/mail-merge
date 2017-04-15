(ns common.utils
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]))

(def third #(nth % 2))
(def fourth #(nth % 3))
(def fifth #(nth % 4))

(defn file-name->lines [file-name]
  (->> file-name
       io/resource
       io/reader
       line-seq))

(def width 120)

(defn pp-str
  ([n x]
   (binding [pp/*print-right-margin* n]
     (-> x clojure.pprint/pprint with-out-str)))
  ([x]
   (pp-str width x)))

(defn pp
  ([n x]
   (binding [pp/*print-right-margin* n]
     (-> x clojure.pprint/pprint)))
  ([x]
   (pp width x)))

(def pp-off identity)

(defn probe-off
  ([x]
   x)
  ([x msg]
   x))

(defn probe-on
  ([x]
   (-> x
       pp)
   x)
  ([x msg]
   (println msg x)
   x))

;;
;; x to be inserted at n in vector v
;;
(defn insert-at [n x v]
  (vec (concat (subvec v 0 n) (vector x) (subvec v n))))

(defn get-edn [file-name]
  (read-string (slurp (io/resource file-name))))

