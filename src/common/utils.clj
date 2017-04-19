(ns common.utils
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clj-time.format :as f]
            [clj-time.core :as t]))

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
   (pp x)
   x)
  ([x msg]
   (println msg x)
   x))

(defn make-filename
  ([n path postfix extension]
   (assert (and (or (zero? n) (pos? n)) (<= n 99)))
   (let [prefix (format "%02d" n)]
     (str path "/" prefix "_" postfix (when extension (str "." extension)))))
  ([n path postfix]
    (make-filename n path postfix nil)))

(defn get-edn [file-name]
  (read-string (slurp (io/resource file-name))))

(defn round-dec-pl
  "Round a double to the given number of significant digits"
  [precision]
  (fn [d]
    (let [factor (Math/pow 10 precision)]
      (/ (Math/round (* d factor)) factor))))

(def round3 (round-dec-pl 3))

(def -short-date-formatter (f/formatter "dd/MM/yyyy"))
(def -long-date-formatter (f/formatter "dd MMMM yyyy"))
(def format-date #(f/unparse -long-date-formatter %))

(defn formatted-now []
  (format-date (t/now)))

(defn x-1 []
  (->> (map round3 [0.001 10.123456 9.5556])
       ;(map type)
       ))

(defn x-2 []
  (make-filename 0 "output" "senators.pdf"))


