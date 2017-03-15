(ns letter.core
  (:require
    [clj-pdf.core :as pdf]
    [clojure.java.io :as io]
    [utils :as u]
    [clojure.string :as s]))

(def letter-file-name "merge_letter.txt")
(def addresses-file-name "addresses.txt")
(def windmills-file-name "Another_Advert.jpg")
(def output-dir "output")

(defn write-pdf-file [paragraphs file-name]
  (pdf/pdf
    [{}
     paragraphs]
    file-name))

(defn make-address [[first-name second-name address]]
  {:first-name  first-name
   :second-name second-name
   :address     (s/split address #",")})

(defn address->file-name [{:keys [first-name second-name]}]
  (str first-name "-" second-name ".pdf"))

(defn blank-line? [x]
  (= "" (first x)))

(defn get-addresses [file-name]
  (let [address-lines (-> file-name io/resource io/reader line-seq)]
    (->> address-lines
         (partition-by #(= % ""))
         (remove blank-line?)
         (map make-address))))

(defn create-paragraph [paragraph-text]
  [:paragraph
   [:chunk paragraph-text]
   [:spacer]])

;;
;; x to be inserted at n in vector v
;;
(defn insert-at [n x v]
  (vec (concat (subvec v 0 n) (vector x) (subvec v n))))

(defn insert-image [image-file-name]
  (fn [paragraphs]
    ;(println paragraphs)
    (assert (vector? paragraphs))
    (insert-at 1 [:image (-> image-file-name io/resource)] paragraphs)))

(defn x-1 []
  (let [insert-img-fn (insert-image windmills-file-name)
        paragraphs (->> letter-file-name
                        io/resource
                        io/reader
                        line-seq
                        (mapv create-paragraph)
                        insert-img-fn)
        addresses (take 1 (get-addresses addresses-file-name))]
    ;(println addresses)
    (doseq [address addresses]
      (let [file-name (address->file-name address)]
        (write-pdf-file paragraphs (str output-dir "/" file-name))))
    (str "Written " (count addresses) " pdf files")))

(defn x-2 []
  (pdf/pdf
    [{}
     [:list {:roman true}
      [:chunk {:style :bold} "a bold item"]
      "another item"
      "yet another item"]
     [:phrase "some text"]
     [:phrase "some more text"]
     [:paragraph "yet more text"]]
    "doc.pdf"))