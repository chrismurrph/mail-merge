(ns letter.core
  (:require
    [clj-pdf.core :as pdf]
    [clojure.java.io :as io]
    [clojure.string :as s]
    [letter.utils :as u]
    [letter.common :as c]))

(def letter-file-name "merge_letter.txt")
(def addresses-file-name "addresses.txt")
(def windmills-file-name "Another_Advert.jpg")
(def output-dir "output")

(defn write-pdf-file [letter file-name]
  (pdf/pdf
    [{}
     letter]
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

(defn insert-image [image-file-name]
  (fn [paragraphs]
    ;(println paragraphs)
    (assert (vector? paragraphs))
    (u/insert-at 1 [:image {:xscale 0.8
                            :yscale 0.8
                            :align  :center}
                    (-> image-file-name io/resource)]
                 paragraphs)))

(defn dear-sir [{:keys [first-name second-name]}]
  (fn [paragraphs]
    (let [para (c/create-spaced-paragraph (str "Dear " first-name " " second-name ","))]
      ;(u/pp paragraphs)
      (->> paragraphs
           (u/insert-at 0 para)))))

(defn left-right-addresses [l r]
  (fn [paragraphs]
    (->> paragraphs
         (u/insert-at 0 [:spacer])
         (u/insert-at 0 (c/create-table)))))

(defn x-1 []
  (let [insert-img-fn (insert-image windmills-file-name)
        paragraphs (->> letter-file-name
                        io/resource
                        io/reader
                        line-seq
                        (mapv c/create-spaced-paragraph)
                        insert-img-fn)
        addresses (take 1 (get-addresses addresses-file-name))]
    (doseq [address addresses]
      (let [formal-intro-fn (dear-sir address)
            address-headers-fn (left-right-addresses :a :b)
            file-name (address->file-name address)
            letter (-> paragraphs
                       formal-intro-fn
                       address-headers-fn)]
        (write-pdf-file letter (str output-dir "/" file-name))))
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