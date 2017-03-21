(ns letter.core
  (:require
    [clj-pdf.core :as pdf]
    [clojure.java.io :as io]
    [clojure.string :as s]
    [common.utils :as u]
    [letter.common :as c]
    [common.common :as cc]))

(def letter-file-name "merge_letter.txt")
(def addresses-file-name "addresses.txt")
(def windmills-file-name "Another_Advert.jpg")
(def output-dir "output")
(def sender-address ["Chris Murphy" "P.O. Box 20" "Adelaide SA 5000"])

(defn -write-pdf-file! [letter file-name]
  (pdf/pdf
    [{:top-margin    5
      :bottom-margin 5}
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

(defn get-contacts [address-lines]
  (->> address-lines
       (partition-by #(= % ""))
       (remove blank-line?)
       (map make-address)))

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
    (let [para (cc/create-spaced-paragraph (str "Dear " first-name " " second-name ","))]
      ;(u/pp paragraphs)
      (->> paragraphs
           (u/insert-at 0 para)))))

(defn left-right-addresses [l r]
  (let [transposed (mapv vector l r)]
    ;(println transposed)
    (fn [paragraphs]
      (->> paragraphs
           (u/insert-at 0 [:spacer])
           (u/insert-at 0 (c/create-addrs l r))))))

(defn write-pdf-files! [paragraphs contacts]
  (for [{:keys [first-name second-name address] :as contact-info} contacts]
    (let [formal-intro-fn (dear-sir contact-info)
          to-address (into [(str first-name " " second-name)] address)
          address-headers-fn (left-right-addresses to-address sender-address)
          file-name (address->file-name contact-info)
          letter (-> paragraphs
                     formal-intro-fn
                     address-headers-fn)]
      (-write-pdf-file! letter (str output-dir "/" file-name))
      file-name)))

;;
;; When make proper function will use all contacts
;;
(defn x-1 []
  (let [insert-img-fn (insert-image windmills-file-name)
        paragraphs (->> letter-file-name
                        u/file-name->lines
                        (mapv cc/create-spaced-paragraph)
                        insert-img-fn)
        contacts (take 1 (get-contacts (u/file-name->lines addresses-file-name)))
        files-written (write-pdf-files! paragraphs contacts)]
    (str "Written " (count contacts) " pdf files (first 3): " (seq (map symbol (take 3 files-written))))))

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