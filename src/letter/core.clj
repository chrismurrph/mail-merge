(ns letter.core
  (:require
    [clj-pdf.core :as pdf]
    [clojure.java.io :as io]
    [clojure.string :as s]
    [common.utils :as u]
    [letter.common :as c]
    [common.common :as cc]))

(def input-letter-file-name "mm/merge-letter-5.md")
(def caption-paragraph-file-name "mm/personal_paragraph.txt")
(def caption-paragraph-lines-file-name "mm/personal_paragraph_lines.txt")
(def addresses-file-name "mm/Senators.txt")
(def windmills-file-name "mm/Another_Advert.jpg")
(def misc-in-file-name "mm/misc.edn")
(def output-dir "output/letters")

(defn -write-pdf-file! [letter file-name]
  (pdf/pdf
    [{:top-margin    35
      :bottom-margin 55
      :pages         true
      :font          {:family :times-roman
                      :size   11}}
     letter]
    file-name))

(defn address->file-name [{:keys [first-name second-name]}]
  (str first-name "-" second-name ".pdf"))

(defn dear-sir [{:keys [title first-name second-name]}]
  (fn [paragraphs]
    (let [para (cc/create-spaced-paragraph (str "Dear " (first (s/split title #" ")) " " second-name ","))]
      (->> paragraphs
           (cc/insert-at 0 para)
           (cc/insert-at 0 [:spacer])
           (cc/insert-at 0 [:spacer])))))

(defn left-right-addresses [l r]
  (fn [paragraphs]
    (->> paragraphs
         (cc/insert-at 0 [:spacer])
         (cc/insert-at 0 (c/create-addrs l r)))))

(defn make-space [n]
  (apply str (repeat n " ")))

(def signature-indent 40)

(defn write-pdf-files! [paragraphs contacts sender-address first-person second-person]
  (for [{:keys [title first-name second-name street-address] :as contact-info} contacts]
    (let [formal-intro-fn (dear-sir contact-info)
          to-address (into [(str title " " first-name " " second-name)] street-address)
          address-headers-fn (left-right-addresses to-address sender-address)
          file-name (address->file-name contact-info)
          letter (-> paragraphs
                     (cc/insert-many [[:spacer] [:spacer]
                                      (cc/create-spaced-paragraph (str (make-space signature-indent) "Yours faithfully,"))
                                      [:spacer] [:spacer] [:spacer] [:spacer] [:spacer]
                                      (cc/create-spaced-paragraph
                                        (str (make-space signature-indent) (str first-person (make-space 50) second-person)))])
                     formal-intro-fn
                     address-headers-fn)]
      (-write-pdf-file! letter (str output-dir "/" file-name))
      file-name)))

;;
;; When make proper function will use all contacts (not take 1 contact and take 3 files)
;;
(defn produce-letters []
  (let [image-at 5
        {:keys [caption-text sender-address first-person second-person]} (u/get-edn misc-in-file-name)
        insert-img-fn (cc/insert-image windmills-file-name {:n       image-at
                                                            :xscale  0.8
                                                            :yscale  0.8
                                                            :caption caption-text})
        ;; :align :left, :center, :right, :justified
        insert-personal-experience-fn (cc/insert-cation-paragraph
                                        caption-paragraph-lines-file-name
                                        {:n      (inc image-at)
                                         :indent 55
                                         :align  :justified})
        get-contacts-fn (partial cc/get-contacts cc/make-address)
        paragraphs (->> input-letter-file-name
                        u/file-name->lines
                        (mapv cc/create-spaced-paragraph)
                        insert-img-fn
                        insert-personal-experience-fn
                        (cc/insert-at 5 [:pagebreak])
                        )
        contacts (->> addresses-file-name
                      u/file-name->lines
                      get-contacts-fn
                      (take 1)
                      )
        files-written (write-pdf-files! paragraphs contacts sender-address first-person second-person)]
    (str "Written " (count contacts) " pdf files (first 3): " (seq (map symbol (take 3 files-written))))))