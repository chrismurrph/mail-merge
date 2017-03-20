(ns cv.core
  (:require [cv.common :as c]
            [common.common :as cc]
            [clojure.java.io :as io]
            [clj-pdf.core :as pdf]
            [common.utils :as u]
            [clojure.string :as s]))

(def cv-in-file-name "cv-paragraphs.md")
(def cv-summary-in-file-name "cv-summary-info.txt")
(def cv-jobs-in-file-name "cv-jobs.edn")
(def cv-me-file-name "me_thin.png")
(def cv-out-file-name "ChrisMurphyCV.pdf")
(def output-dir "output")

(defn write-pdf-file [cv file-name]
  (pdf/pdf
    [{:top-margin    5
      :bottom-margin 5}
     cv]
    file-name))

(def cell-props {:valign :top :align :left})
(def top-props {:valign :top :align :center})

(defn short-version [contact-str]
  (cond
    (clojure.string/index-of contact-str "stackoverflow") "StackOverflow"
    (clojure.string/index-of contact-str "github") "GitHub"
    (clojure.string/index-of contact-str "linkedin") "LinkedIn"
    :default contact-str))

;;
;; Works but going to take too much time with every image being a slightly different size
;;
(defn create-contacts-table-with-image [contact-links]
  (let [img-fn c/flattened-image-here]
    [:pdf-table
     {:width-percent 100
      :cell-border   false}
     [1 15]
     [[:pdf-cell cell-props (img-fn "github-2.png" 1.6)] [:pdf-cell cell-props [:paragraph (first contact-links)]]]
     [[:pdf-cell cell-props (img-fn "github-2.png" 1.6)] [:pdf-cell cell-props [:paragraph (second contact-links)]]]
     [[:pdf-cell cell-props (img-fn "github-2.png" 1.6)] [:pdf-cell cell-props [:paragraph (u/third contact-links)]]]]))

(defn create-contacts-table [contact-links]
  [:pdf-table
   {:width-percent 100
    :cell-border   false}
   [1]
   [[:pdf-cell cell-props [:paragraph (first contact-links)]]]
   [[:pdf-cell cell-props [:paragraph (second contact-links)]]]
   [[:pdf-cell cell-props [:paragraph (u/third contact-links)]]]
   ]
  )

(defn create-intro [name contacts address keywords libs]
  (assert (string? contacts))
  (let [contact-links (mapv (fn [contact]
                              [:anchor {:target contact} (short-version contact)]) (s/split contacts #","))]
    (println contact-links)
    [:pdf-table {
                 ;:cell-border  true
                 ;:horizontal-align :left
                 :width-percent 100
                 }
     [1 5]
     [[:pdf-cell cell-props "Name"] [:pdf-cell cell-props name]]
     [[:pdf-cell cell-props "Links"] [:pdf-cell cell-props (create-contacts-table contact-links)]]
     [[:pdf-cell cell-props "Address"] [:pdf-cell cell-props address]]
     [[:pdf-cell cell-props "Experience"] [:pdf-cell cell-props keywords]]
     [[:pdf-cell cell-props "Libraries"] [:pdf-cell cell-props libs]]
     ]))

(defn image-table [name contact-links address keywords libs image-file-name]
  [:pdf-table
   {:width-percent 100
    :cell-border   true}
   [9 2]
   [(create-intro name contact-links address keywords libs) (c/image-here image-file-name 30)]])

(defn jobs-table [jobs]
  [:pdf-table
   {:width-percent 100
    :cell-border   true}
   [1 1]
   [[:pdf-cell cell-props "Name"] [:pdf-cell cell-props "More"]]])

(defn insert-paragraphs [paragraphs existing]
  (vec (concat paragraphs existing)))

(defn intro-data [[name contact-links address keywords libs] jobs paragraphs]
  (fn []
    (->> []
         (u/insert-at 0 (jobs-table jobs))
         (insert-paragraphs paragraphs)
         (u/insert-at 0 [:spacer])
         (u/insert-at 0 (image-table name contact-links address keywords libs cv-me-file-name))
         )))

(defn get-jobs []
  (read-string (slurp (io/resource "cv-jobs.edn"))))

(defn produce-cv []
  (let [paragraphs (->> cv-in-file-name
                        u/file-name->lines
                        (mapv cc/create-spaced-paragraph))
        summary-info (->> cv-summary-in-file-name
                          u/file-name->lines)]
    (let [intro-data-fn (intro-data summary-info (get-jobs) paragraphs)
          letter (intro-data-fn)
          out-file-path (str output-dir "/" cv-out-file-name)]
      (write-pdf-file letter out-file-path)
      (str "Written " out-file-path " CV file"))))