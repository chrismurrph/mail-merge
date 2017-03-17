(ns cv.core
  (:require [letter.common :as c]
            [clojure.java.io :as io]
            [clj-pdf.core :as pdf]
            [common.utils :as u]))

(def cv-in-file-name "cv-paragraphs.md")
(def cv-summary-in-file-name "cv-summary-info.txt")
(def cv-jobs-in-file-name "cv-jobs.edn")
(def cv-me-file-name "me.png")
(def cv-out-file-name "ChrisMurphyCV.pdf")
(def output-dir "output")

(defn write-pdf-file [cv file-name]
  (pdf/pdf
    [{:top-margin    5
      :bottom-margin 5}
     cv]
    file-name))

(def cell-props {:valign :middle :align :left})

(defn create-intro [name contact address keywords]
  [:pdf-table {
               :cell-border  true
               :horizontal-align :left
               :width-percent 80
               }
   [1 3]
   [[:pdf-cell cell-props "Name"] [:pdf-cell cell-props name]]
   [[:pdf-cell cell-props "Contact"] [:pdf-cell cell-props contact]]
   [[:pdf-cell cell-props "Address"] [:pdf-cell cell-props address]]
   [[:pdf-cell cell-props "Keywords"] [:pdf-cell cell-props keywords]]
   ])

(defn insert-image [image-file-name]
  (fn [paragraphs]
    ;(println paragraphs)
    (assert (vector? paragraphs))
    (let [img (-> image-file-name io/resource)]
      (u/insert-at 1
                   [:chunk {:x 450 :y 10} [:image {:scale 15} img]]
                   #_[:image {:scale 15
                              :align :center}
                      (-> image-file-name io/resource)]
                   paragraphs))))

(defn intro-data [[name contacts address keywords]]
  (fn [paragraphs]
    (->> paragraphs
         (u/insert-at 0 [:spacer])
         (u/insert-at 0 (create-intro name contacts address keywords)))))

(defn produce-cv []
  (let [insert-img-fn (insert-image cv-me-file-name)
        paragraphs (->> cv-in-file-name
                        u/file-name->lines
                        (mapv c/create-spaced-paragraph))
        summary-info (->> cv-summary-in-file-name
                          u/file-name->lines
                          )]
    (u/pp summary-info)
    (let [intro-data-fn (intro-data summary-info)
          letter (-> paragraphs
                     intro-data-fn
                     insert-img-fn)
          out-file-path (str output-dir "/" cv-out-file-name)]
      (write-pdf-file letter out-file-path)
      (str "Written " out-file-path " CV file"))))
