(ns cv.core
  (:require [cv.common :as c]
            [common.common :as cc]
            [clojure.java.io :as io]
            [clj-pdf.core :as pdf]
            [common.utils :as u]
            [clojure.string :as s]
            [cv.tables :as t]))

(def cv-in-file-name "cv/paragraphs-3.md")
(def cv-summary-in-file-name "cv/summary-info.md")
(def cv-jobs-in-file-name "cv/jobs.edn")
(def cv-referees-in-file-name "cv/referees.edn")
(def misc-in-file-name "cv/misc.edn")
(def cv-text-links-file-name "cv/text-links.md")
(def output-dir "output/cv")

(defn -write-pdf-file! [cv file-name]
  (pdf/pdf
    [{:pages         true
      :top-margin    55
      :bottom-margin 33
      ;:left-margin   40
      ;:right-margin  40
      :font          {:family :times-roman
                      :size   11}}
     cv]
    file-name))

(defn long-version [cv-text-links-filename]
  (let [links (->> cv-text-links-filename
                   u/file-name->lines
                   (map #(s/split % #","))
                   (into {}))]
    (fn [found-str]
      (let [res (get links found-str)]
        (assert res (str "Not found a link target for " found-str))
        res))))

(def long-version-fn (long-version cv-text-links-file-name))

(defn make-cv [[name phone email contact-links address keywords libs] referees jobs paragraphs cv-me-file-name]
  (->> []
       ;; spacer only works well when table has a border
       (cc/insert-many [(cc/heading-traditional "Referees") #_[:spacer]
                        (t/referees-table referees)])
       (cc/insert-at 0 [:spacer])
       (cc/insert-many [[:pagebreak]
                        (cc/heading-traditional "Employment History")
                        [:spacer]
                        (t/jobs-table long-version-fn jobs)])
       (cc/insert-many paragraphs)
       (cc/insert-at 0 [:spacer])
       (cc/insert-at 0 [:spacer])
       (cc/insert-at 0 [:spacer])
       (cc/insert-at 0 (t/image-table name phone email contact-links address keywords libs cv-me-file-name))
       ))

(defn produce-cv []
  (let [first-heading-fn (cc/insert-heading-narrow "Current Position" 0)
        second-heading-fn (cc/insert-page-break-heading-narrow "Clojure" 6)
        third-heading-fn (cc/insert-heading-narrow "About Myself" 12)
        {:keys [coy-logo coy-website coy-link-title personal-picture result-pdf]} (u/get-edn misc-in-file-name)
        paragraphs (->> cv-in-file-name
                        u/file-name->lines
                        (mapv (partial cc/create-spaced-paragraph-narrow
                                       (partial cc/word-in-text->chunks
                                                [{:search-word "installed" :op cc/make-italicized-chunk}
                                                 {:search-word "weather" :op cc/make-italicized-chunk}
                                                 ;; These words are in a file. Resist the urge to improve.
                                                 {:search-word "logician" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "date intervals" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "update vector inside reduce" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "element between each pair" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "date periods" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "hobby project" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "eight with a nine wing" :op (cc/anchor-text->anchor long-version-fn)}])))
                        (cc/insert-at 3 [:paragraph
                                         {:indent cc/narrow-indent}
                                         (c/image-here coy-logo 20 0 -9)
                                         [:anchor (assoc cc/anchor-attributes :target coy-website) coy-link-title]
                                         [:spacer]])
                        first-heading-fn
                        second-heading-fn
                        third-heading-fn)
        summary-info (->> cv-summary-in-file-name
                          u/file-name->lines)]
    (let [cv (make-cv summary-info (u/get-edn cv-referees-in-file-name) (u/get-edn cv-jobs-in-file-name) paragraphs personal-picture)
          out-file-path (str output-dir "/" result-pdf)]
      (-write-pdf-file! cv out-file-path)
      (str "Written " out-file-path " CV file"))))