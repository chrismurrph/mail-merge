(ns cv.core
  (:require [cv.common :as c]
            [common.common :as cc]
            [clojure.java.io :as io]
            [clj-pdf.core :as pdf]
            [common.utils :as u]
            [clojure.string :as s]))

(def cv-in-file-name "cv-paragraphs.md")
(def cv-text-links "cv-text-links.md")
(def cv-summary-in-file-name "cv-summary-info.md")
(def cv-jobs-in-file-name "cv-jobs.edn")
(def cv-misc-in-file-name "cv-misc.edn")
(def output-dir "output")

(defn write-pdf-file [cv file-name]
  (pdf/pdf
    [{:pages         true
      :top-margin    25
      :bottom-margin 33
      :font          {:family :times-roman
                      :size   11}}
     cv]
    file-name))

(def cell-props {:valign :top :align :left})
(def top-props {:valign :top :align :center})
(def middle-props {:valign :middle :align :center})
(def middle-left-props {:valign :middle :align :left})

;;
;; Only the Github link actually works. No time to fix this (or put pictures)
;;
(defn short-version [contact-str]
  (cond
    (or (s/index-of contact-str "stackexchange")
        (s/index-of contact-str "stackoverflow")) "StackOverflow"
    (s/index-of contact-str "github") "GitHub"
    (s/index-of contact-str "linkedin") "LinkedIn"
    (s/index-of contact-str "clojurians") "Clojurians Slack"
    :default contact-str))

;;
;; Not all being used because these links are not going to work. Nor is my personal one but I'm
;; "@atroche" "https://clojurians.slack.com/messages/search/atroche/"
;; "@luxbock" "https://clojurians.slack.com/messages/C0PULSD25/search/luxbock/"
;; still going to leave it as a link.
;;
(defn long-version []
  (let [links (->> cv-text-links
                   u/file-name->lines
                   (map #(s/split % #","))
                   (into {}))]
    (fn [found-str]
      (let [res (get links found-str)]
        (assert res (str "Not found a link target for " found-str))
        res))))

(defn top [props]
  (assoc props :valign :top))

(defn create-my-links-table [contact-links]
  (let [img-fn c/image-here]
    [:pdf-table
     {:width-percent 100
      :cell-border   false}
     [1 15]
     [[:pdf-cell (top middle-props) (img-fn "github-2.png" 1.6 0)] [:pdf-cell (top middle-left-props) [:paragraph (first contact-links)]]]
     [[:pdf-cell (top middle-props) (img-fn "apple-touch-icon.png" 11 0 1)] [:pdf-cell (top middle-left-props) [:paragraph (second contact-links)]]]
     [[:pdf-cell (top middle-props) (img-fn "linkedin-button.png" 7 2 2)] [:pdf-cell (top middle-left-props) [:paragraph (u/third contact-links)]]]
     [[:pdf-cell (top middle-props) (img-fn "slack.png" 3 -3)] [:pdf-cell (top middle-left-props) [:paragraph (u/fourth contact-links)]]]
     ]))

(defn create-intro [name contacts address keywords libs]
  (assert (string? contacts))
  (let [props middle-left-props
        details (s/split contacts #",")
        links (->> details
                   (map read-string)
                   (map (juxt (comp str first) (comp str second))))
        contact-links (mapv (fn [[link user-id]]
                              [:anchor (assoc cc/anchor-attributes :target link) (str (short-version link) " (" user-id ")")]) links)]
    [:pdf-table {
                 ;:cell-border  true
                 ;:horizontal-align :left
                 :width-percent 100
                 }
     [1 5]
     [[:pdf-cell props ""] [:pdf-cell {:align  :center
                                       :valign :top
                                       :style  :bold
                                       :size   cc/bigger
                                       :height 25} name]]
     [[:pdf-cell cell-props "Links"] [:pdf-cell props (create-my-links-table contact-links)]]
     [[:pdf-cell props "Address"] [:pdf-cell props address]]
     [[:pdf-cell props "Experience"] [:pdf-cell props keywords]]
     [[:pdf-cell props "Libraries"] [:pdf-cell props libs]]
     ]))

(defn image-table [name contact-links address keywords libs image-file-name]
  [:pdf-table
   {:width-percent 100
    :cell-border   true}
   [6.9 2.08]
   [(create-intro name contact-links address keywords libs) (c/image-here image-file-name 39.1)]])

(defn create-job-row [{:keys [month-from year-from month-to year-to org position]}]
  (assert (string? month-to))
  (let [long-version-fn (long-version)]
    [[:pdf-cell cell-props month-from]
     [:pdf-cell cell-props (str year-from)]
     [:pdf-cell cell-props month-to]
     [:pdf-cell cell-props (str year-to)]
     [:pdf-cell cell-props (cc/word->paragraph {:search-word "www.strandz.org" :op (cc/anchor-text->anchor long-version-fn)} org)]
     [:pdf-cell cell-props position]]))

(defn jobs-table [jobs]
  (into [:pdf-table
         {:width-percent 100
          :cell-border   true}
         [1 1 1 1 4 7]]
        (mapv create-job-row jobs)))

(defn insert-many-at-level [many-of existing]
  (vec (concat many-of existing)))

(defn make-cv [[name contact-links address keywords libs] jobs paragraphs cv-me-file-name]
  (->> []
       (insert-many-at-level [[:heading {:style {:size cc/bigger}} "Employment History"] [:spacer] (jobs-table jobs)])
       (insert-many-at-level paragraphs)
       (u/insert-at 0 [:spacer])
       (u/insert-at 0 (image-table name contact-links address keywords libs cv-me-file-name))))

(defn produce-cv []
  (let [first-heading-fn (cc/insert-heading "Clojure" 0)
        second-heading-fn (cc/insert-heading "Current Position" 3)
        third-heading-fn (cc/insert-heading "About Myself" 8)
        ;fourth-heading-fn (cc/insert-heading "Employment History" 10)
        long-version-fn (long-version)
        {:keys [coy-logo coy-website coy-link-title personal-picture result-pdf]} (u/get-edn cv-misc-in-file-name)
        paragraphs (->> cv-in-file-name
                        u/file-name->lines
                        (mapv (partial cc/create-spaced-paragraph
                                       (partial cc/word-in-text->chunks
                                                [{:search-word "installed" :op cc/make-italicized-chunk}
                                                 {:search-word "logician" :op (cc/anchor-text->anchor long-version-fn)}
                                                 {:search-word "eight with a nine wing" :op (cc/anchor-text->anchor long-version-fn)}])))
                        (u/insert-at 4 [:paragraph (c/image-here coy-logo 20 0 -9) [:anchor (assoc cc/anchor-attributes :target coy-website) coy-link-title] [:spacer]])
                        first-heading-fn
                        second-heading-fn
                        third-heading-fn
                        ;fourth-heading-fn
                        )
        summary-info (->> cv-summary-in-file-name
                          u/file-name->lines)]
    (let [cv (make-cv summary-info (u/get-edn cv-jobs-in-file-name) paragraphs personal-picture)
          out-file-path (str output-dir "/" result-pdf)]
      (write-pdf-file cv out-file-path)
      (str "Written " out-file-path " CV file"))))

(defn x-1 []
  (let [links (->> cv-text-links
                   u/file-name->lines
                   (map #(s/split % #","))
                   )]
    links))