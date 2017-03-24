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
(def cv-misc-in-file-name "cv-misc.edn")
(def output-dir "output")

(defn write-pdf-file [cv file-name]
  (pdf/pdf
    [{:pages         true
      :top-margin    25
      :bottom-margin 30}
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

(defn long-version [found-str]
  (case found-str
    "@atroche" "https://clojurians.slack.com/messages/search/atroche/"
    "@luxbock" "https://clojurians.slack.com/messages/C0PULSD25/search/luxbock/"
    (assert false (str "No match for: " found-str))))

(defn top [props]
  (assoc props :valign :top))

;;
;; Works but going to take too much time with every image being a slightly different size
;;
(defn create-contacts-table-with-images [contact-links]
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

(defn create-contacts-table-no-images [contact-links]
  [:pdf-table
   {:width-percent 100
    :cell-border   false}
   [1]
   [[:pdf-cell cell-props [:paragraph (first contact-links)]]]
   [[:pdf-cell cell-props [:paragraph (second contact-links)]]]
   [[:pdf-cell cell-props [:paragraph (u/third contact-links)]]]
   [[:pdf-cell cell-props [:paragraph (u/fourth contact-links)]]]])

(def create-contacts-table create-contacts-table-with-images)

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
     [[:pdf-cell cell-props "Links"] [:pdf-cell props (create-contacts-table contact-links)]]
     [[:pdf-cell props "Address"] [:pdf-cell props address]]
     [[:pdf-cell props "Experience"] [:pdf-cell props keywords]]
     [[:pdf-cell props "Libraries"] [:pdf-cell props libs]]
     ]))

(defn image-table [name contact-links address keywords libs image-file-name]
  [:pdf-table
   {:width-percent 100
    :cell-border   true}
   [7.25 2.13]
   [(create-intro name contact-links address keywords libs) (c/image-here image-file-name 38.1)]])

(defn create-job-row [{:keys [month-from year-from month-to year-to org position]}]
  (assert (string? month-to))
  [[:pdf-cell cell-props month-from]
   [:pdf-cell cell-props (str year-from)]
   [:pdf-cell cell-props month-to]
   [:pdf-cell cell-props (str year-to)]
   [:pdf-cell cell-props org]
   [:pdf-cell cell-props position]])

(defn jobs-table [jobs]
  (into [:pdf-table
         {:width-percent 100
          :cell-border   true}
         [1 1 1 1 4 7]]
        (mapv create-job-row jobs)))

(defn insert-paragraphs [paragraphs existing]
  (vec (concat paragraphs existing)))

(defn make-cv [[name contact-links address keywords libs] jobs paragraphs cv-me-file-name]
  (->> []
       (u/insert-at 0 (jobs-table jobs))
       (u/insert-at 0 [:spacer])
       (insert-paragraphs paragraphs)
       (u/insert-at 0 [:spacer])
       (u/insert-at 0 (image-table name contact-links address keywords libs cv-me-file-name))))

;;
;; To do this properly divide your text into a [text-1 text-2].
;; Actually put each into a vector and give each formatting context, say {:italics ["installed"]}:
;; [[text-1 {}] [text-2 {"installed" [:italics]}]]
;; Then this function can be mapcat-ed over
;; Thus remaining problem is that this function needs to perform many operations.
;; Really we need a function that takes a vector of chunks and returns same.
;; Only recursion will handle this. Will search thru the texts of the chunks. For each discard the
;; chunk and turn it into 3 using this function. Note may do this many times because the word occurs
;; in multiple places. This is a reduce over the chunks. Actually this reduce can then be reduced over
;; for every operation.
;;
(defn text->chunks [[{:keys [search-word op]} & tail] text]
  (if search-word
    (if-let [idx (s/index-of text search-word)]
      (let [before (subs text 0 idx)
            after (subs text (+ idx (count search-word)))]
        [[:chunk before] (op search-word) [:chunk after]])
      (text->chunks tail text))
    [[:chunk text]]))

(defn produce-cv []
  (let [first-heading-fn (cc/insert-heading "Clojure" 0)
        second-heading-fn (cc/insert-heading "Current Position" 3)
        third-heading-fn (cc/insert-heading "Employment History" 8)
        {:keys [coy-logo coy-website coy-link-title personal-picture result-pdf]} (u/get-edn cv-misc-in-file-name)
        paragraphs (->> cv-in-file-name
                        u/file-name->lines
                        (mapv (partial cc/create-spaced-paragraph
                                       (partial text->chunks
                                                [{:search-word "installed" :op cc/make-italicized-chunk}
                                                 ;; My code works if stick to one of these. Real problem is links don't work
                                                 #_{:search-word "@atroche" :op (cc/anchor-text->chunk long-version)}
                                                 #_{:search-word "@luxbock" :op (cc/anchor-text->chunk long-version)}])))
                        (u/insert-at 4 [:paragraph (c/image-here coy-logo 20 0 -9) [:anchor (assoc cc/anchor-attributes :target coy-website) coy-link-title] [:spacer]])
                        first-heading-fn
                        second-heading-fn
                        third-heading-fn)
        summary-info (->> cv-summary-in-file-name
                          u/file-name->lines)]
    (let [cv (make-cv summary-info (u/get-edn cv-jobs-in-file-name) paragraphs personal-picture)
          out-file-path (str output-dir "/" result-pdf)]
      (write-pdf-file cv out-file-path)
      (str "Written " out-file-path " CV file"))))