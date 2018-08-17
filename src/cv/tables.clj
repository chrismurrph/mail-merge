(ns cv.tables
  (:require [cv.common :as c]
            [common.common :as cc]
            [clojure.string :as s]
            [common.utils :as u]
            [common.dev :as dev]))

(defn top [props]
  (assoc props :valign :top))

(def cell-props {:valign :top :align :left})
(def middle-props {:valign :middle :align :center})
(def middle-left-props {:valign :middle :align :left})
(def top-left-props {:valign :top :align :left})
(def table-props {:width-percent 100 :horizontal-align :right})

(defn img-f [social]
  (condp = social
    "so" (c/image-here "cv/apple-touch-icon.png" 12 0 -4)
    "m" (c/image-here "cv/medium.png" 6 1 0)
    "gh" (c/image-here "cv/github-2.png" 1.6 0)
    "s" (c/image-here "cv/slack.png" 3 -3)))

(defn create-my-links-table [contact-links]
  (dev/log-off "Num contact links" (count contact-links))
  [:pdf-table
   (assoc table-props :cell-border false)
   ;; Medium, SO, Github, Slack
   ;; With Medium I point to one article (as there only is one so far) and have to miss out the @ as read-string
   ;; doesn't like it - never-the-less the lack of a @ seems to get resolved.
   [1 8 1 8]
   [[:pdf-cell (top middle-props) (img-f "m")]
    [:pdf-cell (top middle-left-props) [:paragraph (first contact-links)]]
    [:pdf-cell (top middle-props) (img-f "so")]
    [:pdf-cell (top middle-left-props) [:paragraph (second contact-links)]]]
   [[:pdf-cell (top middle-props) (img-f "gh")]
    [:pdf-cell (top middle-left-props) [:paragraph (u/third contact-links)]]
    [:pdf-cell (top middle-props) (img-f "s")]
    [:pdf-cell (top middle-left-props) [:paragraph (u/fourth contact-links)]]]])

(defn make-anchor [link text]
  [:anchor (assoc cc/anchor-attributes :target link) (str text)])

(defn create-intro [name phone email contacts address keywords libs]
  (assert (string? contacts))
  (let [props middle-left-props
        details (s/split contacts #",")
        links (->> details
                   dev/probe-off
                   (map read-string)
                   dev/probe-off
                   (map (juxt (comp str first) (comp str second))))
        contact-links (mapv (fn [[link user-id]]
                              (make-anchor link user-id)) links)]
    [:pdf-table (assoc table-props :cell-border true)
     [1 6.5]
     [[:pdf-cell props ""] [:pdf-cell {:align  :center
                                       :valign :top
                                       :style  :bold
                                       :size   cc/bigger
                                       :height 27} name]]
     [[:pdf-cell props "Phone"] [:pdf-cell props phone]]
     [[:pdf-cell props "Email"] [:pdf-cell props email]]
     [[:pdf-cell cell-props "Links"] [:pdf-cell props (create-my-links-table contact-links)]]
     [[:pdf-cell props "Address"] [:pdf-cell props address]]
     [[:pdf-cell props "Languages"] [:pdf-cell props keywords]]
     [[:pdf-cell props "Clojure"] [:pdf-cell props libs]]]))

(defn image-table [your-name your-phone your-email your-contact-links
                   your-address your-keywords your-libs your-image-file-name]
  [:pdf-table
   (assoc table-props :cell-border true)
   [6.9 2.06]
   [(create-intro your-name your-phone your-email your-contact-links your-address your-keywords your-libs)
    (c/image-here your-image-file-name 38.9)]])

(defn create-job-row [long-version-fn {:keys [month-from year-from month-to year-to org position]}]
  (assert (string? month-to))
  [
   ;[:pdf-cell cell-props month-from]
   [:pdf-cell cell-props (str year-from)]
   ;[:pdf-cell cell-props month-to]
   [:pdf-cell cell-props (str (if (= "" year-to) "---" year-to))]
   [:pdf-cell cell-props (cc/word->paragraph {:search-word "www.strandz.org" :op (cc/anchor-text->anchor long-version-fn)} org)]
   [:pdf-cell cell-props position]])

(defn jobs-table [long-version-fn jobs]
  (let [create-job-row-fn (partial create-job-row long-version-fn)
        table (into [:pdf-table
                     (assoc table-props :cell-border true :width-percent 100)
                     [1 1 4 6]]
                    (mapv create-job-row-fn jobs))]
    [:paragraph {:indent-left (+ cc/indent 0) :indent-right cc/indent} table]))

(defn create-referees-row [{:keys [name phone handle email org role]}]
  (assert name)
  (assert email)
  (assert org)
  (let [contact (or phone handle)
        desc (if role (str org ", " role) org)]
    [[:pdf-cell cell-props name]
     [:pdf-cell cell-props contact]
     [:pdf-cell cell-props email]
     [:pdf-cell cell-props desc]]))

(defn referees-table [referees]
  (let [table (into [:pdf-table
                     (assoc table-props :cell-border false :horizontal-align :left)
                     [2 2.5 3 4]]
                    (mapv create-referees-row referees))]
    [:paragraph {:indent 1.5} table]))

(defn create-social-bi-cell [{:keys [site desc link]}]
  (assert site)
  (assert desc)
  (assert link)
  [[:pdf-cell middle-props (img-f site)]
   [:pdf-cell top-left-props [:phrase (make-anchor link desc)]]])

(def empty-bi-cell
  [[:pdf-cell middle-props ""]
   [:pdf-cell top-left-props [:phrase ""]]])

(defn create-social-row [[link-a link-b]]
  (into (create-social-bi-cell link-a) (if link-b
                                         (create-social-bi-cell link-b)
                                         empty-bi-cell)))

(defn social-table [social]
  (let [table (into [:pdf-table
                     (assoc table-props :cell-border false
                                        :horizontal-align :left
                                        :width-percent 100)
                     [1 9 1 9]]
                    (->> social
                         (partition-all 2)
                         (mapv create-social-row)
                         dev/probe-off))]
    [:paragraph {:indent-left (+ cc/narrow-indent 0) :indent-right cc/narrow-indent} table]))
