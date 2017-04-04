(ns cv.tables
  (:require [cv.common :as c]
            [common.common :as cc]
            [clojure.string :as s]
            [common.utils :as u]))

(defn top [props]
  (assoc props :valign :top))

(def cell-props {:valign :top :align :left})
(def middle-props {:valign :middle :align :center})
(def middle-left-props {:valign :middle :align :left})

(defn create-my-links-table [contact-links]
  (let [img-fn c/image-here]
    [:pdf-table
     {:width-percent 100
      :cell-border   false}
     [1 8 1 8]
     [[:pdf-cell (top middle-props) (img-fn "cv/github-2.png" 1.6 0)] [:pdf-cell (top middle-left-props) [:paragraph (first contact-links)]]
      [:pdf-cell (top middle-props) (img-fn "cv/apple-touch-icon.png" 11 0 1)] [:pdf-cell (top middle-left-props) [:paragraph (second contact-links)]]]
     [[:pdf-cell (top middle-props) (img-fn "cv/linkedin-button.png" 7 2 2)] [:pdf-cell (top middle-left-props) [:paragraph (u/third contact-links)]]
      [:pdf-cell (top middle-props) (img-fn "cv/slack.png" 3 -3)] [:pdf-cell (top middle-left-props) [:paragraph (u/fourth contact-links)]]]]))

(defn create-intro [name phone email contacts address keywords libs]
  (assert (string? contacts))
  (let [props middle-left-props
        details (s/split contacts #",")
        links (->> details
                   (map read-string)
                   (map (juxt (comp str first) (comp str second))))
        contact-links (mapv (fn [[link user-id]]
                              [:anchor (assoc cc/anchor-attributes :target link) (str #_(short-version link) #_" " user-id)]) links)]
    [:pdf-table {
                 ;:cell-border  true
                 ;:horizontal-align :left
                 :width-percent 100
                 }
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
     [[:pdf-cell props "Libraries"] [:pdf-cell props libs]]]))

(defn image-table [your-name your-phone your-email your-contact-links
                   your-address your-keywords your-libs your-image-file-name]
  [:pdf-table
   {:width-percent 100
    :cell-border   true}
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
  (let [create-job-row-fn (partial create-job-row long-version-fn)]
    (into [:pdf-table
           {:width-percent 100
            :cell-border   true}
           [1 1 4 7]]
          (mapv create-job-row-fn jobs))))

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
  (into [:pdf-table
         {:width-percent 100
          :cell-border   true}
         [2 3 3 4]]
        (mapv create-referees-row referees)))
