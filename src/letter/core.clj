(ns letter.core
  (:require
    [clj-pdf.core :as pdf]
    [clojure.java.io :as io]
    [clojure.string :as s]
    [common.utils :as u]
    [letter.common :as c]
    [common.common :as cc]))

(def letter-file-name "mm/merge-letter-4.md")
(def addresses-file-name-old "mm/addresses.txt")
(def addresses-file-name-new "mm/Senators.txt")
(def addresses-file-name addresses-file-name-new)
(def windmills-file-name "mm/Another_Advert.jpg")
(def misc-in-file-name "mm/misc.edn")
(def output-dir "output")

(defn -write-pdf-file! [letter file-name]
  (pdf/pdf
    [{:top-margin    35
      :bottom-margin 55
      :pages         true
      :font          {:family :times-roman
                      :size   11}}
     letter]
    file-name))

#_(defn make-address [[first-name second-name address]]
    {:first-name  first-name
     :second-name second-name
     :address     (s/split address #",")})

(defn split-by-commas [addr]
  (mapv s/trim (s/split addr #",")))

(defn make-address [[title first-name second-name position state-party street-addr po-box-addr]]
  (let [res-1 {:title          title
               :first-name     first-name
               :second-name    second-name
               :state-party    state-party
               :street-address (split-by-commas street-addr)
               }
        res-2 (if (= "." position)
                res-1
                (assoc res-1 :position position))
        res-3 (if (= "." po-box-addr)
                res-2
                (assoc res-2 :po-box-address (split-by-commas po-box-addr)))]
    res-3))

(defn address->file-name [{:keys [first-name second-name]}]
  (str first-name "-" second-name ".pdf"))

(defn blank-line? [x]
  (= "" (first x)))

#_(defn get-contacts [address-lines]
    (->> address-lines
         (partition-by #(= % ""))
         (remove blank-line?)
         (map make-address)))

(defn get-contacts [address-lines]
  (->> address-lines
       (partition-by #(= "" %))
       (remove #(= ["" ""] %))
       (map make-address)
       ))

(defn dear-sir [{:keys [first-name second-name]}]
  (fn [paragraphs]
    (let [para (cc/create-spaced-paragraph (str "Dear " first-name " " second-name ","))]
      (->> paragraphs
           (u/insert-at 0 para)
           (u/insert-at 0 [:spacer])
           (u/insert-at 0 [:spacer])))))

(defn left-right-addresses [l r]
  (fn [paragraphs]
    (->> paragraphs
         (u/insert-at 0 [:spacer])
         (u/insert-at 0 (c/create-addrs l r)))))

(defn write-pdf-files! [paragraphs contacts sender-address]
  (for [{:keys [first-name second-name street-address] :as contact-info} contacts]
    (let [formal-intro-fn (dear-sir contact-info)
          to-address (into [(str first-name " " second-name)] street-address)
          address-headers-fn (left-right-addresses to-address sender-address)
          file-name (address->file-name contact-info)
          letter (-> paragraphs
                     formal-intro-fn
                     address-headers-fn)]
      (-write-pdf-file! letter (str output-dir "/" file-name))
      file-name)))

;;
;; When make proper function will use all contacts (not take 1 contact and take 3 files)
;;
(defn produce-letters []
  (let [{:keys [caption-text sender-address]} (u/get-edn misc-in-file-name)
        insert-img-fn (cc/insert-image windmills-file-name {:n       1
                                                            :xscale  0.8
                                                            :yscale  0.8
                                                            :caption caption-text})
        paragraphs (->> letter-file-name
                        u/file-name->lines
                        (mapv cc/create-spaced-paragraph)
                        insert-img-fn
                        (u/insert-at 3 [:pagebreak]))
        contacts (->> addresses-file-name
                      u/file-name->lines
                      get-contacts
                      (take 1)
                      )
        files-written (write-pdf-files! paragraphs contacts sender-address)]
    (str "Written " (count contacts) " pdf files (first 3): " (seq (map symbol (take 3 files-written))))))