(ns clj-forum-qa.core
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;;; Types

(defrecord Post [username question answer])

;;; Const

(def url "http://forum.bodybuilding.com/showthread.php?t=146471313&page=")

;;; Utils

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))


;;; Main


(defn get-username [data]
  (first (html/select data [:a.username :> :strong :> html/text-node])))

(defn get-question [data]
  (let [raw (first (html/select data [:div.quote_container :> :div.message]))]
    (str/join (filter (comp not map?) (:content raw)))))

(defn get-post-number [data]
  (str/replace
    (-> (html/select data [:span.nodecontrols :> :a])
         first :attrs :name) "post" ""))

(defn get-answer [data]
  (let [post-number (get-post-number data)
        selector (keyword (str/join ["div#post_message_" post-number]))
        raw (first (html/select data [selector :> :blockquote.postcontent]))]
    (str/replace
     (str/join (filter (comp not map?) (:content raw)))
     "\n\t\t\t\t\t\t\t" "")))

(defn make-post [data]
  (let [username (get-username data)
        question (get-question data)
        answer (get-answer data)]
    (Post. username question answer)))

(defn get-posts [url]
  (let [data (fetch-url url)
        raw (html/select data [:ol.posts :> :li])]
    (map make-post raw)))

(defn get-posts-by-range [from to]
  (apply concat (pmap #(get-posts (str/join [url %])) (range from to))))

(defn filter-by-username [username posts]
  (filter #(= username (:username %)) posts))

(defn to-str [post]
  (str/join ["Q: " (:question post) "\n\n" "A: " (:answer post) "\n\n"]))

(defn -main []
  (spit "data.txt"
        (str/join
         (map to-str
              (filter-by-username "Kelei"
                                         (get-posts-by-range 1 300))))))
;(-main)
