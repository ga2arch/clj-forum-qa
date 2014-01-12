(ns clj-forum-qa.core
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;;; Types

(defrecord Post [username question answer])

;;; Const

(def url "http://forum.bodybuilding.com/showthread.php?t=157587143&page=")

;;; Utils

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

;;; Main


(defn get-username [data]
  (first (html/select data [:a.username :> :strong :> html/text-node])))

(defn get-post-number [data]
  (str/replace
    (-> (html/select data [:span.nodecontrols :> :a])
         first :attrs :name) "post" ""))

(defn get-posts-by-range [from to]
  (apply concat (pmap #(get-posts-raw (str/join [url %])) (range from to))))

(defn to-str [post]
  (str/join ["Q: " (:q post) "\n\n" "A: " (:a post) "\n\n"]))


(defn get-qa-from-post [post]
  (let [content (:content (first (html/select post [:blockquote.postcontent])))]
    (reduce (fn [qas e]
          (if (= (get-in e [:attrs :class]) "bbcode_container")
            (let [q (strip (html/text e) "\t")]
              (conj qas {:q q :a ""}))
            (if (and (string? e) (> (count qas) 0))
              (let [qa (last qas)
                    a (str (:a qa) e)
                    nqa (assoc qa :a a)]
                (conj (pop qas) nqa))
              qas))) [] content)))

(defn get-qas [posts]
  (map get-qa-from-post
         (filter #(= (get-username %) "Kelei") posts)))

(defn -main []
  (spit "data.txt"
        (str/join
         (map to-str
              (filter-by-username "Kelei"
                                  (get-posts-by-range 50 51))))))
;(-main)

(def posts (get-posts-by-range 51 52))
