(ns clj-forum-qa.core
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;;; Const

(def url "http://forum.bodybuilding.com/showthread.php?t=146471313&page=")

;;; Utils

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn strip [coll chars]
  (apply str (remove #((set chars) %) coll)))

(defn rstrip-nt [text]
  (str/join
   (reverse
    (drop-while
     #(or (= \tab %) (= \newline %))
     (reverse text)))))

(defn sanitize [text]
  (first
   (str/split
    (second
     (str/split
      (subs text 25)
      #"\n\t\t\t\t\t\n\t\t\t\t\n\t\t\t\t"))
    #"\n\t\t\t\n\t\t\n\t")))

(defn to-str [post]
  (str/join ["Q: " (sanitize (:q post)) "\n\n" "A: " (:a post) "\n\n"]))

;;; Main

(defn get-username [data]
  (first (html/select data [:a.username :> :strong :> html/text-node])))

(defn get-posts [url]
  (let [data (fetch-url url)
        raw (html/select data [:ol.posts :> :li])]
    raw))

(defn get-posts-by-range [from to]
  (apply concat (pmap #(get-posts (str/join [url %])) (range from to))))

(defn get-qa-from-post [post]
  (let [content (:content (first (html/select post [:blockquote.postcontent])))]
    (reduce (fn [qas e]
          (if (= (get-in e [:attrs :class]) "bbcode_container")
            (let [q (html/text e)]
              (conj qas {:q q :a ""}))
            (if (and (string? e) (> (count qas) 0))
              (let [qa (last qas)
                    a (str (:a qa) (rstrip-nt e))
                    nqa (assoc qa :a a)]
                (conj (pop qas) nqa))
              qas))) [] content)))

(defn get-qas [posts]
  (first
   (map get-qa-from-post
         (filter #(= (get-username %) "Kelei") posts))))

(defn -main []
  (spit "data.txt"
        (str/join
         (map to-str
              (get-qas (get-posts-by-range 47 52))))))
