(ns madoguchi.ssr-test
  (:require [clojure.test :refer [deftest is]]
            [shitsuke.hiccup :as hic]
            [madoguchi.ssr :as ssr]
            [madoguchi.views :as views]))

(deftest root-html-stable-test
  (let [html (ssr/root-html)]
    (is (clojure.string/starts-with? html "<!doctype html>"))
    (is (clojure.string/includes? html "Customer support"))
    (is (clojure.string/includes? html "Parka sizing"))
    (is (clojure.string/includes? html "Alice"))))

(deftest ssr-parity-test
  (is (= (hic/->html (views/root (ssr/sample-db)))
         (hic/->html (views/root (ssr/sample-db))))))
