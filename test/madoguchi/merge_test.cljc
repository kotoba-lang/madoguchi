(ns madoguchi.merge-test
  "Ticket merge: combine duplicate/related tickets."
  (:require [clojure.test :refer [deftest is testing]]
            [madoguchi.ticket :as t]))

(def target (-> (t/ticket {:id "t1" :customer "u1" :subject "Sizing" :priority :normal})
                (t/add-message {:from :customer :body "Is M true to size?" :timestamp 1})))
(def source (-> (t/ticket {:id "t2" :customer "u1" :subject "Sizing follow-up" :priority :high})
                (t/add-message {:from :customer :body "Also, the sleeves?" :timestamp 2})))

(deftest merge-tickets-test
  (let [merged (t/merge-tickets target source)]
    (is (= "t1" (:id merged)))
    (is (= 2 (count (:messages merged))))               ; original 1 + source 1
    (is (= ["t2"] (:merged-from merged)))
    ;; priority upgraded to source's :high (higher than target's :normal)
    (is (= :high (:priority merged)))))

(deftest merge-no-priority-downgrade-test
  (let [high-target (assoc target :priority :urgent)
        merged (t/merge-tickets high-target source)]
    (is (= :urgent (:priority merged)))))               ; urgent > high → no downgrade

(deftest merge-activity-test
  (let [a (t/merge-activity target source {:tenant "gftd"})]
    (is (= :support (:lane a)))
    (is (= :merge (:kind a)))
    (is (= "gftd" (:tenant a)))
    (is (= "t1" (get-in a [:props :target-id])))
    (is (= "t2" (get-in a [:props :source-id])))))
