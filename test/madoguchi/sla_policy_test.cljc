(ns madoguchi.sla-policy-test
  "SLA policy model: priority → target response/resolution times + breach imminent."
  (:require [clojure.test :refer [deftest is testing]]
            [madoguchi.ticket :as t]))

(deftest default-sla-targets-test
  (let [urgent (t/ticket {:id "t1" :priority :urgent})
        normal (t/ticket {:id "t2" :priority :normal})
        low (t/ticket {:id "t3" :priority :low})]
    (is (= 60 (t/response-target-mins urgent)))
    (is (= 240 (t/response-target-mins normal)))
    (is (= 480 (t/response-target-mins low)))
    (is (= 240 (t/resolution-target-mins urgent)))
    (is (= 1440 (t/resolution-target-mins normal)))
    (is (= 2880 (t/resolution-target-mins low)))))

(deftest sla-rule-fallback-test
  (let [unknown (t/ticket {:id "t1" :priority :weird})]
    (is (= 240 (t/response-target-mins unknown)))    ; falls back to :normal
    (is (= 1440 (t/resolution-target-mins unknown)))))

(deftest breach-imminent-test
  (let [tk (t/ticket {:id "t1" :priority :normal :sla-due "2026-06-30T10:00:00"})]
    (is (t/sla-breach-imminent? (t/open tk) "2026-06-30T10:30:00"))   ; past due
    (is (not (t/sla-breach-imminent? (t/resolve-ticket (t/open tk)) "2026-06-30T10:30:00"))))) ; resolved
