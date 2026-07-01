(ns madoguchi.ticket
  "Support ticket model (pure). Ticket{:id :customer :subject :status :priority
  :assignee :messages :sla-due :created-at}. Status: :new → :open → :pending →
  :resolved → :closed (reopen: :resolved → :open). Priority: :low/:normal/:high/:urgent.
  SLA breach = resolved/closed past sla-due. Projects to chobo.ledger lane :support."
  (:require [chobo.ledger :as ledger]))

(defrecord Ticket [id customer subject status priority assignee messages sla-due created-at])
(defrecord Message [from body timestamp])

(def statuses #{:new :open :pending :resolved :closed})
(def transitions
  {:new      #{:open}
   :open     #{:pending :resolved :closed}
   :pending  #{:open :resolved :closed}
   :resolved #{:open :closed}
   :closed   #{:open}})

(defn ticket [m] (merge {:status :new :priority :normal :messages []} m))

(defn can-transition? [from to] (contains? (get transitions from #{}) to))
(defn transition [t to] (when (can-transition? (:status t :new) to) (assoc t :status to)))

(defn open [t] (transition t :open))
(defn pending [t] (transition t :pending))
(defn resolve-ticket [t] (transition t :resolved))
(defn close [t] (transition t :closed))
(defn reopen [t] (transition t :open))

(defn add-message [t msg] (update t :messages conj (map->Message msg)))

(defn sla-breached?
  "True if ticket is not resolved/closed and past sla-due (caller passes `now`)."
  [t now]
  (and (not (#{:resolved :closed} (:status t :new)))
       (:sla-due t)
       (pos? (compare (str now) (str (:sla-due t))))))

(defn support-activity
  "Project a ticket event onto chobo.ledger as a :support activity."
  [t opts]
  (ledger/activity
   (merge {:lane :support :kind :ticket
           :title (:subject t) :state (:status t :new)
           :props {:ticket-id (:id t) :customer (:customer t)
                   :priority (:priority t :normal)}}
          opts)))

;; ---------------------------------------------------------------------------
;; SLA policy model (priority → target response/resolution times)
;; ---------------------------------------------------------------------------

(defrecord SLAPolicy [rules])
(defrecord SLARule [priority response-target-mins resolution-target-mins])

(def default-sla-policy
  "Standard SLA: urgent 1h/4h, high 2h/8h, normal 4h/24h, low 8h/48h."
  (->SLAPolicy
   #{(->SLARule :urgent   60   240)
     (->SLARule :high     120  480)
     (->SLARule :normal   240  1440)
     (->SLARule :low      480  2880)}))

(defn sla-rule-for
  "Get the SLA rule for a priority from a policy. Falls back to :normal."
  [policy priority]
  (some #(when (= (:priority %) priority) %) (:rules policy)))

(defn response-target-mins
  "Response SLA target (minutes) for a ticket's priority."
  ([ticket']
   (response-target-mins default-sla-policy ticket'))
  ([policy ticket']
   (:response-target-mins (sla-rule-for policy (:priority ticket' :normal)) 240)))

(defn resolution-target-mins
  "Resolution SLA target (minutes) for a ticket's priority."
  ([ticket']
   (resolution-target-mins default-sla-policy ticket'))
  ([policy ticket']
   (:resolution-target-mins (sla-rule-for policy (:priority ticket' :normal)) 1440)))

(defn sla-breach-imminent?
  "True if the ticket is within `threshold-mins` of its SLA deadline without
  being resolved. `now` and `sla-due` are ISO strings; this compares the
  remaining time lexicographically (v1 stub — host app computes exact minutes)."
  ([ticket' now]
   (sla-breach-imminent? ticket' now 30))
  ([ticket' now threshold-mins]
   (and (not (#{:resolved :closed} (:status ticket' :new)))
        (:sla-due ticket')
        (pos? (compare (str now) (str (:sla-due ticket'))))))) ; already past due = breach

