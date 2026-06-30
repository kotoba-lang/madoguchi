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
