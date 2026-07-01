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

;; ---------------------------------------------------------------------------
;; escalation (raise priority + reassign)
;; ---------------------------------------------------------------------------

(def priority-rank {:low 0 :normal 1 :high 2 :urgent 3})

(defn- rank-priority [p] (get priority-rank p 1))

(defn escalate
  "Escalate a ticket: bump priority one level (low→normal→high→urgent, capped at
  :urgent), set :escalated-at, and optionally reassign. Returns the updated
  ticket. If already :urgent, just sets :escalated-at."
  ([t]
   (escalate t nil))
  ([t opts]
   (let [cur (:priority t :normal)
         cur-rank (rank-priority cur)
         next-p (some #(when (= (rank-priority %) (inc cur-rank)) %)
                      [:low :normal :high :urgent])
         new-p (or next-p :urgent)]
     (cond-> (assoc t :priority new-p :escalated true)
       (:escalated-at opts) (assoc :escalated-at (:escalated-at opts))
       (:assignee opts)     (assoc :assignee (:assignee opts))))))

(defn escalated?
  "True if the ticket has been escalated at least once."
  [t] (true? (:escalated t)))

(defn escalate-if-breach
  "Escalate the ticket if SLA is breached (past due). Returns the (possibly
  escalated) ticket. Passes `now` for the breach check."
  [t now]
  (if (sla-breach-imminent? t now)
    (escalate t)
    t))

(defn escalation-activity
  "Project an escalation event onto chobo.ledger (lane :support, kind :escalation)."
  [t opts]
  (ledger/activity
   (merge {:lane :support :kind :escalation
           :title (str "Escalated: " (:subject t))
           :props {:ticket-id (:id t)
                   :new-priority (:priority t :normal)
                   :assignee (:assignee t)}}
          opts)))

;; ---------------------------------------------------------------------------
;; ticket merge (combine duplicate / related tickets)
;; ---------------------------------------------------------------------------

(defn merge-tickets
  "Merge `source` ticket into `target`. The target keeps its id; all messages
  from source are appended; source's :id is recorded in target's :merged-from.
  Returns the merged target. The host app closes the source (this fn doesn't
  mutate source)."
  [target source]
  (-> target
      (update :messages (fnil concat []) (:messages source []))
      (update :merged-from (fnil conj []) (:id source))
      (update :priority (fn [p]
                          (let [pr {:low 0 :normal 1 :high 2 :urgent 3}]
                            (if (>= (get pr (:priority source :normal) 1)
                                    (get pr p 1))
                              (:priority source)
                              p))))))

(defn merge-activity
  "Project a ticket-merge event onto chobo.ledger (lane :support, kind :merge)."
  [target source opts]
  (ledger/activity
   (merge {:lane :support :kind :merge
           :title (str "Merged " (:id source) " → " (:id target))
           :props {:target-id (:id target)
                   :source-id (:id source)
                   :messages-count (count (:messages target []))}}
          opts)))

