(ns madoguchi.customer
  "Customer / CRM model (pure). Customer{:id :name :email :tags :order-refs
  :lifetime-value :contacts}. Contact log records inbound/outbound interactions."

  (:refer-clojure :exclude [empty]))

(defrecord Customer [id name email tags order-refs lifetime-value contacts])
(defrecord Contact [id customer channel direction subject timestamp])

(defn customer [m] (merge {:tags #{} :order-refs [] :contacts [] :lifetime-value 0} m))

(defn tag [c t] (update c :tags (fnil conj #{}) t))
(defn untag [c t] (update c :tags disj t))

(defn add-contact [c contact]
  (update c :contacts conj (map->Contact contact)))

(defn contacts-by-channel [c channel] (filterv #(= (:channel %) channel) (:contacts c [])))

(defn add-order-ref [c order-id]
  (update c :order-refs (fnil conj []) order-id))

(defn add-lifetime-value [c amount]
  (update c :lifetime-value (fnil + 0) (max 0 amount)))

;; ---------------------------------------------------------------------------
;; customer 360 stats (derived from the customer record)
;; ---------------------------------------------------------------------------

(defn order-count [c] (count (:order-refs c [])))

(defn last-contact
  "The most recent contact (by :timestamp). nil if no contacts."
  [c]
  (let [contacts (:contacts c [])]
    (when (seq contacts)
      (last (sort-by :timestamp contacts)))))

(defn last-contact-at
  "Timestamp of the most recent contact, or nil."
  [c]
  (:timestamp (last-contact c)))

(defn contact-count [c] (count (:contacts c [])))

(defn avg-order-value
  "Average lifetime value per order. nil if no orders."
  [c]
  (let [n (order-count c)]
    (when (pos? n)
      (/ (:lifetime-value c 0) n))))

(defn customer-360
  "Build a customer 360 snapshot: derived stats from the customer record."
  [c]
  {:id (:id c)
   :name (:name c)
   :email (:email c)
   :tags (:tags c #{})
   :order-count (order-count c)
   :lifetime-value (:lifetime-value c 0)
   :avg-order-value (avg-order-value c)
   :contact-count (contact-count c)
   :last-contact-at (last-contact-at c)})

(defn vip?
  "True if the customer is tagged :vip or has LTV above a threshold (default
  100000)."
  ([c]
   (vip? c 100000))
  ([c threshold]
   (or (contains? (:tags c #{}) :vip)
       (>= (:lifetime-value c 0) threshold))))
