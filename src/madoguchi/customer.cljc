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
