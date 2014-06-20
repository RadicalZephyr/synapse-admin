(ns synapse-admin.client
  (:use [clojure.data.json :only (read-json)])
  (:import java.security.MessageDigest
           org.sagebionetworks.schema.adapter.org.json.JSONObjectAdapterImpl
           org.sagebionetworks.client.exceptions.SynapseNotFoundException))

(defn md5 [bytes]
  (let [digest (.digest (MessageDigest/getInstance "md5") bytes)]
    (.substring (format "%032x" (BigInteger. 1 digest)) 0 32)))

(defn list-methods [obj]
  (when obj
    (let [objMethods (.. obj (getClass) (getDeclaredMethods))]
      (map #(.getName %) objMethods))))

(defn has-method [obj method-name]
  (when obj
    (let [objMethods (.. obj (getClass) (getDeclaredMethods))]
      (contains? (set (list-methods obj)) method-name))))

(defn object->json [obj]
  (if (has-method obj "writeToJSONObject")
    (let [joa (JSONObjectAdapterImpl.)]
      (read-json (.. obj
                     (writeToJSONObject joa)
                     (toJSONString))))
    obj))

(defn query [syn query-string]
  (-> syn
      (.query query-string)
      .toString
      read-json
      :results))

(defn query-size [syn query-string]
  (-> syn
      (.query (str query-string " LIMIT 1"))
      .toString
      read-json
      :totalNumberOfResults))

(defn paginate-query [syn query-string page-size]
  (let [total (query-size syn query-string)]
    (loop [offset 1
           results []]
      (if (> total offset)
        (recur (+ offset page-size 1)
               (conj results
                     (query syn (str query-string
                                     " LIMIT "
                                     page-size
                                     " OFFSET "
                                     offset))))
        (flatten results)))))

(defn get-root-project [entity-path]
  (let [path (:path entity-path)]
    (if (= (:name (first path)) "root")
      (second path)
      (first path))))

(defn get-entity-parent [syn entity-id]
  (->
   (.getEntityPath syn entity-id)
   object->json
   get-root-project
   (dissoc :type)))

(defn get-effective-acl [syn entity-id]
  (->>
   (.getEntityBenefactor syn entity-id)
   object->json
   :id
   (.getACL syn)
   object->json))
