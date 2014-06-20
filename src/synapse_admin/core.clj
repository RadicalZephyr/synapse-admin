(ns synapse-admin.core
  (:use [clojure.string :only (split lower-case)])
  (:use [clojure.data.json :only (read-json)])
  (:use [clojure.set :only (intersection)])
  (:use synapse-admin.client)
  (:import org.sagebionetworks.client.Synapse)
  (:import org.sagebionetworks.schema.adapter.org.json.JSONObjectAdapterImpl)
  (:import org.sagebionetworks.client.exceptions.SynapseNotFoundException))

(declare is-open-acl?)

(defn version-entity [syn entity-id]
  (let [entity (.getEntityById syn entity-id)
        old-md5 (.getMd5 entity)
        new-md5 (md5 (.getBytes old-md5))]
    (.setMd5 entity new-md5)
    (.putEntity syn entity)))

(defn public-or-auth-acl? [acl]
  (map #(if (= (:principalId %) 273948)
          ::Auth_Users ::Public)
       (filter #(or (= (:principalId %) 273948)
                    (= (:principalId %) 273949))
               (:resourceAccess acl))))

(defn classify-acl [acl]
  (if (is-open-acl? acl)
    (public-or-auth-acl? acl)
    ::Closed))

(defn entities->acl-parent [syn entities]
  (map #(let [id (:data.id %)]
          {:id id
           :access (classify-acl (get-effective-acl syn id))
           :parent (get-entity-parent syn id)})
       entities))

(defn get-open-entities [syn table user-id]
  (->>
   (paginate-query syn (str "select id from " table " where createdByPrincipalId == " user-id) 1000)
   (entities->acl-parent syn)
   (map #(assoc % :ownerId user-id))
   (filter #(not (= (:access %) ::Closed)))))

(defn get-users-open-data [syn user-list]
  (filter seq
          (map #(get-open-entities syn "data" (:ownerId %))
               user-list)))

(defn remove-owner-id [map-seq]
  (map #(dissoc % :ownerId) map-seq))

(defn extract-owner-id [map-seq]
  (:ownerId (first map-seq)))

(defn id->display-name [syn id]
  (try
    (->
     (.getUserProfile syn id)
     object->json
     :displayName)
    (catch SynapseNotFoundException e
      (str id))))

(defn transform-open-data [syn map-seq]
  (let [id (extract-owner-id map-seq)
        name (id->display-name syn id)]
    {:name name
     :data (remove-owner-id map-seq)}))

(defn data-to-vector [map-seq user]
  (map #(let [id (:id %)
              access (:access %)
              parent-id (:id (:parent %))]
          [user id parent-id access])
       map-seq))

(defn data-vectors-to-csv [data-vectors size]
  (->>
   (flatten data-vectors)
   (partition size)
   (map #(clojure.string/join "," %))
   (clojure.string/join "\n")))

(defn statistics-of-open-data [open-data]
  (let [num-users (count open-data)
        user-counts (map #(let [user (:name %)
                                data-count (count (:data %))]
                            {:name user
                             :data-count data-count})
                         open-data)]
    {:number-of-users num-users
     :counts-by-user user-counts}))

(defn filter-sage-employees [email-list]
  (let [sage-names (set (map #(lower-case
                               (first (split % #"@")))
                             (filter #(re-find #"(?i)@sagebase\.org" %)
                                     email-list)))
        special-filter #{"earthlingzephyr" "isjang" "xschildwachter"
                         "bennett.k.ng" "bruce_hoff" "mikerkellen"
                         "cbare" "metteptrs" "matthew.furia" "laramangravite"
                         "nicole.deflaux.guest"}]
    (->> email-list
         (filter #(not (re-find #"(?i)@sagebase\.org" %)))
         (filter #(not (contains? sage-names (lower-case (first (split % #"@"))))))
         (filter #(not (re-find #"(?i)@jayhodgson\.com" %)))
         (filter #(not (contains? special-filter (lower-case (first (split % #"@")))))))))

(defn has-read-access [acl]
  (contains? (set (:accessType acl)) "READ"))

(defn is-open-acl? [acl]
  (some has-read-access
        (filter #(or (= (:principalId %) 273948)
                     (= (:principalId %) 273949))
                (:resourceAccess acl))))

(defn acl->id-set [project-acls]
  (set (map #(or (:id %)
                 (:project.id %))
            project-acls)))

(defn partition-profiles [all-profiles]
  (let [all-emails (map :email all-profiles)
        non-sage-emails (set (filter-sage-employees all-emails))
        sage-profiles (filter #(not
                                (contains?
                                 non-sage-emails
                                 (:email %)))
                              all-profiles)
        non-sage-profiles (filter #(contains?
                                    non-sage-emails
                                    (:email %))
                                  all-profiles)]
    [sage-profiles non-sage-profiles]))

(defn synapse-stats [all-profiles all-projects all-project-acls]
  (let [[sage-profiles non-sage-profiles] (partition-profiles all-profiles)
        sage-id->email (apply merge
                              (map #(array-map (:ownerId %)
                                               (:email %))
                                   sage-profiles))
        sage-projects (filter #(contains? sage-id->email
                                          (str (:project.createdByPrincipalId %)))
                              all-projects)
        non-sage-id->email (apply merge
                                  (map #(array-map (:ownerId %)
                                                   (:email %))
                                       non-sage-profiles))
        non-sage-projects (filter #(contains? non-sage-id->email
                                              (str (:project.createdByPrincipalId %)))
                                  all-projects)
        open-projects (acl->id-set (filter is-open-acl? all-project-acls))
        sage-open (intersection open-projects (acl->id-set sage-projects))
        non-sage-open (intersection open-projects (acl->id-set non-sage-projects))

        closed-projects (acl->id-set (filter #(not (is-open-acl? %)) all-project-acls))
        sage-closed (intersection (acl->id-set sage-projects) closed-projects)
        non-sage-closed (intersection (acl->id-set non-sage-projects) closed-projects)]
    {:users {:sage (count sage-profiles)
             :non-sage (count non-sage-profiles)}
     :projects {:sage {:open (count sage-open)
                       :closed (count sage-closed)}
                :non-sage {:open (count non-sage-open)
                           :closed (count non-sage-closed)}
                :total {:all (count all-projects)
                        :open (count open-projects)
                        :closed (count closed-projects)}}}))

(defn get-all-projects [syn]
  (paginate-query syn "select * from project" 1024))

(defn get-all-acls [syn all-projects]
  (map #(object->json (try
                        (.getACL syn (:project.id %))
                        (catch SynapseNotFoundException e
                          %)))
       all-projects))

(defn get-all-users [syn] (object->json (.getUsers syn 0 10000)))

(defn get-owner-ids [users] (map #(:ownerId %) (:results users)))

(defn get-all-profiles [syn]
  (let [all-users (get-all-users syn)
        all-ids (get-owner-ids all-users)]
    (map #(object->json (.getUserProfile syn (str %))) all-ids)))

(defn collect-stats [syn]
  (let [profiles (future (get-all-profiles syn))
        projects (future (get-all-projects syn))
        project-acls (future (get-all-acls syn @projects))]
    (synapse-stats @profiles @projects @project-acls)))