(ns synapse-admin.core
  (:use seesaw.core)
  (:use [clojure.string :only (split lower-case)])
  (:use [clojure.data.json :only (read-json json-str)])
  (:use [clojure.set :only (intersection difference)])
  (:import org.sagebionetworks.client.Synapse)
  (:import org.sagebionetworks.schema.adapter.org.json.JSONObjectAdapterImpl)
  (:import org.sagebionetworks.client.exceptions.SynapseNotFoundException))

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

(declare is-open-acl?)

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

(defn get-open-data [syn user-id]
  (->>
   (paginate-query syn (str "select id from data where createdByPrincipalId == " user-id) 1000)
   (entities->acl-parent syn)
   (map #(assoc % :ownerId user-id))
   (filter #(not (= (:access %) ::Closed)))))

(defn get-users-open-data [syn user-list]
  (filter seq
          (map #(get-open-data syn (:ownerId %))
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
         (filter #(not (contains? sage-names (lower-case (first (clojure.string/split % #"@"))))))
         (filter #(not (re-find #"(?i)@jayhodgson\.com" %)))
         (filter #(not (contains? special-filter (lower-case (first (clojure.string/split % #"@")))))))))

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
  (:results
   (read-json
    (->
     syn
     (.query "select * from project")
     .toString))))

(defn get-all-project-acls [syn all-projects]
  (map #(object->json (try
                        (.getACL syn (:project.id %))
                        (catch SynapseNotFoundException e
                          %)))
       all-projects))

(defn get-all-profiles [syn]
  (let [all-users (object->json (.getUsers syn 0 10000))
        all-ids (map #(:ownerId %) (:results all-users))]
    (map #(object->json (.getUserProfile syn (str %))) all-ids)))

(def ^:dynamic *synapse-client*
  "The main synapse client used by the application")

(def ^:dynamic *synapse-credentials*
  "The credentials of the currently logged in user")

(defn display-main-page
  "Display the main admin page"
  [root]
  (invoke-later
   ))

(defn validate-email [email]
  (re-matches #"(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$" email))

(defn login [synapse username password]
  (def ^:dynamic *synapse-client* synapse)
  (def ^:dynamic *synapse-credentials* {:username username :password password}))

(defn logout []
  (def ^:dynamic *synapse-client* nil)
  (def ^:dynamic *synapse-credentials* nil))

(defn attempt-login [event]
  "Fire a login event"
  (let [root (to-root event)
        {:keys [username password]} (group-by-id root)
        username (value username)
        password (value password)
        synapse (Synapse.)]
    (cond (and (not (= "" password))
               (validate-email username))
          (try
            (.login synapse
                    username
                    password)
            (login synapse username password)
            (alert root "You've successfully logged in to Synapse!")
            (display-main-page root)
            (catch
                org.sagebionetworks.client.exceptions.SynapseBadRequestException ex
              (alert root "Unable to login, make sure your username/password are correct.")))
          (= "" password) (alert root "Your password cannot be blank.")
          :else (alert root "Please enter a valid email address"))))

(defn login-widget
  "Create the login widget"
  []
  (grid-panel :border "Enter credentials"
              :columns 2
              :vgap    10
              :items ["Email:" (text :id :username
                                     :columns 15)
                      "Password:" (password :id :password
                                            :columns 15)
                      " " (button :text "Login"
                                  :mnemonic \n
                                  :listen [:action attempt-login])]))

(defn -main
  "Hello world, seesaw style!"
  [& args]
  (native!)
  (invoke-later
   (-> (frame :title "Synapse Administration")
       (config! :content (login-widget))
       pack!
       show!)))
