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

(defn get-root-project [entity-path]
  (let [path (:path ep)]
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

(defn entities->acl-parent [syn entities]
  (map #(let [id (:data.id %)]
          [(get-effective-acl syn id)
           (get-entity-parent syn id)])
       entities))

(defn get-open-data [syn user-id]
  (->>
   (query syn (str "select * from data where createdByPrincipalId == " user-id))
   (entities->acl-parent syn)))

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

(defn is-open-acl [acl]
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
        open-projects (acl->id-set (filter is-open-acl all-project-acls))
        sage-open (intersection open-projects (acl->id-set sage-projects))
        non-sage-open (intersection open-projects (acl->id-set non-sage-projects))

        closed-projects (acl->id-set (filter #(not (is-open-acl %)) all-project-acls))
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
