(ns synapse-admin.core
  (:use seesaw.core)
  (:use [clojure.string :only (split lower-case)])
  (:use [clojure.data.json :only (read-json json-str)])
  (:import org.sagebionetworks.client.Synapse)
  (:import org.sagebionetworks.schema.adapter.org.json.JSONObjectAdapterImpl))

(defn list-methods [obj]
  (let [objMethods (.. obj (getClass) (getDeclaredMethods))]
    (map #(.getName %) objMethods)))

(defn has-method [obj method-name]
  (let [objMethods (.. obj (getClass) (getDeclaredMethods))]
    (contains? (set (list-methods obj)) method-name)))

(defn object->json [obj]
  (if (has-method obj "writeToJSONObject")
    (let [joa (JSONObjectAdapterImpl.)]
      (read-json (.. obj
                     (writeToJSONObject joa)
                     (toJSONString))))
  obj))

(defn filter-sage-employees [email-list]
  (let [sage-names (set (map #(lower-case
                               (first (split % #"@")))
                             (filter #(re-find #"(?i)@sagebase\.org" %)
                                     email-list)))
        special-filter #{"earthlingzephyr" "isjang" "xschildwachter"
                         "bennett.k.ng" "bruce_hoff" "mikerkellen"
                         "cbare" "metteptrs" "matthew.furia" "laramangravite"
                         "nicole.deflaux.guest" "wangz"}]
    (->> email-list
         (filter #(not (re-find #"(?i)@sagebase\.org" %)))
         (filter #(not (contains? sage-names (lower-case (first (clojure.string/split % #"@"))))))
         (filter #(not (re-find #"(?i)@jayhodgson\.com" %)))
         (filter #(not (contains? special-filter (lower-case (first (clojure.string/split % #"@")))))))))

(defn project-stats [all-profiles all-projects]
  (let [all-emails (map :email all-profiles)
        non-sage-emails (set (filter-sage-employees all-emails))
        sage-profiles (filter #(not
                                (contains?
                                 non-sage-emails
                                 (:email %)))
                              all-profiles)
        sage-id->email (apply merge
                              (map #(array-map (:ownerId %)
                                               (:email %))
                                   sage-profiles))
        sage-projects (filter #(contains? sage-id->email
                                          (str (:project.createdByPrincipalId %)))
                              all-projects)
        non-sage-profiles (filter #(contains?
                                    non-sage-emails
                                    (:email %))
                                  all-profiles)
        non-sage-id->email (apply merge
                                  (map #(array-map (:ownerId %)
                                                   (:email %))
                                       non-sage-profiles))
        non-sage-projects (filter #(contains? non-sage-id->email
                                          (str (:project.createdByPrincipalId %)))
                                  all-projects)
        other-projects (filter #(not (or (contains? non-sage-id->email
                                                    (str (:project.createdByPrincipalId %)))
                                         (contains? sage-id->email
                                                    (str (:project.createdByPrincipalId %)))))
                               all-projects)]
    {:sage (count sage-projects)
     :non-sage (count non-sage-projects)
     :other {:count (count other-projects) :projects other-projects}
     :total (count all-projects)}))


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
