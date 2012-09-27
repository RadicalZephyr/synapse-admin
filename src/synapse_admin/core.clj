(ns synapse-admin.core
  (:use seesaw.core)
  (:import org.sagebionetworks.client.Synapse)
  (:import org.sagebionetworks.schema.adapter.org.json.JSONObjectAdapterImpl))

(defn has-method [obj method-name]
  (let [objMethods (.. obj (getClass) (getDeclaredMethods))]
    (contains? (set (map #(.getName %) objMethods)) method-name)))

(defn object->json [obj]
  (when (has-method obj "writeToJSONObject")
    (let [joa (JSONObjectAdapterImpl.)]
      (.. obj
          (writeToJSONObject joa)
          (toJSONString)))))

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
