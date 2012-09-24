(ns synapse-admin.core
  (:use seesaw.core)
  (:import org.sagebionetworks.client.Synapse))

(defn display-main-page
  "Display the main admin page"
  [root]
  )

(defn validate-email [email]
  (re-matches #"(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$" email))

(defn login [event]
  "Fire a login event"
  (let [{:keys [username password]} (group-by-id (to-root event))
        username (value username)
        password (value password)
        synapse (Synapse.)]
    (cond (and (not (= "" password))
               (validate-email username))
          (try
            (.login synapse
                    username
                    password)
                    (alert "You've successfully logged in to Synapse!")
                    (catch
                        org.sagebionetworks.client.exceptions.SynapseBadRequestException ex
                      (alert "Unable to login, make sure your username/password are correct.")))
          (= "" password) (alert "Your password cannot be blank.")
          :else (alert "Please enter a valid email address"))))

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
                                  :listen [:action login])]))

(defn -main
  "Hello world, seesaw style!"
  [& args]
  (native!)
  (-> (frame :title "Synapse Administration") (config! :content (login-widget)) pack! show!))
