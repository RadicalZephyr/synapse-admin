(ns synapse-admin.core
  (:use seesaw.core)
  (:import org.sagebionetworks.client.Synapse))

(defn login [event]
  (let [{:keys [username password]} (group-by-id (to-root event))
        synapse (Synapse.)]
    (try
      (.login synapse
            (config username :text)
            (config password :text))
      (alert "You've successfully logged in to Synapse!")
      (catch
          org.sagebionetworks.client.exceptions.SynapseBadRequestException ex
        (alert ex)))))

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

(defn fill-frame
  ""
  [frame]
  (config! frame :content (login-widget)))

(defn -main
  "Hello world, seesaw style!"
  [& args]
  (native!)
  (-> (frame :title "Synapse Administration") fill-frame pack! show!))
