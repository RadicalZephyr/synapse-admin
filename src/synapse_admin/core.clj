(ns synapse-admin.core
  (:use seesaw.core)
  (:import org.sagebionetworks.client.Synapse))

(defn fill-frame
  ""
  [frame]
  frame)

(defn -main
  "Hello world, seesaw style!"
  [& args]
  (native!)
  (-> (frame :title "Hello" :content "Hey world!") fill-frame pack! show!))
