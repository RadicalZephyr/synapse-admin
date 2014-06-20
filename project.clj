(defproject synapse-admin "0.1.0-SNAPSHOT"
  :description "An administration client for Synapse"
  :url "http://github.com/ezephyr/synapse-admin"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.3"]
                 [seesaw "1.4.2"]
                 [org.sagebionetworks/synapseJavaClient "47.0-7-g5a31e17"]]
  :repositories [["sagebionetworks-releases-local"
                  "http://sagebionetworks.artifactoryonline.com/sagebionetworks/libs-releases-local"]])
