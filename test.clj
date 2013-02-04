;; test code to help figure out how to run this stuff

(def synapse (org.sagebionetworks.client.Synapse.))
(def response (.login synapse "christopherbare@gmail.com", "san juan island"))

(require ['synapse-admin.core :refer :all :reload true])
(require ['synapse-admin.client :refer :all :reload true])

(def all-users (get-all-users synapse))
(def all-ids (get-owner-ids all-users))

(keys all-users)
; (:totalNumberOfResults :results :paging)

;; how many users are there?
(:totalNumberOfResults all-users)
; 915

;; doesn't work???
(def all-profiles [ids] (map #(object->json (.getUserProfile synapse (str %))) ids))


