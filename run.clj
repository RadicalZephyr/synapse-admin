(require 'synapse-admin.core)
(require 'synapse-admin.client)
(require 'synapse-admin.gui)

; login with a Synapse administrator account
(synapse-admin.gui/-main)

; calculate stats
(def results (synapse-admin.core/collect-stats synapse-admin.gui/*synapse-client*))
results


; individual parts, if you want 'em
;(def allProfiles (synapse-admin.core/get-all-profiles synapse-admin.gui/*synapse-client*))
;(def allProjects (synapse-admin.core/get-all-projects synapse-admin.gui/*synapse-client*))
;(def allProjectAcls (synapse-admin.core/get-all-acls synapse-admin.gui/*synapse-client* allProjects))
;(def results (synapse-admin.core/synapse-stats allProfiles allProjects allProjectAcls))
