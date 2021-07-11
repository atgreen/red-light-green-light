(defpackage #:rlgl.user
  (:use #:cl)
  (:shadow #:package)
  (:export #:make-user #:user-api-key #:user-name #:find-user-by-oidc-id #:find-user-by-keycloak-id-token))
