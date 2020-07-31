FROM jboss/keycloak:latest
MAINTAINER green@moxielogic.com

USER jboss

EXPOSE 8080

CMD ["-Dkeycloak.migration.action=import", "-Dkeycloak.profile.feature.scripts=enabled", "-Dkeycloak.profile.feature.upload_scripts=enabled", "-Dkeycloak.migration.provider=singleFile", "-Dkeycloak.migration.realmName=dev", "-Dkeycloak.migration.file=/opt/jboss/keycloak/rlgl/keycloak-realm.json", "-Dkeycloak.migration.strategy=OVERWRITE_EXISTING", "-b", "0.0.0.0"]
