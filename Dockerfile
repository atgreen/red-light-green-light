FROM quay.io/moxielogic/rlgl-server-base:latest

COPY . /tmp/src
ARG RLGL_VERSION=RLGL_VERSION
ENV RLGL_VERSION=${RLGL_VERSION}
RUN APP_SYSTEM_NAME=rlgl-server /usr/libexec/s2i/assemble
USER 0
RUN dnf install -y file poppler-utils gpg && dnf clean all
RUN mkdir -p /var/rlgl/docs /var/rlgl/policy && chown -R 1001:0 /var/rlgl && chmod -R go+rwx /var/rlgl
USER 1001
CMD DEV_BACKEND=slynk APP_SYSTEM_NAME=rlgl-server APP_EVAL="\"(progn (rlgl-server:start-rlgl-server) (loop do (sleep 1000)))\"" /usr/libexec/s2i/run
