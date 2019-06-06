FROM containerlisp/lisp-10-centos7

COPY . /tmp/src
RUN APP_SYSTEM_NAME=rlgl-server /usr/libexec/s2i/assemble
USER 0
RUN mkdir -p /var/rlgl/docs /var/rlgl/policy && chown -R 1001:0 /var/rlgl
USER 1001
CMD APP_SYSTEM_NAME=rlgl-server APP_EVAL="\"(rlgl-server:start-rlgl-server t)\"" /usr/libexec/s2i/run
