FROM containerlisp/lisp-10-centos7

COPY . /tmp/src
RUN APP_SYSTEM_NAME=rlgl-server /usr/libexec/s2i/assemble
RUN mkdir -p /var/rlgl/docs /var/rlgl/policy
CMD APP_SYSTEM_NAME=rlgl-server APP_EVAL="\"(rlgl-server:start-rlgl-server t)\"" /usr/libexec/s2i/run


