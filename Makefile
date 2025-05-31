all:

rlgl-server: *.lisp */*.lisp *.asd
	sbcl --eval "(asdf:initialize-source-registry \`(:source-registry :inherit-configuration (:tree ,(uiop:getcwd))))" --eval "(asdf:make :rlgl-server)" --eval "(quit)"

check: clean
	openssl genrsa -out /tmp/rlgl-test-key.pem 1024
	openssl rsa -in /tmp/rlgl-test-key.pem -out /tmp/rlgl-public-test-key.pem -outform PEM -pubout
	PRIVATE_KEY_FILE=/tmp/rlgl-test-key.pem \
	PUBLIC_KEY_FILE=/tmp/rlgl-public-test-key.pem \
	      sbcl --dynamic-space-size 4096 \
	     --disable-debugger \
	     --eval "(asdf:initialize-source-registry \`(:source-registry :inherit-configuration (:tree ,(uiop:getcwd))))" \
	     --eval '(asdf:load-system :prove)' \
	     --eval '(asdf:load-system :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(asdf:load-system :test-rlgl-server)' \
	     --eval '(test-rlgl-server:run)' \
	     --eval '(sb-ext:quit)'

podman-check: rlgl-server
	podman-compose -f .ci/docker-compose.yml down || true
	podman container rm -f ci_rlgl-server_1 || true
	podman build -t rlgl-server -f .ci/Containerfile .
	openssl genrsa -out /tmp/rlgl-test-key.pem 1024
	openssl rsa -in /tmp/rlgl-test-key.pem -out /tmp/rlgl-public-test-key.pem -outform PEM -pubout
	podman-compose -f .ci/podman-compose.yml up -d
	./.ci/run-tests.sh

run: clean
	openssl genrsa -out /tmp/rlgl-test-key.pem 1024
	PRIVATE_KEY_FILE=/tmp/rlgl-test-key.pem \
	  	  sbcl --dynamic-space-size 4096 \
	     --eval '(pushnew (truename ".") asdf:*central-registry* )' \
	     --eval '(pushnew (truename "./user") asdf:*central-registry* )' \
	     --eval '(pushnew (truename "./util") asdf:*central-registry* )' \
	     --eval '(pushnew (truename "./db") asdf:*central-registry* )' \
	     --eval '(pushnew (truename "./parsers") asdf:*central-registry* )' \
	     --eval '(pushnew (truename "./test/") asdf:*central-registry* )' \
	     --eval '(asdf:load-system :rlgl-server)' \
	     --eval '(rlgl-server:start-rlgl-server t "test/config.ini")'

cover: clean
	sbcl --disable-debugger \
	   	 --eval '(require :sb-cover)' \
	   	 --eval '(ql:quickload :prove)' \
	   	 --eval '(declaim (optimize sb-cover:store-coverage-data))' \
	   	 --eval '(pushnew (truename ".") asdf:*central-registry* )' \
       --eval '(ql:register-local-projects)' \
	     --eval '(ql:quickload :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(prove:run #P"test.lisp")' \
	     --eval '(sb-cover:report "./coverage/")' \
	     --eval '(sb-ext:quit)'

coveralls: clean
	COVERALLS=true sbcl --disable-debugger \
	  --eval '(ql:quickload :cl-coveralls)' \
	  --eval '(pushnew (truename ".") asdf:*central-registry* )' \
	  --eval '(pushnew (truename "./test/") asdf:*central-registry*)' \
	  --eval '(ql:register-local-projects)' \
	  --eval '(ql:quickload :test-rlgl-server)' \
	  --eval '(coveralls:with-coveralls (:project-dir (directory-namestring (truename "."))) (test-rlgl-server:run))' \
	  --eval '(sb-ext:quit)'

clean:
	@rm -rf system-index.txt test/system-index.txt coverage/* test-policy rlgl-server *~ */*~
