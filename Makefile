all:
	buildapp --output rlgl-server \
		--asdf-tree `pwd`/.. \
		--asdf-tree `pwd`/3rd-party/cl-dbi \
		--asdf-tree `pwd`/3rd-party/cl-json-util \
		--asdf-tree ~/quicklisp/dists/quicklisp/software \
		--load-system rlgl-server \
		--compress-core \
		--entry "rlgl-server:start-rlgl-server"

check: clean
	sbcl --disable-debugger \
	     --eval '(ql:quickload :prove)' \
	     --eval '(pushnew (truename ".") ql:*local-project-directories* )' \
	     --eval '(pushnew (truename "./test/") ql:*local-project-directories* )' \
	     --eval '(ql:register-local-projects)' \
	     --eval '(ql:quickload :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(ql:quickload :test-rlgl-server)' \
	     --eval '(test-rlgl-server:run)' \
	     --eval '(sb-ext:quit)'

cover: clean
	sbcl --disable-debugger \
	     --eval '(require :sb-cover)' \
	     --eval '(ql:quickload :prove)' \
	     --eval '(declaim (optimize sb-cover:store-coverage-data))' \
	     --eval '(pushnew (truename ".") ql:*local-project-directories* )' \
	     --eval '(ql:register-local-projects)' \
	     --eval '(ql:quickload :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(prove:run #P"test.lisp")' \
	     --eval '(sb-cover:report "./coverage/")' \
	     --eval '(sb-ext:quit)'

coveralls: clean
	COVERALLS=true sbcl --disable-debugger \
		--eval '(ql:quickload :cl-coveralls)' \
		--eval '(pushnew (truename ".") ql:*local-project-directories* )' \
		--eval '(pushnew (truename "./test/") ql:*local-project-directories* )' \
		--eval '(ql:register-local-projects)' \
		--eval '(ql:quickload :test-rlgl-server)' \
		--eval '(coveralls:with-coveralls (:project-dir	(directory-namestring (truename "."))) (test-rlgl-server:run))' \
		--eval '(sb-ext:quit)'

clean:
	@rm -rf system-index.txt test/system-index.txt coverage/* test-policy rlgl-server *~ */*~
