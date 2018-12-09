run:
	buildapp --output rlgl-server \
		--asdf-path `pwd`/.. \
		--asdf-tree ~/quicklisp/dists/quicklisp/software \
		--load-system rlgl-server \
		--compress-core \
		--entry "rlgl-server:start-rlgl-server"

check:
	sbcl --eval '(require :sb-cover)' \
	     --eval '(declaim (optimize sb-cover:store-coverage-data))' \
	     --eval '(ql:quickload :prove)' \
	     --eval '(ql:quickload :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :prove :force t)" \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(prove:run #P"test.lisp")' \
	     --eval '(sb-cover:report "./")' \
	     --eval '(sb-ext:quit)'

#	sbcl --eval '(progn (require "asdf") (push "/home/green/git/red-light-green-light/rlgl-server/rlgl-server-test/" asdf:*central-registry*) (asdf:load-system :rlgl-server-test))'

