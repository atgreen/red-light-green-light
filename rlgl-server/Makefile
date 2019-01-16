all:
	buildapp --output rlgl-server \
		--asdf-tree `pwd`/.. \
		--asdf-tree ~/quicklisp/dists/quicklisp/software \
		--load-system rlgl-server \
		--compress-core \
		--entry "rlgl-server:start-rlgl-server"

check:
	rm -rf test-policy
	sbcl --eval '(ql:quickload :prove)' \
	     --eval '(ql:quickload :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(prove:run #P"test.lisp")' \
	     --eval '(sb-ext:quit)'

cover:
	sbcl --eval '(require :sb-cover)' \
	     --eval '(ql:quickload :prove)' \
	     --eval '(declaim (optimize sb-cover:store-coverage-data))' \
	     --eval '(ql:quickload :rlgl-server)' \
	     --eval "(asdf:oos 'asdf:load-op :rlgl-server :force t)" \
	     --eval '(prove:run #P"test.lisp")' \
	     --eval '(sb-cover:report "./coverage/")' \
	     --eval '(sb-ext:quit)'

clean:
	@rm -f coverage/* rlgl-server *~

