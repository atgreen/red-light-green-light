all: rlgl

SBCL_REGISTRY := --eval "(asdf:initialize-source-registry \`(:source-registry :inherit-configuration (:tree ,(uiop:getcwd))))"

# Build the standalone rlgl binary.
rlgl: *.lisp */*.lisp *.asd
	sbcl $(SBCL_REGISTRY) --eval "(asdf:make :rlgl)" --eval "(quit)"

# Run the test suite against the engine (no server required).
check:
	sbcl --dynamic-space-size 4096 \
	     --disable-debugger \
	     $(SBCL_REGISTRY) \
	     --eval '(asdf:load-system :prove)' \
	     --eval '(asdf:load-system :rlgl)' \
	     --eval '(asdf:load-system :test-rlgl)' \
	     --eval '(test-rlgl:run)' \
	     --eval '(sb-ext:quit)'

clean:
	@rm -rf system-index.txt test/system-index.txt coverage/* rlgl *~ */*~
