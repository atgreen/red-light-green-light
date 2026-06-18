all: rlgl

# Load ASDF explicitly (not all SBCL builds preload it) before configuring
# the source registry to include the project tree and its vendored ocicl/.
SBCL_INIT := --eval "(require :asdf)" \
             --eval "(asdf:initialize-source-registry \`(:source-registry :inherit-configuration (:tree ,(uiop:getcwd))))"

# Build the standalone rlgl binary.  --non-interactive makes a build error
# exit non-zero instead of dropping into the debugger and reporting success.
rlgl: *.lisp */*.lisp *.asd
	sbcl --non-interactive $(SBCL_INIT) --eval "(asdf:make :rlgl)"

# Run the test suite against the engine (no server required).
check:
	sbcl --dynamic-space-size 4096 \
	     --disable-debugger \
	     $(SBCL_INIT) \
	     --eval '(asdf:load-system :prove)' \
	     --eval '(asdf:load-system :rlgl)' \
	     --eval '(asdf:load-system :test-rlgl)' \
	     --eval '(test-rlgl:run)' \
	     --eval '(sb-ext:quit)'

clean:
	@rm -rf system-index.txt test/system-index.txt coverage/* rlgl *~ */*~
