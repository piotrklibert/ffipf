
ffipf_backend.so: src/ffipf_backend.nim src/fuzzy_file_finder.nim src/emacs_module.nim src/emacs_helpers.nim Makefile
	nim c \
	  -d:nimMaxHeap=40 \
	  --nimcache:nimcache \
	  --app:lib \
	  --out:ffipf_backend.so \
	  --opt:speed \
	  --gc:arc \
	  src/ffipf_backend.nim

#	  -d:debugLogging \
#	  --debugInfo:on \
#	  --lineTrace:on \
#	  --debugger:native \
#	  --stackTrace:on \
#	  --checks:on \
#	  --assertions:on \
#	  --embedsrc:on \
#	  -d:release \

ffipf: src/fuzzy_file_finder.nim
	nimble c -d:release --opt:speed --out:ffipf src/fuzzy_file_finder.nim

test: ffipf_backend.so elisp/ffipf.el
	emacs -Q -L . -L elisp --batch --eval '(progn (load "ffipf.el") (ffipf-test))'

clean:
	rm -rfv nimcache ffip*.so ffip ffipf

dist: test
	mkdir -p dist
	cp ./ffipf_backend.so ./dist
	cp ./elisp/ffipf.el ./dist
