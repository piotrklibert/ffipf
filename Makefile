
ffipf_backend.so: src/ffipf_backend.nim src/fuzzy_file_finder.nim src/emacs_module.nim src/emacs_helpers.nim src/emacs_consts.nim Makefile
	nim c \
	  -d:nimMaxHeap=120 \
	  --nimcache:nimcache \
	  --app:lib \
	  --out:ffipf_backend.so \
	  --opt:speed \
	  --gc:arc \
	  -d:release \
	  --opt:speed \
	  src/ffipf_backend.nim

#	  --passC:-fPIC \
#	  --dynlibOverride:c \
#	  --passL:/usr/lib64/libc_nonshared.a \

#	  -d:debugLogging \
#	  --debugInfo:on \
#	  --lineTrace:on \
#	  --debugger:native \
#	  --stackTrace:on \
#	  --checks:on \
#	  --assertions:on \
#	  --embedsrc:on \


ffipf: src/fuzzy_file_finder.nim
	nimble c --gc:arc -d:release --opt:speed --out:ffipf src/fuzzy_file_finder.nim

unittest:
#	nim c -o:test1 -r tests/test1.nim
	nimble test

test: ffipf_backend.so elisp/ffipf.el
	emacs -Q -L . -L elisp --batch --eval '(progn (load "ffipf.el") (ffipf-test))'


clean:
	rm -rfv nimcache ffip*.so ffip ffipf tests/test1 test1


dist: ffipf_backend.so ffipf elisp/ffipf.el
	mkdir -p dist
	cp ./ffipf_backend.so ./dist
	cp ./ffipf ./dist
	cp ./elisp/ffipf.el ./dist

testdist: dist
	emacs -Q -L ./dist/ --batch --eval '(progn (load "ffipf.el") (ffipf-test))'
