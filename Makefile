
ffip.so: src/ffip.nim src/fuzzy_file_finder.nim src/emacs_module.nim Makefile
	nim c \
	  -d:nimMaxHeap=40 \
	  --nimcache:nimcache \
	  --app:lib \
	  --out:ffip.so \
	  --opt:speed \
	  --gc:arc \
	  src/ffip.nim

#	  -d:debugLogging \
#	  --debugInfo:on \
#	  --lineTrace:on \
#	  --debugger:native \
#	  --stackTrace:on \
#	  --checks:on \
#	  --assertions:on \
#	  --embedsrc:on \
#	  -d:release \

ffip: src/fuzzy_file_finder.nim
	nimble c -d:release --opt:speed --out:ffip src/fuzzy_file_finder.nim

test: ffip.so
	emacs -Q -L . -L elisp --batch --eval '(progn (load "ffip.el") (ffip-test))'

clean:
	rm -rfv nimcache ffip.so ffip
