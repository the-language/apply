all: arch-scm.rkt arch-js.rkt

arch-%.rkt: arch-%.scm Makefile
	echo "#lang racket" > $@
	echo "(provide z z+prelude)" >> $@
	echo '(include "zscheme.rkt")' >> $@
	echo '(include "z.scm")' >> $@
	echo "(include \"$<\")" >> $@

clean:
	rm -f arch-*.rkt *.scm~ *.rkt~
