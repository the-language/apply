all: arch-scm.rkt

arch-%.rkt: arch-%.scm Makefile
	echo "#lang racket" > $@
	echo "(provide z)" >> $@
	echo '(include "zscheme.rkt")' >> $@
	echo '(include "z.scm")' >> $@
	echo "(include \"$<\")" >> $@

clean:
	rm -f arch-*.rkt *.scm~ *.rkt~
