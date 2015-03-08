
.PHONY: all report clean bundle test run

all: report run

report: a4.pdf

run: dist/build/bayesian-vars/bayesian-vars
	./$<

test: dist/build/bayesian-vars-tests/bayesian-vars-tests
	./$<

clean:
	rm -f a4.tex bayesian-vars

a4.tex: a4.azx
	aztexc $< > $@

a4.pdf: a4.tex fido_bayes_net.png
	pdflatex --shell-escape $<

fido_bayes_net.png: fido_bayes_net.dot
	dot -Tpng $< > $@

HASK_COMMON = bayesian-vars.cabal
HASK_SRC = src/*.hs
HASK_TEST = test/*.hs
HASK_ALL = $(HASK_COMMON) $(HASK_SRC) $(HASK_TEST)

dist/build/bayesian-vars/bayesian-vars: $(HASK_COMMON) $(HASK_SRC)
	cabal build -j8

dist/build/bayesian-vars-tests/bayesian-vars-tests: $(HASK_ALL)
	cabal configure --enable-tests
	cabal build -j8

bundle: ayklen_cs486_a4.tar.gz

ayklen_cs486_a4.tar.gz: Makefile a4.pdf $(HASK_ALL)
	tar cfzv $@ $^

