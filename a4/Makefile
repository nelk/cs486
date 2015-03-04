
.PHONY: all report clean bundle

all: bayesian-vars report

report: a4.pdf

clean:
	rm -f a4.tex bayesian-vars

a4.tex: a4.azx
	aztexc $< > $@

a4.pdf: a4.tex fido_bayes_net.png
	pdflatex --shell-escape $<

fido_bayes_net.png: fido_bayes_net.dot
	dot -Tpng $< > $@

HASK_FILES = Main.hs
HASK_CABAL_FILES = bayesian-vars.cabal $(HASK_FILES)

dist/build/bayesian-vars/bayesian-vars: $(HASK_CABAL_FILES)
	cabal build -j8

bayesian-vars: dist/build/bayesian-vars/bayesian-vars
	cp ./dist/build/bayesian-vars/bayesian-vars $@

bundle: ayklen_cs486_a4.tar.gz

ayklen_cs486_a3.tar.gz: Makefile a4.pdf $(HASK_CABAL_FILES)
	tar cfzv $@ $^

