
.PHONY: all report clean bundle

all: report

report: a5.pdf

test: dist/build/bayesian-vars-tests/bayesian-vars-tests
	./$<

clean:
	rm -f a5.tex; cd q1; cabal clean; cd ../q4_and_5; cabal clean; cd ..

a5.tex: a5.azx
	aztexc $< > $@

a5.pdf: a5.tex q4_and_5/horse.pdf q4_and_5/porto.pdf
	pdflatex --shell-escape $<

fido_bayes_net.png: fido_bayes_net.dot
	dot -Tpng $< > $@

HASK_Q1 = ./q1/*.hs ./q1/value-iteration.cabal
HASK_Q45 = ./q4_and_5/*.hs ./q4_and_5/decision-tree.cabal
HASK_ALL = $(HASK_Q1) $(HASK_Q45)

Q1_EXEC = q1/dist/build/value-iteration/value-iteration
Q45_EXEC = q4_and_5/dist/build/decision-tree/decision-tree

$(Q1_EXEC): $(HASK_Q45)
	cd q4_and_5; cabal build -j8 exe:decision-tree; cd -

$(Q45_EXEC): $(HASK_Q45)
	cd q4_and_5; cabal build -j8 exe:decision-tree; cd -

q4_and_5/horse.dot: $(Q45_EXEC)
	cd q4_and_5; ./horse.sh; cd -

q4_and_5/porto.dot: $(Q45_EXEC)
	cd q4_and_5; ./porto.sh; cd -

q4_and_5/horse.pdf: q4_and_5/horse.dot
	dot -Tpdf $< > $@

q4_and_5/porto.pdf: q4_and_5/porto.dot
	dot -Tpdf $< > $@

bundle: ayklen_cs486_a5.tar.gz

ayklen_cs486_a5.tar.gz: Makefile a5.pdf $(HASK_ALL)
	tar cfzv $@ $^

