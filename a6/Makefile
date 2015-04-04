
.PHONY: all report clean bundle

all: report

report: a6.pdf

clean:
	rm -f a6.tex

a6.tex: a6.azx
	aztexc $< > $@

a6.pdf: a6.tex
	pdflatex --shell-escape $<

bundle: ayklen_cs486_a6.tar.gz

ayklen_cs486_a6.tar.gz: Makefile a6.pdf
	tar cfzv $@ $^

