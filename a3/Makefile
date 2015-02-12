
.PHONY: all report clean bundle

all: sudoku-solver report

report: a3.pdf

clean:
	rm -f a3.tex sudoku-solver

a3.tex: a3.azx
	aztexc a3.azx > a3.tex

a3.pdf: a3.tex data.png
	pdflatex --shell-escape a3.tex

dist/build/sudoku-solver/sudoku-solver: sudoku-solver.cabal Main.hs Backtracking.hs SudokuSolver.hs ConstraintSolver.hs
	cabal build -j8

sudoku-solver: dist/build/sudoku-solver/sudoku-solver
	cp ./dist/build/sudoku-solver/sudoku-solver ./sudoku-solver

bundle: ayklen_cs486_a3.tar.gz

ayklen_cs486_a3.tar.gz: Makefile a3.pdf sudoku-solver.cabal Main.hs Backtracking.hs SudokuSolver.hs ConstraintSolver.hs
	tar cfzv ayklen_cs486_a3.tar.gz Makefile a3.pdf sudoku-solver.cabal Main.hs Backtracking.hs SudokuSolver.hs ConstraintSolver.hs

