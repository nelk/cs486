
.PHONY all: sudoku-solver report

.PHONY report: a3.pdf

.PHONY clean:
	rm a3.tex sudoku-solver

a3.tex: a3.azx
	aztexc a3.azx > a3.tex

a3.pdf: a3.tex
	pdflatex --shell-escape a3.tex

sudoku-solver: sudoku-solver.cabal Main.hs Backtracking.hs SudokuSolver.hs ConstraintSolver.hs
	cabal build
	cp ./dist/build/sudoku-solver/sudoku-solver ./sudoku-solver

