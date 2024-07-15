all: poster_tmLQCD_lattice2024.pdf abstract.pdf

poster_tmLQCD_lattice2024.pdf: poster_tmLQCD_lattice2024.tex commands.tex sample.bib Makefile figures/*.pdf data/*/*pdf
	latexmk -shell-escape -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $<

abstract.pdf: abstract.md
	pandoc $< -o $@

clean:
	latexmk -f -c

distclean:
	latexmk -f -CA
	rm -f poster_tmLQCD_lattice2024.pdf abstract.pdf *bbl
