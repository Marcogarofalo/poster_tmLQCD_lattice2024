all: poster_tmLQCD_lattice2024.pdf abstract.pdf

poster_tmLQCD_lattice2024.pdf: poster_tmLQCD_lattice2024.tex Makefile figures/*.pdf
	latexmk -shell-escape -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $<

abstract.pdf: abstract.md
	pandoc $< -o $@

clean:
	latexmk -f -c

distclean:
	latexmk -f -CA
	rm -f $(addsuffix .pdf,$(NAMES))
	rm -f $(addsuffix .nav,$(NAMES))
	rm -f $(addsuffix .snm,$(NAMES))
