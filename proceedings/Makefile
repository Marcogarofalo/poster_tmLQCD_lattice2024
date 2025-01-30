.PHONY: arXiv skeleton.pdf all clean

all: skeleton.pdf arXiv

skeleton.pdf: skeleton.tex bibliography.bib
	latexmk -bibtex -pdf -pdflatex="pdflatex -interactive=nonstopmode" -use-make skeleton.tex

# for submission to PoS, generate a tar.gz with a flat directory structure, all of the paper in
# one TeX file and the bibliography expanded right into the document
arXiv: skeleton.tex skeleton.bbl
	mkdir -p arXiv
	latexpand --expand-bbl skeleton.bbl skeleton.tex > arXiv/status_etmc_generation_effort_lat24.tex
	cp -r plots arXiv
	cp PoSlogo.pdf pos.sty arXiv
	latexmk -pdf -use-make arXiv/status_etmc_generation_effort_lat24.tex # compile document to make sure it works, but not within arXiv directory
	tar -czf arXiv.tar.gz arXiv

clean:
	latexmk -CA
	rm -f skeleton.spl skeleton.tdo skeleton.pdf arXiv/status_etmc_generation_effort_lat24.*
	rm -rf arXiv

distclean:
	latexmk -CA
	rm -f skeleton.bbl skeleton.spl skeleton.tdo
