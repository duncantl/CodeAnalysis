BIBFILES = citations.bib Rpackages.bib

TEXFILES = code_analysis_packages.tex

main.pdf: main.tex $(TEXFILES) $(BIBFILES)

Rpackages.bib: cite.R Rpackages.txt
	Rscript $< > $@

%.pdf: %.tex
	latexmk -pdf $<
	latexmk -c $<

clean:
	latexmk -C
