all: proyecto.Rnw
	R -e 'library(knitr);knit("proyecto.Rnw")' && pdflatex proyecto.tex && bibtex proyecto.aux && pdflatex proyecto.tex && pdflatex proyecto.tex
