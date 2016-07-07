all: proyecto.pdf

Rnw := $(wildcard capitulos/*.Rnw)

proyecto.pdf: proyecto.tex bibliografia.bib
	pdflatex proyecto.tex && bibtex proyecto.aux && pdflatex proyecto.tex && pdflatex proyecto.tex

proyecto.tex: $(Rnw)
	R -e 'library(knitr);knit("proyecto.Rnw")'

install:
	R -e 'install.packages(c("xgboost","knitr","ggplot2","reshape2"))'
