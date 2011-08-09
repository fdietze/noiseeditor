SRC=bachelorthesis

all: $(SRC).pdf

$(SRC).pdf: $(SRC).tex
	latex -shell-escape $(SRC)
	bibtex   $(SRC)
	latex -shell-escape $(SRC)
	latex -shell-escape $(SRC)
	dvipdf $(SRC).dvi

clean:
	rm -f *~ bachelorthesis.pdf bachelorthesis.log bachelorthesis.aux bachelorthesis.bbl bachelorthesis.blg bachelorthesis.toc bachelorthesis.dvi bachelorthesis.glo affirmation.aux chapter1.aux titlepage.aux
