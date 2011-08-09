all: bachelorthesis.pdf

bachelorthesis.pdf: *.tex
	latex -halt-on-error -file-line-error -shell-escape bachelorthesis
	bibtex bachelorthesis
	latex -halt-on-error -file-line-error -shell-escape bachelorthesis
	latex -halt-on-error -file-line-error -shell-escape bachelorthesis
	dvipdf bachelorthesis.dvi

clean:
	rm -f *~ bachelorthesis.pdf bachelorthesis.log bachelorthesis.aux bachelorthesis.bbl bachelorthesis.blg bachelorthesis.toc bachelorthesis.dvi bachelorthesis.glo affirmation.aux chapter1.aux titlepage.aux
