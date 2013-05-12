LATEX=latex
BIBTEX=bibtex
DVIPS=dvips
PS2PDF=ps2pdf
PAPER=acs-dissertation
FILES=titlepage.tex declaration.tex abstract.tex introduction.tex \
      background.tex implementation.tex evaluation.tex conclusion.tex

all: $(PAPER).pdf
$(PAPER).pdf: $(PAPER).ps
	$(PS2PDF) -dEmbedAllFonts=true $(PAPER).ps test.pdf
	ps2pdf13 -dPDFSETTINGS=/prepress test.pdf $(PAPER).pdf
	rm -f test.pdf

$(PAPER).ps: $(PAPER).dvi
	$(DVIPS) -Pdownload35 -ta4 $(PAPER).dvi

$(PAPER).dvi: $(PAPER).tex $(PAPER).bib $(FILES)
	$(LATEX) $(PAPER)
	$(BIBTEX) $(PAPER)
	$(LATEX) $(PAPER)
	$(LATEX) $(PAPER)

clean:
	$(RM) -f $(PAPER).pdf $(PAPER).ps $(PAPER).dvi
	$(RM) -f *.log *.aux *.toc *.bbl *.blg *.lot *.lof
