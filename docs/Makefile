doc: pdf html
	@echo "rendering document html and pdf..."

demo: ../8puzzle.pl
	@echo "consulting prolog files..."
	python3 demo/preprocess.py $<
	@echo "done!"

pdf: main.rmd
	@echo "rendering pdf book..."
	Rscript -e "bookdown::render_book('$<', 'bookdown::pdf_book', output_file='../book.pdf')"
	@echo "done!"

html: main.rmd
	@echo "rendering html document..."
	Rscript -e "bookdown::render_book('$<', 'bookdown::html_document2')"
	@echo "done!"

beamer: presentation.md
	@echo "rendering beamer presentation..."
	Rscript -e "rmarkdown::render('$<', 'beamer_presentation')"
	@echo "done!"

reveal: presentation.md
	@echo "rendering revealjs presentation..."
	Rscript -e "rmarkdown::render('$<', 'revealjs::revealjs_presentation')"
	@echo "done!"
