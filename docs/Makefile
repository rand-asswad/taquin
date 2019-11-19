demo: ../src/8puzzle.pl
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
