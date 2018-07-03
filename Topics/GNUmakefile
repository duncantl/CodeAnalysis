MD_FILES := title.md abstract.md intro.md cran_survey.md

CodeAnalysis.pdf: $(MD_FILES)
	pandoc --filter pandoc-citeproc -s $^ -o $@
