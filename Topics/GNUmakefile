MD_FILES := title.md abstract.md intro.md cran_survey.md project_structure.md work_cited.md

CodeAnalysis.pdf: $(MD_FILES)
	pandoc --filter pandoc-citeproc -s $^ -o $@
