# Wed Nov 30 10:01:37 PST 2016
# Print out package citations
#
# Rscript cite.R > Rpackages.bib

Rpackages = read.table("Rpackages.txt", stringsAsFactors = FALSE)[, 1]

knitr::write_bib(Rpackages)
