RFILES!= ls R/*.R
TESTFILES!= ls tests/testthat/test*.R

test: $(TESTFILES) $(RFILES)
	R CMD INSTALL .
	cd tests && Rscript testthat.R && cd ..
