Rscript funcToScript.R

Rscript fileRowSums.R 123.txt sums.txt
cat sums.txt
rm sums.txt

# Clark: Need more dependencies to better handle named args
#Rscript fileRowSums.R 123.csv sums.txt sep=","
