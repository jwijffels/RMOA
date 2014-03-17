@echo off


echo Removing building information...
rm -rf output

echo Generate documentation...
Rscript roxygen.R

md output
cd output
R CMD build ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check --as-cran %%1
cd ..