#!/bin/bash


echo "Removing building information..."
rm -rf output

echo "Generate documentation..."
R -q -f roxygen.R

mkdir output
cd output
R CMD build --resave-data ../pkg
for x in *.tar.gz 
do 
    R CMD check --as-cran $x
done
