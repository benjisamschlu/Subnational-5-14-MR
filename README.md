# Space-time smoothing of mortality estimates in children aged 5-14 in Sub-Saharan Africa

Benjamin-Samuel Schlüter, Bruno Masquelier, December 2020

Paper under review with PLOS ONE scientific journal

## Purpose:

This file and all the other files in the repository should enable
you to reproduce our results, plots, tables, ... from our joint
paper on the estimation of sub-national mortality rates for children
aged 5-14 for 20 countries in Sub-Saharan Africa. 

## Directory structure:
There should be a single parent directory on your file system,
where you have to set your R working directory and which will
be divided into subdirectories named:

    /code

        /functions

    /data

	/fbh

	/gps cluster

	/shapefiles

	/HIV_Adjustments

	/auxiliary

	/tidy

    /richardli_github

The repository available on Github where this README stands is */code*.
In order to obtain */richardli_github* subdirectory, go [here](https://github.com/richardli/AfricaU5MR),
download the repo to create the subdirectory as I did above. Within this subdirectory, 
cut/copy *HIV_Adjustments* into */data* in order to have the same directory structure as mine. 
Make sure to exactly reproduce this structure on your laptop before running the R scripts. 

## Data



## R scripts

Within */code* folder, script *main.R* allows you to run all R scripts
on the 20 countries at once. Note that the loop might take time to run 
on all countries (approx. 3 hours). At each iteration
associated to a country, *main.R* will store results in 
*/data/tidy/country_name.RDS*. After 20 iterations, you will have 
a .RDS file for each country. Then use *outputs all ctry.R* 
(script within */code*) in order to obtain plots and tables presented
in the paper.


If you want to create the output for one country at a time, start with 
*prepare data.R* and add these two lines of code:

`ctry <- "country_name"`

`ID <- "country_ID"`

where country_ID should be the ID of the country of interest 
(i.e `ctry <- "Malawi"` and `ID <- "MW"`).

 





