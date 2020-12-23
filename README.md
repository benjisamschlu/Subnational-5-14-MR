# Space-time smoothing of mortality estimates in children aged 5-14 in Sub-Saharan Africa

Benjamin-Samuel Schlüter, Bruno Masquelier, December 2020
Paper under review with PLOS ONE scientific journal

## Purpose:

This file and all the other files in the repository should enable
you to reproduce our results, plots, tables, ... from our joint
paper on the estimation of sub-national mortality rates for children
aged 5-14 in 20 countries in Sub-Saharan Africa. 

## Directory structure:
There should be a single parent directory on your file system,
where you have to set your R working directory and which will
be divided into subdirectories named:
    */code*
        */functions*
    */data* 
	*/fbh*
	*/gps cluster*
	*/shapefiles*
	*/HIV_Adjustments*
	*/auxiliary*
	*/tidy*
    */richardli_github*

The repository available on Github where this README stands is */code*.
In order to obtain */richardli_github* subdirectory, go [here](https://github.com/richardli/AfricaU5MR),
download the repo to create the subdirectory as I did above. Within this subdirectory, 
cut/copy *HIV_Adjustments* into */data* in order to have the same directory structure as mine. 
Make sure to exactly reproduce this structure on your laptop. 

## Data



## R scripts

Within */code*, there is *main.R* which allows to run all R scripts
on the 20 countries. *main.R* will store results for each country 
in /data/tidy/country_name.RDS. When you have a .RDS file for each
country, you can use *outputs all ctry.R* (script within */code*)
in order to obtain plots and tables present in the paper. 





