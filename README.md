# Space-time smoothing of mortality estimates in children aged 5-14 in Sub-Saharan Africa

Benjamin-Samuel Schlüter, Bruno Masquelier, December 2020

Paper under review with PLOS ONE scientific journal

Data section of README has to be finished

## Purpose:

This file and all the other files in the repository should enable
you to reproduce our results, plots, tables, ... from our joint
paper. Our paper consists in the estimation of sub-national mortality rates for children
aged 5-14 for 20 countries in Sub-Saharan Africa, using the method
developped by Mercer et al. (2015). 

## Directory structure:
There should be a single parent directory on your file system,
where you have to set your R working directory and which will
be divided into sub-directories named:

/code  
&nbsp; &nbsp; /functions  
/data  
&nbsp; &nbsp; /fbh  
&nbsp; &nbsp; /gps cluster  
&nbsp; &nbsp; /shapefiles  
&nbsp; &nbsp; /HIV_Adjustments  
&nbsp; &nbsp; /auxiliary  
&nbsp; &nbsp; /tidy  
/richardli_github

The repository available on Github where this README stands is */code*.
In order to obtain */richardli_github* sub-directory, go [here](https://github.com/richardli/AfricaU5MR),
download the repo to create the sub-directory as I did above. Within this sub-directory, 
cut/copy *HIV_Adjustments* into */data* in order to have the same directory structure as mine. 
Make sure to exactly reproduce this structure on your laptop before running the R scripts. 

## Data

The analysis includes 96 DHS surveys. Each country has a certain number of DHS 
surveys (see table below). For each country we needed to select a shapefile which, for some countries, is
used to relocate cluster  within coherent sub-national areas over time. 
To do so we also needed GPS cluster location of the DHS surveys that needed to be re-locate in coherent sub-national areas.
Each DHS survey used in our analysis will be stored in */fbh* for Full-Birth History data. There will be one
shapefile per country hence, */shapefiles* should include 20 folders. The number of data sets with cluster GPS 
locations depends on the country and are in */gps cluster*. 

In each of these three sub-directories, data associated
to a given DHS surveys will have the name "IDcountryYearDHS". Full-Birth history data, shapefile and cluster
GPS location for Ethiopian 2016 DHS survey will be in *ET2016DHS*. The latter folders contain the unzipped files 
downloaded from two sources ([DHS FBH with GPS location](https://dhsprogram.com/) and 
[shapefiles](http://spatialdata.dhsprogram.com/boundaries/#view=table&countryId=AF)).

Below is the table showing the list of files you need to download in order to reproduce our analysis:

| Country       | DHS                                               | Shapefile  | Cluster GPS location              |
| ------------- |---------------------------------------------------| :---------:| ----------------------------------|                    
|Benin          | 1996,2001,2006,2012,2018                          | 2001       |                                   |
|Burkina Faso   | 1993,1999,2003,2010                               | 1999       | 2003,2010                         |
|Cameroon       | 1998,2004,2011,2018                               | 1998       |                                   |
|Ethiopia       | 2000,2005,2011,2016                               | 2016       |                                   |
|Ghana          | 1993,1998,2003,2008,2014                          | 2014       |                                   |
|Guinea         | 1999,2005,2012,2018                               | 1999       | 2005,2012,2018                    |
|Kenya          | 1993,1998,2003,2008,2014                          | 2014       | 2003,2008,2014                    |
|Lesotho        | 2004,2009,2014                                    | 2014       |                                   |
|Madagascar     | 1992,1997,2004,2009                               | 2004       | 2009                              |
|Mali           | 1996,2001,2006,2013,2018                          | 1996       | 1996,2001,2006,2013, 2018         |
|Malawi         | 1992,2000,2004,2010,2016                          | 2016       |                                   |
|Nigeria        | 1990,2003,2008,2013,2018                          | 2018       | 1990                              |
|Niger          | 1992,1998,2006,2012                               | 1998       |                                   |
|Namibia        | 2000,2007,2013                                    | 2013       |                                   |
|Rwanda         | 2005,2008,2010,2015                               | 2015       |                                   |
|Senegal        | 1993,1997,2005,2011,2013,2014,2015,2016,2017,2018,2019| 2017       | All selected DHS except 2017,2018 |
|Tanzania       | 1996,1999,2005,2010,2016                          | 1996       | 1999,2010,2016                    |
|Uganda         | 1995,2001,2006,2011,2016                          | 1995       | 2006,2011,2016                    |
|Zambia         | 1992,1996,2002,2007,2014,2018                     | 2002       | 2014,2018                         |
|Zimbabwe       | 1994,1999,2006,2011,2015                          | 2015       |                                   |

For the selectivity assessment shown in the Supporting Information, we also downloaded UN IGME 10q5 estimates 
[here](https://childmortality.org/) and stored the data set in */data/auxiliary*.

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

 





