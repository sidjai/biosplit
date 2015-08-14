#biosplit README

This package is used to model moth migration on a continental scale across generations. The name comes from the combination of a biological model (bio-) with the HYSPLIT model by US NOAA (-split).

## Installation

The main program can be found as a R package that can be downloaded in R as follows:

```R
# install.packages("devtools")
devtools::install_github("sidjai/biosplit")
```

The HYSPLIT program can be downloaded [here](http://ready.arl.noaa.gov/HYSPLIT_hytrial.php)

To work with ARL data [sets](ftp://arlftp.arlhq.noaa.gov/pub/archives/), a GIS tool is used called MeteoInfo which can be downloaded [here](http://www.meteothinker.com/Downloads.html)

To do the post-processing graphics, a proprietary GIS software, Surfer is used which can be found [here](http://www.goldensoftware.com/products/surfer). This is not required as the products from the R program is in formats and has a structure that can be read by any GIS software or programming language.

The location of these secondary applications have to be given to the program through the config.txt file.


##Biological Model

There are two species that grow in this model, the Corn (host plant) and the Fall Armyworms (FAW). The main growth mechanism used is Growing degree days (GDD) that can track development of species with the temperature on the given days. Planting dates for the corn is assumed to be solely a function of latitude. After the corn is planted, the growth begins using the temperature until it is harvested which is, again, assumed to be solely a function of latitude. 

The FAW are grown during the simulation depending on where they land. The two categories for development are cohorts and moths. The cohorts are defined as all the immature population from a particular migrant. These are grown with a degree days and have a given limit when they turn into moths. The moths are grown using simple age in units of days old. Cohorts are grown using on a week timescale and the Moths are grown and flown on a daily scale. Eggs are laid assuming an equal sex ratio and a fatality ratio from other factors are 50%. Other death factors include flying to a place without corn.

A more detailed explanation of the model can be found in "/docs/BiologicalEqs.pdf".


##Migration Model

Nightly flight of the moth populations is simulated as a dispersion of particles in the HYSPLIT model. There are plans to include flight speed and heading but this is still in development. The moths are given a 12 hr flight time starting at nightfall. There is an optional mechanism using the "insol" package to change this base time of flight with the changing of the seasons.

##Code Structure

###Data Collection

This section is split into two main threads; Crop data and meteorological data. The crop data comes from CropScape web API that is queried in "NASS2TIFs.R". The Meteorological data comes from the EDAS ARL dataset from NOAA. This is parsed using a GIS called MeteoInfo using the automation tool written in Jython. Both of these are processed using R for the required extent, projection, units and format for the simulation. These are combined in one netCDF file with aggregated variables of the different biological assumptions in "aprioriVars.R" This structure is shown below:

![](~/inst/docs/DataDiagram.jpeg)

Mostly, the two sources on the far left can be changed out for either other data files or scripts to query other databases. With the appropriate changes to the config file and the grid definitions, the rest of the process should run smoothly. This is an active area of development so if you run into problems please place a bug report.

###Post-processing

The raw output of the model is 3 fold:
1. xyz'att' snapshot tables of all the moth and Cohort populations at every time step.
2. netCDF (.nc) files of moth and cohort populations summed by origin at every time step in /ncs/
3. The raw HYSPLIT plots outputted every 10 HYSPLIT runs saved as .ps

The third output is mostly for verification and some details of exactly what is happening in the process. The first two outputs can be manipulated by a variety of different programs, languages, and GIS applications. One side of the post-processing is to compare the output to trap captures at a few selected sites. This is done in R with in the file "ncdf2trapdata.R". Later the resultant .csv files were used with SAS's JMP and Golden Software's MapViewer for statistical analysis and more complicated plots seen in the foundational paper (Westbrook et. al, 2015). This was deemed too specific for this project to be included in the package and can be made available on request.

The other side is plotting the results as contour and post maps in the folder "/jpeg/contour" or "/jpeg/ClassPost". This is done with Golden Software's Surfer application (using their automation tool Scripter written in wwb-com). The reason is entirely aesthetic qualities of the output and the fine manipulation tools given. The same type of plotting can be done in other programs as well. 

##Acknowledgement

This project was conducted by Siddarta Jairam, Rob L. Meagher, Jr., Rod N. Nagoshi and John K. Westbrook of the USDA-Agricultural Research Service and  Shelby J. Fleischer of Penn State University with financial supported by USDA project number 6036-22000-025-06 and from the USDA-NIFA (AFRI grant number 2011-67003-30209).  USDC-NOAA provided access to HYSPLIT PC version 4.9 atmospheric transport modeling software.  Mention of trade names or commercial products in this article is solely for the purposes of providing specific information and does not imply recommendation or endorsement by the U.S. Department of Agriculture.  USDA is an equal opportunity provider and employer.


##References

Foundational Paper
J. K. Westbrook, R. N. Nagoshi, R. L. Meagher, and S. J. Fleischer, S. Jairam (2015), Modeling Seasonal Migration of Fall Armyworm Moths, International Journal of Biometeorology, doi: 10.1007/s00484-015-1022-x


Wang, Y. Q. 2014. MeteoInfo: GIS software for meteorological data visualization and analysis. Meteorological Applications, 21: 360-368
http://www.meteothinker.com/

Golden Software
Surfer: http://www.goldensoftware.com/products/surfer
MapViewer: http://www.goldensoftware.com/products/mapviewer

HYSPLIT
Draxler, R.R., and G.D. Hess, 1998: An overview of the HYSPLIT_4 modeling system of trajectories, dispersion, and deposition. Aust. Meteor. Mag., 47, 295-308.
http://www.arl.noaa.gov/HYSPLIT_info.php

JMP http://www.jmp.com/en_us/home.html