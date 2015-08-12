##biosplit v0.2.2 (Release 8/12/2015 on GitHub)
-----------------------------------

-added Simulation and vaildation statistics for regions
-Overhauled Autoreport for new techniques, data sets, graphs and thoughts on the process
-made the new contour + post maps look much better and working for both first occurance and mixing areas
-updated documentation and file structure


##biosplit v0.2.1 (Release 7/20/2015)
-----------------------------------

-added some tests for the modeling
-Validation data manipulation
--scrubing of xlsx dumps of the trap data we are using (PestWatch) and a lab notebook of haplotype data
-new contour plot with a class post overlay for the simulation and validation data


##biosplit v0.2.0 (Release 6/17/2015, 2nd multi-year model)
-----------------------------------

###Major changes
- Made code into a package instead of scripts
- Added ncdf2trapgrid to grid the trap data and compare first week of capture to the simulation
- Changed how and when Met data is collected making it a function.
- Changed file structure to put all the data into one folder. Normalized the names to use the raw data designations as

###Minor changes
- Added take-off thresholds for precipition and temperature
- Made it pass CRAN checks
- Added an interface for AprioriVars to easily make new equations
- Made an interface in runBiosplit called "getContext" which passes only the values that it needs and subsets apr automatically


##biosplit v0.1.3 (Release 5/1/2015)
-----------------------------------

- Can now specify overwinter populations as latitude thresholds


##biosplit v0.1.2 (Release 4/28/2015)
-----------------------------------

- Config can now change simulation parameters in the hysplit config files
- changing config programically added enabling batch processing
- Can now specify an absolute limit to flights made by a population of moths in 1 lifetime instead of just a successive limit.



##biosplit v0.1.1 (Release 4/21/2015)
-----------------------------------

- Added usage of "insol" package for changing the duration of the flight


##biosplit v0.1.0 (Release 4/10/2015)
-----------------------------------

- Added a config file with the parameters and file locations
- changed overwinter start time from the 45th day to the first day that the Corn grows greater than the infestation threshold

