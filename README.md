# mosquito-community-phenology

This repository accompanies the manuscript **"Climate, landscape, and life history jointly predict multidecadal community mosquito phenology"** by Lindsay P. Campbell, Mohamed F. Sallam, Amely M. Bauer, Yasmin Tavares, Robert P. Guralnick

We leveraged long-term mosquito control district monitoring data to characterize annual phenologies of host-seeking female mosquito species over two decades in suburban Illinois, USA. We also assembled data on landscape context, categorized into low and medium development, climate variables including precipitation, temperature and humidity, and key life history traits, i.e. overwintering stage and early versus late-season fliers that were included as predictor variables in linear mixed models seperately fitted for adult onset, peak abundances, and flight termination.


## Code Files

The scripts document data preparation and analyses conducted for this study and contain example code for plotting model outputs. 

* **phenometric_calc.r**
  * Calculates phenometrics for the 7 species included in the analyses using the `rbms` package [^1]
  
* **environ_variables_models.r** 
  * Assembles environmental variables and phenometrics into an analysis-ready dataset. 
  * Runs models

### Data Files

* IL_00_20_red_spp_LONG.csv
  * All recorded mosquito species, used in `phenometric_calc.r` to calculate mosquito phenometrics
 
* IL_LC_clim_sites_summaries.csv
  * Assembled environmental variables (landcover and climate variables), used in `environ_variables_models.r`   
  
* mospheno_clim_summary4_traits.csv 
  * Assembled environmental variables and phenometrics, generated and used for model fitting in `environ_variables_models.r`
 
* mosquitoILphenomets.csv 
  * Phenometrics for the 7 mosquito species included in the analyses, calculated using `phenometric_calc.r`
 
* site_lc_lookup3.csv
  * Coordinates of sampling sites, used in `environ_variables_models.r`

<br/>
<br/>
<br/>

*This repository contains the version of the code and data files at the time of manuscript submission, which may undergo changes in future*

[^1]: Schmucki R., Harrower C.A., Dennis E.B. (2022) rbms: Computing generalised abundance indices for butterfly monitoring count data. R package version 1.1.3. https://github.com/RetoSchmucki/rbms 
