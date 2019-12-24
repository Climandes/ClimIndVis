# ClimIndVis 
* *Climate Indices Visualization* *

The ClimIndVis package provides a set of easy-to-use functions for the generation of climate indices products for observational, reanalysis and seasonal forecast data. 
The main features of the package are:

  - Calculation of indices (most ETCCDI indices (partly modified), threshold indices, consecutive threshold indices, SPI, rainy season indices)

  - Calculation of trends

  - Verification of seasonal forecasts

  - Visualizations of results (as time series, maps, or seasonal forecast graphics)

The special feature of the package are a set of wrapper functions (called autoplot_...) which combine all of the above functionalities in one function. These are designed so that users with little R knowledge can easily produce a set of figures.

The package works with daily data, gridded data as well as station data. This could be station observations or gridded observational datasets, reanalysis datasets and seasonal forecast ensembles (gridded or interpolated to stations).
For large datasets the computational time can be quite high, an issue for which improvement is planned.

**How to install the package**

An easy way to install ClimIndVis is by using the library "devtools". 
If you do not have devtools installed, you have to do this first:

```
install.packages("devtools")
```

Then you can install ClimIndVis :

```
devtools::install_github("Climandes/ClimIndVis")
```

After the installation is complete, you can load ClimIndVis with:

```
library(ClimIndVis)
```

