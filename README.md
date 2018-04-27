# SIDE: New Spatial Data on Ethnicity

https://side.ethz.ch

Description
------------
The Spatially Interpolated Data on Ethnicity (SIDE) dataset is a collection of 253 near-continuous maps of local ethno-linguistic, religious, and ethno-religious settlement patterns in 47 low- and middle-income countries. These data are a generalization of ethnicity-related information in the geo-coded Demographic and Health Surveys (DHS). Many DHS surveys are geo-coded, thus providing a set of spatial sampling points containing local ethnic composition estimates. We use methods from geo-statistics and machine learning to estimate the ethnic composition of areas in between these sampling points to produce a continuous map of ethnic compositions for each surveyed country. 
  <br>
 Please refer to <a href="https://icr.ethz.ch/publications/spatial-data-on-ethnicity/" target="_blank">this article</a> for all details.
  
Limitations
------------
 Please note the following, important limitations of the data. First, DHS sampling may be not always be representative due to social phenomena, such as political violence. Although there is no systematic evidence of this, the data might in some cases be affected by local sampling bias.
Second, even though for many countries SIDE covers multiple years, we caution against relying on this temporal variation for inferential purposes. Due to variation in the sampling and coding of the DHS, a substantial share of intertemporal variance in the SIDE data is random noise.
Third, although the SIDE data are provided as high-resolution rasters, very local variation in the data may not be meaningful. This depends on the imputation parameters, the local density of the DHS data, and their random displacement of up to 2km (10km) in urban (rural) areas.
  <br>
 Given these limitations, we encourage the use of SIDE for cross-national analyses that require consistent, cross-national data on local ethnic demographies, rather than single-country studies that rely on high-precision data.
  
Citation
------------
When using this dataset in your research, please include the following reference:

Müller-Crepon, Carl & Philipp Hunziker. (2018). <a href="https://icr.ethz.ch/publications/spatial-data-on-ethnicity/" target="_blank">New Spatial Data on Ethnicity: Introducing SIDE.</a> <i>Journal of Peace Research</i>, forthcoming.

Installation
------------

You can install the package directly from this repository:

``` r
library(devtools)
install_github("carl-mc/sidedata")
```

Usage
------------

Download SIDE Data for ethnic Groups in Uganda 2010
``` r
library(sidedata)
library(raster)
  
# Download SIDE Data for ethnic Groups in Uganda 2010
side_download(country = "Uganda", year = 2010, marker = "ethnic", dest.dir = getwd())
```

Load SIDE Data for ethnic groups in Uganda 2010
``` r
# ... ethnic map
uga.ethnic <- side_load(country = "Uganda", year = 2010, marker = "ethnic", source.dir = getwd())
plot(uga.ethnic)

# ... get the SIDE meta data for these maps
uga.ethnic.meta.df <- sidemap2data(uga.ethnic)
head(uga.ethnic.meta.df)
names(uga.ethnic) <- uga.ethnic.meta.df$groupname

# ... convex hull of points
uga.convhull <- side_load(country = "Uganda", year = 2010, marker = "ethnic", source.dir = "~/Data/side_v1",
                          conv.hull = TRUE)
plot(uga.convhull)
```

Population-weighted aggregation to some spatial unit
```r
# ... spatial units
library(maptools)
units.shp <- readShapePoly("your.uganda.units.shp")
  
# ... population raster
pop.raster <- raster("your.pop.raster.asc")

# ... align rasters
pop.raster <- raster::crop(pop.raster, extent(uga.ethnic))
pop.raster <- raster::resample(pop.raster, uga.ethnic)

# ... compute group counts
uga.ethnic.counts <- uga.ethnic*pop.raster
names(uga.ethnic.counts) <- names(uga.ethnic)
  
  
# ... aggregate to unit level
uga.ethnic.agg <- raster::aggregate(uga.ethnic.counts, units.shp, fun = sum)
colnames(uga.ethnic.meta.df) <- names(uga.ethnic)

# ... convert back to proportions
uga.ethnic.meta.df <- uga.ethnic.meta.df / rowSums(uga.ethnic.meta.df)

# ... plot proportion of Baganda per spatial unit
plot(units.shp, 
     col = gray(uga.ethnic.meta.df[,"baganda"]))
```
