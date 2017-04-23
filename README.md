# earthquakes

Earthquakes package contains a few functions one can use to clean and visualize data about earthquake from [NOAA database](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1). 

First main function is *geom_timeline()*. It makes a ggplot geom object representing timeline within defined limits with earthquakes. 

Second main function is *eq_map()*. It makes a interactive map with earthquakes epicentrum and popup information. 

There are a few data cleaning and processing function. 
See details in packages vignette.

## Installation

You can install Fars from github with:

```R
# install.packages("devtools")
devtools::install_github("flyeye/earthquakes")
```

## Example

This is a basic example which shows you how to use package:

```R
require(earthquakes)
str(earthquakes)
 require(earthquakes)
 ggplot2::ggplot() +
   theme_void() +
   geom_timeline(data = earthquakes, aes(event_date = DATE,
                                   xmindate = 2000,
                                   xmaxdate = 2003,
                                   richter = EQ_PRIMARY,
                                   death = DEATHS,
                                   transparency = 0.3,
                                   colour = "red",
                                   scale = 0.5, 
                                   layers = COUNTRY))+
   theme(legend.position="none")
```
```R
 require(earthquakes)
   ggplot2::ggplot() +
   theme_void() +
   geom_timeline(data = earthquakes, aes(event_date = DATE,
                                   xmindate = 2000,
                                   xmaxdate = 2010,
                                   richter = EQ_PRIMARY,
                                   death = DEATHS,
                                   transparency = 0.3,
                                   colour = "red",
                                   scale = 0.5,
                                   layers = COUNTRY))+
   theme(legend.position="none") +
   geom_timeline_label(data = earthquakes, aes(event_date = DATE,
                                         xmindate = 2000,
                                         xmaxdate = 2010,
                                         richter = EQ_PRIMARY,
                                         layers = COUNTRY,
                                         labels = LOCATION_NAME))
```
```R

```
