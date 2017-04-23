## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
  require(readr)
  filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
  raw_data <- read_delim(filename, delim = "\t")

## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
 require(earthquakes)
 head(earthquakes)
 str(earthquakes)

## ---- fig.height=5, fig.width=7------------------------------------------
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
                                   scale = 0.5))+
   theme(legend.position="none")

## ---- fig.height=5, fig.width=7------------------------------------------
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

## ---- fig.height=6, fig.width=8------------------------------------------
 require(earthquakes)
   ggplot2::ggplot() +
   theme_void() +
   geom_timeline(data = earthquakes, aes(event_date = DATE,
                                   xmindate = 2000,
                                   xmaxdate = 2005,
                                   richter = EQ_PRIMARY,
                                   death = DEATHS,
                                   transparency = 0.3,
                                   colour = "red",
                                   scale = 0.5,
                                   layers = COUNTRY))+
   theme(legend.position="none") +
   geom_timeline_label(data = earthquakes, aes(event_date = DATE,
                                         xmindate = 2000,
                                         xmaxdate = 2005,
                                         richter = EQ_PRIMARY,
                                         layers = COUNTRY,
                                         labels = LOCATION_NAME))


## ---- message=FALSE, warning=FALSE---------------------------------------
 require(earthquakes)
 filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
 raw_data <- read_delim(filename, delim = "\t")
 data <- eq_clean_data(raw_data)
 head(data)

## ---- message=FALSE, warning=FALSE---------------------------------------
 require(earthquakes)
 filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
 raw_data <- read_delim(filename, delim = "\t")
 data <- eq_location_clean(raw_data)
 head(data$LOCATION_NAME)

## ---- message=FALSE, warning=FALSE---------------------------------------
require(earthquakes)
labels <- eq_create_label(earthquakes)
head(labels)

## ---- fig.height=5, fig.width=7, message=FALSE, warning=FALSE------------
 require(earthquakes)

 filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
 readr::read_delim(filename, delim = "\t") %>%
   eq_clean_data() %>%
   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
   dplyr::mutate(popup_text = eq_create_label(.)) %>%
   eq_map(annot_col = "popup_text")

