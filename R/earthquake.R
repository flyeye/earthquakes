
#raw_earthquake <- read_delim("signif.txt", delim = "\t")
#earthquake <- eq_clean_data(raw_earthquake)
#earthquake <- eq_location_clean(earthquake)
#test2 <- filter(earthquake, (COUNTRY == "CHINA") | (COUNTRY == "RUSSIA") | (COUNTRY == "JAPAN"))
#ggplot2::ggplot() + theme_void() + geom_timeline(data = test2, aes(event_date = DATE, xmindate = 2000, xmaxdate = 2010, richter = EQ_PRIMARY, death = DEATHS, transparency = 0.3, colour = "red", scale = 0.5, layers = COUNTRY))+ theme(legend.position="none") + geom_timeline_label(data = test2, aes(event_date = DATE, xmindate = 2000, xmaxdate = 2010, richter = EQ_PRIMARY, layers = COUNTRY, labels = LOCATION_NAME))


GeomTimeLine <- ggproto("GeomTimeLine", GeomBlank,
                         required_aes = c("xmindate", "xmaxdate", "event_date", "richter", "death"),
                         default_aes = aes(colour = "blue", scale = 1, transparency = 0.3, layers = NA), #alpha = 0.3
                         draw_key = draw_key_blank,
                         draw_panel = function(data, panel_scales, coord) {
                           ## Transform the data first

                           #str(data)
                           #print(data)
                           #cat("---\n")

                           mindate <- as.Date(paste0(data$xmindate[1], "-01-01 00:00:00"))
                           maxdate <- as.Date(paste0(data$xmaxdate[1], "-01-01 00:00:00"))

                           clear_data <- dplyr::filter(data, (event_date>=mindate) & (event_date<=maxdate))
                           clear_data$death[is.na(clear_data$death)] <- 0

                           # str(nrow(clear_data))
                           # cat("---\n")

                           max_death <- max(clear_data$death)

                           xmin <- 0.1
                           xmax <- 0.9
                           y_pos <- 0.35

                           f <- factor(data$layers)
                           nlayers <- nlevels(f)
                           if ( nlayers == 0 )
                           {
                             data$layers <- 1;
                             f <- factor(data$layers)
                             nlayers <- 1
                           }

                           # cat(data$layers)
                           # cat("---\n")
                           # cat(nlayers)
                           # cat("---\n")
                           # cat(f)
                           # cat("---\n")

                           circles_col <- colorRampPalette(colors = c("gray20",clear_data$colour[1]), space="Lab")(100)

                           g <- grid::gList()


                           for (l in 1:nlayers)
                           {
                               if (nlayers > 1)
                                 {
                                   l_data <- dplyr::filter(clear_data, layers == levels(f)[l])
                                   if (nrow(l_data) == 0)
                                     next
                               } else
                               {
                                 l_data <- clear_data
                               }

                               # cat(l, "\n")
                               # cat(nrow(l_data), "\n")
                               # cat("---\n")

                               circles <- gList()

                               for(i in 1:nrow(l_data))
                               {
                                 col_index <- round(100*l_data$death[i]/max_death)
                                 if (col_index == 0)
                                   col_index <- 1
                                 circles <- grid::gList(circles, grid::circleGrob(x = unit(c(xmin + (xmax-xmin)*as.numeric(l_data$event_date[i] - mindate)/as.numeric(maxdate-mindate)), "npc"),
                                                                               y = unit(c(y_pos+(l-1)*0.2), "npc"),
                                                                               r = l_data$scale[i]*l_data$richter[i]/200, #0.025,
                                                                               gp = grid::gpar(col = "gray20",
                                                                                               fill = circles_col[col_index], #"gray60",
                                                                                               alpha = l_data$transparency[i],
                                                                                               lwd = 1.5)))
                               }

                               x1 <- grid::linesGrob(x = unit(c(0.05, 0.95), "npc"), y = unit(c(y_pos+(l-1)*0.2, y_pos+(l-1)*0.2), "npc"),
                                               gp = grid::gpar(col = "gray50", lwd = 2))

                               if (nlayers>1)
                               {
                                 x_txt <- grid::textGrob(l_data$layers[1], x = unit(c(0.05), "npc"), y = unit(c(y_pos+(l-1)*0.2 + 0.05), "npc"), gp=gpar(fontsize=10))
                               } else
                               {
                                 x_txt <- grid::nullGrob()
                               }

                               g <- grid::gList(g, circles, x1, x_txt)

                               # cat("glist: ", length(g), "\n")
                           }


                           # circles <- gList()
                           # l <- 0;
                           # for(i in 1:nrow(clear_data))
                           # {
                           #   col_index <- round(100*clear_data$death[i]/max_death)
                           #   if (col_index == 0)
                           #      col_index <- 1
                           #    circles <- gList(circles, circleGrob(x = unit(c(xmin + (xmax-xmin)*as.numeric(clear_data$event_date[i] - mindate)/as.numeric(maxdate-mindate)), "npc"),
                           #                                        y = unit(c(y_pos+l*40), "npc"),
                           #                                        r = clear_data$scale[i]*clear_data$richter[i]/200, #0.025,
                           #                                        gp = grid::gpar(col = "gray20",
                           #                                                        fill = circles_col[col_index], #"gray60",
                           #                                                        alpha = clear_data$transparency[i],
                           #                                                        lwd = 1.5)))
                           # }
                           #
                           # x1 <- linesGrob(x = unit(c(0.05, 0.95), "npc"), y = unit(c(y_pos+l*40, y_pos+l*40), "npc"),
                           #                 gp = grid::gpar(col = "gray50", lwd = 2))
                           #
                           # g <- grid::gList(g, circles, x1)
                           #

                           xaxis <- grid::linesGrob(x = unit(c(0.05, 0.95), "npc"), y = unit(c(0.25, 0.25), "npc"),
                                              gp = grid::gpar(lwd = 2))

                           l1 <- grid::polylineGrob(x=c(0.1, 0.1, 0.3, 0.3, 0.5, 0.5, 0.7, 0.7, 0.9, 0.9),
                                              id = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
                                         y=c(0.25, 0.22, 0.25, 0.22, 0.25, 0.22, 0.25, 0.22, 0.25, 0.22))

                           mindate <- clear_data$xmindate[1]
                           maxdate <- clear_data$xmaxdate[1]
                           date_step <- (maxdate - mindate) / 4

                           xaxis_text <- grid::gList(textGrob(mindate, x = unit(c(0.1), "npc"), y = unit(c(0.2), "npc"), gp=gpar(fontsize=10)),
                                               textGrob(mindate + date_step, x = unit(c(0.1 + 0.2), "npc"), y = unit(c(0.2), "npc"), gp=gpar(fontsize=10)),
                                               textGrob(mindate + 2*date_step, x = unit(c(0.1 + 2*0.2), "npc"), y = unit(c(0.2), "npc"), gp=gpar(fontsize=10)),
                                               textGrob(mindate + 3*date_step, x = unit(c(0.1 + 3*0.2), "npc"), y = unit(c(0.2), "npc"), gp=gpar(fontsize=10)),
                                               textGrob(mindate + 4*date_step, x = unit(c(0.1 + 4*0.2), "npc"), y = unit(c(0.2), "npc"), gp=gpar(fontsize=10)))

                           xaxis_title <- grid::textGrob("DATE", x = unit(c(0.5), "npc"), y = unit(c(0.15), "npc"))



                           d <- grid::rectGrob(x = unit(seq(0.6,0.7, length=100), "npc") , y = unit(0.05, "npc"),
                                     width = unit(0.01, "npc"), height = unit(0.05, "npc"),
                                     gp = grid::gpar(col = NA, fill = circles_col))
                           d1 <- grid::linesGrob(x = unit(c(0.6+0.033, 0.6+0.033), "npc"), y = unit(c(0.03, 0.075), "npc"),
                                              gp = grid::gpar(col = "white", lwd = 1))
                           d1_txt <- grid::textGrob(round(max_death/3), x = unit(c(0.6+0.033), "npc"), y = unit(c(0.015), "npc"), gp=gpar(fontsize=6))

                           d2 <- grid::linesGrob(x = unit(c(0.6+0.066, 0.6+0.066), "npc"), y = unit(c(0.03, 0.075), "npc"),
                                           gp = grid::gpar(col = "white", lwd = 1))
                           d2_txt <- grid::textGrob(round(2*max_death/3), x = unit(c(0.6+0.066), "npc"), y = unit(c(0.015), "npc"), gp=gpar(fontsize=6))

                           legend_title_death <- grid::textGrob("Deaths:", x = unit(c(0.55), "npc"), y = unit(c(0.05), "npc"))



                           legend_title_richter <- grid::textGrob("Richter scale value:", x = unit(c(0.2), "npc"), y = unit(c(0.05), "npc"))

                           r1 <- grid::circleGrob(x = unit(c(0.31), "npc"), y = unit(c(0.05), "npc"),r = clear_data$scale[1]*2/200,
                                             gp = grid::gpar(col = "gray20",fill = "gray60", alpha = 0.5, lwd = 1.5))
                           r1_txt <- grid::textGrob("2", x = unit(c(0.33), "npc"), y = unit(c(0.05), "npc"), gp=gpar(fontsize=10))

                           r2 <- grid::circleGrob(x = unit(c(0.36), "npc"), y = unit(c(0.05), "npc"),r = clear_data$scale[1]*4/200,
                                            gp = grid::gpar(col = "gray20",fill = "gray60", alpha = 0.5, lwd = 1.5))
                           r2_txt <- grid::textGrob("4", x = unit(c(0.385), "npc"), y = unit(c(0.05), "npc"), gp=gpar(fontsize=10))

                           r3 <- grid::circleGrob(x = unit(c(0.41), "npc"), y = unit(c(0.05), "npc"),r = clear_data$scale[1]*6/200,
                                            gp = grid::gpar(col = "gray20",fill = "gray60", alpha = 0.5, lwd = 1.5))
                           r3_txt <- grid::textGrob("6", x = unit(c(0.44), "npc"), y = unit(c(0.05), "npc"), gp=gpar(fontsize=10))

                           g <- grid::gList(g, xaxis_title, xaxis, l1, d, d1, d2, d1_txt, d2_txt, legend_title_death, xaxis_text, legend_title_richter, r1, r2, r3, r1_txt, r2_txt, r3_txt)


                           # g <- grid::nullGrob()

                           return(g)

                         })

#' Make a time line geom
#'
#' geom_timeline() makes a ggplot2::Geom with timeline and special marks for an earthquakes, occured in the defined
#' by user date interval. User must specify the following parameters: begin date; end date; column in data with earthquakes date to use;
#' column in data with earthquakes magnitude to use, column in data with earthquakes deaths to use.
#'
#'  See other parameters below.
#'
#' @param data Earthquake data in the dataframe in format of NOAA earthquakes database (Dataframe), mandatory.
#' @param mapping ggplot parameter
#' @param stat ggplot parameter
#' @param position ggplot parameter
#' @param na.rm ggplot parameter
#' @param show.legend ggplot parameter
#' @param inherit.aes ggplot parameter
#' @param ... other ggplot parameter
#'
#' The folloing parameters must be passed as parameters of aesthetic:
#'
#' xmindate Minimal date of the considering time frame (Date), mandatory.
#' xmaxdate Maximum date of the considering time frame (Date), mandatory.
#' event_date Date of earthquake, usually name of the column in data (Date), mandatory.
#' richter Richter`s magnitude of earthquakes, usually name of the column in data (numeric), mandatory.
#' death Deaths produced by earthquakes, usually name of the column in data (numeric), mandatory.
#' colour Main colour of earthquakes marks (colour name), optional
#' scale Scale of earthquakes marks, coefficient (numeric), optional
#' transparency Transparency of earthquakes marks (numeric, (0..1)), optional
#' layers Groping of earthquakes marks by some parameters, e.g. COUNTRY, optional
#'
#' @return
#' timeline geom (ggplot2 object)
#'
#' @export
#' @import tidyr dplyr grid ggplot2
#'
#' @examples
#'
#' ggplot2::ggplot() +
#'   theme_void() +
#'   geom_timeline(data = earthquakes, aes(event_date = DATE,
#'                                   xmindate = 2000,
#'                                   xmaxdate = 2010,
#'                                   richter = EQ_PRIMARY,
#'                                   death = DEATHS,
#'                                   transparency = 0.3,
#'                                   colour = "red",
#'                                   scale = 0.5,
#'                                   layers = COUNTRY))+
#'   theme(legend.position="none")
#'
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...)

{
  ggplot2::layer(
    geom = GeomTimeLine, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

GeomTimeLineLabel <- ggproto("GeomTimeLineLabel", GeomBlank,
                        required_aes = c("xmindate", "xmaxdate", "event_date", "richter", "labels"),
                        default_aes = aes(nmax = 10, layers = NA), #alpha = 0.3
                        draw_key = draw_key_blank,
                        draw_panel = function(data, panel_scales, coord) {
                          ## Transform the data first

                          #str(data)
                          #print(data)
                          #cat("---\n")

                          mindate <- as.Date(paste0(data$xmindate[1], "-01-01 00:00:00"))
                          maxdate <- as.Date(paste0(data$xmaxdate[1], "-01-01 00:00:00"))

                          clear_data <- data %>%
                            dplyr::filter((event_date>=mindate) & (event_date<=maxdate)) %>%
                            arrange(desc(richter))

                          # str(nrow(clear_data))
                          # cat("---\n")

                          nmax <- data$nmax[1]

                          xmin <- 0.1
                          xmax <- 0.9
                          y_pos <- 0.35


                          f <- factor(data$layers)
                          nlayers <- nlevels(f)
                          if ( nlayers == 0 )
                          {
                            data$layers <- 1;
                            f <- factor(data$layers)
                            nlayers <- 1
                          }

                          g <- grid::gList()


                          for (l in 1:nlayers)
                          {
                            l_data <- dplyr::filter(clear_data, layers == levels(f)[l])

                            # cat(l)
                            # cat("---\n")
                            # cat(f[l])
                            # cat("---\n")
                            # cat(nrow(l_data))
                            # cat("---\n")

                            for(i in 1:min(nmax, nrow(l_data)))
                            {

                              g <- grid::gList(g, grid::textGrob(l_data$labels[i],
                                                           x = unit(c(xmin + (xmax-xmin)*as.numeric(l_data$event_date[i] - mindate)/as.numeric(maxdate-mindate)), "npc"),
                                                           y = unit(c(y_pos+(l-1)*0.2 + 0.05), "npc"),
                                                           rot = 45,
                                                           just = c("left", "bottom"),
                                                           gp=gpar(fontsize=6))
                                               )


                              g <- grid::gList(g, grid::linesGrob(l_data$labels[i],
                                                            x = unit(rep(xmin + (xmax-xmin)*as.numeric(l_data$event_date[i] - mindate)/as.numeric(maxdate-mindate),2), "npc"),
                                                            y = unit(c(y_pos+(l-1)*0.2, y_pos+(l-1)*0.2 + 0.05), "npc"),
                                                            gp = grid::gpar(col = "gray50", lwd = 1)))

                            }


                            # cat("glist: ", length(g), "\n")
                          }


                          # g <- grid::nullGrob()

                          return(g)

                        })


#' Make a time line label geom
#'
#' geom_timeline_label() function makes a ggplot2::Geom intended for adding annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation (e.g. the location of the earthquake)
#' attached to each line. There is an option to subset to n_max number of earthquakes, where we take the n_max largest
#' (by magnitude) earthquakes. Aesthetics are x, which is the date of the earthquake and label which takes the column
#' name from which annotations will be obtained.
#'
#' @param data Earthquake data in the dataframe in format of NOAA earthquakes database (Dataframe), mandatory.
#' @param mapping ggplot parameter
#' @param stat ggplot parameter
#' @param position ggplot parameter
#' @param na.rm ggplot parameter
#' @param show.legend ggplot parameter
#' @param inherit.aes ggplot parameter
#' @param ... other ggplot parameter
#'
#' The folloing parameters must be passed as parameters of aesthetic:
#'
#' xmindate Minimal date of the considering time frame (Date), mandatory.
#' xmaxdate Maximum date of the considering time frame (Date), mandatory.
#' event_date Date of earthquake, usually name of the column in data (Date), mandatory.
#' richter Richter`s magnitude of earthquakes, usually name of the column in data (numeric), mandatory.
#' death Deaths produced by earthquakes, usually name of the column in data (numeric), mandatory.
#' labels Text labels of earthquakes, usually name of the column in data (character), mandatory.

#'
#' @return
#' Timeline labels geom (ggplot2 object)
#'
#' @export
#' @import tidyr dplyr ggplot2
#'
#' @examples
#'   ggplot2::ggplot() +
#'   theme_void() +
#'   geom_timeline(data = earthquakes, aes(event_date = DATE,
#'                                   xmindate = 2000,
#'                                   xmaxdate = 2010,
#'                                   richter = EQ_PRIMARY,
#'                                   death = DEATHS,
#'                                   transparency = 0.3,
#'                                   colour = "red",
#'                                  scale = 0.5,
#'                                   layers = COUNTRY))+
#'   theme(legend.position="none") +
#'   geom_timeline_label(data = earthquakes, aes(event_date = DATE,
#'                                         xmindate = 2000,
#'                                         xmaxdate = 2010,
#'                                         richter = EQ_PRIMARY,
#'                                         layers = COUNTRY,
#'                                        labels = LOCATION_NAME))
#'
geom_timeline_label <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...)

{
  ggplot2::layer(
    geom = GeomTimeLineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

#' Clean Earthquake location from NOAA Earthquake database
#'
#' Function eq_location_clean() cleans the LOCATION_NAME column by stripping out the country name
#' (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @param raw_earthquake Raw earthquake data in the dataframe in format of NOAA earthquakes database.
#'
#' @return
#' data frame with cleaned up LOCATION_NAME field
#'
#' @export
#'
#' @import stringr tools
#'
#' @examples
#' filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
#' raw_data <- read_delim(filename, delim = "\t")
#' data <- eq_location_clean(raw_data)
#'
eq_location_clean <- function(raw_earthquake)
{

  for (i in 1:nrow(raw_earthquake))
  {
    s <- stringr::str_split(raw_earthquake$LOCATION_NAME[i], ":")
    raw_earthquake$LOCATION_NAME[i] <- tools::toTitleCase(tolower(stringr::str_trim(s[[1]][length(s[[1]])])))
  }
  return(raw_earthquake)
}

#' Cleaning Earthquake data
#'
#' Under the cleaning we understand the following:
#' new DATE column added;
#' LOCATION cleaned up;
#' lines with no LONGITUDE or LATITUDE removed;
#' lines with no Richter magnitude removed;
#' data with YEAR B.C. removed.
#'
#' @param data Raw earthquake data in the dataframe in format of NOAA earthquakes database.
#'
#' @return
#' Data frame with cleaned up data
#'
#'
#' @export
#'
#' @import tidyr dplyr tidyverse
#' @importFrom stats setNames
#'
#' @examples
#'
#' filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
#' raw_data <- read_delim(filename, delim = "\t")
#' data <- eq_clean_data(raw_data)
#'
eq_clean_data <- function(data)
{

  earthquake <- data[(!is.na(data$MONTH)) &
                       (!is.na(data$DAY)) &
                       (data$YEAR>=0),]

  earthquake$DATE <- as.Date(paste0(earthquake$YEAR, "-", sprintf("%02d", earthquake$MONTH), "-", sprintf("%02d", earthquake$DAY)), format = "%Y-%m-%d")

  earthquake <- dplyr::mutate_(earthquake, .dots = setNames("as.numeric(DEATHS)", "DEATHS"))
  earthquake <- dplyr::mutate_(earthquake, .dots = setNames("as.numeric(DEATHS)", "DEATHS"))
  earthquake <- dplyr::mutate_(earthquake, .dots = setNames("as.numeric(LONGITUDE)", "LONGITUDE"))
  earthquake <- dplyr::mutate_(earthquake, .dots = setNames("as.numeric(LATITUDE)", "LATITUDE"))
  earthquake <- dplyr::mutate_(earthquake, .dots = setNames("as.numeric(EQ_MAG_MS)", "EQ_MAG_MS"))
  earthquake <- dplyr::mutate_(earthquake, .dots = setNames("as.numeric(EQ_PRIMARY)", "EQ_PRIMARY"))

  # earthquake <- earthquake %>% dplyr::mutate(DATE = as.Date(paste0(earthquake$YEAR, "-", sprintf("%02d", earthquake$MONTH), "-", sprintf("%02d", earthquake$DAY)), format = "%Y-%m-%d"),
  #                                            LONGITUDE = as.numeric(LONGITUDE),
  #                                            LATITUDE = as.numeric(LATITUDE),
  #                                            DEATHS = as.numeric(DEATHS),
  #                                            EQ_MAG_MS = as.numeric(EQ_MAG_MS),
  #                                            EQ_PRIMARY = as.numeric(EQ_PRIMARY))

  earthquake <- earthquake[(!is.na(earthquake$LONGITUDE)) &
                       (!is.na(earthquake$LATITUDE)) &
                       (!is.na(earthquake$EQ_PRIMARY)),]


  return(eq_location_clean(earthquake))

}


#' Make an Earthquake interactive map
#'
#' Function eq_map() takes an argument data containing the filtered data frame with earthquakes to visualize.
#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in pop up
#' window containing annotation data stored in a column of the data frame.
#' The user is able to choose which column is used for the annotation in the pop-up with
#' a function argument named annot_col. Each earthquake is represented as a circle, and the radius
#' of the circle is proportional to the earthquake's magnitude (EQ_PRIMARY).
#'
#' @param data Dataframe with earthquake data. Structure and columns names must be the same as in NOAA earthquake database.
#' @param annot_col Name of the column in the data frame used for popup annotation.
#'
#' @return
#' leaflet map object
#'
#' @export
#'
#' @import tidyr dplyr tidyverse leaflet
#'
#' @examples
#' filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
#' readr::read_delim(filename, delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & as.integer(YEAR) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
eq_map <- function(data, annot_col = "EQ_PRIMARY")
{

  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = data, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius  = ~ EQ_PRIMARY, popup = ~ paste(as.matrix(data[,annot_col], ncol = 1)))
}

#' Make a HTML description of the earthquakes
#'
#' Function eq_create_label() takes the dataset as an argument and creates an HTML label that can be used
#' as the annotation text in the leaflet map. This function put together a character string for each earthquake that
#' will show the cleaned location (as cleaned by the eq_location_clean() function descibed above),
#' the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS),
#' with boldface labels for each ("Location", "Total deaths", and "Magnitude").
#' If an earthquake is missing values for any of these, both the label and the value are skipped for
#' that element of the tag.
#'
#' @param data Dataframe with earthquake data. Structure and columns names must be the same as in NOAA earthquake database.
#'
#' @return
#' Character vector of earthquakes description. Description consist of 3 lines: Location, Magnitude, deaths.
#' If magnitude or deaths is absent, location only provided.
#'
#' @export
#'
#' @examples
#'
#' eq_create_label(earthquakes)
#'
eq_create_label <- function(data)
{
  label <- paste("<b>Location:</b>", data$LOCATION_NAME)
  b <- (!is.na(data$EQ_PRIMARY) & !is.na(data$DEATHS))
  label[b] <- paste(label[b], "<br/>",
                 "<b>Magnitude:</b>", data$EQ_PRIMARY[b], "<br/>",
                 "<b>Deaths:</b>", data$DEATHS[b], "<br/>")
  return(label)
}
