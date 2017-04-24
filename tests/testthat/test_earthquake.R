library(earthquakes)

test_that("clean data", {

  filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
  raw_data <- read_delim(filename, delim = "\t")

  data <- eq_clean_data(raw_data)

  d1 <- data[(data$COUNTRY == "RUSSIA") & (data$DATE == as.Date("1652-06-08")),]

  expect_that(d1$EQ_PRIMARY, equals(5.8))
  expect_that(d1$YEAR, equals(1652))

  d2 <- data[(data$COUNTRY == "RUSSIA") & (data$DATE == as.Date("2008-10-11")),]
  expect_that(d2$EQ_PRIMARY, equals(5.7))
  expect_that(d2$DEATHS, equals(13))

})


test_that("clean location", {

  data <- tbl_df("PERU:  LIMA,SALINAS-HUAURA,LIMA,CALLAO,CHANCAY")
  colnames(data) <- c("LOCATION_NAME")
  name <- as.character(eq_location_clean(data)[1,1])
  expect_that(name, equals("Lima,salinas-Huaura,lima,callao,chancay"))

})


test_that("create label", {

  test_data <- tbl_df(0)
  test_data$LOCATION_NAME <- "Sanriku"
  test_data$DEATHS <- 27122
  test_data$EQ_PRIMARY<- 8.3

  tag <- eq_create_label(test_data)

  expect_that(tag, equals("<b>Location:</b> Sanriku <br/> <b>Magnitude:</b> 8.3 <br/> <b>Deaths:</b> 27122 <br/>"))

  test_data$DEATHS <- NA
  test_data$EQ_PRIMARY<- 8.3
  tag <- eq_create_label(test_data)
  expect_that(tag, equals("<b>Location:</b> Sanriku"))

  test_data$DEATHS <- 27122
  test_data$EQ_PRIMARY<- NA
  tag <- eq_create_label(test_data)
  expect_that(tag, equals("<b>Location:</b> Sanriku"))

})


test_that("geom_timeline_label", {
  require(digest)

  g <- ggplot2::ggplot() +
     theme_void() +
     # geom_timeline(data = earthquakes, aes(event_date = DATE,
     #                                 xmindate = 2000,
     #                                 xmaxdate = 2010,
     #                                 richter = EQ_PRIMARY,
     #                                 death = DEATHS,
     #                                 transparency = 0.3,
     #                                 colour = "red",
     #                                 scale = 0.5,
     #                                 layers = COUNTRY))+
     # theme(legend.position="none") +
     geom_timeline_label(data = earthquakes, aes(event_date = DATE,
                                           xmindate = 2000,
                                           xmaxdate = 2010,
                                           richter = EQ_PRIMARY,
                                           layers = COUNTRY,
                                           labels = LOCATION_NAME))
  filename <- "test_geom_timeline_label.png"
  ggsave(filename, plot = g, width = 10, height = 10)
  connection <- file(filename, "rb")
  raw_img <- readBin(connection, "raw", n = file.size(filename))
  crc <- digest(raw_img, algo = "crc32", serialize = TRUE)
  expect_that(crc, equals("5e58b604"))
  #expect_that(crc, equals("ade97fc5"))


})

test_that("geom_timeline", {
  require(digest)

  g <- ggplot2::ggplot() +
    theme_void() +
    theme(legend.position="none") +
    geom_timeline(data = earthquakes, aes(event_date = DATE,
                                          xmindate = 2000,
                                          xmaxdate = 2010,
                                          richter = EQ_PRIMARY,
                                          death = DEATHS,
                                          transparency = 0.3,
                                          colour = "red",
                                          scale = 0.5,
                                          layers = COUNTRY))

  filename <- "test_geom_timeline.png"
  ggsave(filename, plot = g, width = 10, height = 10)
  connection <- file(filename, "rb")
  raw_img <- readBin(connection, "raw", n = file.size(filename))
  crc <- digest(raw_img, algo = "crc32", serialize = TRUE)

  expect_that(crc, equals("bbdb0f4"))
  #expect_that(crc, equals("6fa5e984"))

})

test_that("eq_map", {
  require(digest)
  map_ <- eq_map(earthquakes)
  expect_that(digest(map_), equals("db17419b1ea0c4a0ad2d4b9a9966e75e"))
})
