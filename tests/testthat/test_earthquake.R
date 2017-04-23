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

# geom_timeline_label
# geom_timeline

#test_that("eq_map", {
#  map_ <- eq_map(earthquakes)
#  expect_that(earthquakes::map, equals("<b>Location:</b> Sanriku"))
#})
