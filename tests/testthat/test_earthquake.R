library(testthat)
library(earthquakes)

test_check("earthquakes")

test_that("clean data", {

  #filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
  #raw_data <- read_delim(filename, delim = "\t")

  #expect_that(as.integer(a[[1]][1,2]), equals(2015))

  return(TRUE)
})


test_that("clean location", {

  #filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
  #raw_data <- read_delim(filename, delim = "\t")

  #expect_that(as.integer(a[[1]][1,2]), equals(2015))

  return(TRUE)
})


test_that("create label", {

  #filename <- system.file("extdata", "earthquakes.tsv.gz", package = "earthquakes")
  #raw_data <- read_delim(filename, delim = "\t")

  #expect_that(as.integer(a[[1]][1,2]), equals(2015))

  return(TRUE)
})

# geom_timeline_label
# geom_timeline
# eq_map
