library(testthat)
library(shinytest)

#Test if app runs----
test_that("Application works", {
  expect_pass(testApp("shinyapp", compareImages = FALSE))
})


#Test Crop Section input and output value----
# test_that("Output matches Input", {
#
#   #connect the app
#   app <- ShinyDriver$new("shinyapp")
#
#   #set up input selections randomly
#   app$setInputs(input_system = "Intensive vegetable production")
#   app$setInputs(input_system = "Pasture conversion")
#   app$setInputs(input_crop = "Barley_Spring")
#   app$setInputs(input_PlantingDate = "2018-10-25")
#   app$setInputs(input_componentYield = "10")
#   vals <- app$getAllValues()
#
#
#   vals$output$Harvested_value
#
#   #expect
#   expect_equal(vals$output$Seasonal.N.uptake, 224)
#
# })

