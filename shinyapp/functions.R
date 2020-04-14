##############################
# Functions for Modifying UI #
##############################

theme_qtmb <- function(){
  theme_classic() +
    theme(strip.background = element_blank(),
          plot.background = element_rect(inherit.blank = T),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey"),
          text = element_text(family = "sans", size = 16),
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(size = 9, face = "italic")
    )

}


# This section collects a few helper funs for the ui input.

#' selectInput.soilProperty
#'
#' @param id unique ID for each input box
#' @param label a string to name the input box
#' @param choices options
#'
#' @param url the url to the help document
#' @param name
#'
selectInput.soilProperty <- function(id, label = id, choices = soil.texture, name = '(Help Document)',url = 'https://www.far.org.nz/assets/files/blog/files//e7b9c43f-c4f6-52cb-b0f9-1e9e6539bb91.pdf'){
  selectizeInput(inputId = id,
                 label = p(label,
                           a(name, href = url, target = "_blank"
                           )),
                 choices = choices,
                 options = list(
                   placeholder = 'Please select an option below',
                   onInitialize = I('function() { this.setValue(""); }')))
}



#' soil.para.filters
#'
#' @description filter the soil parameters for CF, CF2 and BD based on the input texture moisture and depth
#'
#' @param soil the data frame that holds the soil parameter information
#' @param start a string or a reactive object,the sampling depth start for add into the filtered data frame
#' @param end a string or a reactive object, the sampling depth end for filtering
#' @param inputTexture a reactive object, soil texture input
#' @param inputMoisture a reactive object, soil moisture input
#' @param inputQtest a reactive object, quick test results
#' @param sampleLength a reactive object, sample length in integer
#'
#' @return
#' @export
#'
#' @examples
soil.para.filters <- function(soil = soil, start, end, inputTexture, inputMoisture, inputQtest, sampleLength){
  if(end > 0){
    if(end <= 30L ){
      df <- soil %>%
        filter(Texture == inputTexture,
               Moisture == inputMoisture,
               Sampling.Depth == "0-30") %>%
        mutate(qtest_user.input = inputQtest,
               Sample.length = sampleLength,
               Sampling.Depth = paste0(start, "-", end))
    } else if(end > 30L & end <= 60L){
      df <- soil %>%
        filter(Texture == inputTexture,
               Moisture == inputMoisture,
               Sampling.Depth == "30-60") %>%
        mutate(qtest_user.input = inputQtest,
               Sample.length = sampleLength,
               Sampling.Depth = paste0(start, "-", end))
    } else {
      df <- soil %>%
        filter(Texture == inputTexture,
               Moisture == inputMoisture,
               Sampling.Depth == "60-90") %>%
        mutate(qtest_user.input = inputQtest,
               Sample.length = sampleLength,
               Sampling.Depth = paste0(start, "-", end))
    }
  }
}




evaluate_inputDepth <- function(inputDepth){
  depth <- as.integer(inputDepth)
  depth <- ifelse(is.na(depth) | is.null(depth), 0, depth)

}

