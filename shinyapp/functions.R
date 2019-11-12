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
          plot.title = element_text(hjust = 0.5)
    )

}
