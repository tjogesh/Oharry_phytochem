# options - the purpose of this script is to set up default charting and other parameters


# captions
caption <- 'Analysis performed by DataSF'

# custom ggtheme
# http://joeystanley.com/blog/custom-themes-in-ggplot2

# theme_datasf <- function () { 
#   theme_bw(base_size=12, base_family="Roboto Condensed Bold") + 
#     theme(plot.subtitle = element_text(color="#666666"),
#           plot.title = element_text(family="Roboto Condensed Bold"),
#           plot.caption = element_text(color="#AAAAAA", size=6)
#     ) 
# }

graph_theme<- theme_bw() +
  theme( axis.text.y = element_text( size=16),
         axis.text.x = element_text( size=16),
         axis.title.x = element_text( size=16),
         axis.title.y = element_text( size=16))