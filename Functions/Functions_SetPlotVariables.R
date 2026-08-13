### Set plot variables
# Font sizes
mybasesize <- 9
mytitlesize <- 9
myaxislabelsize <- 8
mystarsize <- 4
myannotatetextsize <- 2

# General plot area
myaxislinewidth <- 0.4
mylinewidth <- 1

# Plot variables
mylinewidth <- .6
myreglinewidth <- 1.5
mycolwidth <- .8
mycollinewidth <- 0.8
myerrorbarwidth <- .5
myerrorbarlinewidth <- .28
mypointsize <- 1.2
mysubjectpointsize <- 1
mysubjectpointstroke <- .4
mypointstroke <- .5
mydodgewidth <- .94

#legendposition <- c(0.2,0.85)
legendposition <- c(0.8,0.85)

# Inj label size
myinjtrianglesize <- 4

### Make plot theme
mytheme <-  theme_prism(base_size = mybasesize, base_fontface = "plain") + 
            theme(legend.position = 'none', axis.line.x=element_line(linewidth=myaxislinewidth), axis.line.y=element_line(linewidth=myaxislinewidth),
                  axis.ticks=element_line(linewidth=myaxislinewidth), axis.ticks.length=unit(1, "mm"),
                  axis.text.x=element_text(size=myaxislabelsize), axis.text.y=element_text(size=myaxislabelsize), 
                  axis.title=element_text(size=mybasesize, face='plain'), title=element_text(size=mytitlesize, face='plain'), 
                  axis.title.y=element_text(margin=margin(r=10)))


mytheme_legend <-  theme_prism(base_size = mybasesize, base_fontface = "plain") + 
  theme(legend.position = 'top', axis.line.x=element_line(linewidth=myaxislinewidth), axis.line.y=element_line(linewidth=myaxislinewidth),
        axis.ticks=element_line(linewidth=myaxislinewidth), axis.ticks.length=unit(1, "mm"),
        axis.text.x=element_text(size=myaxislabelsize), axis.text.y=element_text(size=myaxislabelsize), 
        axis.title=element_text(size=mybasesize, face='plain'), title=element_text(size=mytitlesize, face='plain'), 
        axis.title.y=element_text(margin=margin(r=10)))#, axis.title.x = element_blank())

### Load plot colors
colorkey <- read_csv("C:/Users/rmdon/Box/Roitman Data Repository/Projects/2026_FPSignalCharacterizationAutomation_RD/Analysis/FPSignalCharacterizationAutomation_ColorKey.csv") # read in raw data

colorlists <- colorkey %>%
  group_by(Category) %>%
  summarise(colvec = list(setNames(HexCode, ColorVariableName)), .groups = "drop") %>%
  deframe()


colors_signal <- colorlists$Signal
colors_signalquality <- colorlists$SignalQuality
colors_sensor <- colorlists$Sensor


colors_signal_Y <- colors_signal[[1]]
colors_signal_N <- colors_signal[[2]]

### Set shapes
shapes_signalquality <- c("0"=6,"1"=2,"2"=0,"3"=5)


### Set plot output widths
figpanelwidth <- 7.3
figpanelheight <- 1.5

RFplotwidth_line <- 4.5
RFplotheight_line <- 1.75

RFplotwidth_bar <- 1.5
RFplotheight_bar <- 1.75
