#' @title theme_WAC
#' @description Sets some ggplot2 preferences
#' @details Sets some plot defaults according to WAC preferences
#' @aliases theme_WAC
#' @author William Cooper
#' @import ggplot2
#' @import ggthemes 
#' @export theme_WAC
#' @return A ggplot theme descriptor.
#' @examples 
#' \dontrun{g <- g + theme_WAC ()}

theme_WAC <- function () {
  themeWAC <- theme_gdocs() + theme(
    plot.title = element_text(hjust=0.5, vjust=1.3),
    # axis.text = element_text(size = 16),
    panel.grid.major = element_line(color = "lightblue", linetype=5, size=0.6),
    panel.grid.minor = element_line(color = "gray90", linetype=5, size=0.6),
    panel.background = element_rect(fill = "gray95"),
    # axis.title=element_text(face="plain", size=18, colour="blue"),
    line=element_line(size=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(-0.3,"cm"),
    axis.title.x = element_text (face='plain', size=16, color='blue', margin=margin(5,0,0,0)),
    axis.text.x = element_text (size=16, margin=margin(15,0,0,0)),
    axis.title.y = element_text (face='plain', size=12, color='blue', margin=margin(0,5,0,0), angle=90),
    axis.text.y = element_text (size=12, margin=margin(0,15,0,0)),
    # axis.ticks.margin = unit (0.6,"cm"), # before ggplot2 v2.0
    legend.position=c(0.5,0.92),
    plot.margin=unit(c(0.3,0.3,1.1,1.3),"lines"),
    legend.background=element_rect(colour='black', size=0.3, fill="ivory"),
    legend.direction="horizontal",
    legend.key.width=unit(1.3,'lines'), legend.key.height=unit(0.7,'lines'),
    legend.text=element_text(size=8),
    panel.border=element_rect(colour="black",size=0.7),
    strip.background=element_rect(fill='ivory')
    # axis.title.y=element_text(angle=90))
    )
  
  return (themeWAC)
}
