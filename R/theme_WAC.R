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
    plot.title = element_text(hjust=0.5),
    axis.text = element_text(size = 16),
    panel.grid.major = element_line(color = "lightblue", linetype=5),
    panel.background = element_rect(fill = "gray95"),
    axis.title=element_text(face="plain",size=18,colour="blue"),
    line=element_line(size=1),
    axis.ticks = element_line(size=1),
    axis.ticks.length = unit(-0.35,"cm"),
    axis.ticks.margin = unit (0.6,"cm"),
    legend.position=c(0.5,0.96),
    plot.margin=unit(c(1.5,1,0.5,0.5),"lines"),
    plot.title=element_text(vjust=1.3),
    legend.background=element_rect(colour='black', size=0.3, fill="ivory"),
    legend.direction="horizontal",
    legend.title=element_text(size=12),
    panel.border=element_rect(colour="black",size=0.7),
    axis.title.y=element_text(angle=90))
  
  return (themeWAC)
}
