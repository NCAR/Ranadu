
#' @title setFileName
#' @description GUI for selecting from available projects and files
#' @details Generates a set of buttons from which to select the project, and 
#' then another set from which to select the data file. Clicking on the
#' buttons selects that item. 
#' @aliases setFileName
#' @author William Cooper
#' @export setFileName
#' @import tcltk
#' @param Project The character-name of the project. If provided, the function proceeds to
#' display the files in that project directory. Otherwise, if "Project" is omitted
#' or NA (the default) a set of available projects is displayed. Click on one to
#' select it.
#' @param Flight A character name specifying the data file. If omitted, a list
#' of available files will be displayed from which you can select the file by
#' clicking on the button. If omitted, '.nc' will be appended to the name.
#' If "Flight" is specified as, for example, "rf01", the
#' name will be modified by prefixing the name of the project; e.g., CSETrf01.nc
#' @return A character variable specifying the location of the data.file, 
#' suitable for use in "getNetCDF()" or "DataFileInfo()".
#' @examples 
#' \dontrun{setFileName("CSET", "CSETrf05.nc")}

setFileName <- function (Project=NA, Flight=NA) {
  if (!interactive() && (is.na(Project) || is.na(Flight))) return (NA)
  requireNamespace("tcltk")
  ## callback functions:
  varClick <- function(v) {
    # print (sprintf("entry to varClick, argument %s", v))
    n <- as.integer(v)
    if (vnSel[n]) {
      vnSel[n] <<- FALSE
      eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='black', background='gray90')", n)))
      PRJS <<- PRJS[-match(vn[n], PRJS)]
    } else {
      vnSel[n] <<- TRUE
      eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='blue', background='yellow')", n)))
      PRJS[length(PRJS)+1] <<- vn[n]
    }
    #tclvalue(labelName[2]) <- paste(v,"#", sep='')
    #label1 <- tklabel(guiVar, text = tclvalue(labelText))
    #tkmessageBox (message = sprintf("Clicked button %s", v))
    tkdestroy (guiVar)
  }
  GoBack <- function () {
    # assign("VarList", VarNames, envir=.GlobalEnv)
    tkdestroy (guiVar)
  }
  SelectAll <- function () {
    PRJS <<- vn
    for (m in 1:length(vn)) {
      vnSel[m] <<- TRUE
      eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='blue', background='yellow')", m)))
    }
  }
  RemoveAll <- function () {
    PRJS <<- vector()
    for (m in 1:length(vn)) {
      vnSel[m] <<- FALSE
      eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='black', background='gray90')", m)))
    }
  }
  ## start of main tcltk function
  if (is.na(Project[1])) {
    # select the project:
    PRJ <- sort(list.dirs(DataDirectory(), full.names = FALSE, recursive = FALSE))
    vnSel <- vector('logical', length(PRJ))
    PRJS <- PRJ
    vn <- PRJ
    guiVar <- tktoplevel()
    tktitle(guiVar) <- sprintf ("Available Projects")
    topMenu <- tkmenu(guiVar)           # Create a menu
    tkconfigure(guiVar, menu = topMenu) # Add it to the 'guiVar' window
    #txt <- tktext(guiVar)       # Create a text widget
    #tkgrid(txt)             #
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)
    allMenu <- tkmenu (topMenu, tearoff=FALSE)
    noneMenu <- tkmenu (topMenu, tearoff=FALSE)
    tkadd(fileMenu, "command", label = "Return Selections and Hide Window", command = function () GoBack())
    tkadd (fileMenu, "command", label = "Select ALL variables", command = function () SelectAll ())
    tkadd (fileMenu, "command", label = "Clear ALL selections", command = function () RemoveAll ())
    tkadd(fileMenu, "command", label = "Quit without saving", command = function() tkdestroy(guiVar))
    tkadd(topMenu, "cascade", label = "Actions", menu = fileMenu)
    myFont <- tkfont.create(family="times",size=7, weight='bold')
    NC <- 16
    for (i in seq(0,length(vn),NC)) {
      for (j in 1:NC) {
        eval(parse(text=sprintf("lbl%d <- tkbutton (guiVar, text=vn[%d], font=myFont, 
                              command=function() varClick(%d))", i+j, i+j, i+j)))
        if (vn[i+j] %in% PRJS) {
          vnSel[i+j] <- FALSE
          eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='yellow', background='blue')",
                                  i+j)))
        }
      }
      eval (parse (text=sprintf("tkgrid(lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d)",
                                i+1, i+2, i+3, i+4, i+5, i+6, i+7, i+8, i+9, i+10, i+11, i+12, i+13, i+14, i+15, i+16)))
    }
    tkfocus(guiVar)
    tkwait.window(guiVar)
    Project <- PRJS[vnSel]  ## have the project
  }
  # print (sprintf ('Project is %s', Project))
  ## now get the desired file
  if (is.na(Flight)) {
    FL <- sort(list.files(file.path(DataDirectory(), Project), pattern = '*.nc'))
    print(FL)
    ## Choose the file:
    vnSel <- vector('logical', length(FL))
    varClick <- function(v) {
      # print (sprintf("entry to varClick, argument %s", v))
      n <- as.integer(v)
      if (vnSel[n]) {
        vnSel[n] <<- FALSE
        eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='black', background='gray90')", n)))
        FLS <<- FLS[-match(vn[n], FLS)]
      } else {
        vnSel[n] <<- TRUE
        eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='blue', background='yellow')", n)))
        FLS[length(FLS)+1] <<- vn[n]
      }
      #tclvalue(labelName[2]) <- paste(v,"#", sep='')
      #label1 <- tklabel(guiVar, text = tclvalue(labelText))
      #tkmessageBox (message = sprintf("Clicked button %s", v))
      tkdestroy (guiVar)
    }
    FLS <- FL
    vn <- FL
    guiVar <- tktoplevel()
    tktitle(guiVar) <- sprintf ("Available netCDF files")
    topMenu <- tkmenu(guiVar)           # Create a menu
    tkconfigure(guiVar, menu = topMenu) # Add it to the 'guiVar' window
    #txt <- tktext(guiVar)       # Create a text widget
    #tkgrid(txt)             #
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)
    allMenu <- tkmenu (topMenu, tearoff=FALSE)
    noneMenu <- tkmenu (topMenu, tearoff=FALSE)
    tkadd(fileMenu, "command", label = "Return Selections and Hide Window", command = function () GoBack())
    tkadd (fileMenu, "command", label = "Select ALL variables", command = function () SelectAll ())
    tkadd (fileMenu, "command", label = "Clear ALL selections", command = function () RemoveAll ())
    tkadd(fileMenu, "command", label = "Quit without saving", command = function() tkdestroy(guiVar))
    tkadd(topMenu, "cascade", label = "Actions", menu = fileMenu)
    myFont <- tkfont.create(family="times",size=7, weight='bold')
    NC <- 16
    for (i in seq(0,length(vn),NC)) {
      for (j in 1:NC) {
        eval(parse(text=sprintf("lbl%d <- tkbutton (guiVar, text=vn[%d], font=myFont, 
                              command=function() varClick(%d))", i+j, i+j, i+j)))
        if (vn[i+j] %in% FLS) {
          vnSel[i+j] <- FALSE
          eval(parse(text=sprintf("tkconfigure (lbl%d, foreground='yellow', background='blue')",
                                  i+j)))
        }
      }
      eval (parse (text=sprintf("tkgrid(lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d, lbl%d)",
                                i+1, i+2, i+3, i+4, i+5, i+6, i+7, i+8, i+9, i+10, i+11, i+12, i+13, i+14, i+15, i+16)))
    }
    tkfocus(guiVar)
    tkwait.window(guiVar)
    Flight <- FLS[vnSel]  ## have the file name
    # print (sprintf ('Project %s File %s', Project, Flight))
  } else {
    if (!grepl('.nc$', Flight)) {Flight <- paste0(Flight, '.nc')}
    if (!grepl(Project, Flight)) {Flight <- paste0(Project, Flight)}
  }
  fname <- file.path(DataDirectory(), sprintf('%s/%s', Project, Flight))
  return(fname)
}
#  tkbind(lbl1, "<1>", varClick(i+1))


# Project <- "WINTER"
# Flight <- "rf11"
# VarList <- standardVariables ()
# fname = sprintf ("%s%s/%s%s.nc", DataDirectory(), Project, Project, Flight)
# vnames <- setVariableList (fname, VarList)
# ## cat (vnames)


