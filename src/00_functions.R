
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}



readxl_online <- function(url, type = NULL, ...) {
  test <- stringr::str_detect(url, "[.]xls|[.]zip")
  if (test == FALSE) {
    print(message("Expecting file extension of type .xlsx, .xls or .zip. Check the URL or the data source for the correct file extension and use the type argument"))
  }
  # test for individual file extensions for xls use look forward, xls not
  # followed by x
  t1 <- stringr::str_detect(url, "[.]xlsx")
  t2 <- stringr::str_detect(url, "[.]xls(?!x)")
  tz <- stringr::str_detect(url, "[.]zip")
  if (t1 == TRUE) {
    type = ".xlsx"
  }
  if (t2 == TRUE) {
    type = ".xls"
  }
  if (tz == TRUE) {
    httr::GET(url, write_disk("tmp.zip", overwrite = TRUE))
    tmp <- unzip("tmp.zip")
    # On osx more than one file name is returned, select first element.
    df <- readxl::read_excel(tmp[[1]])
    return(df)
  }
  if (!is.null(type)) {
    type = type
    
  }
  df <- httr::GET(url, write_disk(paste0("tmp", type), overwrite = TRUE))
  df <- readxl::read_excel(paste0("tmp", type))
  
}













# Multiple plot function   (http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

