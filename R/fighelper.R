# TODO
# build_figures should return the figfiles list
# with the file name modified with the appropriate extension
# and the plot slot changed to pre_rendered

build_figures <- function(figfiles){
  debug <- FALSE
  for (fig in figfiles) {
    # default settings
    r <- 72
    w <- 7
    h <- 7
    u <- "in"
    if("width" %in% names(fig)) { w <- fig$width }
    if("height" %in% names(fig)) { h <- fig$height }
    if("res" %in% names(fig)) { r <- fig$res }
    if("units" %in% names(fig)) { u <- fig$units }
    
    if("plot" %in% names(fig)) {
      if(debug) { print(paste(fig$file,w,h,r,u,sep=",")) }
      if(fig$plot[1]!="pre_rendered") {
        file <- paste(fig$file,".png",sep="")
        png(filename=file,width=w,height=h,res=r,units=u)  
        print(fig$plot)
        dev.off()  
      }
    }
    if("table" %in% names(fig)) {
      write.csv(fig$table,fig$file)
    }
  }
}

display_figures <- function(figfiles, fig_start=0, tab_start=0, supp_start=0){
  figure_index <- fig_start
  table_index <- tab_start
  supp_index <- supp_start
  extras_index <- 0
  dw <- 800
  trunclen <- 10
  
  for (name in names(figfiles)) {
    display_width <- dw
    fig <- figfiles[[name]]
    file <- fig$file
    if("display_width" %in% names(fig)) { display_width <- fig$display_width }
    cat("<br><blockquote>")

    
    if( ("plot" %in% names(fig)) & !("supplementary" %in% names(fig))) {
      file <- paste(fig$file,".png",sep="")
      figure_index <- figure_index + 1
      cat(paste("<img src='",file,"' width='",display_width,"'>",sep=""))
      cat(paste("<p><a href='",file,"'><br><b>Figure ",figure_index,". [",name,"].</b></a> ",fig$legend,"</p>",sep=""))
    }
    if( ("table" %in% names(fig)) & !("supplementary" %in% names(fig)) ) {
      table_index <- table_index + 1
      print(xtable(fig$table[1:trunclen,]), type="html",sanitize.text.function = force,comment=F)
      if(nrow(fig$table)>trunclen) { 
        cat(paste("<small>Table was truncated to",trunclen,"rows for display. Click link to see whole table</small>",sep=" ")) 
      } 
      cat(paste("<p><b><a href='",file,"'>Table ",table_index,". [",name,"].</b></a> ",fig$legend,"</p>",sep=""))
    }
    
    if( "supplementary" %in% names(fig) ) {
      if (grepl(pattern="extra",x=fig$supplementary,ignore.case=TRUE)) {
        extras_index <- extras_index + 1
        supp_identifier <- paste("E",extras_index,sep="")
      } else {
        supp_index <- supp_index + 1
        supp_identifier <- paste("S",supp_index,sep="")
      }
      if("table" %in% names(fig)) {
        print(xtable(fig$table[1:trunclen,]), type="html",sanitize.text.function = force,comment=F)
        if(nrow(fig$table)>trunclen) { 
          cat(paste("<small>Table was truncated to",trunclen,"rows for display. Click link to see whole table</small>",sep=" ")) 
        }
      }    
      if("plot" %in% names(fig)) {
        file <- paste(fig$file,".png",sep="")
        cat(paste("<img src='",file,"' width='",display_width,"'>",sep=""))
      }
      
      #supp_index <- supp_index + 1
      cat(paste("<p><b><a href='",file,"'>",fig$supplementary, " ",supp_identifier,". [",name,"].</b></a> ",fig$legend,"</p>",sep=""))
    }
    cat("</blockquote><hr>")
  }
}