#' The aim of this function is to show how to represent a qualitative variable as proportion or as percent: numerically and graphically.
#' @param x the data as a factor with labels
#' @param plot barplot as default diagram. It is possible to show a piechart but with the percentage. 
#' @param cols is for the color of the bars: orange and green are on default but they can be easily changed.
#' @param place is for the legend to placed on top left, top right, bottom right or bottom left.
#' @param title_x is for the title of the plot
#' @param ylims is for the extreme values on the y-axis.
#' @return Plot and table for the qualitative variable.
#' @export
#' @examples
#'library(SUdatasets)
#'str(smoking)
#'smoking$Gender<-ifelse(smoking$Gender=="1","Women","Men")
#'describeQL(smoking$Gender,plot="pie",title_x = "Gender",place="bottomleft")
#'describeQL(smoking$Gender,plot="barplot",title_x = "Gender",place="topleft")


describeQL<-function(x,plot="barplot",cols=c("orange","dark green"),place="",title_x,ylims=c(0,1)){
  
  props<-prop.table(table(x))
  if(plot=="barplot"){
    barplot(props,las=1,col=cols,main=title_x,ylim=ylims)
    legend(place,  paste(names(props),round(props,4), sep=" "), cex=0.7, fill=cols)
    
  }
  if(plot=="pie"){
    
    #Förbättra diagram
    slices <- props
    
    # Procent och avrundning av decimaler
    slices_labels <- round(props * 100, 2)
    
    # symbol '%' i diagram
    slices_labels <- paste(names(props),slices_labels, "%", sep=" ")
    
    # paj med etiketter och färgerna 
    pie(slices, main=title_x, col=cols, labels=slices_labels, cex=0.8)
    
    # Externt etikett   
    #px2$parents
    legend(place, names(props), cex=0.7, fill=cols)
    
  }
  
  
  return(props)
}






describeQL<-function(x,plot="barplot",cols=c("orange","dark green"),place="",title_x,ylims=c(0,1)){
  
  props<-prop.table(table(x))
  if(plot=="barplot"){
    barplot(props,las=1,col=cols,main=title_x,ylim=ylims)
    legend(place,  paste(names(props),round(props,4), sep=" "), cex=0.7, fill=cols)
    
  }
  if(plot=="pie"){
    
    #Förbättra diagram
    slices <- props
    
    # Procent och avrundning av decimaler
    slices_labels <- round(props * 100, 2)
    
    # symbol '%' i diagram
    slices_labels <- paste(names(props),slices_labels, "%", sep=" ")
    
    # paj med etiketter och färgerna 
    pie(slices, main=title_x, col=cols, labels=slices_labels, cex=0.8)
    
    # Externt etikett   
    #px2$parents
    legend(place, names(props), cex=0.7, fill=cols)
    
  }
  
  
  return(props)
}


