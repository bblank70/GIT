
library(dplyr)
library(tcltk)
library(utils)

x<-c(1,2,5)
y<-c(7,2.2,8)
c<-c("red", "yellow", "blue")

df<-as.data.frame(x,y,c,stringsAsFactors=FALSE)

answer<-tk_select.list(df$c, preselect = NULL, multiple = TRUE,
            title = "Select items to keep:")

selected_df<-filter(df, c %in% answer)

selected_df