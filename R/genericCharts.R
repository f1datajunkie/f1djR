#source('ergastR-core.R')
#library(ggplot2)

#' Generate grid style plot with y-axis showing laptime difference
#'
#' \code{gridPlotTime}
#' @param df dataframe containing driver name/label, position and time
#' @param ptime the name of the time column in df
#' @param label the name of the driver name/label column in df
#' @param pos the name of the position column in df
#' @param session additional title info
#' @param intercept list of intercept line times
#' @return ggplot chart
gridPlotTime=function(df,ptime,label="driverName",pos="pos",session='',intercepts=c()){
  dd=subset(df,select=c(ptime,label,pos))
  colnames(dd)= c("time", "label", "pos")
  g=ggplot(dd)

  #Add in intercept lines on lowest layer
  for (intercept in intercepts) {
    g = g + geom_vline(xintercept=intercept, col='grey',linetype='dotted')
  }

  g=g+ geom_point(aes(x=time,y="Time"),shape=1)
  #Split the drivers into two groups - odd position number and even position number
  #Use each group as a separate y-axis categorical value
  g = g + geom_text(data=subset(dd, subset=(pos %% 2!=0)),
                    aes(x=time, y="1,3,5,...", label=label), size=3)

  g = g + geom_text(data=subset(dd, subset=(pos %% 2==0)),
                    aes(x=time, y="2,4,6,...", label=label), size=3)

  #Tweak the theme
  g = g + theme_classic() + ylab(NULL) + xlab('Laptime (s)')

  #g = g + geom_vline(xintercept=101.3, col='grey')
  g=g+coord_flip() #Flip the co-ordinates

  txt="Session Best Time Comparisons"
  if (session !='') txt=paste0(txt,' (',session,')')
  g=g+ggtitle(txt)
  g
}
