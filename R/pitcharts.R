pitChartTheme=function(g){
  g = g + theme_minimal(base_family="Arial Narrow")
  g = g + theme(panel.grid.major.y=element_blank())
  g = g + theme(panel.grid.minor=element_blank())
  g = g + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
  g = g + theme(axis.text.y=element_text(margin=margin(r=0, l=0)))
  g = g + theme(plot.margin=unit(rep(30, 4), "pt"))
  g = g + theme(plot.title=element_text(face="bold"))
  g = g + theme(plot.subtitle=element_text(margin=margin(b=10)))
  g = g + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
  g
}


pitLollipop=function(pitStops, title='Pit Report',limits=NULL){
  pitTotals = ddply( pitStops, .(code), summarise,
                   totduration=sum(rawduration))

  g = ggplot(pitTotals,aes(x=reorder(code, totduration), y=totduration))
  g = g + geom_lollipop(point.colour="steelblue", point.size=3) + coord_flip()

  g = g + scale_y_continuous(expand=c(0,1), limits=limits)
  g = g + labs(x=NULL, y=NULL,
               title=title,
               subtitle="Overall pit stop times",
               caption="Data from Ergast database, ergast.com/mrd")
  g = pitChartTheme(g)
  g
}

race.plot.pits.cumulativeTime=function(pitStops){
  #The reorder() function sums the durations for stops grouped by driverRef
  #The grouping operation results from setting the fill parameter
  g=ggplot(pitStops,aes(x=reorder(code,rawduration,sum),y=rawduration,fill=factor(stopnum)))
  g=g+geom_bar(stat='identity')
  #Do we want a legend identifying pitstop number or not?
  #g=g+guides(fill=guide_legend(title="Stop"))
  g=g+guides(fill=FALSE)
  #Add some styling - colour the bars and simplify the background
  g=g+scale_fill_grey(start=0.4,end=0.8)+theme_bw()
  #Work on the labeling
  g=g+ylab("Cumulative Pit Time (s)")
  #Flip the chart to a horizontal bar chart
  g+coord_flip()
}


race.plot.pits.dodged=function(.racePits){
  g=ggplot(.racePits,aes(x=reorder(code,rawduration,sum),y=rawduration))
  g=g+geom_bar(aes(fill=factor(stopnum)),stat='identity',position='dodge')
  #Annotate the chart with mediam and mean pit stop times, ignoring null values
  g=g+geom_hline(yintercept=mean(na.omit(.racePits$rawduration)),
                 linetype='dotted',colour='grey')
  g=g+geom_hline(yintercept=median(na.omit(.racePits$rawduration)),
                 linetype='dashed',colour='grey')
  g=g+guides(fill=guide_legend(title="Stop"))
  g=g+ylab("Individual Pit Time (s)")
  g+coord_flip()+xlab(NULL)+theme_bw()
}


#TO DO - highlight penalty stops? Drive thru should be easy?
pitHistory=function(pitStops){
  g=ggplot(pitStops, aes(x=lap,y=reorder(code,rawduration,sum)))
  g=g+geom_tile(aes(fill=rawduration),colour = "white")
  g=g+scale_fill_gradient(low="lightblue",high="steelblue")
  g+xlab('Lap')+ylab(NULL)+guides(fill=guide_legend(title="Pit time (s)"))+theme_bw()
}

#line segment to show elapsed time of stop?
#pitHistoryLine=function(pitStops){
  #g=ggplot(pitStops, aes(lap,code))
  #g=g+geom_tile(aes(fill=rawduration),colour = "white")
  #g=g+scale_fill_gradient(low="white",high="steelblue")
  #g+xlab('Lap')+ylab(NULL)+theme_bw()
#}
