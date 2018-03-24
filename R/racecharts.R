battlemap_encoder=function(lapTimes){
  lapTimes=plyr::rename(lapTimes, c("cuml"="acctime"))

  #Order the rows by accumulated lap time
  lapTimes=arrange(lapTimes,acctime)
  #This ordering need not necessarily respect the ordering by lap.

  #Flag the leader of a given lap - this will be the first row in new leader lap block
  lapTimes$leadlap= (lapTimes$position==1)

  #Calculate a rolling count of leader lap flags.
  #Recall that the cars are ordered by accumulated race time.
  #The accumulated count of leader flags is the lead lap number each driver is on.
  lapTimes$leadlap=cumsum(lapTimes$leadlap)
  lapTimes$lapsbehind=lapTimes$leadlap-lapTimes$lap

  lapTimes=arrange(lapTimes,leadlap,acctime)
  lapTimes=ddply(lapTimes,.(leadlap),transform,
                 trackpos=1:length(position))

  #Order the drivers by lap and position
  lapTimes=arrange(lapTimes,lap,position)
  #Calculate the DIFF between each pair of consecutively placed cars at the end of each race lap
  #Then calculate the GAP to the leader as the sum of DIFF times
  lapTimes=ddply(lapTimes, .(lap), mutate,
                 diff=c(0,diff(acctime)),
                 gap=cumsum(diff)  )

  lapTimes=ddply(lapTimes, .(driverId), mutate,lapped=c(0,diff(lapsbehind)))

  #Order the drivers by lap and reverse position
  lapTimes=arrange(lapTimes,lap, -position)
  #Calculate the DIFF between each pair of consecutively reverse placed cars at the end of each race lap
  lapTimes=ddply(lapTimes, .(lap), mutate,
                 chasediff=c(0,diff(acctime)) )


  lapTimes$tradgap=as.character(lapTimes$gap)
  lapsbehind=function(lap,leadlap,gap){
    if (lap==leadlap) return(gap)
    paste("LAP+",as.character(leadlap-lap),sep='')
  }

  lapTimes$tradgap=mapply(lapsbehind,lapTimes$lap,lapTimes$leadlap,lapTimes$gap)


  #http://en.wikipedia.org/wiki/Template:F1stat
  #driverCodes=c("hamilton"= "HAM", "vettel"= "VET", "rosberg"= "ROS", "ricciardo"= "RIC",
  #              "kvyat"= "KVY", "max_verstappen"= "VES", "massa" = "MAS", "grosjean"= "GRO",
  #              "bottas"= "BOT", "ericsson"= "ERI", "raikkonen"= "RAI", "maldonado" = "MAL",
  #              "hulkenberg"= "HUL", "perez"= "PER", "sainz"= "SAI", "nasr"= "NAS",
  #              "button" = "BUT", "alonso"= "ALO", "merhi"= "MER", "stevens"="STE",
  #              "gutierrez" = "GUT","wehrlein" = "WEH","jolyon_palmer" = "PAL",
  #              "haryanto" = "HAR","kevin_magnussen"="MAG","ocon" ="OCO","stroll"="STR",
  #              "giovinazzi"="GIO","vandoorne" = "VAN","kobayashi"="KOB", "chilton"="CHI",
  #              "jules_bianchi"="BIA", "resta"="DIR"
  #)

  #driverCode=function(name) unname(driverCodes[name])
  lapTimes['code']=apply(lapTimes['driverId'],2,function(x) driverCodeMap(x))

  #TO DO - need to add something to add a dummy label if we get a mismatch

  #Arrange the drivers in terms of increasing accumulated race time
  lapTimes = plyr::arrange(lapTimes, acctime)
  #For each car, calculate the DIFF time to the car immediately ahead on track
  lapTimes$car_ahead=c(0,diff(lapTimes$acctime))
  #Identify the code of the driver immediately ahead on track
  lapTimes$code_ahead=c('',head(lapTimes$code,n=-1))
  #Identify the race position of the driver immediately ahead on track
  lapTimes$position_ahead=c('',head(lapTimes$position,n=-1))

  #Now arrange the drivers in terms of decreasing accumulated race time
  lapTimes = plyr::arrange(lapTimes, -acctime)
  #For each car, calculate the DIFF time to the car immediately behind on track
  lapTimes$car_behind=c(0,diff(lapTimes$acctime))
  #Identify the code of the driver immediately behind on track
  lapTimes$code_behind=c('',head(lapTimes$code,n=-1))
  #Identify the race position of the driver immediately behind on track
  lapTimes$position_behind=c(NA,head(lapTimes$position,n=-1))

  #put the lapTimes dataframe back to increasing accumulated race time order.
  lapTimes = arrange(lapTimes, acctime)

  lapTimes = arrange(lapTimes, lap,position)
  lapTimes = ddply(lapTimes,.(lap),transform,
                   code_raceahead=c(NA,head(code,n=-1)))

  lapTimes = arrange(lapTimes, -lap,-position)
  lapTimes = ddply(lapTimes,.(lap),transform,
                   code_racebehind=c('',head(code,n=-1)))

  arrange(lapTimes, acctime)
}

dirattr=function(attr,dir='ahead') paste(attr,dir,sep='')

#We shall find it convenenient later on to split out the initial data selection
##lapTimes=lapsData.df(2015,2)
battlemap_df_driverCode=function(lapTimes,driverCode){
  lapTimes[lapTimes['code']==driverCode,]
}

battlemap_core_chart=function(df,g,dir='ahead'){
  car_X=dirattr('car_',dir)
  code_X=dirattr('code_',dir)
  factor_X=paste('factor(position_',dir,'<position)',sep='')
  code_race_X=dirattr('code_race',dir)
  if (dir=='ahead') diff_X='diff' else diff_X='chasediff'

  if (dir=="ahead") drs=1 else drs=-1
  g=g+geom_hline(aes_string(yintercept=drs),linetype=5,col='grey')

  #Plot the offlap cars that aren't directly being raced
  g=g+geom_text(data=df[df[dirattr('code_',dir)]!=df[dirattr('code_race',dir)],],
                aes_string(x='lap',
                           y=car_X,
                           label=code_X,
                           col=factor_X),
                angle=45,size=2)
  #Plot the cars being raced directly
  g=g+geom_text(data=df,
                aes_string(x='lap',
                           y=diff_X,
                           label=code_race_X),
                angle=45,size=2)
  g=g+scale_color_discrete(labels=c("Behind","Ahead"))
  g+guides(col=guide_legend(title="Intervening car"))
}

battlemap_theme=function(g){
  g
}

battlemapFull_byDriver=function(lapTimes,code,title,ylim=c(-50,25)){
  #eg title="F1 Malaysia 2015 - Rosberg's Race"
  battle_code=battlemap_df_driverCode(lapTimes,code)
  battle_code[["code_raceahead"]][is.na(battle_code[["code_raceahead"]])] = code
  g=ggplot()
  #Need to fix this for car ahead of leader
  if (nrow(battle_code[battle_code['code_raceahead']!=code,]) > 0) {
    g=battlemap_core_chart(battle_code[battle_code['code_raceahead']!=code,],g,'ahead')
  }
  if (nrow(battle_code[battle_code['code_racebehind']!=code,] > 0)) {
    g=battlemap_core_chart(battle_code[battle_code['code_racebehind']!=code,],g,dir='behind')
  }
  #g=g+geom_text(data=lapTimes[lapTimes['code']==code,],aes(x=lap,y=0,label=position),size=2)
  g=g+ylim(ylim)+ylab("Gap (s)")+xlab("Lap")+xlim(1,max(lapTimes['lap']))
  g=g+theme_bw()+guides(colour=FALSE)#+ theme(legend.position="none")
  g=g+ggtitle(title)
  #battlemap_theme(g)
  g
}

quickbattlemapFull_byDriver=function(year,round,code,country){
  lapTimes=lapsData.df(year,round)
  lapTimes=battlemap_encoder(lapTimes)
  battlemapFull_byDriver(lapTimes,code,paste("F1",country,year,"-",code))
}


track_encoder=function(lapTimes){
  #Find the accumulated race time at the start of each leader's lap
  lapTimes = ddply(lapTimes, .(leadlap), transform, lstart = min(acctime))

  #Find the on-track gap to leader
  lapTimes['trackdiff'] = lapTimes['acctime'] - lapTimes['lstart']
  lapTimes
}

track_position_lead_trackdiff=function(lapTimes){
  #Construct a dataframe that contains the difference between the
  #leader accumulated laptime on current lap and next lap
  #i.e. how far behind current lap leader is next-lap leader?
  ll = data.frame(t = diff(lapTimes[lapTimes['position'] == 1, 'acctime']),
                  n= lapTimes[lapTimes['position'] == 1,][-1,'leadlap'])
  #Grab the code of the lap leader on the next lap
  ll['c'] = lapTimes[lapTimes['position'] == 1 &
                       lapTimes['lap'] > 1, 'code']
  ll
}

#for code pass in s/thing like: list(c("BOT","+"),c("RAI","*"))
track_position_chart = function(lapTimes,code=NA,xlimit=NA,pitloss=NA) {
  lapTimes = track_encoder(lapTimes)

  #Overplot unlaps
  lapTimes=ddply(lapTimes,.(leadlap,code),transform,unlap= seq_along(leadlap))

  #Find the offset for the race leader to backmarker
  ll=track_position_lead_trackdiff(lapTimes)
  #Find offsets for lapped cars
  ll=merge(ll,lapTimes[,c('leadlap','trackdiff','lapped','lap','code')],by.x='n',by.y='leadlap')
  ll['t3']=ll['t']+ll['trackdiff']

  #Plot the on-track gap to leader versus leader lap
  g = ggplot(lapTimes)
  g = g + geom_text(data = lapTimes[lapTimes['position'] == 1, ],
                    aes(x = -3, y = leadlap, label = code),
                    size = 2)
  #Plot cars
  g = g + geom_point(aes( x = trackdiff, y = leadlap,
                          col = (lap == leadlap),
                          pch= (unlap==1) ))+scale_shape_identity()

  if (!is.na(code)){
    for (t in code) {
      g = g + geom_point(data = lapTimes[lapTimes['code'] == t[1], ],
                         aes(x = trackdiff, y = leadlap),
                         pch = t[2])
    }
  }

  g = g + geom_text(data = lapTimes[lapTimes['lapsbehind'] > 0, ],
                    aes(x = trackdiff, y = leadlap, label = lapsbehind),
                    size = 3)

  #Lapped cars behind #,col=(n-lap)
  g = g + geom_point(data = ll[ll['lapped']>0,],
                     aes(x = t3, y = n-lapped, col = FALSE), pch = 1)

  #Chart lead driver behind
  g = g + geom_point(data = ll, aes(x = t, y = n), pch = 'x')
  g = g + geom_text(data = ll, aes(x = t + 5, y = n, label = c), size = 2)

  #Pit loss line
  if (!is.na(pitloss))
    g = g + geom_vline(aes(xintercept = pitloss), linetype = 3)

  if (is.na(xlimit)) { xlimit= max(c(max(ll['t']),max(lapTimes['trackdiff'])))+10}
  g + xlim(-4,xlimit) + guides(colour = FALSE)+theme_bw()+xlab('Interval to Leader (s)')+ylab('Lead Lap')
}


raceHistory_encoder=function(lapTimes){
  winner = with( lapTimes,
                 lapTimes[lap==max(lapTimes$lap) & position==1,'driverId'][[1]] )

  winnerMean = mean( lapTimes[ lapTimes$driverId==winner,'rawtime' ] )
  lapTimes$raceHistory=winnerMean*lapTimes$lap - lapTimes$acctime
  lapTimes
}


#track position chart rebased to driver
#rebase the track position relative to a specified driver on lead lap
#find trackdiff for specified driver
#subtract that trackdiff from trackdiff of every driver on that lead lap
track_position_relative=function(lapTimes,code){
  lz = track_encoder(lapTimes)

  rebase=lz[lz['code']==code,c('leadlap','trackdiff')]
  rebase=plyr::rename(rebase,c('trackdiff'='trackrebase'))
  lz=merge(lz,rebase,by='leadlap')
  lz['trackdiff2']=lz['trackdiff']-lz['trackrebase']
  lz=ddply(lz,.(leadlap,code),transform,unlap= seq_along(leadlap))

  #Find the offset for the race leader to backmarker
  ll=track_position_lead_trackdiff(lz)
  #Find offsets for lapped cars
  ll=merge(ll,rebase,by.x='n',by.y='leadlap')
  ll['t2']=ll['t']-ll['trackrebase']
  ll=merge(ll,lz[,c('leadlap','trackdiff','lapped','lap')],by.x='n',by.y='leadlap')
  ll['t3']=ll['t2']+ll['trackdiff']

  g = ggplot(lz)
  #https://github.com/tidyverse/ggplot2/issues/519 on pch
  g=g+geom_point(aes(x=trackdiff2, y=leadlap, col=(leadlap-lap),
                     pch= (unlap==1))) +scale_shape_identity()

  #Race leader behind
  g = g + geom_point(data = ll, aes(x = t2, y = n), pch = 'x')

  #Lapped cars behind
  g = g + geom_point(data = ll[ll['lapped']>0,],
                     aes(x = t3, y = n-lapped,col=(n-lap)), pch = 1)

  g=g+scale_colour_gradientn(colours=c('green','blue','skyblue1','orange','red'), limits=c(-2, 2))
  g=g+theme_bw()+ theme(legend.position='none')+ggtitle(paste('Track position rebase for ',code))
  g+xlab('Interval to Leader (s)')+ylab('Lead Lap')
}

#For this we need cars +/- time away on track
#for highlights pass in s/thing like: list(c("BOT","+"),c("RAI","*"))
#pitstops can be a list of pit lap numebr or the pitstops df, eg pitsData.df(2017,4)
track_position_concordance=function(lapTimes,code,limits=c(-20,20),highlights=NA,pitloss=NA,pitstops=NA){
  #take accumulated time and show people within that
  #plot on my lap
  #for each of my laps, find people within a range
  inscope=sqldf(paste0('SELECT l1.code as code,l1.acctime-l2.acctime as acctimedelta,
                       l2.lap-l1.lap as lapdelta, l2.lap as focuslap
                       FROM lapTimes as l1 join lapTimes as l2
                       WHERE l1.acctime < (l2.acctime + ', abs(limits[2]), ')
                       AND l1.acctime > (l2.acctime - ', abs(limits[1]),')
                       AND l2.code="',code,'";'))

  g = ggplot(inscope)

  pitish=3
  if (!is.na(pitloss)) {
    #g=g+geom_vline(xintercept=c(-pit,pit), linetype="dashed",colour='mediumorchid')
    g=g+ geom_rect(aes(xmin=-pitloss-pitish, xmax=-pitloss, ymin=0, ymax=Inf),fill='ivory2')
    g=g+ geom_rect(aes(xmin=pitloss, xmax=pitloss+pitish, ymin=0, ymax=Inf),fill='ivory2')
  }

  if (is.data.frame(pitstops)){
    pitstops['code']=apply(pitstops['driverId'],2,function(x) driverCodeMap(x))
    pitstops=pitstops[pitstops['code']==code,]$lap
  }

  if (!is.na(pitstops)){
    for (l in pitstops)
      g=g+geom_hline(yintercept=l, linetype="dashed",colour='lightpink')
  }

  g=g+geom_point(aes(x=acctimedelta,y=focuslap,col=(lapdelta)),pch=1)

  if (!is.na(highlights)){
    for (t in highlights) {
      g = g + geom_point(data = inscope[inscope['code'] == t[1], ],
                         aes(x = acctimedelta, y = focuslap),
                         pch = t[2])
    }
  }

  g=g+xlim(limits)
  g=g+scale_colour_gradientn(colours=c('green','blue','skyblue1','orange','red'),
                            limits=c(-2, 2))
  g=g+theme_bw()+ theme(legend.position='none')+ggtitle(paste('Track concordance for ',code))
  g+xlab('Gap (s)')+ylab('Driver Lap')
}


track_position_concordance_line=function(lapTimes,code,limits=c(-20,20),highlights=NA,pitloss=NA,pitstops=NA){
  #take accumulated time and show people within that
  #plot on my lap
  #for each of my laps, find people within a range
  inscope=sqldf(paste0('SELECT l1.code as code,l1.acctime-l2.acctime as acctimedelta,
                       l2.lap-l1.lap as lapdelta, l2.lap as focuslap
                       FROM lapTimes as l1 join lapTimes as l2
                       WHERE l1.acctime < (l2.acctime + ', abs(limits[2]), ')
                       AND l1.acctime > (l2.acctime - ', abs(limits[1]),')
                       AND l2.code="',code,'";'))
  #if consecutive rows for same driver have different sign in the acctinedelta, add NA between focuslaps
  #inscope=ddply(inscope,.(code),transform,g=cumsum(c(0,diff(sign(acctimedelta))!=0)))
  inscope=ddply(inscope,.(code),transform,g=cumsum(c(0,diff(focuslap)>1)))
  g = ggplot(inscope[inscope['code']!=code,])

  g=g+geom_line(aes(y=acctimedelta,x=focuslap, col=code,group=interaction(code, g)))+ coord_flip()

  g=g+ylim(limits)
  g=g+theme_bw()+ theme(legend.position='none')+ggtitle(paste('Track concordance for ',code))
  g+ylab('Gap (s)')+xlab('Driver Lap')
}

raceHistory_chart=function(lapTimes){

  lapTimes=raceHistory_encoder(lapTimes)

  g=ggplot(lapTimes)
  g=g+geom_line(aes(x=lap,y=raceHistory, group=driverId, colour=driverId))
  g=g+labs(title='Race history chart',x='Lap number', y='Race history time (s)')
  g
}
