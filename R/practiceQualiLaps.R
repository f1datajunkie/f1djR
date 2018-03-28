#library(plyr)
#library(ggplot2)
#df <- read.csv("~/Dropbox/wranglingf1datawithr/src/df.csv")
#df <- read.csv("~/Dropbox/wranglingf1datawithr/src/quali.csv")

#https://stackoverflow.com/questions/25995257/r-shift-values-in-single-column-of-dataframe-up#comment40708706_25995330
shift <- function(x, n){
  c(tail(x, -n), rep(NA, n))
}

#A function to augment raw laptime from practice, race and qualifying sessions
## with derived data columns
#TO DO - add an optional colour to show laps slower than previous lap as red
#TO DO - prev lap should be prev flying lap (not inlap or outlap or 1st lap)
rawLap_augment_laptimes = function(df){
  #This is a fudge for TH scraped data legacy
  if(!"code" %in% colnames(df)) {
    if ("name" %in% colnames(df))
      df['code']=apply(df['name'],2,function(x) driverCode(x))
    else if ("driverId" %in% colnames(df))
      df['code']=apply(df['driverId'],2,function(x) driverCodeErgast(x))
  }
  df=plyr::ddply(df,.(code),transform,cuml=cumsum(rawtime))
  df$pit= df$pit %in% c(TRUE,'True')
  df=arrange(df,code, -lap)
  df=plyr::ddply(df,.(code),transform,stint=1+sum(pit)-cumsum(pit))
  df=arrange(df,code, lap)
  df=plyr::ddply(df,.(code,stint),transform,lapInStint=1:length(stint))

  #Difference to car ahead by lap
  df=arrange(df,lap,cuml)
  df=plyr::ddply(df,.(lap),transform,difftoprev=c(0,diff(cuml)))
  #Then cumulative sum on those to give time to leader
  df=plyr::ddply(df,.(lap),transform,difftolead=cumsum(difftoprev))
  #Diff to car behind is a shift
  df$difftocarposbehind=shift(df$difftoprev, 1)

  df=arrange(df,code, lap)
  df=plyr::ddply(df,.(code),transform,driverbest=cummin(c(9999,rawtime[2:length(rawtime)])))
  #Need a patch in case there is only an entry time.. ie rawtime length==1
  #TO DO - another correction to make a singleton time a pit lap
  df=df[!(is.na(df$driverbest)), ]
  df=arrange(df,cuml)
  df['purple']=sapply(df['driverbest'],cummin)
  df['colourx']=ifelse(df['rawtime']==df['purple'],
                       'purple',
                       ifelse(df['rawtime']==df['driverbest'],
                              'green',
                              'black'))
  df=arrange(df,code, lap)
  df= plyr::ddply(df,.(code),transform,outlap=c(FALSE, diff(pit)==-1))
  df['outlap']= df['outlap'] | df['lapInStint']==1 |  (df['rawtime'] > 2.0 * min(df['purple']) & (!df['pit']) )
  df=plyr::ddply(df,
           .(code),
           transform,
           stint=cumsum(outlap),
           lapInStint=1:length(stint))
  df=plyr::ddply(df,
           .(code, stint),
           transform,
           lapInStint=1:length(stint))
  df
}

plot_session_utilisation_chart = function (df,size=2,session=''){
  g = ggplot(df)
  #Layer showing in-laps (laps on which a driver pitted) and out-laps
  g = g + geom_point(data=df[df['outlap'] | df['pit'],],
                     aes(x=cuml, y=code#, color=factor(colourx)
                         ), pch=1)
  #Further annotation to explicitly identify pit laps (in-laps)
  g = g + geom_point(data=df[df['pit']==TRUE,],
                     aes(x=cuml, y=code), pch='.')
  #Layer showing full laps with rounded laptimes and green/purple lap highlights
  g = g + geom_text(data=df[!df['outlap'] & !df['pit'],],
                    aes(x=cuml, y=code,
                        label=floor(rawtime*10)/10, color=factor(colourx)
                        ),
                    size=size, angle=45)
  g = g + scale_colour_manual(values=c('darkgrey','darkgreen','purple'))

  g = g + xlab(NULL) + ylab(NULL)
  g + guides(colour=FALSE) + theme_bw()
}

#This chart shows times within each stint after outlap and excluding inlap
#The second lap in the stint (first flying lap) shows laptime
#Further laps are show as colour coded lap delta from previous lap
#Example usage - show evolution of purple across whole session
##qlaps= read.csv(paste0("~/Dropbox/various/",stub,"17_qualilaptimes.csv"))
##qlapsb=rawLap_augment_laptimes(qlaps)
##plot_session_utilisation_chart_toggle_gap(qlapsb,2)
plot_session_utilisation_chart_toggle_gap=function(df,size=2){
   df=plyr::ddply(df,.(code,stint),transform,diff=c(0,diff(rawtime)))
   df['coloury']=ifelse(df$colourx=='black',
                                ifelse(df$diff>=0.0,'red','yellow'),
                                df$colourx)
   g = ggplot(df)
   #Layer showing in-laps (laps on which a driver pitted) and out-laps
   g = g + geom_point(data=df[df['outlap'] | df['pit'],],
                      aes(x=cuml, y=code, color=factor(colourx)), pch=1)
   #Further annotation to explicitly identify pit laps (in-laps)
   g = g + geom_point(data=df[df['pit']==TRUE,],
                      aes(x=cuml, y=code),pch='.')
   #Layer showing start of stint laptimes and green/purple lap highlights
   g = g + geom_text(data=df[df['lapInStint']==2 & !df['pit'],],
                     aes(x=cuml, y=code,
                         label=rawtime,#floor(rawtime*10)/10,
                         color=factor(colourx)),
                     size=size, angle=45)
   #Layer showing stint laptime deltas and green/purple lap highlights
   g = g + geom_text(data=df[df['lapInStint']>2 & !df['pit'],],
                     aes(x=cuml, y=code,
                         label=round(diff,2),
                         color=factor(coloury)),
                     size=size, angle=45)
   g = g + scale_colour_manual(values=c('darkgrey','darkgreen','purple','red','blue'))

   g + xlab(NULL) + ylab(NULL) + guides(colour=FALSE) + theme_bw()
 }

#This chart shows times within each stint after outlap and excluding inlap
#The second lap in the stint (first flying lap) shows laptime
#Further laps are show as colour coded lap delta from second lap (first flying lap) in stint
#TO DO? It might be interesting to also have a differ that shows the difference to the min purple to date?
plot_session_utilisation_chart_stint_diff=function(df,size=2){
  df=plyr::ddply(df,.(code,stint),mutate,sf=rawtime[2],sfd=sf-rawtime)

  #sfd is junk in rows 1 and 2 of a stint
  df['colourz']=ifelse(df$colourx=='black',
                               ifelse(df$sfd>=0.0,'red','yellow'),
                               df$colourx)
  g = ggplot(df)
  #Layer showing in-laps (laps on which a driver pitted) and out-laps
  g = g + geom_point(data=df[df['outlap'] | df['pit'],],
                     aes(x=cuml, y=code), pch=1) # aes: color=factor(colourx),
  #Further annotation to explicitly identify pit laps (in-laps)
  g = g + geom_point(data=df[df['pit']==TRUE,],
                     aes(x=cuml, y=code),pch='.')
  #Layer showing start of stint laptimes and green/purple lap highlights
  g = g + geom_text(data=df[df['lapInStint']==2 & !df['pit'],],
                    aes(x=cuml, y=code,
                        label=rawtime,#floor(rawtime*10)/10,
                        color=factor(colourx)),
                    size=size, angle=45)
  #Layer showing stint laptime deltas and green/purple lap highlights
  g = g + geom_text(data=df[df['lapInStint']>2 & !df['pit'],],
                    aes(x=cuml, y=code,
                        label=-round(sfd,2),
                        color=factor(colourz)),
                    size=size, angle=45, fontface="italic")
  g = g + scale_colour_manual(values=c('darkgrey','darkgreen','purple','blue','red'))

  g + xlab(NULL) + ylab(NULL) + guides(colour=FALSE) + theme_bw()
}

augmented_session_utilisation_chart=function(df,size=2,lapcount=TRUE,
                                             gap=TRUE,besttime=TRUE,
                                             session='', neworder=NA,
                                             ordertype=''){
  df=arrange(df,code,lap)
  spurple=min(df['purple'])

  dfClass=plyr::ddply(df[df['driverbest']<9999,],
                      .(code),
                      here(summarise),
                      driverbest=min(driverbest),
                      gap=min(driverbest)-spurple)

  #Cope with 0 time
  dfClass$driverbest[is.na(dfClass$driverbest)] = 9999
  dfClass=arrange(dfClass,driverbest)
  dfClass['diff']=c(0,diff(dfClass$gap))
  dfClass$pos=1:nrow(dfClass)
  #Cope with 0 time
  dfClass$driverbest[dfClass$driverbest == 9999] = 0


  #dfClass
  g=plot_session_utilisation_chart(df,size)
  #g=g+geom_text(data=dfClass,aes(x=-800,y=code,label=pos),size=size,fontface='bold')

  if (gap) {
    g=g+geom_text(data=dfClass,
                  aes(x=-400,y=code,label=paste(round(gap,3)," (",round(diff,3),")",sep='')),
                  size=size)
  }

  sess_util_formatter = function(x) {
    lab=ifelse(x<0, '', x)
  }
  txt="Session Utilisation Chart"
  if (session !='') txt=paste0(txt,' (',session,')')
  g=g+scale_x_continuous(label=sess_util_formatter)+ggtitle(txt)

  dfn=count(df,"code")
  dfClass = merge(dfClass, dfn, by = "code", all = TRUE)
  if (lapcount) {
    g=g+geom_text(data=dfClass,aes(x=-900,y=code,label=paste('(',freq,')',sep='')),
                  size=size,fontface='bold')
  }

  if (besttime){
    g=g+geom_text(data=dfClass,aes(x=-1250,y=code,label=driverbest),size=size,fontface='bold')
  }

  if (is.na(neworder)) {if (ordertype=='') {

    #Order the chart by driver session fastlap position
    #If there are extra levels in the codes, clear them out
    #df$code=droplevels(df$code)
    #dfClass$code=droplevels(dfClass$code)
    #levels(df$code) = factor(dfClass$code, levels = levels(dfClass$code[order(dfClass$pos)]))

   # neworder = rev(levels(df$code))
    neworder = dfClass$code[order(dfClass$pos )]
  }

  if (ordertype=='lapcount'){
    neworder = dfClass$code[order(dfClass$freq,-dfClass$driverbest )]
  }

  if (ordertype=='besttime'){

    #Cope with 0 time
    dfClass$driverbest[is.na(dfClass$driverbest)] = 9999
    neworder = dfClass$code[order(-dfClass$driverbest,dfClass$freq  )]
    #Cope with 0 time
    dfClass$driverbest[dfClass$driverbest == 9999] = NA
  }

  if (ordertype=='ontrack'){
    dfOnTrack = plyr::ddply(df, .(code), summarise, driverontrack=min(cuml))

    neworder = dfOnTrack$code[order(-dfOnTrack$driverontrack)]
  }}

  g = g+scale_y_discrete(limits=neworder) +xlab('Accumulated session time (s)') +ylab(NULL)

  g
}

#esp_p2a=rawLap_augment_laptimes(esp_p2)
#augmented_session_utilisation_chart(esp_p2a,3)

#source('streakiness.R')
stintFinder=function(df){
  stints=data.frame()
  for (code in levels(df$code)){
    dft=df[df$code==code,]
    dft=streaks(dft$stint)
    dft['code']=code
    dft=dft[c('code','start','end','l')]
    stints=rbind(stints,dft)
  }

  stints['code']=factor(stints$code)
  plyr::ddply(stints,.(code),transform,stintNumber=1:length(l))
}

longrunFinder=function(stints,df,stintlen=8){
  longruns=merge(stints[abs(stints['l'])>=stintlen,],
                 df,by.x=c('code','stintNumber'),
                 by.y=c('code','stint'))
  arrange(longruns,code,lap)
}

longrunsplot_min=function(longruns){
  g= ggplot(longruns[!longruns['outlap'] & !longruns['pit'],])
  g=g+geom_line(aes(x=lapInStint, y=rawtime, group=stintNumber,
                    colour=factor(stintNumber)))
  g+facet_wrap(~code)
}

longrunsplot_model=function(longruns,
                            m='loess',
                            cutoffpc=1.07,
                            drivers=c('L. HAMILTON', 'K. RAIKKONEN','S. VETTEL' )){
  lr=longruns[!longruns['outlap'] & !longruns['pit'] & longruns$code %in% drivers
              & longruns['rawtime']<cutoffpc*min(longruns['purple']),]
  g= ggplot(lr,
            aes(x=lapInStint, y=rawtime,colour=interaction(stintNumber,code)))
  g+geom_smooth(method = m,
                aes( group=interaction(stintNumber,code))) + geom_point(aes(shape=interaction(stintNumber,code)))+ scale_colour_brewer(palette="Set1")
}
#st=stintFinder(esp_p2a)
#longruns=longrunFinder(st,esp_p2a)
#longrunsplot_min(longruns)
#longrunsplot_model(longruns,'lm')
#longrunsplot_model(longruns[longruns['rawtime']<96,],'lm')


#Try to identify gaps between qualifying sessions

rawLap_augment_quali=function(df){
  df=rawLap_augment_laptimes(df)
  df=arrange(df,cuml)
  df['gap']=c(0,diff(df[,'cuml']))
  df['gapflag']= (df['gap']>=300)
  df['qsession']=1+cumsum(df[,'gapflag'])
  df
}

qsessionOverride=function(df,t1start,t2start,t3start){
  df[(df['cuml']>t3start),]['qsession']=3
  df[(df['cuml']>t2start) & (df['cuml']<t3start),]['qsession']=2
  df[(df['cuml']>t1start) & (df['cuml']<t2start),]['qsession']=1
  df
}

#Colour laptimes according to purple/green within separate quali sessions
quali_purplePatch=function(df){
  df=arrange(df,code, lap)
  df=plyr::ddply(df,.(qsession,code),transform,driverqbest=if (length(rawtime)==1) 9999 else cummin(c(9999,rawtime[2:length(rawtime)])))
  df=arrange(df,cuml)
  df=plyr::ddply(df,.(qsession),transform,qpurple=cummin(driverqbest))
  df['colourx']=ifelse(df['rawtime']==df['qpurple'],
                       'purple',
                       ifelse(df['rawtime']==df['driverqbest'] & !df['pit'] & !df['outlap'],
                             'green',
                              'black'))
  df=arrange(df,code, lap)
  df
}



