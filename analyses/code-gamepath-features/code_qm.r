###############################################################################
# Analysis of the wiggle data set
# Created by Jana Jarecki jj@janajarecki.com
###############################################################################

# Libraries and directories
library(data.table)
library(ggplot2)
source("../../../96-Resources/R/theme_jj.r")    #user-defined theme, comment this out, load scales package
source("../../../96-Resources/R/graphics_jj.r") #user-defined source, comment this out
theme_set(theme_jj())                           #replace this with theme_bw()  
# library(scales)
# library(adehabitatLT)   #spatiotemporal analysis, http://cran.r-project.org/web/packages/adehabitatLT/adehabitatLT.pdf
ddir = "../4-Data"
odir = "../6-Output"
idir = "../8-Img"
collev = c(lev1="plum",lev2="steelblue",lev3="goldenrod3",lev4="darkslategrey") #colors for the levels
colsuc = c(failed="darkslategrey", succeeded="mediumspringgreen")


###################################################################################################################
# Load data from all 4 levels (lev1 - lev4)
d = rbind(  fread(file.path(ddir,"data_qm_lev1.csv"), sep=";"), #16459 rows (23. March 18:59)
            fread(file.path(ddir,"data_qm_lev2.csv"), sep=";"), #4781 rows
            fread(file.path(ddir,"data_qm_lev3.csv"), sep=";"), #25164 rows
            fread(file.path(ddir,"data_qm_lev4.csv"), colClasses=list(character=15:19), sep=";")) #7094 rows
d = d[!duplicated(iHit)]
# Exclude the team users:
d = d[!nam %in% c("jarecki.jana@gmail.com",         # Jana B. Jarecki
                  "8x59dGBTJwM73cVeadUEZ8Ga2",      # Lars K. Kroll
                  "blaaman@gmail.com")              # Kristian Bak - he also has a fb account w his name!
                    ]     

# Substitute guest users (i equals NA) with the device id
d[is.na(i), "i" := iDev]
# 24164 rows

# Convert time, stored with 3 digit ms into 2 digit ms
options(digits=3)
d[, tHit := as.POSIXct(sub("Z","",sub("T"," ",tHit)))]
#d[, cre := as.POSIXct(sub("Z","",sub("T"," ",cre)))]
#d[, tSes := as.POSIXct(sub("Z","",sub("T"," ",tSes)))]
#d[, updSes := as.POSIXct(sub("Z","",sub("T"," ",updSes)))]  

# Generate new variables
setkey(d,tHit)
d[, hitNum := 1:.N, by=c("i","iLev")]       #hit count per user and level
d[, lasLev := length(unique(iLev)), by=i]    #level after which user dropped out


  

#######################################################################################################################
du = d[!duplicated(paste(i,iLev))] #Deduplicated user dataset with levels

# The game was started by __ people
du[!duplicated(i), .N] # [1] 815

# But only __ made it through level 2, 3, 4
du[!duplicated(i), .N, by="iLev"] # 314, 245, 227, 9
# ggplot(du[, lasLev := factor(length(unique(iLev)), levels=as.character(4:1), ordered=T), by=i][, .N, by="iLev,lasLev"], aes(x=iLev,y=N)) +geom_bar(aes(alpha=lasLev,order=lasLev), stat="identity", fill="darkslategray") +geom_bar(aes(alpha=lasLev,order=lasLev), stat="identity", color="white", fill=NA, show_guide=FALSE) +scale_alpha_discrete("Played until", range=c(.35,1), breaks=1:4) +theme(legend.position=c(.72,.95), legend.direction="horizontal") +xlab("Levels") +ylab("Player count") +geom_text(data=du[, .N, by=iLev], aes(label=N,y=N+20))
# ggsavejj("playersMadeLevels", p)

# Out of the __ registered users, there were __ ( __ %) woman
du[!duplicated(i), .N, by=gen][!is.na(gen), list(registered=sum(N), women=N[gen=="female"], percent=round(N[gen=="female"]/sum(N),2)*100)]
#    registered women percent
# 1:        313    48      15
cols=c(male="slategrey",female="violet")
pgender <- ggplot(du[!duplicated(i), .N, by=gen]) +geom_bar(aes(x=gen,y=N,fill=gen), binwidth=.2, position="dodge", alpha=1/2, stat="identity", width=.5) +geom_bar(aes(x=gen,y=N,fill=gen), binwidth=.2, position="dodge", alpha=1/2, color="gray66", stat="identity", width=.5) +scale_fill_manual(values=cols) +scale_color_manual(values=cols) +xlab("Gender") +theme(legend.position="none") +ggtitle("Gender")

# The players, which reported their age, were between __ and __ years
fbage <- c("0-17","18-20","21+") #vector of ages reported by facebook
d[!duplicated(i) & !age %in% fbage, summary(as.numeric(age))]
d[!duplicated(i) & age %in% fbage, summary(age)]
  #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 13.00   22.00   28.00   31.72   38.00  123.00     403
set.seed(121)
page <- 
ggplot(du[!duplicated(i) & !age %in% fbage & !is.na(gen)], aes(x=gen,y=as.numeric(age),color=gen,drop=T)) +geom_point(aes(fill=gen), size=2, alpha=.7, position=position_jitterdodge(jitter.width=1.3,jitter.height=0)) +geom_violin(alpha=.3,color="gray66") +scale_color_manual(values=cols) +geom_boxplot(width=.15) +theme(legend.position="none") +ggtitle("Age") +ylab("Years") +xlab("Gender")
library(gridExtra)

# Players came from a diverse set of countries
library(png)
library(grid)
img <- readPNG(file.path(idir,"playercountries.png"))
g <- rasterGrob(img, interpolate=TRUE, name="A") 
pcountries <- qplot(1:10, 1:10, geom="blank") +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  ggtitle("Countries") +theme(axis.ticks=element_line(color=NA), axis.text=element_text(color=NA), axis.title=element_text(color=NA), panel.grid=element_blank())

# Players registered on the site
dr <- fread(file.path(ddir,"analytics.csv"))
setnames(dr,1:2,c("x","y"))
pusers <- ggplot(dr, aes(x,y)) +geom_line(color="darkslategrey") +xlab("Time") +ylab("N") +ggtitle("Users") +theme(axis.text.x=element_text(color=NA))
        
p <- arrangeGrob(pusers,pcountries,pgender,page,ncol=2)
ggsavejj("demographics", p)


# The players generated __ hits or plays
d[, .N] # [1] 24164
pplay <- qplot(2, d[, .N], geom="blank") +geom_bar(stat="identity") +theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +scale_x_continuous("Play count",limits=c(1,3)) +scale_y_continuous("N", limits=c(0,d[, .N])) +ggtitle("Play count")
    
# Across levels they generated __ hits or plays
d[, .N, by=iLev]
# 1: lev1  7655
# 2: lev2  3826
# 3: lev3 12433
# 4: lev4   250
pplaylev <- ggplot(d[, .N, by=iLev], aes(x=iLev,y=N,fill=iLev)) +geom_bar(stat="identity") +theme(legend.position="none") +ggtitle("Play count/levels") +ylab("N") +xlab("Play count") +scale_fill_manual(values=collev)

# The average player generated __ hits or plays
d[, .N, by=i][, list(from=min(N), to=max(N), mean=round(mean(N)))]
#    from  to mean
# 1:    1 620   77
pplayuser <- ggplot(d[, .N, by=i], aes(2,N)) +geom_point(size=2, alpha=.3, position=position_jitter(w=.5)) +geom_violin(alpha=.3,color="gray66",scale="width") +scale_color_manual(values=collev) +geom_boxplot(width=.3) +theme(legend.position="none") +ggtitle("Plays") +ylab("Levels") +ylim(0,180) +xlab("N") +ggtitle("Play count/users") +theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +scale_x_continuous("Play count",limits=c(1,3))

# Within levels, the average player generated __ hits or plays
d[, .N, by="i,iLev"][, list(from=min(N), to=max(N), mean=round(mean(N))), by=iLev]
#    iLev from  to mean
# 1: lev1    1 543   24
# 2: lev2    1 168   16
# 3: lev3    1 456   55
# 4: lev4    1  89   28
pplayuserlev <- ggplot(d[, .N, by="i,iLev"], aes(x=iLev,y=N,color=iLev)) +geom_point(aes(fill=iLev), size=2, alpha=.2, position=position_jitterdodge(jitter.width=1.8,jitter.height=0)) +geom_violin(alpha=.3,color="gray66", scale="width") +scale_color_manual(values=collev) +geom_boxplot(width=.4,) +theme(legend.position="none") +ggtitle("Play count/user/level") +ylab("N") +xlab("Levels") +ylim(0,175)
p <- arrangeGrob(pplay,pplaylev,pplayuser,pplayuserlev,ncol=2)
ggsavejj("playCounts", p)


# The average player reached a consecutive-play, score of __ in the levels
d[!is.na(scoRun), list(N=max(scoRun)), by="iLev,i"][, list(from=min(N), to=max(N), mean=round(mean(N))), by=iLev]
#    iLev from to mean
# 1: lev1    0 19    3
# 2: lev2    1 22    8
# 3: lev3    0 13    2
# 4: lev4    0  8    3
ggplot(d[!is.na(scoRun), list(N=max(scoRun)), by="iLev,i"], aes(x=iLev,y=N,color=iLev)) +geom_point(aes(fill=iLev), size=2, alpha=.2, position=position_jitterdodge(jitter.width=2,jitter.height=.4)) +geom_violin(alpha=.3,color="gray66", scale="width") +scale_color_manual(values=collev) +geom_boxplot(width=.2,) +theme(legend.position="none") +ggtitle("Best score/user/level") +scale_y_continuous("Best Score", breaks=d[!is.na(scoRun), max(scoRun), by="iLev,i"][, c(range(V1), median(V1)), by=iLev]$V1) +xlab("Levels")
ggsavejj("scoresBoxplot")

# __ players succeeded in level 4
d[iLev=="lev4"][!is.na(scoRun), list(sco=max(scoRun)), by="i"][, sum(sco>=3)]
# [1] 44
psuc <- ggplot(d[iLev=="lev4"][!is.na(scoRun), list(sco=max(scoRun),suc=ifelse(max(scoRun)>=3,"succeeded","failed")), by="i"], aes(x=suc,fill=suc)) +geom_bar(width=.33) +scale_fill_manual("Three consecutive times",values=colsuc) +theme(legend.position="top", legend.direction="horizontal") +ylab("N") +ggtitle("Level 4 performance") +xlab("")

# The players who made it in level 4 reached a score of __ plays
psco <- ggplot(d[iLev=="lev4"][!is.na(scoRun), list(sco=max(scoRun),suc=ifelse(max(scoRun)>=3,"succeeded","failed")), by="i"], aes(x=suc,y=sco,color=suc)) +geom_point(aes(color=suc,fill=suc), size=3, alpha=.4, position=position_jitterdodge(jitter.width=.33,jitter.height=.05)) +geom_violin(alpha=.3, color="gray66", fill="white") +scale_color_manual("Three consecutive times", values=colsuc) +geom_boxplot(width=.07,) +scale_fill_manual("Three consecutive times",values=colsuc) +guides(fill="none") +theme(legend.position="top", legend.direction="horizontal") +ggtitle("Level 4 scores") +xlab("") +scale_y_continuous("Score", breaks=0:12)
p <- arrangeGrob(psuc,psco,nrow=1)
ggsavejj("level4performance", p, w=12)




# The players succeeded in the levels after __ hits or plays
d[, list(N=max(hitNum)), by="i,lasLev,iLev"][, list(from=min(N), to=max(N), mean=round(mean(N))), keyby="lasLev,iLev"]
#     lasLev iLev from  to mean
#  1:      1 lev1    1 543   34
#  2:      2 lev1    3  46   22
#  3:      2 lev2    1  98   23
#  4:      3 lev1    1 136   20
#  5:      3 lev2    1 168   15
#  6:      3 lev3    1 456   56
#  7:      4 lev1    4 270   52
#  8:      4 lev2    3  82   24
#  9:      4 lev3    5  91   30
# 10:      4 lev4    1  89   28
ggplot() +
    geom_step(
        data=d[lasLev>1, list(N=max(hitNum)), keyby="i,lasLev,iLev"][, list(Level=c(1,1:lasLev),N=c(1,cumsum(N))), by="i,lasLev"],
        aes(x=N,y=Level,group=i,color=factor(lasLev)), alpha=.2, dir="hv", size=.7)+
    geom_step(
        data=d[lasLev>1, list(N=max(hitNum)), keyby="i,lasLev,iLev"][, list(Level=c(1,1:lasLev),N=c(1,cumsum(N))), by="i,lasLev"][, list(N=median(N)), by="lasLev,Level"],
        aes(x=N,y=Level,group=lasLev,color=factor(lasLev)), dir="hv", size=2) +
    scale_color_manual("Last level", values=collev) +
    scale_x_continuous("Play count", limits=c(1,100)) +
    facet_wrap(~lasLev, 3, scales="free_x")
ggsavejj("playCountsAcrossLevels")

dn = d[lasLev>1, list(N=max(hitNum)), keyby="i,lasLev,iLev"][, list(Level=1:lasLev,N=cumsum(N)), by="i,lasLev"]
setkey(dn,Level)
dn[, c("N1","N2","N3") := list(N[Level==1],N[Level==2],N[Level==3]), by="i"]
library(reshape2)
dn2 = dcast(dn, i + lasLev + N1 + N2 +N3 ~ lasLev)
setnames(dn2, 6:8, c("lev2","lev3","lev4"))
dn2 = as.data.table(dn2)
dn2[, lev2 := ifelse(lev2!=0,1,0)]
dn2[, lev3 := ifelse(lev3!=0,1,0)]
dn2[, lev4 := ifelse(lev4!=0,1,0)]


# The pure experience in terms of play counts in level j is independent of success in terms of reaching level x+1
linMod = lm(lev2 ~ N1, family="binomial", data=dn2)
summary(linMod)
# p <- ggplot() +xlab("Play counts in previous level") +theme(plot.margin=unit(rep(0,4),"lines")) +scale_y_continuous("", limits=c(0,1), breaks=c(0,.5,1), labels=percent) + scale_color_manual("", values=c(lev2="steelblue",lev3="goldenrod3",lev4="darkslategrey"))
# p1 <- p + stat_smooth(data=dn2, aes(x=N1,y=lev2,color="lev2"), method="glm", formula = y ~ x, family="binomial", add=T)+ theme(axis.title.x=element_blank(), plot.margin=unit(c(2,-.1,-.1,-.1), "lines"))
# p2 <- p +stat_smooth(data=dn2, aes(x=N2,y=lev3,color="lev3"), method="glm", formula = y ~ x, family="binomial", add=T) +theme(axis.title.x=element_blank()) +scale_y_continuous("P(level | play count)", limits=c(0,1), breaks=c(0,.5,1), labels=percent)
# p3 <- p + stat_smooth(data=dn2, aes(x=N3,y=lev4,color="lev4"), method="glm", formula = y ~ x, family="binomial", add=T)
# library(gridExtra)
# p4 <- arrangeGrob(p1,p2,p3,
#         main = textGrob("Play count independent of maximum level", gp=gpar(fontfamily="fontjj", fontsize=14, fontface="bold")))
# ggsavejj("playCountMaxLevel", p4)
max(hitNum)

# Players individual learning curves showed variation
d4 = d[iLev=="lev4"]
d4[, "suc" := as.integer(max(scoRun, na.rm=TRUE)), by=i][, suc := ifelse(suc>=3,"succeeded","failed")]
ggplot(d4, aes(x=hitNum, y=fidHit, color=suc, fill=suc)) +geom_point(size=1) +geom_line(aes(group=i)) +facet_wrap(~i, scales="free_x") +theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), panel.margin.x=unit(.1,"lines"), panel.margin.y=unit(-.7,"lines"), strip.text=element_blank(), panel.grid=element_blank(), aspect.ratio=1) +scale_color_manual("Three\nconsecutive\ntimes", values=colsuc) +scale_fill_manual("Three\nconsecutive\ntimes", values=colsuc) +ylab("Fidelity") +ggtitle("Level 4 learning curves")
ggsavejj("level4learningCurves", w=12)



# Is the probability of success a function of time playing?
ggplot(d4[, list(N=max(hitNum),suc=as.numeric(ifelse(suc=="succeeded",1,0))), by=i], aes(x=N,y=suc)) + stat_smooth(, method="glm", formula = y ~ x, family="binomial", add=T)+ theme(axis.title.x=element_blank(), legend.position="left") +geom_point(aes(color=suc)) +ylab("P(succeed)") +scale_color_continuous("Three\nconsecutive\ntimes",low=colsuc[1],high=colsuc[2])
ggsavejj("level4RegresNHit-Success")
mod <- glm(family="binomial", suc ~ N, data=d4[, list(N=max(hitNum),suc=as.numeric(ifelse(suc=="succeeded",1,0))), by=i])
# If we play one more, the likelihood to suceed increases by __ percentage points, for a randomly picked N
library(mfx)
mfx <- logitmfx(suc ~ N, robust=T, data=d4[, list(N=max(hitNum),suc=as.numeric(ifelse(suc=="succeeded",1,0))), by=i])
round(mfx$mfxest,6)*100
# [1] 0.0755

# For the mean N, if we play one more, the likelihood to suceed increases by __ percentage points
mfx <- logitmfx(suc ~ N, robust=T, atmean=T, data=d4[, list(N=max(hitNum),suc=as.numeric(ifelse(suc=="succeeded",1,0))), by=i])
round(mfx$mfxest,6)*100  
# [1] 0.0755










#######################################################################################################################
# Scores by gender
ggplot(d, aes(x=gen, y=fidHit, color=gen)) + geom_boxplot() +facet_wrap(~iLev, nrow=1)


ggplot(d[!duplicated(i)], aes(x=strftime(cre,"%d%b"))) +geom_bar(width=.7) +ggtitle("New registered users") +xlab("Day") +theme(legend.position="none")



d[, dHit := as.numeric(tHit-tHit[1]), by="i,iLev"]

setkey(d,i,iLev,tHit)
ggplot(d[i %in% d[, i[iLev=="lev4"]]], aes(x=hitNum,y=fidHit,group=i,color=i)) +geom_point() +geom_path() +facet_wrap(~iLev, scales="free_x")

d["5rMZ1XFQ8S"]

d[i %in% 
d[, i[iLev=="lev4"]]
        

d[, by="i,tHit"]
ggplot(d[ilev=="lev4"], aes(x=fidHit)) 



setkey(d,i,iLev)
ggplot(d[, list(max=max(scoRun), col=max(scoRun)>=3), by="i,iLev"], aes(x=max,fill=col)) + geom_histogram() + facet_wrap(~iLev, scales="free_y") +ggtitle("Scores within one Run")


setkey(d,iLev,i,gen)   
ph = 
ggplot(d[!is.na(gen), list(maxFid = max(fidHit)), by="iLev,i,gen"], aes(x=maxFid, fill=gen)) + facet_wrap(~iLev, scales="free_y", nrow=1) +geom_bar(aes(y=..count..), binwidth=.2, position="dodge", alpha=1/3, color="gray") +scale_fill_manual(values=cols) +scale_color_manual(values=cols) +scale_x_continuous("Highest fidelity / level / player", limits=c(0,1))

pd = ggplot(d[!is.na(gen), list(maxFid = max(fidHit)), by="iLev,i,gen"], aes(x=maxFid, fill=gen)) + facet_wrap(~iLev, scales="free_y", nrow=1) +geom_density(aes(y=..scaled.., color=gen), alpha=1/4, size=.5) +xlim(0,1) +scale_fill_manual(values=cols) +scale_color_manual(values=cols) +scale_x_continuous("Highest fidelity / level / player", limits=c(0,1)))
library(gridExtra)
p = arrangeGrob(ph,pd,ncol=1)
idir = "../8-Img"
ggsavejj("gender", p)

ggplot(d[iLev=="lev2"][, max(maxFid), by=c("i","gen")], aes(x=gen,y=V1)) + geom_boxplot()





# Variables in data-set:
# hit=hit-id, i=player-id, hitNum=nth-game-ever-played, t=time, x-value, a-value, score
# Note: The first column (hit-id) is a unique-path-identifier for plotting with matlab!

## GENERATE ADDITIONAL VARIABLES
setkey(d,i,hitNum)
d[t==0.002, hitNum :=1:.N, by=i][, hitNum := hitNum[1], by="i,hit"] #temporal order of hits
d[, relHitNum := hitNum/max(hitNum), by=i]                          #pp's % of all hits     
d[, totHitNum := max(hitNum), by=i]                                 #pp's total number of Hits




###########################################################################################################
# EXPLORE WHICH PEOPLE HAVE A POSITIVE LEARNING CURVE
# New, aggregated data-set with score
setkey(d,i,hitNum,t)
dh = d[t==0.002, .SD, by="i,hitNum"]
ids = dh[, unique(i[sco>=100])]
length(unique(d$i)) - 148
d[, "iniHit" := i %in% i[sco > 100 & hitNum==1]]

# All PP's learning curves given PP played > 1 time
ggplot(d[totHitNum!=1], aes(x=hitNum,y=sco,group=i,color=iniHit)) +geom_path(size=.6) +geom_point() +facet_wrap(~i, scales="free_x") +scale_color_manual(values=c("black","cyan3")) +scale_x_continuous(breaks = seq(0, 150, 1), expand = c(0, 1)) +theme(strip.text = element_blank())
ggsavejj("fig1_allIndLearning")

# decision_node_1: What range of total plays to include?
dn1 = c(5,20) 
d = d[totHitNum>=dn1[1] & totHitNum<=dn1[2] & iniHit!=T]

# All PP's learning curves given PP didn't succed on inital (1.) play & 5 >= play >= 15 
ggplot(d, aes(x=hitNum,y=sco,group=i,color=iniHit)) +geom_path(size=.6) +geom_point() +facet_wrap(~i, scales="free_x") +scale_color_manual(values=c("black","cyan3")) +scale_x_continuous(breaks = seq(0, 150, 1), expand = c(0, 1)) +theme(strip.text = element_blank()) +ggtitle("Scores for Players with [5-20] total Hits")
ggsavejj("fig2_subIndLearning")


######################################################################################################################
# COMPUTE PATH CHARACTERISTICS WITH MEASURES FROM DOI: 10.5923/j.ajgis.20140302.01
# If there are time stamps, convert data to poxIX format. Note, the code below needs to be redone in this case
#fps = 30 #frames per second, framerate
#options(digits.secs=6) #switch to sub-second display
#da = as.POSIXct(d$t * 1/fps, origin = "2014-12-31")
#head(da)
dl = as.ltraj(xy = as.matrix(d[, c("x","a"), with=F]), typeII=F, id = d[, paste(i,hitNum,sep=":")])
str(dl) #list of class ltraj containing "bursts" of relocations corresponding to ids x hitNum.
# head(dl[[4]])
# plot(dl)

# Path key to match dl and d
setkey(d,i,hitNum,t)
d[, date := 1:.N]
setkey(d,date)
dl = as.data.table(ld(dl))
setkey(dl,date)
d = merge(d,dl[, c("date","dx","dy","dist","dt","R2n","abs.angle","rel.angle"), with=F],by="date")
d$date = NULL
rm(dl) #remove helper ds

# GLOBAL DESCRIPTIVE STATISTICS
# Path length
d[!is.na(dist), "totDis" := sum(dist), by="i,hitNum"]
# Turning angle
d[!is.na(rel.angle), "totRan" := sum(rel.angle), by="i,hitNum"]
# Velocity/speed vel_j = dis_j / delta(t_j, t_j+1), j = {1, ..., T-1}
d[, "vel" := dist / dt]
d[!is.na(dist), "totVel" := sum(dist) / sum(dt), by="i,hitNum"] #total path velocity
# Accelleration acc_j = deltaVel_j / delta(t_j, t_j+1), j = {1, ..., T-1}
d[, "acc" := vel / dt]
d[!is.na(dt), "totAcc" := totVel / sum(dt), by="i,hitNum"] #total path acceleration
# Straightess index (Dodge, Weibel, Forootan, 2009) str=1 means straight, str=0 means convoluted
# str = (dist{p_j,p_j+1} + dist{p_j+1,p_j+2}) / dist{p_j,p_j+2}
d[, "str" := c(roll_sum(dist, n=2) / sqrt(roll_sum(dx, n=2)^2 + roll_sum(dy, n=2)^2), NA), by="i,hitNum"]




###########################################################################################################
## PLOT LEARNING CURVES
## Bin people with similar total numbers of hits together
d[, binTotHit := cut(totHitNum,c(0,1,4,10,50,150))]     #bin pp's total hit number
d[binTotHit=="(1,4]", binHit_binTotHit := cut(relHitNum,c(0,.5,1)), by=i]
d[binTotHit=="(1,4]", binHit_binTotHit_ch := levels(binHit_binTotHit)[binHit_binTotHit]]
d[binTotHit=="(4,10]", binHit_binTotHit := cut(relHitNum,seq(0,1,.2)), by=i]
d[binTotHit=="(4,10]", binHit_binTotHit_ch := levels(binHit_binTotHit)[binHit_binTotHit]]
d[binTotHit=="(10,50]", binHit_binTotHit := cut(relHitNum,seq(0,1,.1)), by=i]
d[binTotHit=="(10,50]", binHit_binTotHit_ch := levels(binHit_binTotHit)[binHit_binTotHit]]
d[binTotHit=="(50,150]", binHit_binTotHit := cut(relHitNum,seq(0,1,.1)), by=i]
d[binTotHit=="(50,150]", binHit_binTotHit_ch := levels(binHit_binTotHit)[binHit_binTotHit]]
d[, binHit_binTotHit := binHit_binTotHit_ch]
d$binHit_binTotHit_ch = NULL
d[, learned := ifelse(i %in% ids,">= 100","< 100")]

dl = d[totHitNum!=1, list(sco=mean(sco)), by="i,binHit_binTotHit,binTotHit,learned"][, list(scoMW = mean(sco), scoME = median(sco), scoSD = sd(sco)), by="binHit_binTotHit,binTotHit,learned"]

ggplot(d[!duplicated(i)], aes(totHitNum)) +geom_bar() +facet_wrap(~binTotHit, scales="free_x")

pj = position_jitter(h=0,w=.01)
ggplot(dl, aes(x=binHit_binTotHit,y=scoMW,group=factor(learned),color=factor(learned))) +geom_line(position=pj) +geom_pointrange(aes(ymin=scoMW-scoSD,ymax=scoMW+scoSD), alpha=.7,position=pj) +theme(panel.grid.major=element_blank(), axis.text.x=element_text(angle=90, vjust=.4, hjust=1)) +ggtitle("Learning Curves") + scale_x_discrete("Bins with % of trials per person") +scale_y_continuous("Score") +facet_grid(.~binTotHit, scales="free_x")
ggsave(file.path(idir,"fig_learnCurves.png"))

ggplot(d, aes(x=hitNum,y=sco,group=interaction(i,learned),color=factor(learned))) +geom_line() +theme(panel.grid.major=element_blank(), axis.text.x=element_text(angle=90, vjust=.4, hjust=1)) +ggtitle("Learning Curves") + scale_x_discrete("Trial number (p person)") +scale_y_continuous("Score") +facet_grid(learned~binTotHit, scales="free_x")


# Learners: How many times in a row did people reach a score > 100?
n = 1 #how many times in a row do they need to be > 100

dh[, comRolMW := c(rep(NA,n), head(cumsum(com)/(seq_len(.N)), -n)), by=i]
dh[comRolMW==1, min(hitNum), by=i]

ggplot(dh, aes(x=comRolMW)) + geom_bar()

# How many times did people play?
di = d[!duplicated(i), list(totHitNum=totHitNum, sco=round(sco[1],0)), by=i]
ggplot(di, aes(x=totHitNum)) +geom_bar(binwidth=1) +scale_x_continuous("Number of plays") +ggtitle("How Often Do Players Play?")
ggsave(file.path(idir,"fig_nPlays.png"))








ggplot(d[, list(mH = max(hitNum)), by=i], aes(x=mH)) + geom_histogram()
