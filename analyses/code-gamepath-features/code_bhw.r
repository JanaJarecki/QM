############################################################################
# Jana Jarecki
# Analyzes bring home water
############################################################################
rm(list=ls())
# Libs
library(psych)          #Descriptive statistics
library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(adehabitatLT)   #spatiotemporal analysis, http://cran.r-project.org/web/packages/adehabitatLT/adehabitatLT.pdf
library(RcppRoll)       #Sliding window functions in c++, http://cran.r-project.org/web/packages/RcppRoll/RcppRoll.pdf
#library(fractaldim)    #calculate fractal dimensions, http://cran.r-project.org/web/packages/fractaldim/fractaldim.pdf
    #user-defined theme, comment this out
theme_set(themejj())
ddir = "../4-Data"
odir = "../6-Output"
idir = "../7-Img"


# Load data
d = fread("../../data/data_bhw_ind.csv", head=FALSE, col.naems = c("hitId", "optId", "i", "hitNum", "t", "x", "a", "oriFid", "optFid"))


# distributions of plays
ggplot(d[, max(hitNum), by=i][V1<500, .N, by=V1], aes(V1,N)) +geom_bar(stat="identity")

# how many players?
d[!duplicated(i), .N]

# how long did they play?
d[, max(hitNum), by=i][,summary(V1)]

# which original fidelity did they reach?
d[, max(oriFid), by=i][,summary(V1)]

# did they learn?
dh = d[, list(oriFid=oriFid[1],optFid=optFid[1]), by="i,hitNum"]
setkey(dh,i,hitNum)

dh[, totHit10 := cut(totHit10,seq(0,max(totHit10),10),include.lowest=T)]

ggplot(d[, aes(hitNum,group=i)) +geom_path()


# # Define 10%-quantiles of original fidelity
# d[!duplicated(i), oriFidQua := cut(oriFid, quantile(oriFid, probs=0:10/10), include.lowest=TRUE)]
# d[, oriFidQua := oriFidQua[1], by=i]
# # Define maximum duration
# d[, maxT := tail(t,1), by=i]
# d[, maxTCut := cut(maxT,10)]

ggplot(d, aes(x=x,y=a,group=hitNum,colour=factor(optFid))) +geom_path(arrow=arrow(angle=10, length=unit(5,"mm"), end="first", type="closed"), size=1) +facet_grid(.~i+hitNum) +scale_color_brewer(type="div", palette=3)


######################################################################################################################
# Path measures similar to DOI: 10.5923/j.ajgis.20140302.01
#######################################################################################################################
# Make data poxIX format
#fps = 30 #frames per second, framerate
#options(digits.secs=6) #switch to sub-second display
#da = as.POSIXct(d$t * 1/fps, origin = "2014-12-31")
#head(da)
dl = as.ltraj(xy = as.matrix(d[,c("x","a"), with=F]), typeII=F, id = d[, paste(i,hitNum,sep=":")])
dl #list of class ltraj containing "bursts" of relocations corresponding to ids x hitNum.
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


# d[, way := ifelse(t<t[which.max(x)],1L,2L), by="i,hitNum"]
# setcolorder(d,c("i","hitNum","way",names(d)[3:(ncol(d)-1)]))

# GLOBAL DESCRIPTICE STATISTICS
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

col = c("totVel","totAcc","totDis","totRan","str","optFid")
d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, describe(.SD), .SDcols=col]
d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, cor(.SD, method="spearman"), .SDcols=col]
figVel = ggplot(d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, lapply(.SD, FUN = function(x) { cut(x, breaks=quantile(x, seq(0,1,1/4), names=F), include.lowest=T) } ) , .SDcols=col] , aes(totVel, fill=factor(optFid))) +geom_bar(position="dodge") +scale_fill_brewer("Optimized\nFidelity", type = "div", palette = 1) +ggtitle("Velocity of Total Path") +scale_x_discrete("Velocity Quantile")
figAcc = ggplot(d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, lapply(.SD, FUN = function(x) { cut(x, breaks=quantile(x, seq(0,1,1/4), names=F), include.lowest=T) } ) , .SDcols=col] , aes(totAcc, fill=factor(optFid))) +geom_bar(position="dodge") +scale_fill_brewer("Optimized\nFidelity", type = "div", palette = 1) +ggtitle("Acceleration of Total Path") +scale_x_discrete("Acceleration quantile")
figDis = ggplot(d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, lapply(.SD, FUN = function(x) { cut(x, breaks=quantile(x, seq(0,1,1/4), names=F), include.lowest=T) } ) , .SDcols=col] , aes(totDis, fill=factor(optFid))) +geom_bar(position="dodge") +scale_fill_brewer("Optimized\nFidelity", type = "div", palette = 1) +ggtitle("Length of Total Path") +scale_x_discrete("Distance quantile")
figRan = ggplot(d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, lapply(.SD, FUN = function(x) { cut(x, breaks=quantile(x, seq(0,1,1/4), names=F), include.lowest=T) } ) , .SDcols=col] , aes(totRan, fill=factor(optFid))) +geom_bar(position="dodge") +scale_fill_brewer("Optimized\nFidelity", type = "div", palette = 1) +ggtitle("Turning Angles of Total Path") +scale_x_discrete("Turning angle quantile")
figStr = ggplot(d[, lapply(.SD, mean, na.rm=T), .SDcols=col, by="i,hitNum"][, lapply(.SD, FUN = function(x) { cut(x, breaks=quantile(x, seq(0,1,1/4), names=F), include.lowest=T) } ) , .SDcols=col] , aes(str, fill=factor(optFid))) +geom_bar(position="dodge") +scale_fill_brewer("Optimized\nFidelity", type = "div", palette = 1) +ggtitle("Mean Straightness of Total Path") +scale_x_discrete("Straightness index (0=zigzag, 1=straight) quantile")


grid.arrange.onelegend <- function(..., position="right") {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- lheight <- sum(legend$width)
    if(position=="bottom") {
        grid.arrange(
            do.call(arrangeGrob, lapply(plots, function(x)
                x + theme(legend.position="none"))),
            legend,
            ncol = 1,
            heights = unit.c(unit(1, "npc") - lheight, lheight))
    }
    if(position=="right") {
        grid.arrange(
            do.call(arrangeGrob, lapply(plots, function(x)
                x + theme(legend.position="none") +scale_y_continuous(lim=c(0,700)))),
            legend,
            ncol = 2,
            widths = unit.c(unit(1, "npc") - lwidth, lwidth))
    }
}


grid.arrange.onelegend(figVel,figAcc,figDis,figRan,figStr)

leg <- ggplotGrob(figVel)
legend <- leg[which(leg$name == "guide-box")]
widthDetails.legendGrob <- function(x) unit(3, "cm")
grid.arrange(figVel,figAcc,figDis,figRan,figStr,legend=legend)



# Straightness of directed paths strInd = dist(p_1,p_j+1) / sum_i=j^J dist_i, j = {1, ..., T-1} Benhamou (2004)
# strInd = 1 for straight path, strInd = 0 for area-spanning path
d[, "str" := sqrt(cumsum(dx)^2 + cumsum(dy)^2) / cumsum(dist), by="i,hitNum"]
source("nSignChanges.r")
# Number of x-sign changes
d[, "nscX" := nSignChanges(na.omit(dx)), by="i,hitNum"]
# Number of y-sign changes
d[, "nscY" := nSignChanges(na.omit(dy)), by="i,hitNum"]
# Metrices by way, way=1 is the path rithtwards, way=2 is the path leftwards

d[, "strWay" := sqrt(cumsum(dx)^2 + cumsum(dy)^2) / cumsum(dist), by="i,hitNum,way"]


# LOCAL FEATURE EXTRACTION




# Fractal or Hausdorff dimension D: frD_j = (dis_j + dis_j+1) / dist(p_j,p_j+2)
# Measures area-fillingness of the path, frD=[1,2], frD=1 for p_j,p_j+1,p_j+2 on line, frD=2 for square
# d[, "frD" := (dis + c(tail(dis,-1),NA)) / sqrt((dx + c(tail(dx,-1),NA))^2 + (dy + c(tail(dy,-1),NA))^2), by="i,hitNum"]
# d[, sqrt((dx + c(tail(dx,-1),NA))^2 + (dy + c(tail(dy,-1),NA))^2)]

# Save data
write.csv(d, file.path(ddir,"data_bhw_mf.csv"))
rm(dl,dl2)




































# Fit a polynomial to the curves
d[i==1 & hitNum==1, lm(a ~ poly(x,3))]


library(stats)
require(graphics)


## and one from Fritsch and Carlson (1980), Dougherty et al (1989)
x. <- c(7.09, 8.09, 8.19, 8.7, 9.2, 10, 12, 15, 20)
f <- c(0, 2.76429e-5, 4.37498e-2, 0.169183, 0.469428, 0.943740,
       0.998636, 0.999919, 0.999994)
s0 <- splinefun(x., f)
s1 <- splinefun(x., f, method = "monoH.FC")
s2 <- splinefun(x., f, method = "hyman")
plot(x., f, ylim = c(-0.2, 1.2))
curve(s0(x), add = TRUE, col = 2, n = 1001) -> m0
curve(s1(x), add = TRUE, col = 3, n = 1001)
curve(s2(x), add = TRUE, col = 4, n = 1001)
legend("right",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")



# Functions for the different properties
# amplitude
# curvature
# max x-position
funMaxX = function(xVec){
    max(xVec)
}
# inflection points

# trend from start to end
funTrend = function(aVec){
    tail(aVec,1) - head(aVec,1)
}
# velocity