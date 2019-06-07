## This file contains various helper functions
## Author Jana Jarecki
##
## This file is usually sourced from other files doing the path-analysis.

if(!require(data.table)) library(data.table)

# Prepare game data for path analysis
prep.gdata <- function()
{
    participants <- fread("../../data/processed/participants.csv")$x
    # Clean data, create variables
    gdata <- gdata[ID %in% participants & levelName %in% c("l3","L4")]

    gdata[, hitNr := frank(createdat, ties.method = "dense"), by = list(ID,levelName)]

    gdata <<- gdata[]
}

prep.pdata <- function()
{
    # make hitnr variable
    pdata[, id := 1:.N, by = list(hitID)]
    pdata[, hitNr := frank(createdAt, ties.method = "dense"), by = list(ID,levelName)]
        
    # Smooth the curves
    pdata[, X.smooth := loess(X ~ id, span = 3/.N)$fitted, by = list(hitID)] 
    pdata[, Y.smooth := loess(Y ~ id, span = 3/.N)$fitted, by = list(hitID)] 

    pdata <<- pdata[] 
}


# Compute the euc. distance between two paths
compute.distance <- function(dt)
{
    hitNr <- dt[, hitNr]

    if(length(unique(hitNr)) < 2)
        return(as.double(NA))

    if (length(unique(hitNr)) > 2)
        stop("Can only use two hit numbers for dists calculation.")
    
    col1  <- paste("X",  unique(hitNr), sep="_")
    col2  <- paste("Y",  unique(hitNr), sep="_")
    col3  <- paste("dt", unique(hitNr), sep="_")

    wdata <- dcast(dt, id ~ hitNr, value.var = c("X", "Y","dt"))

    # distance x-y-t space
    wdata[(complete.cases(wdata)), dist.x.y.t := dist(
        cbind(c(get(col1[1]), get(col1[2])),
              c(get(col2[1]), get(col2[2])),
              c(get..(col3[1]), get(col3[2])))), by = id]
    # distance only x-axis
    wdata[(complete.cases(wdata)), dist.x := dist(
        cbind(c(get(col1[1]), get(col1[2])))), by = id]
    # distance only y-axis
    wdata[(complete.cases(wdata)), dist.y := dist(
        cbind(c(get(col2[1]), get(col2[2])))), by = id]
    # distance only t-axis
    wdata[(complete.cases(wdata)), dist.t := dist(
        cbind(c(get(col3[1]), get(col3[2])))), by = id]
    # distance x-y space
    wdata[(complete.cases(wdata)), dist.x.y := dist(
        cbind(c(get(col1[1]), get(col1[2])),
              c(get(col2[1]), get(col2[2])))), by = id]
    # distance x-t space
     wdata[(complete.cases(wdata)), dist.x.t := dist(
        cbind(c(get(col1[1]), get(col1[2])),
              c(get(col3[1]), get(col3[2])))), by = id]
    # distance y-t space
    wdata[(complete.cases(wdata)), dist.y.t := dist(
        cbind(c(get(col2[1]), get(col2[2])),
              c(get(col3[1]), get(col3[2])))), by = id]  

    ldata <- melt(wdata, measure = list(col1, col2, col3), value.name = c("X","Y","dt"), variable.name = "hitNr", variable.factor = FALSE)

    ldata[, hitNr := as.numeric(hitNr)]
    ldata[, dist.x.y.t := as.vector(dist.x.y.t)]
    ldata[, dist.x     := as.vector(dist.x)]
    ldata[, dist.y     := as.vector(dist.y)]
    ldata[, dist.t     := as.vector(dist.t)]
    ldata[, dist.x.y   := as.vector(dist.x.y)]
    ldata[, dist.x.t   := as.vector(dist.x.t)]
    ldata[, dist.y.t   := as.vector(dist.y.t)]

    cols <- c('dist.x.y.t', 'dist.x', 'dist.y', 'dist.t', 'dist.x.y', 'dist.x.t', 'dist.y.t')

    ldata[hitNr==1, c(cols) := NA]

    distances <- ldata[hitNr %in% c(1,max(hitNr)) & !is.na(X), cols, with = F][]

    return(as.list(distances))
}