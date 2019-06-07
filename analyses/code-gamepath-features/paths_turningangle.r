### Jana Jarecki
### 19. July 2017
### Calculate turning angles
if (!require(adehabitatLT)) { install.packages("adehabitatLT") } else { library(adehabitatLT) } # spatiotemporal analyses


compute.turning.angle <- function(X, Y, hitID) { 
  # Normalize the data
  game_width <- 1562
  game_height <- 546
  X_range <- max(X)
  Y_range <- -min(Y)
  Xnorm <- game_width/X_range * X]
  YNorm <- game_height/Y_range * Y]

  d <- data.table(Xnorm, Ynorm, hitID)

  # Calculate the turning angle
  d[, c("dx","dy","dist","dt","R2n","abs.angle","rel.angle") := lapply(as.list(as.ltraj(cbind(Xnorm,YNorm), typeII = FALSE, id=hitID[1], slsp="remove")[[1]][c("dx","dy","dist","dt","R2n","abs.angle","rel.angle")]), as.numeric), by=hitID]

  # Relative turning angles
  rel.angle <- d[, list(
      mean_relangle = mean(rel.angle, na.rm=TRUE), 
      var_relangle = var(rel.angle, na.rm=TRUE), 
      min_relangle = min(rel.angle, na.rm=TRUE), 
      max_relangle = max(rel.angle, na.rm=TRUE)), by = hitID]

  rel.angle[min_relangle == Inf, min_relangle := NA]
  rel.angle[max_relangle == - Inf, max_relangle := NA]

  return(rel.angle)
  # fwrite(rel.angle, "rel.angle.csv")
}

