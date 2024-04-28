# modified simulation of a forest of growing trees
# Authors: Barbara Oliveira De Loreto, modified from : Jan Engelstaedter & Daniel Ortiz-Barrientos
# Date: 28/4/2024

library(animation)
library(plotrix)

# core simulation function:

simulateForest <- function(iniForest, tmax, p) {
  ftt <- as.vector(rep(NA, tmax + 1), "list") # "forest through time"
  ftt[[1]] <- iniForest
  nextID <- max(ftt[[1]]$ID, na.rm = TRUE) + 1
  for(i in 1:tmax) {
    ftt[[i + 1]] <- data.frame(ID = rep(NA, p$maxTrees), alive = NA,                 
                               x = NA, y = NA, r = NA, a = NA)                  
    # forest in new time step copied over from previous one:
    ftt[[i + 1]][1:nrow(ftt[[i]]),] <- ftt[[i]]
    # indices of living trees:
    alive <- which(ftt[[i + 1]]$alive)
    # aging all trees:
    ftt[[i + 1]]$a[alive] <- ftt[[i + 1]]$a[alive] + 1
    # growing trees:
    canGrow <- which(ftt[[i + 1]]$alive & ftt[[i + 1]]$r < p$maxr)
    ftt[[i + 1]]$r[canGrow] <- ftt[[i + 1]]$r[canGrow] + runif(1, max = p$gamma_max, min = p$gamma_min)  #growth based on a random number from a range
    # make sure they're not growing beyond maxr:
    ftt[[i + 1]]$r[ftt[[i + 1]]$r > p$maxr] <- p$maxr
    
    # death under 3 years
    alive_under_3 <- which(ftt[[i + 1]]$alive & ftt[[i + 1]]$a <=3)
    ftt[[i + 1]]$alive[alive_under_3] <- (runif(length(alive_under_3)) > p$mu_under_3)
    # death others:
    alive_over_3 <- which(ftt[[1 + 1]]$alive & ftt[[1 + 1]]$a >3)
    ftt[[i + 1]]$alive[alive_over_3] <- (runif(length(alive_over_3)) > p$mu)
    
    # reproduction:
    emptySlot <- which(is.na(ftt[[i + 1]]$ID))[1] # next empty row in dataframe
    for(j in alive) {
      if ((ftt[[i + 1]]$r[j]>= p$maturityr) && (runif(1) < p$lambda)) { # if r is at maturity or larger, reproduction happening?
        ftt[[i + 1]]$ID[emptySlot] <- nextID
        ftt[[i + 1]]$alive[emptySlot] <- TRUE
        ftt[[i + 1]]$x[emptySlot] <- runif(1, min = (ftt[[i + 1]]$x[j] + 1), max = (ftt[[i + 1]]$x[j] + p$d_d)) # location of new plant within a certain distance from the mother plant
        ftt[[i + 1]]$y[emptySlot] <- runif(1, min = (ftt[[i + 1]]$y[j] + 1), max = (ftt[[i + 1]]$y[j] + p$d_d)) # location of new plant within a certain distance from the mother plant
        ftt[[i + 1]]$r[emptySlot] <- 0
        ftt[[i + 1]]$a[emptySlot] <- 0
        nextID <- nextID + 1
        emptySlot <- emptySlot + 1
      }
    }
    # get rid of empty rows:
    ftt[[i + 1]] <- ftt[[i + 1]][!is.na(ftt[[i + 1]]$ID),]
  }
  return(ftt)
}

# plotting the forest at a given time:
plotForest <- function(forest, p) {
  forest <- forest[order(forest$alive),]
  par(mar=c(0,0,0,0))
  plot(0, xlim = c(0, p$maxx), ylim = c(0, p$maxy), type='n', axes=FALSE, ann=FALSE)
  cols <- ifelse(forest$alive, hsv(0.15 + (forest$ID %% 11)/30, 
                                   0.8 + (forest$ID %% 5)/40, 
                                   0.4 + (forest$ID %% 7)/30), "grey")
  for(i in 1:nrow(forest))
    draw.circle(forest$x[i], forest$y[i], forest$r[i], col = cols[i], border = cols[i])
}

# plotting forest growth through time as a video:
forestMovie <- function(ftt, p, interval = 0.1) {
  ani.options(interval = interval)
  for (i in 1:length(ftt)) {
    dev.hold()
    plotForest(ftt[[i]], p)
    text(x = 0, y = 1, adj = c(0, 1), label = paste0("time=",i),
         col = "red", cex = 1.5)
    ani.pause()
  }
  invisible(NULL)
}

# all parameters bundled into a list:
p <- list()
p$maxTrees <- 100 # number of trees (alive or dead) that the simulation can handle
p$maxx <- 200      # x-size of forest in meters
p$maxy <- 200      # y-size of forest in meter
p$maturityr <- 3  #size which marks the maturity for a tree - seed production
p$maxr   <- 4      # maximum radius of a tree
p$gamma_max  <- 0.5     # growth per time step max
p$gamma_min <- 0.01  #growth per step min
p$mu     <- 0.02 # mortality probability per time step
p$mu_under_3 <- 0.3 # mortality probability for plants under 3 years
p$lambda <- 0.2     # probability of producing surviving offspring after reaching maturity, per time step
p$d_d <- 30 # dispersal maximum distance

tmax <- 50 # number of time steps to simulate
nini <- 15  # initial number of trees

# initial random forest:
iniForest <- data.frame(ID    = 1:nini,                             # ID for each tree 
                        alive = TRUE,                               # living or dead?
                        x     = runif(nini, min = 0, max = p$maxx), # x position
                        y     = runif(nini, min = 0, max = p$maxy), # y position
                        r     = runif(nini, min = 0, max = p$maxr), # radius
                        a     = NA)                                 # age
iniForest$a <- iniForest$r/((p$gamma_max + p$gamma_min)/2) # expected age based on their size using mean o gamma_mx and gamma_min

# running the simulation:
simResult <- simulateForest(iniForest, tmax, p)
saveGIF(forestMovie(simResult, p), movie.name = "first_forest_movie_test8.gif")