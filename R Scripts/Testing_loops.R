library(tidyverse)


ftt <- as.vector(rep(NA, tmax + 1), "list") # "forest through time"
ftt[[1]] <- iniForest
nextID <- max(ftt[[1]]$ID, na.rm = TRUE) + 1

ftt[[2]] <- data.frame(ID = rep(NA, p$maxTrees), alive = NA,                 
                       x = NA, y = NA, r = NA, a = NA)                  
# forest in new time step copied over from previous one:
ftt[[1 + 1]][1:nrow(ftt[[1]]),] <- ftt[[1]]
# indices of living trees:
alive <- which(ftt[[1 + 1]]$alive)
# aging all trees:
ftt[[1 + 1]]$a[alive] <- ftt[[1 + 1]]$a[alive] + 1
# growing trees:
canGrow <- which(ftt[[1 + 1]]$alive & ftt[[1 + 1]]$r < p$maxr)
ftt[[1 + 1]]$r[canGrow] <- ftt[[1 + 1]]$r[canGrow] + runif(1, max = p$gamma_max, min = p$gamma_min)  #growth based on a random number from a range
# make sure they're not growing beyond maxr:
ftt[[1 + 1]]$r[ftt[[1 + 1]]$r > p$maxr] <- p$maxr
# death:
ftt[[1 + 1]]$alive[alive] <- (runif(length(alive)) > p$mu)
# reproduction:
emptySlot <- which(is.na(ftt[[1 + 1]]$ID))[1] # next empty row in dataframe
if (ftt[[1 + 1]]$r[1]>= p$maturityr) && (runif(1) < p$lambda))  # reproduction happening?

# probability of deth under 3 test
alive_under_3 <- which(ftt[[1 + 1]]$alive & ftt[[1 + 1]]$a <=3)
ftt[[1 + 1]]$alive[alive_under_3] <- (runif(length(alive_under_3)) > p$mu_under_3)
alive_over_3 <- which(ftt[[1 + 1]]$alive & ftt[[1 + 1]]$a >3)

# death:
ftt[[1 + 1]]$alive[alive_over_3] <- (runif(length(alive_over_3)) > p$mu)

# reproduction:
emptySlot <- which(is.na(ftt[[1 + 1]]$ID))[1] # next empty row in dataframe
for(j in alive) {
  if ((ftt[[1 + 1]]$r[4]>= p$maturityr) && (runif(1) < p$lambda)) { # if r is at maturity or larger, reproduction happening?
    ftt[[1 + 1]]$ID[emptySlot] <- nextID
    ftt[[1 + 1]]$alive[emptySlot] <- TRUE
    ftt[[1 + 1]]$x[emptySlot] <- runif(1, min = (ftt[[1 + 1]]$x[4] + 1), max = (ftt[[1 + 1]]$x[4] + p$d_d))
    ftt[[1 + 1]]$y[emptySlot] <- runif(1, min = (ftt[[1 + 1]]$y[4] + 1), max = (ftt[[1 + 1]]$y[4] + p$d_d))
    ftt[[1 + 1]]$x[ftt[[1 + 1]]$x > p$maxx] <- p$maxx 
    ftt[[1 + 1]]$y[ftt[[1 + 1]]$y > p$maxy] <- p$maxy 
    ftt[[1 + 1]]$r[emptySlot] <- 0
    ftt[[ + 1]]$a[emptySlot] <- 0
    nextID <- nextID + 1
    emptySlot <- emptySlot + 1

    # make sure they're not growing beyond maxr:
    ftt[[i + 1]]$r[ftt[[i + 1]]$r > p$maxr] <- p$maxr 
    
#test loop to establish death of plants outside the maximum forest area    
if (ftt[[1 + 1]]$x[emptySlot] > p$maxx | ftt[[1 + 1]]$y[emptySlot] > p$maxy) {
  ftt[[1 + 1]]$alive[emptySlot] <- FALSE
}
    
simResult[[51]]$alive[simResult[[51]]$alive == TRUE]
simResult[[51]][alive[simResult[[51]]$alive == TRUE],]

length(which(simResult[[51]]$alive == TRUE))

simResult[[51]]["ID"]

simResult[[51]]|>
  filter(alive)|>
  count()
                                       