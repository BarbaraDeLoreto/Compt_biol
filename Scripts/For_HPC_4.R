#####FOR HPC SIMULATION###############################

## step 1: obtain batch number from command line argument:
# not required for this one simulation

## step 2: run simulation:

# establish tmax and the initial number of trees
tmax <- 70  # number of time steps to simulate
nini <- 250  # initial number of trees

# establish the list of parameters
p <- list()
p$maxTrees <- 10^5      # number of trees (alive or dead) that the simulation can handle
p$maxx <- 500          # x-size of forest in meters (equals a hectare)
p$maxy <- 500           # y-size of forest in meter
p$maturityr <- 0.3      #size which marks the maturity for a tree - seed production
p$maxr   <- 1           # maximum radius of a tree
p$gamma_max  <- 0.1     # growth per time step max
p$gamma_min <- 0.01     #growth per step min
p$mu     <- 0.02        # mortality probability per time step
p$mu_under_3 <- 0.2     # mortality probability for plants under 3 years
p$lambda <- 0.2         # probability of producing surviving offspring after reaching maturity, per time step
p$d_d <- 30             # dispersal maximum distance
p$min_d <- 4            #minimum distance from an existing tree for new seedlings to start
p$d_f <- 3              #number of time steps it takes for a dead tree to "fall"and the space be open for other trees


# establish a dataframe with the initial forest

# initial random forest: 
iniForest <- data.frame(ID    = 1:nini,                             # ID for each tree 
                        alive = TRUE,                               # living or dead?
                        x     = runif(nini, min = 0, max = p$maxx), # x position
                        y     = runif(nini, min = 0, max = p$maxy), # y position
                        r     = runif(nini, min = 0, max = p$maxr), # radius
                        a     = NA,                                 # age
                        d     = NA)                                 # how long tree has been dead

iniForest$a <- iniForest$r/((p$gamma_max + p$gamma_min)/2) # expected age based on their size using mean o gamma_mx and gamma_min
iniForest$d <- 0 # all trees are alive


# core simulation function:

simulateForest <- function(iniForest, tmax, p) {
  ftt <- as.vector(rep(NA, tmax + 1), "list") # "forest through time"
  ftt[[1]] <- iniForest
  nextID <- max(ftt[[1]]$ID, na.rm = TRUE) + 1
  for(i in 1:tmax) {
    ftt[[i + 1]] <- data.frame(ID = rep(NA, p$maxTrees), alive = NA,                 
                               x = NA, y = NA, r = NA, a = NA, d= 0)                  
    
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
    
    # death dynamics
    # death under 3 years
    alive_under_3 <- which(ftt[[i + 1]]$alive & ftt[[i + 1]]$a <=3)
    ftt[[i + 1]]$alive[alive_under_3] <- (runif(length(alive_under_3)) > p$mu_under_3)
    
    # death others:
    alive_over_3 <- which(ftt[[i + 1]]$alive & ftt[[i + 1]]$a >3)
    ftt[[i + 1]]$alive[alive_over_3] <- (runif(length(alive_over_3)) > p$mu)
    
    # time steps a tree has been dead
    dead <- which(!ftt[[i + 1]]$alive) #establishing an index of dead ones
    ftt[[i + 1]]$d[dead] <- ftt[[i + 1]]$d[dead] + 1
    
    
    # Updating the forest state, removing dead trees 
    dead_falls <- which(ftt[[i + 1]]$d > p$d_f) #establishing an index of trees to be removed
    
    if (length(dead_falls) > 0) {ftt[[i + 1]] <-ftt[[i + 1]][-dead_falls,] #removing dead trees that have been there for some steps
    add_NA <- data.frame(ID = rep(NA, length(dead_falls)), alive = NA,                 
                         x = NA, y = NA, r = NA, a = NA, d= 0) 
    ftt[[i + 1]] <- rbind(ftt[[i + 1]], add_NA) #bind empty entries
    }
    
    alive <- which(ftt[[i + 1]]$alive) #reestablishing index after trees died
    
    # reproduction:
    for(j in alive) {
      if ((ftt[[i + 1]]$r[j]>= p$maturityr) && (runif(1) < p$lambda)){ #&& (is.na(simResult[[49 + 1]]$ID) == T))  # if r is at maturity or larger, reproduction happening?
        emptySlot <- which(is.na(ftt[[i + 1]]$ID))[1] # next empty row in dataframe
        ftt[[i + 1]]$ID[emptySlot] <- nextID
        ftt[[i + 1]]$alive[emptySlot] <- TRUE
        ftt[[i + 1]]$r[emptySlot] <- 0
        ftt[[i + 1]]$a[emptySlot] <- 0
        
        ## setting up check of space available
        attempt <- 0
        max_attempts <- 50 #maximum number of placement attempts
        
        #check if a new tree position overlaps with existing trees
        is_space_available <- function(new_x, new_y, exiting_trees, min_d) {
          min_d <- p$min_d
          existing_trees <- na.omit(ftt[[i + 1]])
          distances <- sqrt((existing_trees$x - new_x)^2 + (existing_trees$y- new_y)^2) #squared Euclidean distance between each existing tree and the proposed location
          return (all(distances>=p$min_d)) #returns true if space is available - includes min distance from parent tree
        }
        
        # place a new tree, ensuring it doesn't overlap with existing trees
        existing_trees <- na.omit(ftt[[i + 1]]) #establish the existing trees with all attributes
        repeat{  
          new_x <- runif(1, ftt[[i + 1]]$x[j] - p$d_d, ftt[[i + 1]]$x[j] + p$d_d) # proposed location of new plant within a certain distance from the parent plant
          new_y <- runif(1, ftt[[i + 1]]$y[j] - p$d_d, ftt[[i + 1]]$y[j] + p$d_d) # proposed location of new plant within a certain distance from the parent plant
          # checking if space is available
          if (is_space_available(new_x, new_y, existing_trees, p$min_d)) {
            ftt[[i + 1]]$x[emptySlot] <- new_x
            ftt[[i + 1]]$y[emptySlot] <- new_y
            break #break out of the repeat loop
          }
          attempt <- attempt +1
          if(attempt>=max_attempts){ #after 50 attempts, place the tree anywhere even if not available
            ftt[[i + 1]]$x[emptySlot] <- new_x
            ftt[[i + 1]]$y[emptySlot] <- new_y
            ftt[[i + 1]]$alive[emptySlot] <- FALSE
            break #after too many attempts to prevent infinite loop
          }
        }
        nextID <- nextID + 1
        emptySlot <- which(is.na(ftt[[i + 1]]$ID))[1]
      }
    }
    # get rid of empty rows:
    ftt[[i + 1]] <- ftt[[i + 1]][!is.na(ftt[[i + 1]]$ID),]
  }
  return(ftt)
}


# running the simulation:
simResult <- simulateForest(iniForest, tmax, p)

## step 3: saving the result as a file:

#save results
dput(simResult, "Tree_growth_sim_HPC_150.txt")



#bring output back
HPC_results <- source("Tree_growth_sim_HPC_150.txt")



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


################################################################################
###Visuals


# producing the giff:
saveGIF(forestMovie(HPC_results, p), movie.name = "C:/Users/badel/Documents/Assignment_Compt_biol_2/Images/forest_growth_movie_25h70t.gif")


# Plotting tree numbers is achieved
data_HPC <- data.frame(time = 1:tmax, n_trees = NA)

for (i in 1:tmax) {
  data_HPC$n_trees[i] <- sum(HPC_results[[i + 1]]$alive)
}

# Call the pdf command to start the plot
pdf(file = "../Plots/HPC_25h_70t.pdf",   
    width = 4, # The width of the plot in inches
    height = 4) # The height of the plot in inches

#density plot
ggplot(data_r, aes(time, n_trees))+
  geom_line() +
  labs(x = "Time", y = "Number of Trees") +
  ggtitle("Number of Trees Over Time") +
  geom_hline(yintercept = 150, linetype = "dashed", color = "red3") +
  geom_hline(yintercept = 200, linetype = "dashed", colour = "red3")

# create the file
dev.off()


