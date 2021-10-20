library(parallel)

detectCores() #get the number of cores in your system
c1<-makeCluster(detectCores()) 
#degree to radians
to.radians <- function(degrees){
  degrees * pi / 180
}
#the haversine function to calculate the distance between 2 geo-locations
haversine <- function(lat1, long1, lat2, long2, unit="km"){
  radius <- 6378      # radius of Earth in kilometers
  delta.phi <- to.radians(lat2 - lat1)
  delta.lambda <- to.radians(long2 - long1)
  phi1 <- to.radians(lat1)
  phi2 <- to.radians(lat2)
  term1 <- sin(delta.phi/2) ^ 2
  term2 <- cos(phi1) * cos(phi2) * sin(delta.lambda/2) ^ 2
  the.terms <- term1 + term2
  delta.sigma <- 2 * atan2(sqrt(the.terms), sqrt(1-the.terms))
  distance <- radius * delta.sigma
  if(unit=="km") return(distance)
  if(unit=="miles") return(0.621371*distance)
}

#-------------------------------------------------------------------------


# https://www.kaggle.com/max-mind/world-cities-database

set.seed(1)

all.cities.locs <- read.csv("~/DASCA_WORK/Global_Data/worldcitiespop.csv",
                            stringsAsFactors=FALSE)

head(all.cities.locs)

# Let's start off with 100 cities
smp.size <- 100

# choose a random sample of cities
random.sample <- sample((1:nrow(all.cities.locs)), smp.size)
cities.locs <- all.cities.locs[random.sample, ]
row.names(cities.locs) <- NULL

head(cities.locs)

#-------------------------single_core-----------------------
single.core <- function(cities.locs){
  running.sum <- 0
  for(i in 1:(nrow(cities.locs)-1)){
    for(j in (i+1):nrow(cities.locs)){
      # i is the row of the first lat/long pair
      # j is the row of the second lat/long pair
      this.dist <- haversine(cities.locs[i, 6],
                             cities.locs[i, 7],
                             cities.locs[j, 6],
                             cities.locs[j, 7])
      running.sum <- running.sum + this.dist
    }
  }
  # Now we have to divide by the number of
  # distances we took. This is given by
  return(running.sum /
           ((nrow(cities.locs)*(nrow(cities.locs)-1))/2))
}

system.time(ave.dist <- single.core(cities.locs))
print(ave.dist)

#----------------------Multi-core-------------
clusterExport(c1, c("haversine", "to.radians"))

multi.core <- function(cities.locs){
  all.combs <- combn(1:nrow(cities.locs), 2)
  numcombs <- ncol(all.combs)
  results <- parLapply(c1, 1:numcombs, function(x){
    lat1  <- cities.locs[all.combs[1, x], 6]
    long1 <- cities.locs[all.combs[1, x], 7]
    lat2  <- cities.locs[all.combs[2, x], 6]
    long2 <- cities.locs[all.combs[2, x], 7]
    return(haversine(lat1, long1, lat2, long2))
  })
  return(sum(unlist(results)) / numcombs)
}

system.time(ave.dist <- multi.core(cities.locs))
print(ave.dist)

#-----------------------simulation----------------------

# declaring an empty data frame with 3 columns and null entries
df = data.frame(matrix(
  vector(), 0, 3, dimnames=list(c(), c("Sample_Size","Cores","Time"))),
  stringsAsFactors=F)


i = 8 # set to the number of cores in the system

for(j in seq(1000,10000,1000)){
    clusterExport(makeCluster(i), c("haversine", "to.radians")) #number of cores to be assigned
    # choose a random sample of cities
    smp.size <- j #assign sample size
    random.sample <- sample((1:nrow(all.cities.locs)), smp.size)
    cities.locs <- all.cities.locs[random.sample, ]
    row.names(cities.locs) <- NULL
    #get the simulation results
    df<-rbind(df,c(j, i, system.time(ave.dist <- multi.core(cities.locs))[3]))
  }


colnames(df)<-c("Sample_Size","Cores","Time")

write.csv(df, 
          "~/DASCA_WORK/Global_Data/runtime_test.csv",
          row.names = TRUE)


#----single core testing------------------

# declaring an empty data frame with 3 columns and null entries
df = data.frame(matrix(
  vector(), 0, 3, dimnames=list(c(), c("Sample_Size","Cores","Time"))),
  stringsAsFactors=F)

i=1

for(j in seq(1000,10000,1000)){
        # choose a random sample of cities
    smp.size <- j #assign sample size
    random.sample <- sample((1:nrow(all.cities.locs)), smp.size)
    cities.locs <- all.cities.locs[random.sample, ]
    row.names(cities.locs) <- NULL
    #get the simulation results
    df<-rbind(df,c(j, i, system.time(ave.dist <- single.core(cities.locs))[3]))
  }

colnames(df)<-c("Sample_Size","Cores","Time")

write.csv(df, 
          "~/DASCA_WORK/Global_Data/runtime_test_single_core.csv",
          row.names = TRUE)




