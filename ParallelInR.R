# exploring parallel computing in R
# note that this works on a Mac, but won't on Windows
# due to differences in using multiple cores

# use of package parallel and "apply" variants
# note there are other options, "foreach" for example
# but this approach is actually quite flexible
# as long as you can figure out a way to "apply" your code, you can run it
# in parallel on any multi-core machine (which most are these days)

library(parallel)

# the following section is taken from Glenn Lockwood's excellent
# webpages on parallel computing and R
# http://www.glennklockwood.com/data-intensive/r/parallel-options.html
# and the corresponding GitHub repo:
# https://github.com/glennklockwood/paraR/tree/master/kmeans

#    generate a dataset for use with the k-means clustering
#    examples included in the repository
#

nrow <- 50000
sd <- 0.5

real.centers <- list( x=c(-1.3, -0.7, 0.0, +0.7, +1.2), 
                      y=c(-1.0, +1.0, 0.1, -1.3, +1.2) )

data <- matrix(nrow=0, ncol=2)
colnames(data) <- c("x", "y")

for (i in seq(1, 5)) {
      x0 <- rnorm(nrow, mean=real.centers$x[[i]], sd=sd)
      y0 <- rnorm(nrow, mean=real.centers$y[[i]], sd=sd)
      data <- rbind( data, cbind(x0,y0) )
}

# the job in serial (standard single-thread)
result <- kmeans( data,
                  centers=4,
                  nstart=99 )
print(result)
plot(data, col = result$cluster)
points(result$centers, col = 1:2, pch = 8, cex = 2)

system.time(result <- kmeans( data,
                              centers=4,
                              nstart=99 ))

# same thing, but run via "apply"
# this won't be faster, but is the way the problem needs to
# be structured to move on with parallel apply methods
parallel.function <- function(i) {
      kmeans( x=data, centers=4, nstart=i )
}

results <- lapply( c(33, 33, 33), FUN=parallel.function )
temp.vector <- sapply( results, function(result) { result$tot.withinss } )
result <- results[[which.min(temp.vector)]]

print(result)
system.time({results <- lapply( c(33, 33, 33), FUN=parallel.function )
            temp.vector <- sapply( results, function(result) { result$tot.withinss } )
            result <- results[[which.min(temp.vector)]]})

# with code that works in "apply," it's simple to move to parallel apply
# the function mcapply uses multiple cores on the same machine

# note this parallel function is the exact same as above for use with lapply
parallel.function <- function(i) {
      kmeans( x=data, centers=4, nstart=i )
}

# however this time, we use "mcapply" instead of "lapply"
# this should indeed be faster than previous attempts
results <- mclapply( c(33, 33, 33), FUN=parallel.function )
temp.vector <- sapply( results, function(result) { result$tot.withinss } )
result <- results[[which.min(temp.vector)]]

print(result)
system.time({results <- mclapply( c(33, 33, 33), FUN=parallel.function )
temp.vector <- sapply( results, function(result) { result$tot.withinss } )
result <- results[[which.min(temp.vector)]]})

# what about multiple cores spread out on multiple machines?
# there's a different, yet analagous, function for that, parLapply

# not run as we don't have easy access to a remote set of cores

# probably requires the library snow
# and also maybe Rmpi, etc.

# parallel.function <- function(i) {
#       kmeans( x=data, centers=4, nstart=i )
# }
# 
# cl <- makeCluster( mpi.universe.size(), type="MPI" )
# clusterExport(cl, c('data'))
# 
# results <- parLapply( cl, c(33, 33, 33), fun=parallel.function )
# temp.vector <- sapply( results, function(result) { result$tot.withinss } )
# result <- results[[which.min(temp.vector)]]
# print(result)
# 
# stopCluster(cl)
# mpi.exit()

#########################################################################
#
# ecology example
#
#########################################################################

# Bayesian movement models
# uses Bayesian approach to make a movement path, which may have multiple
# states (movement behaviors) from a series of timestamped locations
# such as satellite telemetry fixes

library(bsam)
# load example data, which are satellite track of two elephant seals
data(ellie)
head(ellie)

# to get a better view of what these look like
library(ggmap)
elliehab <- make_bbox(lon, lat, ellie, f=0.2)
elliehab.map <- get_map(location= elliehab, maptype="watercolor", zoom=4)
q <- ggmap(elliehab.map)
q + geom_path(mapping=aes(x=lon, y=lat, group=id, color=id), data=ellie)


# Fit DCRWS model for state filtering, regularization and behavioural state estimation
# the standard, serial method
# there are two individuals in this set, so models are fit in sequence
fit.s <- fit_ssm(ellie, model = "DCRWS", tstep = 2, adapt = 5000, samples = 5000, 
                 thin = 5, span = 0.2) # 4.3 minutes the time I last ran it
plot_fit(fit.s)
map_ssm(fit.s)
# make a prettier map, but first needs to be delisted
fit.s.df <- do.call(rbind.data.frame, lapply(fit.s, function(x) x[[1]]))
q + geom_path(mapping=aes(x=lon, y=lat, group=id, color=b), data=fit.s.df) + scale_colour_gradient2(low="blue", high="red", midpoint=1.5)


# fitting models by individual, one after another? sounds like a situation for parallel!

# make a function to apply this by animal
ffit_ssm <- function(x) {
      x$id <- factor(x$id)
      fit_ssm(x, model="DCRWS", tstep=2, adapt = 5000, samples = 5000, 
              thin = 5, span = 0.2)
}

# and now send these data through the wrapper function
fit.mclapply <- mclapply(split(ellie, ellie$id), ffit_ssm, mc.cores=2) # done in like 2 minutes
# again, make a prettier map, but first needs to be delisted
# slightly different list structure than the serial output
fit.mc.df <- do.call(rbind.data.frame, lapply(fit.mclapply, function(x) x[[1]][[1]]))
q + geom_path(mapping=aes(x=lon, y=lat, group=id, color=b), data=fit.mc.df) + scale_colour_gradient2(low="blue", high="red", midpoint=1.5)


###############################################################################
#
#  final example, canned parallel example for JAGS (Bayesian) in package runjags
#
###############################################################################
library(runjags) # need to have JAGS installed in your computer somewhere too

# example from runjags help file
# run a model to calculate the intercept and slope of the expression 
# y = m x + c, assuming normal observation errors for y:

# Simulate the data
X <- 1:100
Y <- rnorm(length(X), 2*X + 10, 1)
plot(X,Y)

# Model in the JAGS format
model <- "model { 
      # the actual business part which fits a Gaussian linear model
      for(i in 1 : N){ 
            Y[i] ~ dnorm(true.y[i], precision);
            true.y[i] <- (m * X[i]) + c
      } 
      # specifying the prior distribution for the parameters
      m ~ dunif(-1000,1000)
      c ~ dunif(-1000,1000) 
      precision ~ dexp(1)
}"

# Data and initial values in a named list format, 
# with explicit control over the random number
# generator used for each chain: 
# note that you must explicitly provide inits for each core/chain when running parallel
data <- list(X=X, Y=Y, N=length(X))
inits1 <- list(m=1, c=1, precision=1,
               .RNG.name="base::Super-Duper", .RNG.seed=1)
inits2 <- list(m=0.1, c=10, precision=1,
               .RNG.name="base::Wichmann-Hill", .RNG.seed=2)
inits3 <- list(m=0.5, c=5, precision=1,
               .RNG.name="base::Super-Duper", .RNG.seed=1)

# first run the model with the standard serial method, method="rjags"
# Run the model and produce plots 
results <- run.jags(model=model, monitor=c("m", "c", "precision"), 
                    data=data, n.chains=3, method="rjags", inits=list(inits1,inits2,inits3))

# Standard plots of the monitored variables:
plot(results)

# Look at the summary statistics:
print(results)


# The same model using parallel chains:

# Run the 3 chains in parallel 
# note the only change to the run.jags call is to set method="parallel"
results <- run.jags(model, monitor=c("m", "c", "precision"), 
                    data=data, n.chains=3, inits=list(inits1,inits2,inits3),
                    method="parallel")

# View the results using the standard print method:
results

# Look at some plots of the intercept and slope on a 3x3 grid:
plot(results, c('trace','histogram','ecdf','crosscorr','key'),
     vars=c("m","^c"),layout=c(3,3))
