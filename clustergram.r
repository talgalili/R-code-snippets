###### published on:
#   http://www.r-statistics.com/2010/06/clustergram-visualization-and-diagnostics-for-cluster-analysis-r-code/
## Main author of the function:  Tal Galili (tal.galili@gmail.com)


 
clustergram.kmeans <- function(Data, k, ...)
{
	# this is the type of function that the clustergram
	# 	function takes for the clustering.
	# 	using similar structure will allow implementation of different clustering algorithms
 
	#	It returns a list with two elements:
	#	cluster = a vector of length of n (the number of subjects/items)
	#				indicating to which cluster each item belongs.
	#	centers = a k dimensional vector.  Each element is 1 number that represent that cluster
	#				In our case, we are using the weighted mean of the cluster dimensions by 
	#				Using the first component (loading) of the PCA of the Data.
 
	cl <- kmeans(Data, k,...)
 
	cluster <- cl$cluster
	centers <- cl$centers %*% princomp(Data)$loadings[,1]	# 1 number per center
												# here we are using the weighted mean for each
 
	return(list(
				cluster = cluster,
				centers = centers
			))
}		
 
clustergram.plot.matlines <- function(X,Y, k.range, 
											x.range, y.range , COL, 
											add.center.points , centers.points)
	{
		plot(0,0, col = "white", xlim = x.range, ylim = y.range,
			axes = F,
			xlab = "Number of clusters (k)", ylab = "PCA weighted Mean of the clusters", main = c("Clustergram of the PCA-weighted Mean of" ,"the clusters k-mean clusters vs number of clusters (k)"))
		axis(side =1, at = k.range)
		axis(side =2)
		abline(v = k.range, col = "grey")
 
		matlines(t(X), t(Y), pch = 19, col = COL, lty = 1, lwd = 1.5)
 
		if(add.center.points)
		{
			require(plyr)
 
			xx <- ldply(centers.points, rbind)
			points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
 
			# add points	
			# temp <- l_ply(centers.points, function(xx) {
									# with(xx,points(y~x, pch = 19, col = "red", cex = 1.3))
									# points(xx$y~xx$x, pch = 19, col = "red", cex = 1.3)
									# return(1)
									# })
						# We assign the lapply to a variable (temp) only to suppress the lapply "NULL" output
		}	
	}
 
 
 
clustergram <- function(Data, k.range = 2:10 , 
							clustering.function = clustergram.kmeans,
							clustergram.plot = clustergram.plot.matlines, 
							line.width = .004, add.center.points = T)
{
	# Data - should be a scales matrix.  Where each column belongs to a different dimension of the observations
	# k.range - is a vector with the number of clusters to plot the clustergram for
	# clustering.function - this is not really used, but offers a bases to later extend the function to other algorithms 
	#			Although that would  more work on the code
	# line.width - is the amount to lift each line in the plot so they won't superimpose eachother
	# add.center.points - just assures that we want to plot points of the cluster means
 
	n <- dim(Data)[1]
 
	PCA.1 <- Data %*% princomp(Data)$loadings[,1]	# first principal component of our data
 
	if(require(colorspace)) {
			COL <- heat_hcl(n)[order(PCA.1)]	# line colors
		} else {
			COL <- rainbow(n)[order(PCA.1)]	# line colors
			warning('Please consider installing the package "colorspace" for prittier colors')
		}
 
	line.width <- rep(line.width, n)
 
	Y <- NULL	# Y matrix
	X <- NULL	# X matrix
 
	centers.points <- list()
 
	for(k in k.range)
	{
		k.clusters <- clustering.function(Data, k)
 
		clusters.vec <- k.clusters$cluster
			# the.centers <- apply(cl$centers,1, mean)
		the.centers <- k.clusters$centers 
 
		noise <- unlist(tapply(line.width, clusters.vec, cumsum))[order(seq_along(clusters.vec)[order(clusters.vec)])]	
		# noise <- noise - mean(range(noise))
		y <- the.centers[clusters.vec] + noise
		Y <- cbind(Y, y)
		x <- rep(k, length(y))
		X <- cbind(X, x)
 
		centers.points[[k]] <- data.frame(y = the.centers , x = rep(k , k))	
	#	points(the.centers ~ rep(k , k), pch = 19, col = "red", cex = 1.5)
	}
 
 
	x.range <- range(k.range)
	y.range <- range(PCA.1)
 
	clustergram.plot(X,Y, k.range, 
											x.range, y.range , COL, 
											add.center.points , centers.points)
 
 
}




if(F) {

#Examples:

png("d:\\clustergram_plots_%03d.png",650,650, pointsize  = 15)

data(iris)
set.seed(250)
par(cex.lab = 1.5, cex.main = 1.2)
Data <- scale(iris[,-5]) # notice I am scaling the vectors)
clustergram(Data, k.range = 2:8, line.width = 0.004) # notice how I am using line.width.  Play with it on your problem, according to the scale of Y.

set.seed(500)
Data <- scale(iris[,-5]) # notice I am scaling the vectors)
par(cex.lab = 1.2, cex.main = .7)
par(mfrow = c(3,2))
for(i in 1:6) clustergram(Data, k.range = 2:8 , line.width = .004, add.center.points = T)
par(mfrow = c(1,1))

set.seed(250)
Data <- rbind(
				cbind(rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3)),
				cbind(rnorm(100,1, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,1, sd = 0.3)),
				cbind(rnorm(100,2, sd = 0.3),rnorm(100,2, sd = 0.3),rnorm(100,2, sd = 0.3))
				)				
clustergram(Data, k.range = 2:5 , line.width = .004, add.center.points = T)

set.seed(250)
Data <- rbind(
				cbind(rnorm(100,1, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3)),
				cbind(rnorm(100,0, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3)),
				cbind(rnorm(100,0, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,1, sd = 0.3),rnorm(100,0, sd = 0.3)),
				cbind(rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,0, sd = 0.3),rnorm(100,1, sd = 0.3))
				)				
clustergram(Data, k.range = 2:8 , line.width = .004, add.center.points = T)


dev.off()
}

