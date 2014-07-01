
#find the distance between points 1 and 2 in n-dimensional space, where n is the number of features of the objects being clustered
distance <- function(pt1, pt2) {
	r <- 0
	dimensions <- length(pt1)
	stopifnot(length(pt1) == length(pt2))

	for(i in 1:dimensions) {
		r <- r + (pt1[i] - pt2[i])*(pt1[i] - pt2[i])
	}
	return(sqrt(r))
}

#find the number of neighbors for each point. Point A is a neighbor of point B if their distance is less than dc
local.density <- function(data, dc) {
	n.points <- dim(data)[[1]]
	rho <- c(rep(0, n.points))
	for(i in 1:(n.points-1)) {
		for(j in (i+1):n.points) {
			pt1 <- data[i,]
			pt2 <- data[j,]
			if(distance(pt1, pt2) < dc) {
				rho[i] <- rho[i] + 1
				rho[j] <- rho[j] + 1
			}
		}
	}
	return(rho)
}

#find a value of dc such that the mean number of neighbors is within the desired range
find.dc <- function(data, neighborRateLow, neighborRateHigh) {
	dcstep <- 0.01

	n.points <- dim(data)[[1]]
	n.low <- neighborRateLow * n.points
	n.high <- neighborRateHigh * n.points
	dc <- 0.0
	avg.neighbors <- 0.0

	while((avg.neighbors < n.low) || (avg.neighbors > n.high)) {
		rho <- local.density(data, dc)
		avg.neighbors <- mean(rho)
		if(avg.neighbors < n.low) {
			dc <- dc + dcstep
		}
		else if(avg.neighbors > n.high) {
			dc <- dc - dcstep
		}
	}
	return(dc)
}

#find the minimum distance between each point and another point with higher density
distance.to.higher.density(data, rho) {
	n.points <- dim(data)[[1]]
	delta <- c(rep(0, times=n.points))

	for(i in 1:n.points) {
		min.dist.to.higher <- .Machine$double.xmax

		has.highest.density <- TRUE
		for(j in 1:n.points) {
			if(i == j) next
			if(rho[j] > rho[i]) {
				has.highest.density <- FALSE
				pt1 <- data[i,]
				pt2 <- data[j,]
				dist.to.higher <- distance(pt1, pt2)
				if(dist.to.higher < min.dist.to.higher) {
					min.dist.to.higher <- dist.to.higher
				}
			}
		}
		delta[i] <- min.dist.to.higher
	}
	return(delta)
}

assign.to.clusters(data, cluster.centers, dc) {
	n.points <- dim(data)[[1]]
	cluster.assignments <- c(rep(0, times=n.points))

	for(i in 1:n.points) {
		min.distance.to.cluster <- .Machine$double.xmax
		for(j in 1:length(cluster.centers)) {
			c <- cluster.centers[j]
			distance.to.this.cluster <- distance(data[i,], data[c,])
			if(distance.to.this.cluster < min.distance.to.cluster) {
				min.distance.to.cluster <- distance.to.this.cluster
				cluster.assignments[i] <- j
			}
		}
	}
	return(cluster.assignments)
}

#cluster the objects. Data matrix should be (objects x features). 
dpc <- function(data, n.clusters, neighborsLow, neighborsHigh) {
	n.points <- dim(data)[[1]]

	dc <- find.dc(data, dcmin, dcmax)
	rho <- local.density(data, dc)
	delta <- distance.to.higher.density(data, rho)


	rho.norm <- rho / sum(rho)
	delta.norm <- delta / sum(delta)

	#find objects with the maximum rho*delta
	rho.delta <- c(rep(0, times=n.points))
	for(i in 1:n.points) {
		rho.delta[i] <- rho.norm[i]*delta.norm[i]
	}
	cluster.centers <- c(rep(0, times=n.points))
	for(c in 1:n.clusters) {
		cluster.centers[c] <- which.max(rho.delta)
		rho.delta[cluster.centers[c]] <- 0
	}
	return(assign.to.clusters(data, cluster.centers, dc))
}
