
#find the distance between points 1 and 2 in the feature space
distance <- function(pt1, pt2) {
	r <- 0
	dimensions <- length(pt1)
	#print(dimensions)
	#print(length(pt2))
	stopifnot(length(pt1) == length(pt2))

	for(i in 1:dimensions) {
		r <- r + (pt1[i] - pt2[i])*(pt1[i] - pt2[i])
	}
	#cat("distance = ", sqrt(r), "\n")
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
	dcstep <- 0.1

	n.points <- dim(data)[[1]]
	n.low <- neighborRateLow * n.points
	n.high <- neighborRateHigh * n.points
	dc <- 0.0
	avg.neighbors <- 0.0

	while((avg.neighbors < n.low)) {
		cat("dc = ", dc, "\n")
		rho <- local.density(data, dc)
		#print(rho)
		avg.neighbors <- mean(rho)
		if(avg.neighbors < n.low) {
			dc <- dc + dcstep
		}
		if(avg.neighbors > n.high) {
			#dc <- dc - dcstep
		}
	}
	return(dc)
}

#find the minimum distance between each point and another point with higher density
distance.to.higher.density <- function(data, rho) {
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
		if(!has.highest.density) {
			delta[i] <- min.dist.to.higher
		}
		else {
			delta[i] <- distance.to.furthest(data, i)
		}
	}
	return(delta)
}

#find the distance from this point to the furthest data point. This distance is assigned as delta for the highest density point
distance.to.furthest <- function(data, point) {
	n.points <- dim(data)[[1]]
	max.distance <- 0
	pt1 <- data[point,]
	for(i in 1:n.points) {
		if(i == point) next
		pt2 <- data[i,]
		this.distance <- distance(pt1, pt2)
		if(this.distance > max.distance) {
			max.distance <- this.distance
		}
	}
	return(max.distance)
}

#assign each point to one of the cluster centers. The cluster centers are determined by ranking rho*delta
assign.to.clusters <- function(data, cluster.centers, dc) {
	n.points <- dim(data)[[1]]
	cluster.assignments <- c(rep(0, times=n.points))

	for(i in 1:n.points) {
		min.distance.to.cluster <- .Machine$double.xmax
		nearest.cluster <- -1
		for(j in 1:length(cluster.centers)) {
			c <- cluster.centers[j]
			distance.to.this.cluster <- distance(data[i,], data[c,])
			if(distance.to.this.cluster < min.distance.to.cluster) {
				min.distance.to.cluster <- distance.to.this.cluster
				nearest.cluster <- j
			}
		}
		cluster.assignments[i] <- nearest.cluster
	}
	return(cluster.assignments)
}

#cluster the objects. Data matrix should be (objects x features). 
dpc <- function(data, n.clusters, neighborsLow=0.016, neighborsHigh=0.02) {
	n.points <- dim(data)[[1]]

	dc <- find.dc(data, neighborsLow, neighborsHigh)
	rho <- local.density(data, dc)
	delta <- distance.to.higher.density(data, rho)

	rho.norm <- rho / max(rho)
	delta.norm <- delta / max(delta)
	rho.delta <- c(rep(0, times=n.points))
	for(i in 1:n.points) {
		rho.delta[i] <- rho.norm[i]*delta.norm[i]
	}
	cluster.centers <- c(rep(0, times=n.clusters))
	distance.from.a.cluster <- c(rep(1, times=n.points))  #ensures that new cluster centers are far from existing clusters
	#cat("rho.delta :")
	#print(rho.delta)
	for(c in 1:n.clusters) {
		rho.delta.distance <- c(rep(0,times=n.points))
		for(i in 1:n.points) {
			rho.delta.distance[i] <- rho.delta[i]*distance.from.a.cluster[i]
		}
		cluster.centers[c] <- which.max(rho.delta.distance)
		new.cluster <- cluster.centers[c]
		rho.delta[new.cluster] <- 0
		for(i in 1:n.points) {
			distance.to.new.cluster <- distance(data[i,], data[new.cluster,])
			if((distance.from.a.cluster[i] == 1) || (distance.from.a.cluster[i] > distance.to.new.cluster)) {
				distance.from.a.cluster[i] <- distance.to.new.cluster
			}
		}
	}
	#cat("cluster centers: ")
	#print(cluster.centers)

	return(assign.to.clusters(data, cluster.centers, dc))
}
