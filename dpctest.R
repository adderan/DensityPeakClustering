main <- function() {
	source("dpc.R")
	
	features <- 2
	objects <- 100
	data <- matrix(0, objects, features)
	for(i in 1:objects) {
		for(j in 1:features) {
			if(i %% 3 == 0) {
				data[i,j] <- rnorm(1, -3, 1)
			}
			if(i %% 3 == 1) {
				data[i,j] <- rnorm(1, 0, 1)
			}
			if(i %% 3 == 2) {
				data[i,j] <- rnorm(1, 3, 1)
			}
		}
	}
	#print(data)
	cluster.assignments <- dpc(data, 3, 0.016, 0.020)
	print(cluster.assignments)
	colors <- c(rep("", times=objects))
	for(i in 1:objects) {
		if(cluster.assignments[i] == 1) {
			colors[i] <- "green"
		}
		if(cluster.assignments[i] == 2) {
			colors[i] <- "blue"
		}
		if(cluster.assignments[i] == 3) {
			colors[i] <- "red"
		}
	}
	plot(data[,1], data[,2], col=colors)

}

