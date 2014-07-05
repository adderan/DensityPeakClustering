main <- function() {
	source("dpc.R")
	
	features <- 10
	objects <- 10
	data <- matrix(0, objects, features)
	for(i in 1:features) {
		for(j in 1:objects) {
			data[i,j] <- 1
		}
		if(i %% 2 == 0) {
			data[i,] <- data[i,]*2
		}
	}
	print(data)
	cluster.assignments <- dpc(data, 2, 0.016, 0.020)
	print(cluster.assignments)
}

