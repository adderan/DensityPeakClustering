main <- function() {
	source("dpc.R")
	
	features <- 10
	objects <- 10
	data <- matrix(0, objects, features)
	for(i in 1:features) {
		for(j in 1:objects) {
			if(i > 5) {
				data[i,j] <- data[i,j] + 2
			}
			if(j > 3) {
				data[i,j] <- data[i,j] + 4
			}
		}
	}
	cluster.assignments <- dpc(data, 10, 0.016, 0.020)
	print(cluster.assignments)
}

