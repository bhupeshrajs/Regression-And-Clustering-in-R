kmeans_clustering <- function(data,centers,header = F)
{

	colname <- names(data)

	if( header == T )
	{
		data <- as.matrix(data)
	}
		
	# represents the number of data to be clustered
	n = nrow(data);
	
	# Gives the random assignment for each data their cluster center index
	data_assignment <- sample(1:centers , n , replace = T ) 
	
	# Gives the random cluster centers points for starting from the data given
	random_centers <- sample(1:n , centers , replace = F ) 
	
	# Gets the random cluster data center points
	cluster_centers <- data[random_centers,]
	
	# Distance function used to calculate the eucledian distance between the data points x and y
	distance <- function(x,y)
	{
		d <- sum(( x - y )^2)
		return(d)
	}
	
	# Updates the cluster centers based on the new assignment of points to the clusters
	update_cluster <- function(data_assignment)
	{
		for( i in 1:centers )
		{
			data_assigned <- which( data_assignment == i )
			data_points <- data[data_assigned,]
			num_data_points <- nrow(data_points)
			cluster_centers[i,] <- sum(data_points)/num_data_points
		}
	}
	
	changed <- FALSE 
	
	num_iter <- 0
	
	while( !changed )
	{
	
		# Increasing the number of iterations by 1 
		num_iter <- num_iter + 1
	
		# Creating a matrix for the distance matrix
		distance_matrix <- matrix( nrow = centers , ncol = n )
		
		# Old data assignment 
		old_assignment <- data_assignment
		
		# For each cluster center calculate the distance to all the data points 
		for( i in 1:centers )
		{
			for( j in 1 : n )
			{
				distance_matrix[i,j] <- distance(cluster_centers[i,],data[j,])
			}
		}
		
		# Update the data assignment to the cluster for each point based on the minimum distance to the cluster center 
		for( j in 1:n )
		{
			data_assignment[j] <- which.min( distance_matrix[,j] )
		}
		
		# Check if the all the data's cluster centers didn't change. If not then the kmeans ended with these cluster centers
		if( all( old_assignment == data_assignment ) )
		{
			changed <- TRUE
			break
		}
		
		# If the data assignment changed recalculate the cluster centers
		update_cluster(data_assignment)

	}

	# Printing the cluster centers
	print("The cluster centers are : ")
	print(cluster_centers)

	# Printing the number of iterations took to converge
	print("The number of iterations to converge : ")
	print(num_iter)
	
	# Returning the cluster_centers and the assignment of points to the clusters
	return(list(cluster_centers,data_assignment))
}