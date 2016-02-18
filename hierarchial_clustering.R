hierarchial_clustering <- function(data,header = F , linkage = "SINGLE")
{

	# Get the column names 
	colname <- names(data)
	
	# If the header is present then remove the header.
	if( header == T )
	{
		data <- as.matrix(data)
	}
	
	# Initially every data point is the cluste. So Getting the total number of clusters
	num_clusters <- nrow(data)

	# Initializing the clusters
	clusters <- NULL 
	for( i in 1:num_clusters )
	{
		clusters <- c( clusters , list(rbind(data[i,])))
	}

	print(clusters)
	
	# Distance function used to get the eucledian distance between the data points x and y.
	distance <- function(x,y)
	{
		d <- sum(( x - y )^2)
		return(d)
	}
	
	
	while( TRUE )
	{
		# Till the number of clusters become one. Repeat the process 
		if( num_clusters == 1 )
		{
			break
		}
		
		
		# Creates the matrix to calculate the distance between the clusters
		cluster_distance_matrix = matrix( nrow = num_clusters , ncol = num_clusters )
	
		for( i in 1:num_clusters )
		{
			for( j in 1:num_clusters )
			{
				# If the clusters are same no need to find the distance between them
				if( i == j )
				{
					next
				}
				
				# We get the ith cluster and jth cluster size 
				original_cluster_size <- nrow(clusters[[i]])
				current_cluster_size <- nrow(clusters[[j]])

				# WE create a distance matrix to find the distance between each of the cluster points 
				distance_matrix <- matrix( nrow = original_cluster_size , ncol = current_cluster_size )
				for( k in 1:original_cluster_size )
				{
					for( l in 1:current_cluster_size )
					{
						distance_matrix[k,l] <- distance(clusters[[i]][k,],clusters[[j]][l,])
					}
				}
			
				# Depending upon the linkage type we select the appropriate distance between the clusters

				# This is single linkage. This will use the minimum distance between the clusters 
				
				if( linkage == "SINGLE" )
				{
					cluster_distance_matrix[i,j] = min(distance_matrix)
				}
				
				# This is complete linkage. This will use the maximum distance between the clusters 
				if( linkage == "COMPLETE" )
				{
					cluster_distance_matrix[i,j] = max(distance_matrix)
				}
				# This is called average linkage clustering. This will use distance in between single and complete linkage 
				if( linkage == "AVERAGE" )
				{
					cluster_distance_matrix[i,j] = sum(distance_matrix)/(original_cluster_size*current_cluster_size)
				}
				
			}
		}
		
		minimum_distance <- which( cluster_distance_matrix == min(cluster_distance_matrix, na.rm = TRUE ) , arr.ind = TRUE )
	
		
		print(cat("Merging two clusters " ,minimum_distance[1], " " , minimum_distance[2] , " into single cluster " , minimum_distance[1] , " distance : " , min(cluster_distance_matrix , na.rm = TRUE) , " -- " ) )
		
		clusters[[minimum_distance[1]]] <- rbind( clusters[[minimum_distance[1]]] , clusters[[minimum_distance[2]]] )
		
		clusters[[minimum_distance[2]]] <- NULL
		
		num_clusters <- num_clusters - 1
	}
}

hierarchial_clustering(food[,-1],header=T,linkage="COMPLETE")