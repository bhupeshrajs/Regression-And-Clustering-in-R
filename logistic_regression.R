logistic_regression <- function( dependent , num_independent ,  ... )
{
	# Get the arguments from the list passed 
	arguments <- c(as.list(environment()), list(...))

	# The parameters of the logistic regression are :

	# To start the debug flag
	debug <- 0

	# The learning parameter
	alpha <- 1.0e-3
	
	# Threshold to determine the convergence
	threshold <- 1.0e-10

	# To get the total number of iterations for convergence
	iterations <- 2000
	
	# print the arguments 
    	# print(arguments)

	# Getting the independent variables data
	independent <- arguments[3:length(arguments)]

	# Create the thetas to start the algorithm. All the thetas start at 0.
	thetas = numeric(num_independent+1)	

	# Generating the matrix of independent variables 
	independent_matrix <- do.call(cbind, arguments[3:length(arguments)] )

	# The first of the independent matrix is appended with column 1 for the intercept
	independent_matrix <- cbind(1,independent_matrix)

	# This gives the number of rows in the dataset.
	datasize = length(independent)

	# The sigmoid function in the logistic regression.
	sigmoid <- function(z)
	{
		sig <- 1/(1+exp(-z))
		return(sig)
	}
	
	# The cost function in which here we are maximizing the log of the objective function
	cost <- function(thetas)
	{
		h_x <- sigmoid(independent_matrix%*%thetas)
		first <- dependent*log(h_x)
		second <- ( 1 - dependent )*log(1 - h_x)
		cost_value <- (1/datasize)*sum( -dependent*log(h_x) - ( ( 1 - dependent )*log(1 - h_x) ) )
		return(cost_value)
	}
	
	cost_function = cost(thetas) 

	print(cost_function)
	
	# variables to determine how the convergence or divergence of the cost function working
	converged <- FALSE
	diverged <- FALSE
	num_iter <- 0

	# The variable max alpha is used to get min alpha for which the regression diverged
	max_alpha <- alpha;
	
	# This variable is used to get all the cost functions over the iterations
	cost_iter <- cbind(cost_function)

	while( !converged )
	{
		diverged <- FALSE
		thetas <- numeric(num_independent+1)
		num_iter <- 0
		cost_iter <- cbind(cost_function)


		while(!diverged)
		{
			
			# Increase the number of iteration by 1
			num_iter <- num_iter + 1

			# The cost function for the given thetas
			previous_cost = cost(thetas)

			old_thetas = thetas

			# Calculating thetas for the next step
			
			for( i in 1:(num_independent+1) )
			{
				thetas[i] = old_thetas[i] + alpha*( sum(( dependent - sigmoid(independent_matrix%*%old_thetas) )*independent_matrix[,i]) )
			}

			# The new cost after changing the thetas
			new_cost = cost(thetas)
			
			# In case any of the thetas are na then we know that the logistic regression diverged
			if( all(is.na(new_cost)) )
			{
				diverged = TRUE
				max_alpha <- alpha
				alpha = alpha / 2
				if( debug )
				{
					print(c("With alpha",alpha," the logistic regression diverged. The thetas are so big. So the cost function is undefined. So reducing the Alpha by 1/2"))
				}
				break
			}
			
			# If the cost didn't decrease then the learning parameter is decreased by 1/2
			
			if( new_cost > previous_cost )
			{
				diverged = TRUE
				max_alpha <- alpha
				alpha = alpha / 2
				if( debug )
				{
					print(c("With alpha",alpha," the logistic regression diverged. So reducing the Alpha by 1/2"))
				}
				break
			}
		
			# If the cost is within the threshold then the linear regression converged
			if( abs(new_cost-previous_cost) < threshold )
			{
				converged <- TRUE
				if( debug )
				{
					print(c("With alpha",alpha," the logistic regression converged"))
				}
				break
			}

			# If the iterations are over 2000 then the convergence is too slow
			if( num_iter > iterations )
			{
				alpha <- alpha + alpha / 2
				if( debug )
				{
					print(c("With alpha",alpha," the linear regression is converging too slow. So increasing the alpha by alpha/2 "))
				}
				if( alpha >= max_alpha )
				{
					if( debug )
					{
						print(c("After increasing the alpha to ",max_alpha," we know the linear regression already diverged "))
						print(c("So stopping the process right now"))
					}
					converged = TRUE
				}
				break
			} 
			
			cost_iter <- cbind(cost_iter,new_cost)

		}
	}

	print("The thetas are : ")
	print(c("Intercept - Theta 0 : ",thetas[1]))
	for( i in 2:(num_independent+1) )
	{
		print(c("theta",i-1,thetas[i]))
	}
	
	return(thetas)

}