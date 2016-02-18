linear_regression <- function( dependent , num_independent ,  ... )
{
	
	# Get the arguments from the list passed 
	arguments <- c(as.list(environment()), list(...))

	# Parameters used in the linear regression.
	debug <- 0
	
	# The learning parameter
	alpha <- 10
	
	# Threshold to determine the convergence
	threshold <- 1.0e-4

	# To determine whether the convergence is too slow.
	iterations <- 2000

	
	# Getting the independent variables data
	independent <- arguments[3:length(arguments)]

	# Create the thetas to start the algorithm. All the thetas start at 0.
	thetas = numeric(num_independent+1)	

	# Generating the matrix of independent variables 
	independent_matrix <- do.call(cbind, arguments[3:length(arguments)] )

	# The first of the independent matrix is appended with column 1 for the intercept
	independent_matrix <- cbind(1,independent_matrix)

	# This gives the number of rows in the dataset.
	datasize <- length(dependent)

	# The cost function. given the thetas it will calculate the cost
	cost <- function(thetas)
	{
		h_theta <- independent_matrix%*%thetas
		square_errors <- ( h_theta - dependent )^2
		cost_value <- (1/(2*datasize))*sum(square_errors)
		return(cost_value)
	}
	
	
	# variables to determine how the convergence or divergence of the cost function working
	converged <- FALSE
	diverged <- FALSE
	num_iter <- 0

	# The variable max alpha is used to get min alpha for which the regression diverged
	max_alpha <- alpha;
	
	#Find the value of the cost
	cost_function <- cost(thetas) 

	
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
			previous_cost <- cost(thetas)
			
			old_thetas <- thetas
			
			if( debug )
			{
				print(old_thetas)
			}
			
			# Calculating thetas for the next step
			for( i in 1:(num_independent+1) )
			{
				thetas[i] = old_thetas[i] + alpha*( sum( ( dependent - (independent_matrix%*%old_thetas) )*independent_matrix[,i]) )
			}

			# The new cost after changing the thetas
			new_cost <- cost(thetas)

			# If the cost didn't decrease then the learning parameter is decreased by 1/2
			if( new_cost > previous_cost )
			{
				diverged = TRUE
				max_alpha <- alpha
				alpha = alpha / 2
				if( debug )
				{
					print(c("With alpha",alpha," the linear regression diverged. So decreasing the alpha by 1/2"))
				}
				break
			}
		
			# If the cost is within the threshold then the linear regression converged
			if( abs(new_cost-previous_cost) < threshold )
			{
				converged <- TRUE
				if( debug )
				{
					print(c("With alpha",alpha," the linear regression converged"))
				}
				break
			}

			# If the iterations are over 2000 then the convergence is too slow
			if( num_iter > iterations )
			{
				if( debug )
				{
					print(c("With alpha",alpha," the linear regression is converging too slow . So increasing the alpha by alpha/2 "))
				}
				alpha <- alpha + alpha / 2
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

	iter <- cbind(1:length(cost_iter))
	plot(iter,cost_iter)

	# Fitted values by the linear regression
	fitted_values <- independent_matrix%*%thetas

	# The residuals of the linear regression
	residuals <- dependent - fitted_values

	# Find the mean of the observed dependent data 
	mean_observed <- sum(dependent)/datasize

	# Calculate the total sum of squares
	SS_total <- sum((dependent - mean_observed)^2) 

	# Calculate the residual sum of squares 
	SS_res = sum(residuals^2)
	
	SS_reg = sum((fitted_values - mean_observed)^2)

	# The value of R-squared is given  by
	R <- 1 - ( SS_res / SS_total )
	
	print(c("R Squared is : ",R))

}

#linear_regression(mphr,7,hr,bp,pkhr,sbp,age,baseef,gender)

#linear_regression(houseprice$price , 2 , houseprice$size , houseprice$bedroom )
linear_regression(dmf,1,flor)