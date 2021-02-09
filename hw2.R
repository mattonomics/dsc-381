# Express the table from the problem as an array so we can pass it to `Ih`
nxy <- array( c( c(10,10,17,22), c(9,7,11,18), c(2,2,5,7) ), c(4,3) )

# A function that returns the mutual information (MI) of a matrix of observations
# of events (Xi, Yi) using an approximation of Kullback-Liebler divergence.
#
# Accepts a matrix (nxy) of observational data for events (Xi, Yi) of size f = nx x ny
# Returns a double representing the mutual information (MI) I(X,Y)
Ih <- function(nxy) {
  # The total number of observations
  n <- sum(nxy)
  
  # The relative frequencies 
  f <- nxy / n
  
  # Create a 4x1 vector with the sums of the ROWS (1) of f
  fx <- apply(f, 1, sum)
  
  # Create a 3x1 vector with the sums of the COLUMNS (2) of f
  fy <- apply(f, 2, sum)
  
  # Check the fx vector for 0 values and replace them with 0.01
  fx[fx == 0] <- 0.01
  
  # Check the fy vector for 0 values and replace them with 0.01
  fy[fy == 0] <- 0.01
  
  # Check the f vector for 0 values and replace them with 0.01 
  f[f == 0] <- 0.01
  
  # Transpose the fy vector to a 1x3 matrix
  fyT <- t(fy)
  
  # Multiply the matrices fx (4x1) * fyT (1x3) to get fxfy a 4x3 matrix
  fxfy <- fx %*% fyT
  
  # Kullback-Leibler approximation to estimate MI (mutual information)
  I <- sum( f * log( f / fxfy ) )
  
  # Finally, return the MI estimate
  return(I)
}

# fxh: marginal prob. of x, similar to what was done in Ih()
# fyh: marginal prob. of y, similar to what was done in Ih()
# n: sum of the (Xi, Yi) observations
# M: number of simulations to run
sim <- function(fxh, fyh, n = 120, M = 1000) {
  # The total number of observations in fxh
  nx <- length(fxh)
  
  # The total number of observations in fyh
  ny <- length(fyh)
  
  # Initialize a placeholder vector of size M (i.e. the number of simulations)
  Ihm <- rep(0, M)
  
  # Loop over Ihm (which is how we "start" the sims)
  for (m in 1:M) {
    # Create a vector of length n, selecting from values 1:nx (with replacement) and with a marginal prob. of fxh
    xm <- sample(1:nx, n, prob = fxh, replace = T)
    
    # Create a vector of length n, selecting from values 1:ny (with replacement) and with a marginal prob. of fxh
    ym <- sample(1:ny, n, prob = fyh, replace = T)
    
    # Magic. Creates 4x3 matrix whose sum() is 120 and abides by the marginal prob. calculated above
    nxym <- table(xm, ym)

    # Overwrites the m-th entry in Ihm with the MI estimate
    Ihm[m] <- Ih(nxym)
  }
  
  # return Ihm which is M number of simulated MI estimates
  return(Ihm)
}

# Ihm: List of simulated MI estimates from Ihm()
Ihmstar <- function(Ihm) {
  # The number of entries in the Ihm vector
  M <- length(Ihm)
  
  # Determine where the 95th percentile line is (e.g. if num of observations is 1000 and we sorted asc, the 95th percentile would be at entry 950)
  istar <- round(0.95 * M)
  
  # Sort the MI estimates and determine the 95th percentile threshold
  Is <- sort(Ihm)[istar]

  # output: 95th percentile threshold Ih*
  return(Is)
}

showAnswers <- readline('Confirm that you want to display the answers. ("y" or "n") ' )

print('--------:[ HW2, Q10 ]:--------')

#---------- 10a ----------#
Î <- Ih(nxy)

print(paste('10a) The estimated Î is:', Î))


#---------- 10b ----------#
# First, find the marginal prob of x and y
Xmargin <- apply( nxy / sum(nxy), 1, sum )
Ymargin <- apply( nxy / sum(nxy), 2, sum )

# Second, find the Ihmstar i.e. the 95th percentile threshold for I > 0
simulations <- sim( Xmargin, Ymargin )
hamster <- Ihmstar( simulations )

print(paste('10b) The Î* is', hamster))

#---------- 10c ----------#
# We are looking for the number of simulations whose Î > Î*, divided by total number of simulations
PrDivergent <- sum( simulations >= hamster ) / length( simulations )

print(paste('10c) Pr(Î > Î*) is', PrDivergent))

#---------- 10d ----------#
print(paste('10d) Since the Pr(Î > Î*) = ', PrDivergent, 'which is less than the 95% threshold of', hamster, ', we can say X ⊥ Y.'))
