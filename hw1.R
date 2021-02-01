disease_test <- function(prior = 0.02, target_population_size = 10000){
  # If 0, the person does not have the disease.
  # If 1, they do have the disease.
  has_disease <- c(0,1)
  
  # In this example, the prior is the observation
  # that 2% of the population has the disease.
  # If 2% has the disease, 98% does not have the
  # disease.
  prior_complement = 1 - prior
  
  # Generate the experiment population.
  # 1. This creates a numeric vector of size target_population_size
  # 2. The elements will have a value of either 0 or 1
  # 3. The vector elements will APPROXIMATE the 2% occurrence. This will vary, as I saw anywhere from 1.6% to 2.1%
  B <- sample(
    has_disease, # A vector of values to sample from. If replace means that we can get a value from the vector, then put it back for future experiments.
    target_population_size, # How large of a simulated population  we want to create
    prob = c(prior_complement, prior), # See walker_ProbSampleReplace in https://github.com/wch/r-source/blob/trunk/src/main/random.c#L346
    replace = TRUE # Setting replace to TRUE ensures we can sample the elements of has_disease multiple times.
  )
  
  # The true positive rate.
  true_positive <- 0.999
  
  # The true negative rate.
  true_negative <- 1 - 0.005
  
  # Vector of size target_population_size with random values 0 ≤ n ≤ 1
  u <- runif(target_population_size)
  
  # Create a vector where each element is calculated by the following:
  # 1. If the value in B is 1
  #    - If the corresponding value in u is less than true_positive, the value is true
  #      that means the person has the disease and the test confirmed it.
  #    - If the corresponding value in u is greater than true_positive,
  #      then the value is false which means a false negative.
  # 2. If the value in B is 0
  #    - If the corresponding value in u is greater than the false negative rate (1 - true_negative),
  #      then the patient is negative and had a true negative.
  #    - If the corresponding value in u is less than the false negative rate,
  #      then the person had a false positive.
  A <- ifelse(B == 1, u < true_positive, u < 1 - true_negative)
  
  # 1. Compares the value of A with the corresponding value in B (they should match!)
  # 2. Sums the values of the resultant vector from step 1 (true = 1, false = 0),
  #    which gives us the total number of positives in the population that tested positive. 
  test_result_given_presence <- sum( A & B )
  
  # Tells us how many people in the population actually have the disease.
  presence <- sum(A)
  
  # Pr( B | A ) or the probability we identify the disease via testing
  # given that we already know whether someone has the disease.
  probability_positive_given_presence = test_result_given_presence / presence
  
  return(probability_positive_given_presence)
}

# Create a vector that goes from 1% to 50% by approximately 0.5%
pgrid <- seq(from = 0.01, to = 0.50, length = 100)

# A vector that will be filled with results of disease_test() (i.e. Pr(B|A)) for values in pgrid.
positives_by_disease_presence <- rep(0,100)

# Loop over integers 1 to 100 and test the corresponding value from pgrid
for (i in 1:100) {
  positives_by_disease_presence[i] <- disease_test( pgrid[i] )
}

# Plot the results of our test.
plot(pgrid, positives_by_disease_presence, type="l", bty="l", xlab="Pr(A)", ylab="Pr(A | B)")

# Add the line to the plot
abline(h = 0.999, lty = 2, col = 2)

