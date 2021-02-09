# DSC 381 – Probability and Simluation Based Inference

## Homework 1, Problem 11
In this problem, we simulate the following:
1. A population of 10,000 (the problem shows 10,000 and 1,000 and I chose  bigger) who have a disease.
2. We then simluate a test on the population to determine the presence of the disease, with an accuracy of 99.9% (0.999) and a false positive rate of 0.5% (0.005)
3. Next, we compare the test result to the known presence of the disease
4. Finally, we plot the outcome as the disease presence in the population ranges from 0.01 to 0.50

**Interpretation:** As the presence of the disease in the population increases, the probability of the test returning a true positive increases exponentially, seemingly approaching the expected accuracy rate of 0.999 (99.9%).

## Homework 2, Problem 10
We are presented with an initial challenge of representing the table in R. This is done by:
```R
nxy <- array( c( c(10,10,17,22), c(9,7,11,18), c(2,2,5,7) ), c(4,3) )
```

In the `Ih()` function, we do the following:

First, sum all of the (X<sub>i</sub>, Y<sub>j</sub>) entries, then divide each observation by the sum to get a measure of relative frequency.

Next, we create an `fx` vector where each entry represents something like:
Pr(X<sub>1</sub>,Y<sub>1</sub>) ∪ Pr(X<sub>1</sub>,Y<sub>2</sub>) ∪ (X<sub>1</sub>,Y<sub>3</sub>)

And similarly for `fy`:
Pr(X<sub>1</sub>,Y<sub>1</sub>) ∪ Pr(X<sub>2</sub>,Y<sub>1</sub>) ∪ (X<sub>3</sub>,Y<sub>1</sub>) ∪ (X<sub>4</sub>,Y<sub>1</sub>)

For both of the aforementioned vectors, instances of `0` are replaced with `0.01`.

We then transpose `fy` to a 1 by 3 matrix, `fyT`, and multiply `fx • fyT`, then use the Kullback-Leibler approximation to estimate the Mutual Information ("MI").

In the `sim()` function, we use the relative frequencies from `Ih()` to create `M` number simulated tables. Take note of the `for` loop, how specifically how the sampling to table occurs. Pretty cool trick!

Finally, in `Ihmstar()`, we take the mass of simulated tables from `sim()` and determine the 95% threshold that will help us determine if `Î` is "too far" off to consider a KL divergence of `0`.

In each of the questions, we are simply feeding in the data from the table to generate results. 10c seems to be the exception where we need to figure out the proportion of simulated tables that produce a result where `Pr(Î > Î*)`is greater than the 95% threshold.