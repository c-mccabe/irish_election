# Predicting the 2020 Irish General Election

Unlike the First Past the Post electoral system used in the US or UK, Ireland
uses the Single Transferable Vote system where candidates are ranked and excess
votes are redistributed in several counts. Predicting election results is then a
much more difficult problem.

For each constituency we can consider a "transfer matrix" where the (i, j) entry
corresponds to the proportion of candidate i's votes which are transferred to
candidate j. While this information could in theory be collected by pollsters, in
practice only first preference votes are polled and so there is little transfer
data available.

Building my model, I make the assumption that transfer patterns in the 2019 local
elections are likely to be predictive of the 2020 general election. We then have one
data point on which to develop our model. In such a low data regime, a Bayesian
approach which allows for the encoding of prior belief seems like a natural choice.

## Model

I decide to model each row of the transfer matrix as a draw from a Dirichlet
distribution on whose parameters I place semi-informative priors. While informative
priors can bias a model with so little data available, there are some inescapable
political realities which should be included (e.g. leftwing candidates are more
likely to transfer to other leftwing candidates than rightwing ones).

I will then perform MCMC and generate posterior distributions for the parameters
of the Dirichlet distribution. Using this posterior I can then sample values and use
these to simulate different election outcomes.

## Simulating Elections

I have written a function which simulates a Single Transferable Vote election given the
trasfer matrix of candidates running. This is currently uploaded to the repository.

## Next Steps 7/1/2020

1. Determine the full list of candidates running in the 2020 general election.
2. Set priors on the likelihood of each candidate transferring to each other
   candidate in each constituency.
3. Scrape Irish political websites for the transfer data of the 2019 local election
   and build the observed transfer matrix for each constituency.
4. Perform MCMC and generate a posterior for the Dirichlet parameters.
5. Sample 10,000 values from the posterior and use them to simulate 10,000 transfer matrices.
6. Feed each of these 10,000 transfer matrices to the function simulating an election
   and observe the number of times each candidate wins a seat to determine the final
   probability of a candidate being elected.



