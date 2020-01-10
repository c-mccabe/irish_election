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

Accurately predicting transfer patterns is the key to successfully modelling an
Irish election.


##### Table of Contents  
[Model](#model)  
[Simulating Elections](#simulate)
[Model](#model)  
[Simulating Elections](#simulate)


<a name="model"/>

## Model

Building my model, I make the assumption that transfer patterns in the 2019 local
elections are likely to be predictive of the 2020 general election. Using the 2019
local election then means there is one data point on which to develop our model. 
In such a low data regime, a Bayesian approach which allows for the encoding of prior
belief seems like a natural choice.

I decide to model each row of the transfer matrix as a draw from a Dirichlet
distribution on whose parameters I place semi-informative priors. While informative
priors can bias a model with so little data available, there are some inescapable
political realities which should be included (e.g. leftwing candidates are more
likely to transfer to other leftwing candidates than rightwing ones). The ability to encode
political 'common sense' makes a Bayesian setting very appealing here.

<a name="mcmc"/>

# MCMC

The Metropolis-Hastings algorithm is used to draw samples from the posterior distribution
of the Dirichlet parameters. This is contained in the MCMC.R file in the repository. 
These posterior samples can then be used to simulate elections results.

<a name="simulate"/>

## Simulating Elections

simulate_election.R contains functions used to simulate an election in a single constituency
given constituency level poll results and a sampled transfer matrix.

<a name="web"/>

## Web Scraping

Unfortunately the Irish government does not provide election transfer results but the data has been
compiled by a website called irelandelection.com. While we cannot be 100% sure about data quality,
it is the only resource currently available. I used the *rvest* R package to scrape transfer
data from the site, the code for which is contained in the file web_scraping.R.

<a name="test"/>

## Test Case - Dublin Rathdown 2016

I tested the model on a single constituency of the 2016 general election using data drawn from the 2014
local elections. The model correctly predicted the first seat in 100% of simulations, the second seat in 90.75%
of simulations, and the third seat in 31.43% of simulations (which I later discovered was one of the major upsets of the
2016 election and hence probably not the best test case, oops). It's possible that better conceived priors
might have more accurately captured the final results of the election, but at the very least it shows that
the code is working. test_dublin_rathdown.R contains the code.

<a name="next"/>

## Next Steps 11/1/2020

1. Scrape data for all local and general election contituencies and write a scrpit to automate the
   data cleaning process.
2. Set priors for *all* candidates running in the election and on their likelihood of transferring to
   the other candidates in their constituencies. Since we have so little data, accurate priors are very
   important. The number of parameters *per constituency* is n^2 where n is the number of candidates, so
   this will take a lot of time to get right.
3. Implement some sort of uniform swing model that can take a national poll and output constituency level
   predictions.
4. Constituency by constituency, sample from the posteriors using MCMC, feed the results to the script
   which simulates election results, and tally the results.
   
Unfortunately it seems likely that the Irish election is only a few weeks away - hopefully I'll be able to finish
this in time!
