# as a test case we are using the 2014 local elections to predict the 2016 general
# and will be using the GE constituency Dublin Rathdown, which contains three local
# constituencies - Blackrock, Sandyford, and Dundrum

# download data

LE_alpha_1 = scrape_transfer_matrix(elecid = 157, constitid = 147, electype = 5)
LE_alpha_2 = scrape_transfer_matrix(elecid = 157, constitid = 149, electype = 5)
LE_alpha_3 = scrape_transfer_matrix(elecid = 157, constitid = 151, electype = 5)

# data cleaning section. the code is a mess because I haven't yet gotten around to
# autmoating the cleaning process.

LE_alpha_3 = cbind(LE_alpha_3, rep(0, 3)) # add missing column

# replace NA with average proportion
LE_alpha_1[is.na(LE_alpha_1)] <- 1/7
LE_alpha_2[is.na(LE_alpha_2)] <- 1/7
LE_alpha_3[is.na(LE_alpha_3)] <- 1/7

# add missing rows
LE_alpha_1 = rbind(LE_alpha_1, rep(1/7, 7))
LE_alpha_1 = rbind(LE_alpha_1, rep(1/7, 7))

LE_alpha_2 = rbind(LE_alpha_2, rep(1/7, 7))

LE_alpha_3 = rbind(LE_alpha_3, rep(1/7, 7))
LE_alpha_3 = rbind(LE_alpha_3, rep(1/7, 7))
LE_alpha_3 = rbind(LE_alpha_3, rep(1/7, 7))
LE_alpha_3 = rbind(LE_alpha_3, rep(1/7, 7))

# interchange rows

inter_1 = c(1, 2, 3, 6, 7, 4, 5)
inter_2 = c(1, 2, 3, 4, 5, 7, 6)
inter_3 = c(4, 5, 1, 2, 7, 3, 6)

LE_alpha_1 = LE_alpha_1[inter_1,]
LE_alpha_2 = LE_alpha_2[inter_2,]
LE_alpha_3 = LE_alpha_3[inter_3,]

LE_alpha = (LE_alpha_1 + LE_alpha_2 + LE_alpha_3)/3

# NB need to merge Ind and IA columns and renormalise

###############################################

# now check that the rows sum to one and normalise if they don't

rowSums(LE_alpha)

if (prod(rowSums(LE_alpha) != 1)){
  # if rowSums != 1 then normalise
  for(j in 1:length(LE_alpha[1, ])){
    # noramlise the rows
    LE_alpha[j, ] = LE_alpha[j, ]/sum(LE_alpha[j, ])
  }
}

# set the priors

alpha_prior = matrix(c(
  60, 3, 15, 5, 10, 5, 2, # Labour
  3, 60, 10, 7, 2, 15, 3, # FF
  10, 8, 60, 2, 5, 10, 5, # FG
  3, 6, 2, 70, 3, 15, 2,  # SF
  15, 4, 5, 4, 60, 10, 2, # Greens
  2, 10, 3, 10, 3, 70, 2, # Ind
  2, 15, 15, 2, 1, 5, 60  # Renua
)/100 , nrow = 7, byrow = TRUE)


# perform MCMC on the first row of the prior matrix

alpha_0 = alpha_prior[1,]*50
a = 0.2*(alpha_prior[1,]*50)
mu = alpha_prior[1,] # set the mean of the prior as the initial points for the mcmc
n = 15000
B = 1000
k = 1 # no thinning
x = LE_alpha[1, ]
beta = 10

MCMC_output = perform_MCMC(a, n, B, k, x, alpha_0, mu, beta)

# MCMC convergence checks

MCMC_output[[2]]
effectiveSize(MCMC_output[[3]])

normalised_output = MCMC_output[[1]]/50
summary(normalised_output)

list_of_outputs = vector('list', 7) # initialise list

for (i in 1:7){
  # loop over the rows of the prior matrix and perform mcmc
  
  alpha_0 = alpha_prior[i,]*50
  a = 0.2*(alpha_prior[i,]*50)
  mu = alpha_prior[i,] # set the mean of the prior as the initial points for the mcmc
  n = 100000
  B = 5000
  k = 1 # no thinning
  x = LE_alpha[i, ]
  beta = 10
  
  MCMC_output = perform_MCMC(a, n, B, k, x, alpha_0, mu, beta)
  list_of_outputs[[i]] = MCMC_output[[1]]
}

alpha_posterior = transfer_matrix = matrix(NA, nrow = 7, ncol = 7)

for (i in 1:7){
  alpha_posterior[i, ] = colMeans(list_of_outputs[[i]])
  transfer_matrix[i, ] = rdirichlet(1, alpha_posterior[i, ])
}

# now simulate the Dublin Rathdown election

seats   = 3
turnout = 41099
candidate_party = c(1,    # Labour
                    2,    # FF
                    3, 3, # FG
                    4,    # SF
                    5,    # Greens
                    6, 6, # Independents
                    7)    # Renua

party_proportion = c(1, 1,                #Labour/FF
                     c(16.2, 14.4)/sum(c(16.2, 14.4)),  # FG
                     1, 1,                # SF/Greens
                     c(24.8, 4.9)/sum(c(24.8, 4.9)),  # Ind
                     1)                   # Renua

first_preferences = c(4048, 4220, 6668, 5905, 2858, 4122, 10202, 2021, 1055)
turnout = sum(first_preferences)
candidates = data.frame('party' = candidate_party, 'first_preferences' = first_preferences, 'party_proportion' = party_proportion)

simulate_election(alpha = transfer_matrix, candidates = candidates, seats = seats, turnout = turnout)

election_results = matrix(0, nrow = 10000, ncol = 3)

for (j in 1:10000){
  for (i in 1:7){
    alpha_posterior[i, ] = list_of_outputs[[1]][sample(nrow(list_of_outputs[[1]]),size = 1,replace = TRUE),i]
    transfer_matrix[i, ] = rdirichlet(1, alpha_posterior[i, ])
  }
  election_results[j, ] = simulate_election(alpha = transfer_matrix, candidates = candidates, seats = seats, turnout = turnout)
}

election_results[is.na(election_results)] = 0
hist(election_results)

sum(election_results == 7)/10000 # correctly predicted the first seat 100% of the time
sum(election_results == 6)/10000 # correctly predicted the second seat 90.75% of the time
sum(election_results == 3)/10000 # correctly predicted the third seat 31.43% of the time



