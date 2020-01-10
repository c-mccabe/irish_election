# functions to calculate prior and posterior densities, carry out MCMC, and generate final transfer matrices.

prior_density = function(alpha, mu, beta){
  # this function gives the density of our prior function for a single row
  # of Dirichlet parameters. the priors are independent gamma distributions
  # with mean equal to 50 times the percentages in the transfer matrix prior.
  
  # inputs: alpha - vector of Dirichlet parameters
  #         mu    - a single row of the prior transfer matrix
  #         beta  - the beta parameter of the gamma distributions. increasing
  #                 this decreases the variance of the gamma priors.
  #
  # output: density - the density of the combined priors
  
  n = length(mu)
  priors = dgamma(alpha, shape = 50*beta*mu, rate = rep(beta, n))
  density = prod(priors)
  
  return(density)
}#prior_density

posterior_density = function(x, alpha, mu, beta){
  # this function gives the density of our posterior distribution
  # up to a constant of proportionality. we will pass this to our
  # Metropolis-Hastings algorithm to perform MCMC.
  #
  # inputs: x     - an n-dimensional vector of observed data (i.e. a 
  #                 single row of Local Election transfers)
  #         alpha - an n-dimensional vector of parameters of the Dirichlet distribution
  #         mu    - an n-dimensional vector, the mean value of the priors of the Dirichlet
  #               - parameters
  #         beta  - a scalar, increase to decrease variance of the gamma priors
  #
  # output: density - the posterior density
  
  # post  =     likelihood       * prior
  density = ddirichlet(x, alpha) * prior_density(alpha, mu, beta)
  
  return(density)
}#posterior_density

perform_MCMC = function(a, n, B, k, x, alpha_0, mu, beta){
  # this function performs the MCMC and generates samples from the posterior distribution.
  # we will use the Metropolis-Hastings algorithm
  #
  # inputs: a       - a vector containing the dimensions of the 'box' which the MCMC will search through
  #         n       - the number of posterior samples to generate
  #         B       - the burn in (number of samples to be discarded)
  #         k       - the thinning factor (only accept every k samples)
  #         x       - the observed data
  #         alpha_0 - inital vector of Dirichlet parameters
  #         mu      - vector of mean values of the Dirichlet parameters passed to the prior
  #         beta    - the rate factor of the gamma distribution
  #
  # outputs: posterior_samples - samples from the posterior distribution of the Dirichlet parameters alpha
  #          acceptance_prob   - the acceptance probability of the MH algorithm
  #          mcmc(t(X))        - an 'mcmc' object for debugging and convergence checks
  
  m = length(alpha_0)
  X = matrix(NA, m, n) # initialise a matrix to store the samples
  X[, 1] = alpha = alpha_0 # set the first column as the initial values of the parameters
  accepted = 0 # number of samples we accept
  
  for (t in 1:(n - 1)){
    # each iteration of this loop samples a value, forms the MH ratio and decides
    # whether or not to accept it.
    
    # sample a new point in parameter space
    alpha_new = alpha + (2*runif(m) - 1)*a
    
    # form the Metropolis-Hastings ratio
    MHR = posterior_density(x = x, alpha = alpha_new, mu = mu, beta = beta)/posterior_density(x = x, alpha = alpha, mu = mu, beta = beta)
    
    if (runif(1) < MHR){
      accepted = accepted + 1
      alpha = alpha_new
    }
    X[, t + 1] = alpha
  }
  acceptance_prob = accepted/length(X[1, ]) # calculate appectance probability
  
  Y                 = X[, B:n] # drop burn in terms
  posterior_samples = t(Y[, seq(1, length(Y[1, ]), k)]) # drop thinned terms
  
  return(list(posterior_samples, acceptance_prob, mcmc(t(X))))
  
}#perform_MCMC

generate_transfer_matrix = function(alpha_matrix){
  # this function takes the values of alpha simulated from the posterior distribution
  # and generates a transfer matrix using the rdirichlet function.
  #
  # inputs: alpha_matrix - a matrix whose rows correspond to the parameters of a dirchlet distribution
  #                        sampled from the posterior distribution
  #         n            - the number of transfer matrices to generate
  #
  # outputs: transfer_matrix - transfer matrix
  
  m = length(alpha_matrix[1, ])
  
  transfer_matrix = matrix(NA, nrow = m, ncol = m)
  for (i in 1:m){
    transfer_matrix[i, ] = rdirichlet(1, alpha_matrix[1, ])
  }
  return(transfer_matrix)
}#generate_transfer_matrices

