eliminate_party = function(alpha, party){
  # this function eliminates a party from the transfer matrix and renormalises
  # the rows
  #
  # inputs: alpha       - the matrix of transfer proportions
  #         party       - the party (an integer) to be deleted from the matrix
  #
  # outputs: new_alpha  - the updated transfer_matrix

  alpha[, party] = 0

  new_alpha = alpha/rowSums(alpha) # renormalise
  return(new_alpha)
} 

redistribute_votes = function(quota, alpha, candidates){
  # function which redistributes a candidate's excess votes after they have 
  # been elected.
  #
  # inputs: quota      - the quota required for election
  #         alpha      - matrix of transfer proportions
  #         candidates - data frame with the first column containing candidate party,
  #                      the second column containing the first preferences achieved,
  #                      and the third column containing the proportion of their party's
  #                      transfers they are alotted.
  
  
  # first find the leader and determine the number of surplus votes
  leader  = which.max(candidates$first_preferences)
  surplus = candidates$first_preferences[leader] - quota
  
  if (surplus < 0){stop('No candidate has reached the required quota.')}
  
  # which party is the leader a member of
  leading_party = candidates$party[leader]
  
  # delete the leading candidate from the data frame and renormalise their
  # party colleagues' weightings
  
  new_candidates = candidates[-leader,]
  new_party_prop = new_candidates[new_candidates$party == leading_party,]$party_proportion
  
  new_candidates[new_candidates$party == leading_party,]$party_proportion = new_party_prop/sum(new_party_prop) # normalise
  
  # check if any parties have been fully eliminated
  remaining_parties = unique(new_candidates$party)
  
  if (!(leading_party %in% remaining_parties)){
    # if the leading party is no longer in the remaining parties eliminate it
    alpha = eliminate_party(alpha, leading_party)
  }
  
  # determine to which parties the votes are transfered
  party_transfers = surplus * alpha[leading_party, ]
  
  # now redistribute the surplus to the individual candidates
  
  for (i in 1:(length(new_candidates[, 1]))){
    # here we loop over the candidates and redistribute the votes according to their party's
    # allocation and the proportion of their party's votes they are entitled to
    party = new_candidates[i, ]$party
    allocation = round(party_transfers[party]*new_candidates[i, ]$party_proportion)
    
    new_candidates[i, ]$first_preferences = new_candidates[i, ]$first_preferences + allocation
  }
  # return the updated data frame with the leading candidate removed and their
  # votes having been redistributed.
  return(new_candidates)
}#redistribute_votes

eliminate_lowest = function(alpha, candidates){
  # function which redistributes the votes of the candidate with the
  # lowest number of votes
  #
  # inputs: alpha      - matrix of transfer proportions
  #         candidates - data frame with the first column containing candidate party,
  #                      the second column containing the first preferences achieved,
  #                      and the third column containing the proportion of their party's
  #                      transfers they are alotted.
  
  # first find the lowest ranked candidate and their party
  loser  = which.min(candidates$first_preferences)
  losing_party = candidates$party[loser]
  
  # delete the losing candidate from the data frame and renormalise their
  # party colleagues' weightings
  
  new_candidates = candidates[-loser,]
  new_party_prop = new_candidates[new_candidates$party == losing_party,]$party_proportion
  
  new_candidates[new_candidates$party == losing_party,]$party_proportion = new_party_prop/sum(new_party_prop)
  
  # check if any parties have been fully eliminated
  remaining_parties = unique(new_candidates$party)
  
  if (!(losing_party %in% remaining_parties)){
    # if the losing party is no longer in the remaining parties eliminate it
    alpha = eliminate_party(alpha, losing_party)
  }
  
  # determine to which parties the votes are transfered
  party_transfers = candidates$first_preferences[loser] * alpha[losing_party, ]
  
  # now redistribute the surplus to the individual candidates
  
  for (i in 1:(length(new_candidates[, 1]))){
    # here we loop over the candidates and redistribute the votes according to their party's
    # allocation and the proportion of their party's votes they are entitled to
    
    party = new_candidates[i, ]$party
    allocation = round(party_transfers[party]*new_candidates[i, ]$party_proportion)
    
    new_candidates[i, ]$first_preferences = new_candidates[i, ]$first_preferences + allocation
  }
  # return the updated data frame with the leading candidate removed and their
  # votes having been redistributed.
  return(new_candidates)
  
}#eliminate_lowest



simulate_election = function(alpha, candidates, seats, turnout){
  
  # function which simulates a single election count of an Irish general
  # election consituency.
  
  # inputs: alpha            - a matrix of transfer probabilities where the (i, j) entry
  #                            corresponds to the proportion of party i's votes transferring to 
  #                            party j.
  #         candidates       - a dataframe with three columns named 'party', 'first_preferences' and 'party_proportion'
  #                            with each row corresponding to an election candidate
  #         seats            - an integer corresponding to the number of seats available
  #         turnout          - an integer corresponding to the number of votes cast
  #
  # outputs: the seats filled by the various parties running for election
  
  # calculate the quota required to become elected
  quota = ceiling(turnout/(seats + 1))
  
  leader = which.max(candidates$first_preferences)
  elected = c()
  i = 0
  num_candidates = length(candidates[, 1])
  seats_to_fill = seats - length(elected) # seats left to fill
  
  while ( (seats_to_fill > 0) & (num_candidates > seats_to_fill) ){
    i = i + 1
    
    if ((num_candidates == 2) & (seats_to_fill == 1) & (candidates$first_preferences[1] == candidates$first_preferences[2])){
      # this clause fixes a bug where two remaining candidates have the same number of votes and neither reaches
      # the quota
      if (runif(1) < 0.5){
        elected = c(elected, as.integer(row.names(candidates[1, ])))
      } else { elected = c(elected, as.integer(row.names(candidates[2, ]))) }
    }
    
    # while there are still seats left to be filled
    if (max(candidates$first_preferences) >= quota){
      # if a candidate has reached the quota, redistribute their votes and deem them elected
      leader        = which.max(candidates$first_preferences)
      leading_party = candidates$party[leader]
      elected       = c(elected, as.integer(row.names(candidates[leader, ])))
      candidates    = redistribute_votes(quota = quota, alpha = alpha, candidates = candidates)
      
      # check if any parties have been fully eliminated
      remaining_parties = unique(candidates$party)
      if (!(leading_party %in% remaining_parties)){
        # if the leading party is no longer in the remaining parties eliminate it
        alpha = eliminate_party(alpha, leading_party)
      }
    } else { # no candidate has reached the quota for election
      loser        = which.min(candidates$first_preferences)
      losing_party = candidates$party[loser]
      # otherwise eliminate the candidate with the lowest number of votes
      candidates = eliminate_lowest(alpha = alpha, candidates = candidates)
      
      # check if any parties have been fully eliminated
      remaining_parties = unique(candidates$party)
      if (!(losing_party %in% remaining_parties)){
        # if the losing party is no longer in the remaining parties eliminate it
        alpha = eliminate_party(alpha, losing_party)
      }
    }
    seats_to_fill = seats - length(elected) # update the number of seats remaining
    num_candidates = length(candidates[, 1]) # update number of candidates remaining
  }
  
  elected = elected[1:seats]
  # now deem the remaining candidates elected and return the vector of elected candidates
  return(elected)
}#simulate_election













