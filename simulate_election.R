
simulate_election = function(national_support, local_multiplier, alpha, candidate_party, party_proportion, seats, turnout){
  
  # function which simulates a single election count of an Irish general
  # election consituency. The parties tracked are FG, FF, SF, Labour, Greens
  # Aontu, SP-PBP, Renua, Independents, Others.
  
  # inputs: national_support - a vector containing the national proportion of first
  #                            preference votes a party receives.
  #         local_multiplier - the past difference between the national result and the
  #                            previous local election constituency results.
  #         alpha            - a matrix of transfer probabilities where the (i, j) entry
  #                            corresponds to the proportion of party i's votes transferring to 
  #                            party j.
  #         candidate_party  - a vector containing the party of each candidate
  #         party_proportion - a vector containing a proportion for each candidate corresponding to
  #                            the proportion of their party's first preference votes that they received
  #         seats            - an integer corresponding to the number of seats available
  #         turnout          - an integer corresponding to the number of votes cast
  #
  # outputs: the seats filled by the various parties running for election
  
  # first calculate the local number of first preferences per party
  
  party_fp = (national_support * local_multiplier)/(sum(national_support * local_multiplier)) * turnout
  
  # now assign first preference votes to each candidate
  
  first_preferences = rep(0, length(candidate_party))
  for (i in 1:length(candidate_party)){
    
    first_preferences[i] = round(party_fp[candidate_party[i]]*party_proportion[i])
    
  }
  
  # calculate the quota required to become elected
  quota = ceiling(turnout/(seats + 1))
  
  # create a data frame to store candidate information
  candidates = data.frame('party' = candidate_party, 'first_preferences' = first_preferences,
                          'party_proportion' = party_proportion)
  leader = which.max(candidates$first_preferences)
  
  
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
    
    # determine to which parties the votes are transfered
    party_transfers = surplus * alpha[leading_party, ]
    
    # delete the leading candidate from the data frame and renormalise their
    # party colleagues' weightings
    
    new_candidates = candidates[-leader,]
    new_party_prop = new_candidates[new_candidates$party == leading_party,]$party_proportion
    
    new_candidates[new_candidates$party == leading_party,]$party_proportion = new_party_prop/sum(new_party_prop)

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
  }
  
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
    
    # determine to which parties the votes are transfered
    party_transfers = candidates$first_preferences[loser] * alpha[losing_party, ]
    
    # delete the losing candidate from the data frame and renormalise their
    # party colleagues' weightings
    
    new_candidates = candidates[-loser,]
    new_party_prop = new_candidates[new_candidates$party == losing_party,]$party_proportion
    
    new_candidates[new_candidates$party == losing_party,]$party_proportion = new_party_prop/sum(new_party_prop)
    
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
    
  }
  elected = c()
  i = 0
  while ((seats - length(elected) > 0) & (length(candidates[, 1]) > (seats - length(elected)))){
    i = i + 1
    # while there are still seats left to be filled
    if (max(candidates$first_preferences) >= quota){
      # if a candidate has reached the quota, redistribute their votes and deem them elected
      leader = which.max(candidates$first_preferences)
      elected = c(elected, as.integer(row.names(candidates[leader, ])))
      candidates = redistribute_votes(quota = quota, alpha = alpha, candidates = candidates)
    }
    else{
      # otherwise eliminate the candidate with the lowest number of votes
      candidates = eliminate_lowest(alpha = alpha, candidates = candidates)
    }
  }

  # now deem the remaining candidates elected and return the vector of elected candidates
  
  elected = c(elected, as.integer(row.names(candidates)))
  
  return(elected)
  
}













