# scraping script

scrape_transfer_matrix = function(elecid, constitid, electype){
  # this function scrapes the website irelandelection.com transfer data from previous
  # Irish elections.
  #
  # inputs: elecid    - unique id for the election in question
  #         constitid - unique id for the constituency of interest
  #         electype  - the elction type. 5 = local election, 1 = general election
  #
  # output: alpha     - transfer matrix between parties in the constituency
  
  # first download the site's html code
  
  site      = paste('http://irelandelection.com/electiontransfers.php?elecid=', elecid, '&constitid=', constitid, '&electype=', electype, sep='')
  html_code = read_html(site)
  
  # then extract the table from the html
  
  table <- html_code %>% 
    html_node('body') %>% 
    xml_find_all("//div[contains(@class, 'col-md-12')]") %>%
    xml_find_all("//table[contains(@class, 'table')]") %>%
    html_table(header = TRUE)
  
  #transfers = table[[1]][, -2] # delete an empty column
  
  # now extract only the percentage of votes which were transferred between parties
  
  #for(i in 1:nrow(transfers)){
  #  for(j in 2:(ncol(transfers))){
  #    a = transfers[i, j]
  #    transfers[i, j] = as.numeric(sub("\\%.*", "", sub(".*\\(", "", a)))/100
  #  }
  #}
  
  #alpha = data.matrix(transfers)[, -1]
  
  #return(alpha)
  return(table)
}#scrape_transfer_matrix

clean_transfer_matrix = function(table){
  # this function cleans the scraped transfer data and normalises the rows to account
  # for missing entries.
  # input:  table         - the scraped data table in a list form
  # output: full_transfer - the cleaned transfer matrix
  
  # initialise vectors containing the indices for subsetting the data table
  from_party = rep(0, 10)
  to_party   = rep(0, 10)
  
  # we use the try function here to suppress error messages when a certain party is not
  # observed in the scraped matrix
  
  try((from_party[1] = which(table[[1]] == 'from Fine Gael')) , silent = TRUE)
  try((from_party[2] = which(table[[1]] == 'from Fianna Fáil')) , silent = TRUE)
  try((from_party[3] = which(table[[1]] == 'from Sinn Féin')) , silent = TRUE)
  try((from_party[4] = which(table[[1]] == 'from Labour Party')) , silent = TRUE)
  try((from_party[5] = which(table[[1]] == 'from Green Party')) , silent = TRUE)
  try((from_party[6] = which(table[[1]] == 'from People Before Profit')) , silent = TRUE)
  try((from_party[7] = which(table[[1]] == 'from Social Democrats')) , silent = TRUE)
  try((from_party[8] = which(table[[1]] == 'from Aontú')) , silent = TRUE)
  try((from_party[9] = which(table[[1]] == 'from Independent')) , silent = TRUE)
  try((from_party[10] = which(table[[1]] == 'from Independent Alliance')) , silent = TRUE)
  
  
  try((to_party[1] = which(colnames(table[[1]]) == 'to Fine Gael')) , silent = TRUE)
  try((to_party[2] = which(colnames(table[[1]]) == 'to Fianna Fáil')) , silent = TRUE)
  try((to_party[3] = which(colnames(table[[1]]) == 'to Sinn Féin')) , silent = TRUE)
  try((to_party[4] = which(colnames(table[[1]]) == 'to Labour Party')) , silent = TRUE)
  try((to_party[5] = which(colnames(table[[1]]) == 'to Green Party')) , silent = TRUE)
  try((to_party[6] = which(colnames(table[[1]]) == 'to People Before Profit')) , silent = TRUE)
  try((to_party[7] = which(colnames(table[[1]]) == 'to Social Democrats')) , silent = TRUE)
  try((to_party[8]= which(colnames(table[[1]])== 'to Aontú')) , silent = TRUE)
  try((to_party[9] = which(colnames(table[[1]]) == 'to Independent')) , silent = TRUE)
  try((to_party[10] = which(colnames(table[[1]]) == 'to Independent Alliance')) , silent = TRUE)
  
  for(i in 1:nrow(table[[1]])){
    # this loop takes the data stored as character strings and extracts the tranfer
    # proportions as numeric objects
    for(j in 3:(ncol(table[[1]]))){
      a = table[[1]][i, j]
      table[[1]][i, j] = as.numeric(sub("\\%.*", "", sub(".*\\(", "", a)))/100
    }
  }
  
  
  # now we want to normalise the rows to account for missing entries. the normalising factor is equal to
  # the proportion of observed entries in a given row.
  normal_factor = rep(0, nrow(table[[1]]))
  for (i in 1:nrow(table[[1]])){
    normal_factor[i] = sum(!is.na(table[[1]][i, -c(1, 2)]))/10
    table[[1]][i, -c(1, 2)] = data.matrix(table[[1]][i, -c(1, 2)])*normal_factor[i]
  }
  
  # now finally build a full transfer matrix containing all parties in the election
  full_transfer = matrix(NA, nrow = 10, ncol = 10)
  for (i in 1:10){
    for (j in 1:10){
      if ((from_party[i] != 0) & (to_party[j] != 0)){ # only include observed entries
        full_transfer[i, j] = as.numeric(table[[1]][from_party[i], to_party[j]]) 
      }
    }
  }
  
  return(full_transfer)
}#clean_transfer_matrix

download_data = function(elecid, constitid, electype){
  # this function downloads the data from all constituencies. scraped from irelandelections.com.
  # inputs: elecid          - election id
  #         electype        - the election type (5 = local, 1 = general)
  #         constitid       - a vector of consituency id's
  # output: national_matrix - the final cleaned national transfer matrix 
  
  all_transfers = vector(mode = 'list', length = length(constitid))
  
  for (i in 1:length(constitid)){
    table = scrape_transfer_matrix(elecid = elecid, constitid = constitid[i], electype = electype)
    all_transfers[[i]] = clean_transfer_matrix(table)
    print(i)
  }
  
  national_matrix = matrix(0, nrow = 10, ncol = 10)
  
  for (i in 1:10){
    for (j in 1:10){
      observations = rep(NA, length(constitid))
      for (k in 1:length(constitid)){observations[k] = all_transfers[[k]][i, j]}
      national_matrix[i, j] = mean(observations, na.rm = TRUE)
    }
  }
  
  return(national_matrix)
}#download_data

