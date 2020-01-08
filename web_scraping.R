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
  
  transfers = table[[1]][, -2] # delete an empty column
  
  # now extract only the percentage of votes which were transferred between parties
  
  for(i in 1:nrow(transfers)){
    for(j in 2:(ncol(transfers))){
      a = transfers[i, j]
      transfers[i, j] = as.numeric(sub("\\%.*", "", sub(".*\\(", "", a)))/100
    }
  }
  
  alpha = data.matrix(transfers)[, -1]
  
  return(alpha)
}#scrape_transfer_matrix

