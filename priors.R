# here we will set the priors on the parameters of the Dirichlet
# distributions for each party in each constituency.

# as a test case we are using the 2014 local elections to predict the 2016 general
# and will be using the GE constituency Dublin Rathdown, which contains three local
# constituencies - Blackrock, Sandyford, and Dundrum

GE_alpha   = scrape_transfer_matrix(elecid = 231, constitid = 20, electype = 1)
LE_alpha_1 = scrape_transfer_matrix(elecid = 157, constitid = 147, electype = 5)
LE_alpha_2 = scrape_transfer_matrix(elecid = 157, constitid = 149, electype = 5)
LE_alpha_3 = scrape_transfer_matrix(elecid = 157, constitid = 151, electype = 5)

LE_alpha_3 = cbind(LE_alpha_3, rep(0, 3)) # add missing column

# replace NA with 0
LE_alpha_1[is.na(LE_alpha_1)] <- 0
LE_alpha_2[is.na(LE_alpha_2)] <- 0
LE_alpha_3[is.na(LE_alpha_3)] <- 0

# add missing rows
LE_alpha_1 = rbind(LE_alpha_1, rep(0, 7))
LE_alpha_1 = rbind(LE_alpha_1, rep(0, 7))

LE_alpha_2 = rbind(LE_alpha_2, rep(0, 7))

LE_alpha_3 = rbind(LE_alpha_3, rep(0, 7))
LE_alpha_3 = rbind(LE_alpha_3, rep(0, 7))
LE_alpha_3 = rbind(LE_alpha_3, rep(0, 7))
LE_alpha_3 = rbind(LE_alpha_3, rep(0, 7))

# interchange rows

inter_1 = c(1, 2, 3, 6, 7, 4, 5)
inter_2 = c(1, 2, 3, 4, 5, 7, 6)
inter_3 = c(4, 5, 1, 2, 7, 3, 6)

LE_alpha_1 = LE_alpha_1[inter_1, inter_1]








