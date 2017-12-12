loyalty <- read.table('loyalty.txt')
colnames(loyalty) <- c('visits',  'age',  'family',  'amount_spent')
head(loyalty)
description <- "The loyalty data consist of 509 observations on the behaviour of customers with loyalty cards from a supermarket chain in Northern Italy. The response 
(y) is the amount in euros spent at the shop over six months and the explanatory variables are: X1, the number of visits to the supermarket in the six month period; 
X2, the age of the customer; X3, the number of members of the customer s family. To find out more about this data set please see Atkinson and Riani (2006), JCGS"
source <- "Atkinson and Riani (2006), JCGS"
save(loyalty, file='loyalty.rda')
prompt(loyalty)
