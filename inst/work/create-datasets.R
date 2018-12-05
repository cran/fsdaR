## Create the swissbanknotes dataset

library(alr3)
data(banknote)

swissbanknotes <- banknote
colnames(swissbanknotes) <- tolower(colnames(swissbanknotes))
colnames(swissbanknotes)[ncol(swissbanknotes)] <- "class"
head(swissbanknotes)
swissbanknotes$class <- swissbanknotes$class+1

head(swissbanknotes)

library(rrcov)
plot(CovClassic(swissbanknotes[,1:6]), which="pairs", col=swissbanknotes$class)

##prompt(swisbanknotes)

save(swissbanknotes, file="swissbanknotes.rda")

## Example

data(swissbanknotes)
head(swissbanknotes)
plot(CovClassic(swissbanknotes[, 1:6]), which="pairs", col=swissbanknotes$class)

##-------------------------------------------------------------------------
df <- read.table("diabetes.txt")
head(df)
library(mclust)
data(diabetes)
rownames(diabetes) <- NULL
rownames(df) <- NULL

## sort by class
diabetes <- rbind(diabetes[which(diabetes[,1]=="Normal"),], diabetes[which(diabetes[,1]=="Chemical"),], diabetes[which(diabetes[,1]=="Overt"),])

## only one difference in insulin, observation 104
all.equal(diabetes[, 2:4], df, check.names=FALSE)
diabetes[,2:4]-df

colnames(diabetes)
diabetes <- diabetes[,c("glucose", "insulin", "sspg", "class")]
save(diabetes, file="diabetes.rda")

## The diabetes dataset, introduced by Reaven and Miller (1979),
##  consists of 145 observations (patients). For each patient three
##  measurements are reported: plasma glucose response to oral glucose,
##  plasma insulin response to oral glucose, degree of insulin resistance.

##---------------------------------------------------------------
##
## Swiss Heads data

##  The Swiss Heads data set was introduced by B. Flury and H. Riedwyl (1988).
##  It contains information on six variables describing the dimensions of the
##  heads of 200 twenty year old Swiss soldiers.

swissheads <- read.table("head.txt")
colnames(swissheads) <- c('minimal_frontal_breadth',  'breadth_angulus_mandibulae',  'true_facial_height',  'length_glabella_nasi',  'length_tragion_nasion',  'length_tragion_gnathion')
head(swissheads)
save(swissheads, file="swissheads.rda")
