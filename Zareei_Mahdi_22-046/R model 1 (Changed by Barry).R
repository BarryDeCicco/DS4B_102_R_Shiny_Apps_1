install.packages("logitr")
library(logitr)

library(tidyverse)

FlintFresh <- read_csv("interaction logit model-Age40.csv")
# FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/mixed logit model2.csv")
head(FlintFresh, 20)

#Multinominal Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "online", "Fruits10", "Fruits14", "Ownselected", "localG")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c( "online", "Fruits10", "Fruits14", "Ownselected", "localG"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
  )

summary(mnl_wtp)

# Mixed Logit Model

mxl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "online", "Fruits6", "Fruits14", "Preselected", "Ownselected", "Nonlocal", "localG"),
  randPars = c(online = 'n', Fruits6 = 'n', Fruits14 = 'n', Preselected = 'n', Ownselected ='n', Nonlocal = 'n',localG = 'n'  ),
  numMultiStarts = 10
  
)

summary(mxl_pref)

########## New Reference Points ############

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/mixed logit model4.csv")
head(FlintFresh, 20)

#Multinominal Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG")
)

summary(mnl_pref)

##The variance-covariance matrix

vcov(mnl_pref)


## Predicting Probabilities and outcomes:

data <- subset(
  FlintFresh, obsID %in% c(19, 10, 1),
  select = c('obsID', 'alt', 'choice', 'price', 'online', 'Fruits10', 'Fruits14', 'LimmitedChoice', 'Ownselected', 'localM', 'localG')
)

data

probs <- predict(
  mnl_pref,
  newdata = data,
  obsID = "obsID", 
  ci = 0.95
)

probs


outcomes <- predict(
  mnl_pref,
  type = "outcome",
  returnData = TRUE
)

head(outcomes[c('obsID', 'choice', 'predicted_outcome')], 100)

####### The model can predict 60 of true results based on the probability analysis###

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)



# Mixed Logit Model

mxl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG"),
  randPars = c(online = 'ln', Fruits10 = 'ln', Fruits14 = 'ln', LimmitedChoice = 'ln', Ownselected ='ln', localM = 'ln',localG = 'ln'  ),
  numMultiStarts = 10
  
)

summary(mxl_pref)


## Logit Regression Model for Group 1 (choice questions) of Flint Fresh

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/logistic Regression model results for Group 1-1.csv")
head(FlintFresh, 20)


#Multinominal Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)

## Logit Regression Model for Group 2 (choice questions) of Flint Fresh

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/logistic Regression model results for Group 2-2.csv")
head(FlintFresh, 20)


#Multinominal Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "Fruits10", "Fruits14", "LimmitedChoice", "Ownselected", "localM", "localG"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)

########## Interaction Model--Two Groups by Age--New Reference Points ############

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/interaction logit model-Age40.csv")
head(FlintFresh, 20)

#Multinominal Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price","priceGroupB", "online","onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)

########## Interaction Model--Two Groups by Income--Above 40K and under 40K--New Reference Points ############

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/interaction logit model-IncomeAbove40k.csv")
head(FlintFresh, 20)

#Multinominal Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price","priceGroupB", "online","onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)


########## Interaction Model--Two Groups by Education--Above Bachelor Degree and under Bachelor Degree--New Reference Points ############

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/interaction logit model-EducationBachelor2.csv")
head(FlintFresh, 20)

#Multinational Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price","priceGroupB", "online","onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)

########## Interaction Model--Two Groups by Race--White and And people who are not white--New Reference Points ############

FlintFresh <- read_csv("C:/Mahdi/My Projects/Flint Fresh Project/Survey Data/interaction logit model-RaceWhite2.csv")
head(FlintFresh, 20)

#Multinational Model in Preference Space:

mnl_pref <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price","priceGroupB", "online","onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB")
)

summary(mnl_pref)

#Willingness to Pay: 

mnl_wtp <- logitr(
  data    = FlintFresh,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("online", "onlineGroupB", "Fruits10","Fruits10GroupB", "Fruits14","Fruits14GroupB", "LimmitedChoice","LimmitedChoiceGroupB", "Ownselected","OwnselectedGroupB", "localM","localMGroupB", "localG", "localGGroupB"),
  price   = "price",
  modelSpace = "wtp",
  numMultiStarts = 20
  
)

summary(mnl_wtp)