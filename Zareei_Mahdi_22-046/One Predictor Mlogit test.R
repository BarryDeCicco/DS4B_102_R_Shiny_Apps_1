
# File:  One Predictor Mlogit test.R

# Goal: To see what can be extracted from a simple model, with no complications.

# RESULT:  


######### MLOGITR #######

# install.packages("mlogit")
library(mlogit)
library(dfidx)
#install.packages("bbmle")
library(bbmle)
library(broom)
library(readr)
library(tidyverse)




# From https://cran.r-project.org/web/packages/mlogit/vignettes/e3mxlogit.html and
# https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html
# figuring out how to convert a data frame to an dfidx object:

FlintFresh <- read_csv("mixed logit model2.csv")
FF <- dfidx(FlintFresh, idx = list(c("obsID", "id")))
View(FlintFresh)
View(FF)

#### Run models:                                         ####
FFonline.ml <- mlogit(choice ~ online  | -1 , FF)
summary(FFonline.ml)     


FFonline.mxl <- mlogit(choice ~ online  | -1 , FF,
                       rpar=c(online = 'n'), 
                       R = 100, halton = NA, panel = TRUE)
summary(FFonline.mxl)     

# I was not able to spot anything useful in the structure:
# str(FFonline.ml)
# str(FFonline.mxl)


#### Extract the fitted values from the model, and save as a data frame: ####

FFonline.ml_fitted.values <-  FFonline.ml$fitted.values %>% as.data.frame() %>% 
  dplyr::rename(fitted_value_ml = ".")

FFonline.mxl_fitted.values <- FFonline.mxl$fitted.values %>% as.data.frame() %>% 
  dplyr::rename(fitted_value_mxl = ".")

View(FFonline.ml_fitted.values)
View(FFonline.mxl_fitted.values)

#### Examinine the number of unique values:                      ####
# There are 3 different fitted values (based on the value of 'online'?):
FFonline.ml_fitted.values %>% unique() %>% tally()

# There are 103 different fitted values, one for each subject.
# This means that these values are based on something by-subject:

FFonline.mxl_fitted.values %>% unique() %>% tally()
# The values in 'FFonline.ml_fitted.values' seem to be the probabilities of 
# alternatives being selected.  They are probabilities.

# The values in 'FFonline.mxl_fitted.values' are not probabilities,
# and are the same for all observations of each subject.  They seem to 
# be some form of the by-subject random effects.


#### Combine with the ID's from the original data set:

FlintFresh_Results <- FlintFresh %>% 
  dplyr::select(id) %>% 
  cbind(.,FFonline.mxl_fitted.values) %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(fitted_value_mxl = mean(fitted_value_mxl)) 


View(FlintFresh_Results)

summary(FlintFresh_Results$fitted_value_mxl)
qqnorm(FlintFresh_Results$fitted_value_mxl, pch = 1, frame = FALSE,
       main = "Random Effects - Online Only Model")
qqline(FlintFresh_Results$fitted_value_mxl, col = "steelblue", lwd = 2)


#### FURTHER WORK:                                                     ####

# These values can be examined by demographics, to add insight.


#### Not Necessarily Useful:                                          ####
                                             
# Probabilities of choosing each alternative.  
# These will be based on the alternatives offered in each choice set.  
# Here, there is only one predictor, 'online', so that they will only 
# depend on that.  In the full model, they'll depend on the combinations 
# of all predictors.

FFonline.ml_fitted_prob <- fitted(FFonline.ml, type = "probabilities") %>% 
  as.data.frame() %>% 
  dplyr::rename(ml_prob_1 = "1",
                ml_prob_2 = "2")

View(FFonline.ml_fitted_prob)

names(FFonline.ml_fitted_prob)  

FFonline.mxl_fitted_prob <- fitted(FFonline.mxl, type = "probabilities")%>% 
  as.data.frame() %>% 
  dplyr::rename(mxl_prob_1 = "1",
                mxl_prob_2 = "2")

View(FFonline.mxl_fitted_prob)
