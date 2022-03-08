
######### MLOGITR #######

# install.packages("mlogit")
library(mlogit)
library(dfidx)
#install.packages("bbmle")
library(bbmle)
library(broom)

#price + online + Fruits6 + Fruits14 + Preselected + Ownselected + Nonlocal + localG



# From https://cran.r-project.org/web/packages/mlogit/vignettes/e3mxlogit.html and
# https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html
# figuring out how to convert a data frame to an dfidx object:

FlintFresh <- read_csv("mixed logit model2.csv")
FF <- dfidx(FlintFresh, idx = list(c("obsID", "id")))

FF.ml <- mlogit(choice ~ price + online + Fruits6 + Fruits14 + 
                   Preselected + Ownselected +
                  Nonlocal + localG | -1 , FF)
summary(FF.ml)     


FF.mxl <- mlogit(choice ~ price + online + Fruits6 + Fruits14 + 
                  Preselected + Ownselected + Nonlocal + localG | 1 , FF,
                  rpar=c(online = 'n', price='n'), 
                 R = 100, halton = NA, panel = TRUE)
                 
summary(FF.mxl) 


FF.mxl <- mlogit(choice ~ price + online + Fruits6 + Fruits14 + 
                   Preselected + Ownselected + Nonlocal + localG | 1 , FF,
                 rpar=c(online = 'n'), 
                 R = 100, halton = NA, panel = TRUE)

summary(FF.mxl) 

FF2.mxl <- mlogit(choice ~ price + online + Fruits6 + Fruits14 + 
                   Preselected + Ownselected + Nonlocal + localG | 1 , FF,
                 rpar=c(online = 'n', price='n'), 
                 R = 100, halton = NA, panel = TRUE)

summary(FF2.mxl) 


AIC(FF2.mxl)

bbmle::AICtab(FF2.mxl,FF.mxl)

lrtest(FF2.mxl,FF.mxl)


AICtab(m0,m1,m2)

class(rpar(FF.mxl, 'online'))



fitted_df <- fitted(FF.mxl)
View(fitted_df)

tidy(FF.mxl)
augment(FF.mxl, FF)
browseVignettes(package="broom")




test_df <- FlintFresh %>% dplyr::filter(alt==1) %>% 
  dplyr::select(id) %>% 
  cbind(.,FF.mxl$fitted.values %>% as.data.frame() ) %>% 
  as.data.frame() %>% 
  unique() %>% 
  dplyr::rename(fitted.values = ".")
View(test_df)                  

qqnorm(test_df$fitted.values, 
       ylab="Fitted Values", 
       xlab="Normal Scores", 
       main="Model:  FF.mxl") 
qqline(test_df$fitted.values)                    
