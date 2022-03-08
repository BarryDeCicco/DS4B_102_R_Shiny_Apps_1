
# Random utility model and the multinomial logit model

# From:  https://cran.r-project.org/web/packages/mlogit/vignettes/c3.rum.html

library("mlogit")
data("ModeCanada", package = "mlogit")

MC <- dfidx(ModeCanada, subset = noalt == 4)
ml.MC1 <- mlogit(choice ~ cost + freq + ovt | income | ivt, MC)


# If the data set was a data frame, rather than a dfidx object, 
# then the command would be:

# ml.MC1b <- mlogit(choice ~ cost + freq + ovt | income | ivt, ModeCanada,
#                   subset = noalt == 4, idx = c("case", "alt"))


summary(ml.MC1)

head(fitted(ml.MC1, type = "outcome"))


N# The command below uses only the alternatives 'car', 'train' and 'air',
# sets the reverence level to 'car', and creates a 'total transport time'
# variable 

MC$time <- with(MC, ivt + ovt)
ml.MC1 <- mlogit(choice ~ cost + freq | income | time, MC, 
                 alt.subset = c("car", "train", "air"), reflevel = "car")


