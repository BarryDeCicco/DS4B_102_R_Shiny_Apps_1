

# The random parameters (or mixed) logit model

# From:  https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html


library("mlogit")
data("Train", package = "mlogit")
Train$choiceid <- 1:nrow(Train)
Tr <- dfidx(Train, choice = "choice", varying = 4:11, sep = "_",
            opposite = c("price", "comfort", "time", "change"),
            idx = list(c("choiceid", "id")), idnames = c("chid", "alt"))
Tr$price <- Tr$price / 100 * 2.20371
Tr$time <- Tr$time / 60
Train.ml <- mlogit(choice ~ price + time + change + comfort | - 1, Tr)
coef(summary(Train.ml))