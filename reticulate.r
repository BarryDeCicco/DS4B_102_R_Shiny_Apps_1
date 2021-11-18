library(reticulate)

getwd()

library(reticulate)
os <- import("os")
os$listdir(".")
