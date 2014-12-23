#!/usr/bin/env Rscript
library(FNN)

args <- commandArgs(trailingOnly=TRUE)
file <- args[1]

k <- 1
if (length(args)>=3) {
    k <- as.numeric(args[2]);
}

tree <- "cover_tree"
#tree <- "kd_tree"
#tree <- "brute"
if (length(args)>=4) {
    tree <- args[3]
}


file_neighbors <- "neighbors_R.csv"
file_distances <- "distances_R.csv"

#######################################
## load file

cat("file = ", file, "\n")

t0 <- Sys.time()
cat("loading file......................")
data <- read.csv(file, header=FALSE)
t1 <- Sys.time()
cat("done. ",difftime(t1,t0,units="secs"), " sec\n")

#######################################
## do allknn search

t0 <- Sys.time()
cat("finding neighbors.................")
#print (data)
#res <- get.knn(data[1:10000,],k=k,algorithm=tree)
res <- get.knn(data,k=k,algorithm=tree)
t1 <- Sys.time()
cat("done. ",difftime(t1,t0,units="secs"), " sec\n")

#######################################
## output to files

t0 <- Sys.time()
cat("outputing neighbors...............")
neighbors <- res[1]
write.table(neighbors, file=file_neighbors,row.names=FALSE,col.names=FALSE,sep=",")
t1 <- Sys.time()
cat("done. ",difftime(t1,t0,units="secs"), " sec\n")

t0 <- Sys.time()
cat("outputing distances...............")
write.csv(res[2], file=file_distances,row.names=FALSE)
t1 <- Sys.time()
cat("done. ",difftime(t1,t0,units="secs"), " sec\n")
