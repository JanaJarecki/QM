# -----------------------------------------------------------------------------
# Analyze qualitative data
# Pertains to the Data of the Uiversity of Virginia
# Author: Jana B. Jarecki
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# A coding manual was developed using 20% of the open-ended questions
# The question asked participants to describe their game-play experience
# This question was only asked in the student sample collected at the University of Virginia
# -----------------------------------------------------------------------------
if (!require("rel")) { install.packages("rel") } else { library(rel) }
library(data.table)


# Read and prepare
# Note: All paths are relative to the current file location
# Coder 1
d1 <- fread("../../../data/uva (university virginia student sample)/coding_coder1.csv", check.names=TRUE, col.names = c("id","category","categorylabel","markedtext"), key = 'id')
d1[, category := gsub("IC-","",category)]
# Coder 2
d2 <- fread("../../../data/uva (university virginia student sample)/coding_coder2.csv", check.names=TRUE, , col.names = c("id","category","categorylabel","markedtext"), key = 'id')
d1[, id := gsub(".txt", "", id)]
d2[, id := gsub(".txt", "", id)]


# Correct unequal numbers of categories
mat1 <- dcast.data.table(d1, Document ~ Category, value.var = "CategoryTitle")
mat2 <- dcast.data.table(d2, Document ~ Category, value.var = "CategoryTitle")
mat1 <- dcast(d1, Document ~ Category, fun.aggregate = length)
mat2 <- dcast(d2, Document ~ Category, fun.aggregate = length)
mat1 <- as.matrix(mat1[, Document := NULL])
mat2 <- as.matrix(mat2[, Document := NULL])


# Kappa (intr-rater agreement) for all categories individually
ncategories <- ncol(mat1)
ri <- vector("numeric", length=ncategories)
for (i in 1:ncategories)
{
    ri[i] <- ckap(cbind(mat1[,i],mat2[,i]))$est
}

# Mean Kappa
summary(ri)
