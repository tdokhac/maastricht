install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("PerformanceAnalytics")
library(corrplot)
library(RColorBrewer)
library(car) # for scatterplot
data <- read.csv("C:\\Users\\Pui Pui\\Desktop\\maastricht\\maastricht\\GermanCredit.csv", sep = ";", header=TRUE)
subset <- data[,2:32]
data_matrix <- data.matrix(subset)
corr_matrix <- cor(data_matrix, use="complete.obs", method="kendall") 
head(round(corr_matrix,2))
corrplot(corr_matrix, type="upper", order="hclust", sig.level=0.01)


#create data with some correlation structure
##subset

#create corrleation matrix
cor_subset=cor(subset, use="complete.obs")
cor_subset
#plot cor matrix
##squares, variables as %
corrplot(cor_subset, order="AOE", method="square", tl.pos="lt", 
         type="upper", tl.col="black",tl.cex=0.6,tl.srt=45,
         addCoef.col="black", addCoefasPercent = TRUE,
         p.mat = 1-abs(cor_subset), sig.level=0.50,insig="blank")


## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

# get some data from the mtcars built-in dataset
#mydata <- mtcars[, c(1,3,4,5,6)]

# correlation matrix
cor(subset)

# correlation matrix with p-values
cor.prob(subset)

# "flatten" that table
flattenSquareMatrix(cor.prob(subset))

# plot the data
library(PerformanceAnalytics)
chart.Correlation(subset)

# Scatterplot
numerical.set=data.frame(data[3],data[11],data[14],data[23],data[27],data[29]) # substract all numerical variables from subset
pairs(~DURATION+AMOUNT+INSTALL_RATE+AGE+NUM_CREDITS+NUM_DEPENDENTS, 
                  data=numerical.set)
