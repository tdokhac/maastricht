##### Packages and libraries
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("PerformanceAnalytics")
install.packages("Hmisc")
library(corrplot)
library(RColorBrewer)
library(car) # for scatterplot
library(Hmisc)
##### Include data set
data <- read.csv("C:\\Users\\Pui Pui\\Desktop\\maastricht\\maastricht\\R\\GermanCredit.csv", sep = ";", header=TRUE)
##### Excluding OBS#
subset <- data[,2:32]
##### Create correlation matrix either pearson, kendall or spearman method
data_matrix <- data.matrix(subset)
corr_matrix_pearson <- cor(data_matrix, use="complete.obs") 
corr_matrix_kendall <- cor(data_matrix, use="complete.obs", method="kendall") 
corr_matrix_spearman <- cor(data_matrix, use="complete.obs", method="spearman") 
head(round(corr_matrix_pearson,2))
##### Plot correlation matrix (different styles)
corrplot(corr_matrix_pearson, method="color")
corrplot(corr_matrix_pearson, type="upper", order="hclust", sig.level=0.05)

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(subset, 0.95)
res2 <- cor.mtest(subset, 0.99)
corrplot(corr_matrix_pearson, method="color", p.mat = res1[[1]], diag=FALSE, sig.level = 0.05, tl.col = "black", tl.offset = 0.4, tl.srt = 90, type="upper")
##### Scatterplots
numerical.set=data.frame(log(data[3]),log(data[11]),data[14],log(data[23]),data[27],data[29]) # substract all numerical variables from subset
pairs(~DURATION+AMOUNT+INSTALL_RATE+AGE+NUM_CREDITS+NUM_DEPENDENTS, 
                  data=numerical.set)

numerical.set=data.frame(log(data[3]),log(data[11]),log(data[23])) # substract all numerical variables from subset
pairs(~DURATION+AMOUNT+AGE, 
      data=numerical.set)
