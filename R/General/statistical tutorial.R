url <- "http://rstatistics.net/wp-content/uploads/2015/09/ozone.csv"  
# alternate source:  https://raw.githubusercontent.com/selva86/datasets/master/ozone.csv
inputData <- read.csv(url)  # import data

outlier_values <- boxplot.stats(inputData$pressure_height)$out  # outlier values.
boxplot(inputData$pressure_height, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

quantile(inputData$pressure_height, probs = c(.25,.75), na.rm = T)
quantile(inputData$pressure_height, probs = c(.05,.95), na.rm = T)
IQR(inputData$pressure_height, na.rm = T)

pretty(5:20, n=10)
x <- inputData$pressure_height
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
x
rm(x)
