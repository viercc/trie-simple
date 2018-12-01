library(ggplot2)

srcfile <- '../benchdata/bench_preprocessed.csv'

data <- read.csv(srcfile)

data$Method <- with(data, reorder(Method, MeanTime))

dataMap <- data[data$Name=='Map' | data$Name=='TMap',]
dataSet <- data[data$Name=='Set' | data$Name=='TSet' | data$Name=='TSetPC',]
dataSetURI <- data[data$Name=='Set_URI' | data$Name=='TSet_URI' | data$Name=='TSetPC_URI',]

plotMap <- ggplot(dataMap, aes(x=Method, y=MeanTime, ymin=MeanLB, ymax=MeanUB, color=Name)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_errorbar(width=0.5) +
  geom_point()
ggsave(file="benchTMap.png", plot=plotMap, dpi=100, width=14, height=8)

plotSet <- ggplot(dataSet, aes(x=Method, y=MeanTime, ymin=MeanLB, ymax=MeanUB, color=Name)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_errorbar(width=0.5) +
  geom_point()
ggsave(file="benchTSet.png", plot=plotSet, dpi=100, width=14, height=8)

plotSetURI <- ggplot(dataSetURI, aes(x=Method, y=MeanTime, ymin=MeanLB, ymax=MeanUB, color=Name)) +
  scale_y_log10(breaks=10^(0:-8)) +
  geom_errorbar(width=0.5) +
  geom_point()
ggsave(file="benchTSetURI.png", plot=plotSetURI, dpi=100, width=14, height=8)
