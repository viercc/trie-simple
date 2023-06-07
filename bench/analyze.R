library(ggplot2)

srcfile <- 'benchdata/bench_preprocessed.csv'

data <- read.csv(srcfile)

plotComparison <- function(data, filename) {
  plt <- ggplot(data) +
    aes(x=Method, y=MeanTime, ymin=MeanLB, ymax=MeanUB, color=Name) +
    scale_y_log10(breaks=10^(0:-8)) +
    geom_errorbar(width=0.5) +
    geom_point()
  ggsave(file=filename, plot=plt, dpi=100, width=14, height=8)
}

plotRatio <- function(data, target_name, base_name, filename) {
  data_baseline <- subset(data, Name == base_name)
  data_target   <- subset(data, Name == target_name)
  ratio_data <- data.frame(data_target)
  ratio_data$MeanTime <- ratio_data$MeanTime / data_baseline$MeanTime
  ratio_data$MeanLB <- ratio_data$MeanLB / data_baseline$MeanUB
  ratio_data$MeanUB <- ratio_data$MeanUB / data_baseline$MeanLB
  plt <- ggplot(ratio_data) +
    aes(x=Method, y=MeanTime, ymin=MeanLB, ymax=MeanUB) +
    scale_y_log10(limits=c(0.001,1000), breaks=10^(-2:2)) +
    geom_hline(linewidth=1, color=alpha('red', .6), yintercept=1) + 
    xlab("") +
    ylab("Ratio to baseline") +
    ggtitle(sprintf("%s / %s", target_name, base_name)) +
    geom_errorbar(width=0.5) +
    geom_point()
  ggsave(file=filename, plot=plt, dpi=100, width=14, height=8)
}

data$Method <- with(data, reorder(Method, MeanTime))

# isEnglish <- data$Dataset=='English'
# isWiki    <- data$Dataset=='Wiki'
# onSetOps <- data$Name=='Set' | data$Name=='TSet'
# onMapOps <- data$Name=='Map' | data$Name=='TMap'
# plotComparison(data[isEnglish & onSetOps,], "benchdata/set-english.png")
# plotComparison(data[isEnglish & onMapOps,], "benchdata/map-english.png")
# plotComparison(data[isWiki & onSetOps,], "benchdata/set-wiki.png")
# plotComparison(data[isWiki & onMapOps,], "benchdata/map-wiki.png")

plotRatio(subset(data, Method != 'stringCount'), 'TSet', 'Set', "benchdata/ratio-set.png")
plotRatio(subset(data, Method != 'stringCount'), 'TMap', 'Map', "benchdata/ratio-map.png")