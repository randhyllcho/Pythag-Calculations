library(ggplot2)
library(ggthemes)
library(dplyr)

mRuns <- read.csv("Mariners2016.csv", header = TRUE)
p <- ggplot(mRuns)
p <- p + geom_bar(aes(Opp, fill=W.L), width = 0.78, alpha = 0.85)
p <- p + scale_y_continuous(breaks = 0:16) + theme_classic(base_size = 15, base_family = "mono")
p <- p + scale_fill_manual(values = c("#0c685f", "#898e90", "#0c243e", "#af7323"), name = "", labels = c("Loss", "Walk-Off Loss", "Win", "Walk-Off Win"))
p <- p + labs(title = "Seattle Mariners Outcome by Opponent", x = "Opponents", y = "")
p + theme(plot.title = element_text(hjust = 0))

getPythag <- function(r, ra) {
  r <- sum(r)
  ra <- sum(ra)
  pyth <- round(r^2 / (r^2 + ra^2), 3)
  return(pyth)
}

getWPercent <- function(df, x) {
  set <- x
  Runs <- 0
  RunsAllowed <- 0
  plist <- list()
  glist <- list()
  for (i in set) {
    games <- filter(df, Gm. == i)
    Runs <- Runs + games$R
    RunsAllowed <- RunsAllowed + games$RA
    pythagg <- getPythag(Runs, RunsAllowed)
    plist[[i]] <- pythagg
    glist[[i]] <- i
  }
  df <- data.frame(cbind(plist, glist))
  print(length(df))
  #g <- ggplot(df, aes(glist, plist)) + geom_point()
  return(pythagg)
}
getWPercent(mRuns, c(124:134))
