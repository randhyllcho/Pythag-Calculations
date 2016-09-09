runsData <- read.csv("runData2014.csv", header = TRUE)

factor <- function(R_G, RA_G) {
  R_G <- R_G * 162
  RA_G <- RA_G * 162
  (1.5 + .45) * log10((R_G + RA_G) / 162)
}

pythag <- function(R, RA) {
  round((R ** factor(R, RA))/(R ** factor(R,RA) + RA ** factor(R, RA)), 4)
}

runsData$predWPercent <- pythag(runsData$R_G, runsData$RA_G)
runsData$Delta <- runsData$WinPercent - runsData$predWPercent
runsData

attach(runsData)


cor.test(predWins, W)
model <- lm(predWins ~ W)
plot(W, predWins)
abline(model, col="red", lwd = 0.5)
res <- signif(residuals(model), 3)
pre <- predict(model)

segments(W, predWins, W, pre, col = "blue")
install.packages("calibrate")
library(calibrate)
textxy(W, predWins, res, cx = 0.7)

RMSE <- round(sqrt(mean((W - predWins)^2)), 2)
RMSE

round(predict(model), 0)
runsData
