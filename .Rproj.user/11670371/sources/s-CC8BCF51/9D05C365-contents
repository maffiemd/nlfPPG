### Loading Data
ppg <- read.csv('ppg.csv', header=T)

### Adding Draft Pick Columns
dat <- addDraftColumns(ppg)
dat$pctOfCap <-dat$pctOfCap*100 #Adjusting for readability
dat$totalDraftCapital <- rowSums(dat[5:32], na.rm = T)
mod <- lm(PPG~pctOfCap+totalDraftCapital+TEAM+as.factor(dat$YEAR), data = dat)
summary(mod)
plot(mod)

#expected PPG
expectedPPG = 17.59+0.1020*34.61 + 0.00005112*13309 + 0.689

### Summary Stats
sd(dat$PPG) #4.23 points
mean(dat$PPG) # 22.98
mean(dat$pctOfCap) #34.51. Note this is lower than 2023.
dat$pctOfCap[dat$TEAM == "Pittsburgh Steelers" & dat$YEAR == 2023] #34.61

library(ggplot2)
library(dplyr)
#Histograms
# Percent of Cap Allocated to Offense
ggplot(dat, aes(x = pctOfCap)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(xintercept = 34.61, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$pctOfCap), color = "green", linetype = "dashed", size = 1) +
  labs(x = "Percent of Salary Cap", y = "Frequency", title = "Percent of Salary Cap Allocated to Offense")

# Total Draft Capital
ggplot(dat, aes(x = totalDraftCapital)) +
  geom_histogram(binwidth = 400, fill = "white", color = "black") +
  geom_vline(xintercept = 11799, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$totalDraftCapital), color = "green", linetype = "dashed", size = 1) +
  labs(x = "Total Draft Capital", y = "Frequency", title = "Team Draft Capital Spent on Offense, 2018-2022")

# PPG with Expected PPG
ggplot(dat, aes(x = PPG)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(xintercept = 17.2, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = expectedPPG, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$PPG), color = "green", linetype = "dashed", size = 1) +
  labs(x = 'Points Per Game', y = 'Frequency', title = 'Points Per Game, 2018-2022')

# Predicted vs. Actual values in the model
ggplot(dat, aes(x = predict(mod), y = PPG)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = 'Predicted Values', y = 'Actual Values', title = 'Predicted vs. Actual Values')


  