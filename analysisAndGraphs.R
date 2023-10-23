### Analysis
dat <- addDraftColumns(ppg)
plot(dat$PPG~dat$pctOfCap)
dat$pctOfCap <-dat$pctOfCap*100
## total draft value
dat$totalDraftCapital <- rowSums(dat[5:32], na.rm = T)
hist(dat$totalDraftCapital)
mod <- lm(PPG~pctOfCap+totalDraftCapital+TEAM+as.factor(dat$YEAR), data = dat)#10% of variance
plot(x=predict(mod), y = dat$PPG, ylim= c(0,35), xlim = c(0,35))
abline(a = 0, b = 1)
library(ggplot2)
library(dplyr)

# Filter the data for "Pittsburgh Steelers" and create a new column for the row index
filtered_dat <- dat %>% 
  filter(TEAM == "Pittsburgh Steelers") %>%
  mutate(row_index = row_number())

# Create a histogram
ggplot(dat, aes(x = pctOfCap)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(xintercept = 34.61, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$pctOfCap), color = "green", linetype = "dashed", size = 1) +
  labs(x = "Percent of Salary Cap", y = "Frequency", title = "Percent of Salary Cap Allocated to Offense")

# Create a histogram
ggplot(dat, aes(x = totalDraftCapital)) +
  geom_histogram(binwidth = 400, fill = "white", color = "black") +
  geom_vline(xintercept = 11799, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$totalDraftCapital), color = "green", linetype = "dashed", size = 1) +
  labs(x = "Total Draft Capital", y = "Frequency", title = "Team Draft Capital Spent on Offense, 2018-2022")

# Create the ggplot with points and the 1:1 line
ggplot(dat, aes(x = pctOfCap, y = PPG)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = 'Predicted Values', y = 'Actual Values', title = 'Predicted vs. Actual Values') +
  ylim(0, 35) +
  xlim(0, 35)

# Create the ggplot with points and the 1:1 line
ggplot(dat, aes(x = predict(mod), y = PPG)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(x = 'Predicted Values', y = 'Actual Values', title = 'Predicted vs. Actual Values')

# Create the ggplot with points and the 1:1 line
ggplot(dat, aes(x = PPG)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(xintercept = 17.2, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$PPG), color = "blue", linetype = "dashed", size = 1) +
  labs(x = 'Points Per Game', y = 'Frequency', title = 'Points Per Game, 2018-2022')

# Create the ggplot with points and the 1:1 line
ggplot(dat, aes(x = PPG)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(xintercept = 17.2, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$PPG), color = "black", linetype = "dashed", size = 1) +
  labs(x = 'Points Per Game', y = 'Frequency', title = 'Points Per Game, 2018-2022')


# Create the ggplot with points and the 1:1 line
ggplot(dat, aes(x = PPG)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(xintercept = 17.2, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 22.49, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = mean(dat$PPG), color = "green", linetype = "dashed", size = 1) +
  labs(x = 'Points Per Game', y = 'Frequency', title = 'Points Per Game, 2018-2022')

sd(dat$PPG)

summary(mod)
17.59+0.1020*34.61 + 0.00005112*13309 + 0.689
  