# Draft History Construction
draft <- read.csv('draftHistory.csv')
shortppg <- read.csv("shortppg.csv")
pickValue <- read.csv("pickValue.csv")

cat(isOffense(draft[1,])== 1)
cat(isOffense(draft[1,]) + isOffense(draft[2,])== 1)
cat(isOffense(draft[2,]) + isOffense(draft[3,])== 0)
cat(isOffense(draft[1,]) + isOffense(draft[4,])== 2)

isOffense <- function(row) {
  offensePositions = c("OL", "C", "G", "QB", "RB", "TE", "WR")
  if(row$Position %in% offensePositions) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# takes a dataframe and returns the number of rounds they had picks
cat(roundExists(draft, 8) == 0)
cat(roundExists(draft[1,], 1) == 1)
cat(roundSubset(draft[1:3,], 2) == 2)
cat(roundSubset(draft[1:3,], 2) == 4)

roundExists(draft[1,], 1) == TRUE
roundExists(draft[1,], 2) == FALSE
roundExists(draft[1:8,], 8) == FALSE
roundExists(draft[1:8,], 2) == TRUE

roundExists <- function(df, rd) {
  for(i in 1:nrow(df)) {
    if(df[i,]$Round == rd) {
      return(TRUE)
    } else {
      next
    }
  }
  return(FALSE)
}

roundSubset <- function(df, rd) {
  if(roundExists(df, rd)){
    return(
      df[df$Round == rd,])
    }
}

cat(roundValue(draft[26:27,], 4) ==1063)
roundValue <- function(df, rd) {
  value = 0
  for(i in 1:nrow(df)){
    if(isOffense(df[i,])) {
      value = value + pickValue[as.numeric(as.character(df[i,]$Selection)),]$Value
    } else {
      value = value
    }
  }
  return(value)
}

#Take a dataframe and return the values of the first four rounds
totalRoundValues(draft[1:7,], 1)
totalRoundValues <- function(df, round) {
  # subset into the first year
  a = list()
  for(i in 1:round){
    if(roundExists(df,i)){
      d <- roundSubset(df,i)
      e <- roundValue(d)
      a <- append(a, e)
    }
    else {
      a <- append(a, 0)
    }
  }
  return(a)
}

#takes in a dataframe, returns the draft from that year
cat(nrow(subsetYearTeam(draft, "Pittsburgh Steelers", 2023, 4)) == 5)

subsetYearTeam <- function(df, team, year, numberOfRounds) {
  df1 <- subset(df, df$Year == year)
  df1 <- df1[df1$Team == team,]
  df1 <- df1[df1$Round <= numberOfRounds, ]
  return(df1)
}

returnYearValue(draft, shortppg[2,], 5)
returnYearValue <- function(df, row, rounds) {
  a = list()
  for(i in 0:4){
  d <- subsetYearTeam(df, as.character(row$TEAM), row$YEAR -1 -i, rounds)
  b <- totalRoundValues(d, rounds)
  for(k in b){
    a = append(a, k)
  }
  }
  return(a)
}

addValueColumns <- function(row) {
  a <- returnYearValue(draft, row, 7)
  
  for (i in 1:length(a)) {
    row[[paste0("selection", i)]] <- as.numeric(a[i])
  }
  
  return(row)
}

addDraftColumns <- function(df) {
  for (i in 1:35) {
    df[[paste0("selection", i)]] <- NA
  }
  
  for(i in 1:nrow(df)) {
    df[i,] <- addValueColumns(df[i,])
  }
  return(df)
}

