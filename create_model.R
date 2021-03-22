library(randomForest)
library(ggplot2)
library(scales)
library(grid)
library(randomForestExplainer)
library(ggpubr)


source("plotTree.R")

all_stocks = read.csv("../data/eqy_perf.csv")
#all_stocks = read.csv("../data/eqy_transformed.csv")
all_stocks$Date = as.Date(all_stocks$Date)

all_cds = read.csv("../data/cds_spread_hard.csv")
all_cds$Date = as.Date(all_cds$Date)

all_default = read.csv("../data/6m_default_prob_hard.csv")
all_default$Date = as.Date(all_default$Date)

#treasury = read.csv("../data/treasury_yld.csv")
treasury = read.csv("../data/yld_transformed.csv")
treasury$Date = as.Date(treasury$Date)
colnames(treasury) = c("Date", "TreasuryYield")

sentimentData = read.csv("../data/tech_sentiment.csv", col.names = c("Date", "Ticker", "StoryCount", "Sentiment", 
                                                                "Sent_Pos", "Sent_Neut", "Sent_Neg"))
sentimentData$Date = as.Date(sentimentData$Date)
sentimentData$Ticker = as.character(sentimentData$Ticker)
sentimentData$Ticker = gsub( "\\..*", "", sentimentData$Ticker)

source("ratings_data.R")

tickers = colnames(all_stocks)[3:ncol(all_stocks)]


getTickerData <- function(ticker) {
  # Load in data from all files that were extracted from Bloomberg/created with quantmod
  stock = all_stocks[, c("Date", "S5INFT.Index", ticker)]
  stock_tmp = data.frame(Date=stock$Date[2:nrow(stock)], sapply(log(stock[, c(2,3)]), diff))
  stock = data.frame(Date=stock_tmp$Date, Return=stock_tmp[, ticker]-stock_tmp[, "S5INFT.Index"])
  
  data = NA
  tryCatch({
    cds = all_cds[, c("Date", ticker)]
    colnames(cds) = c("Date", "CDS.Spread")
    default = all_default[, c("Date", ticker)]
    colnames(default) = c("Date", "Default.Prob")
    sent = sentimentData[sentimentData$Ticker == ticker, c("Date", "Sentiment")]
    rating = ratings[ratings$Ticker == ticker, ]
    
    # Merge all together and return df
    data = merge(default, stock, by="Date")
    data = merge(data, cds, by="Date")
    data = merge(data, treasury, by="Date")
    data = left_join(data, sent, by="Date")
    data$Sentiment = replace(data$Sentiment, which(is.na(data$Sentiment)), 0)
    if(nrow(rating) > 0 && !all(is.na(rating$SPRating))) {
      #rating = fillRatings(rating)
      rating$RatingChange = c(NA, diff(rating$Rating))
      rating = rating[2:nrow(rating), ]
      rating = addRatingClassifier(rating)
      data = merge(data, rating[, c("Date", "RatingEvent")])
    }
    data = na.exclude(data)
  },
  error = function(e) {
    print(paste(ticker, "not found in data frame, skipping..."))
  })
  return(data)
}


data_summary = function() {
  all_rating = c()
  for(ticker in tickers) {
    rating = ratings[ratings$Ticker == ticker, ]
    if(nrow(rating) > 0 && !all(is.na(rating$SPRating))) {
      #rating = fillRatings(rating)
      rating$RatingChange = c(NA, diff(rating$Rating))
      rating = rating[2:nrow(rating), ]
      rating = addRatingClassifier(rating)
    }
    all_rating = c(all_rating, rating$RatingEvent)
  }
  return(all_rating)
}
all_rating = na.omit(data_summary())


rfModel <- function(ticker) {
  data = getTickerData(ticker)
  if(!(class(data) == "data.frame") || nrow(data) == 0) {
    return(NA)
  }
  
  train_index = round(nrow(data) * 0.7)
  train_data = data[1:train_index, ]
  test_data = data[(train_index+1):nrow(data), ]
  
  model = randomForest(Default.Prob ~ ., data=train_data[,-1], localImp=TRUE)
  test_pred = predict(model, newdata=test_data[,-c(1,2)])
  rmse = sqrt(mean((test_data$Default.Prob - test_pred) ^ 2))
  srmse = rmse / sd(test_pred)
  return(list(model=model, data=data, rmse=rmse, srmse=srmse, preds=test_pred))
}

aapl_RF = rfModel("AAPL")

all_rf_models = list()
for(t in tickers) {
  model = rfModel(t)
  if(!is.na(model)) {
    all_rf_models[[t]] = rfModel(t)
  }
}

rmse_df = data.frame(Ticker = names(all_rf_models))
rmse_df$RMSE = rep(NA, nrow(rmse_df))
rmse_df$SRMSE = rep(NA, nrow(rmse_df))
for(i in 1:nrow(rmse_df)) {
  rmse_df[i, "RMSE"] = all_rf_models[[rmse_df[i, "Ticker"]]]$rmse
  rmse_df[i, "SRMSE"] = all_rf_models[[rmse_df[i, "Ticker"]]]$srmse
}

ggplot(data=rmse_df) + 
  geom_bar(stat="identity", aes(x=Ticker, y=RMSE)) +
  theme(axis.text.x = element_text(angle=90, vjust=-0.03)) +
  labs(title="RMSE for Random Forest Regressions by Ticker")

# Explain why DBD rmse is so high
dbd = merge(all_stocks[, c("Date", "DBD")], all_default[, c("Date", "DBD")], by="Date")
dbd_diff = data.frame(Date=dbd$Date[-1], Default=na.omit(diff(dbd$DBD.y)))
dbd_diff$Return = na.omit(diff(log(dbd$DBD.x)))
ggplot(data=dbd_diff) +
  geom_point(aes(x=Return, y=Default), stat="identity")
  geom_line(aes(x=Date, y=Return)) +
  geom_line(aes(x=Date, y=Default)) +
  scale_x_date(expand=c(0, 0))


tree_func(aapl_RF$model)


plotRelationships = function(ticker) {
  model = all_rf_models[[ticker]]$model
  data = all_rf_models[[ticker]]$data
  
  vs_return = plot_predict_interaction(model, data, "Return", "Default.Prob", main="")
  vs_cds = plot_predict_interaction(model, data, "CDS.Spread", "Default.Prob", main="")
  vs_treas = plot_predict_interaction(model, data, "TreasuryYield", "Default.Prob", main="")
  vs_sent = plot_predict_interaction(model, data, "Sentiment", "Default.Prob", main="")
  plots = c(vs_return, vs_cds, vs_treas, vs_sent)
  
  if("RatingEvent" %in% colnames(data)) {
    vs_rating = plot_predict_interaction(model, data, "RatingEvent", "Default.Prob", main="")
    ggarrange(vs_return, vs_cds, vs_treas, vs_sent, vs_rating,
              labels=c("vs Return (vs S&P 500, %)", "vs CDS Spread (bps)", "vs Treasury Yield (%)", 
                       "vs Sentiment Classifier", "vs Rating Event"), 
              ncol=3, nrow=2)
  } else {
    ggarrange(vs_return, vs_cds, vs_treas, vs_sent, 
              labels=c("vs Return (vs S&P 500, %)", "vs CDS Spread (bps)", "vs Treasury Yield (%)", "vs Sentiment Classifier"), 
              ncol=2, nrow=2)
  }
}

plotRelationships("AAPL")

tree_func(all_rf_models[["AMD"]]$model)
plotRelationships("AMD")

numRatingChanges = sapply(tickers, 
                          function(ticker){ 
                            ifelse(
                              all(is.na(ratings[ratings$Ticker==ticker,])), NA, 
                              sum(sign(abs((ratings[ratings$Ticker==ticker,] %>% fillRatings)$Rating %>% diff))))
                            })
numRatingChanges = numRatingChanges %>% as.data.frame %>% na.exclude
View(t(numRatingChanges))


# Create heat map for variable importance across stocks
var_imp_df = as.data.frame(matrix(ncol=length(all_rf_models)+1, nrow=5))
var_imp_df[, 1] = c("Return", "CDS.Spread", "TreasuryYield", "Sentiment", "RatingEvent")
for(i in 1:length(all_rf_models)) {
  imp = importance(all_rf_models[[i]]$model, type=1)
  most = which(imp == max(imp))[1]
  var_imp_df[, i+1] = 0
  #var_imp_df[most, i+1] = 1
  var_imp_df[1, i+1] = imp[which(rownames(imp)=="Return")]
  var_imp_df[2, i+1] = imp[which(rownames(imp)=="CDS.Spread")]
  var_imp_df[3, i+1] = imp[which(rownames(imp)=="TreasuryYield")]
  var_imp_df[4, i+1] = imp[which(rownames(imp)=="Sentiment")]
  var_imp_df[5, i+1] = ifelse("RatingEvent" %in% rownames(imp), imp[which(rownames(imp)=="RatingEvent")], NA)
}
colnames(var_imp_df) = c("Factor", names(all_rf_models))
imp_melted = melt(var_imp_df, variable.name="Ticker", value.name="VariableImportance")
#imp_melted$MostImportant = as.factor(imp_melted$MostImportant)

ggplot(data=imp_melted, aes(x=Ticker, y=Factor, fill=VariableImportance)) +
  geom_tile() +
  #scale_fill_manual(values=c("firebrick", "green")) +
  theme(axis.text.x = element_text(angle=90, vjust=-0.03)) +
  labs(title="Variable Importance by Ticker")
