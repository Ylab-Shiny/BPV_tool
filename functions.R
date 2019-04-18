# 外れ値処理の関数化 ---------------------------------------------------------------------
# 必要なパッケージ
library(dplyr)

repOutliersNA <- function(Column) {
  Column <- tbl_df(Column)
  xx1 <- Column %>% mutate(
    Norm = (Column-min(Column, na.rm = T)) / (max(Column, na.rm = T)-min(Column, na.rm = T)) * (1-0) + 0
  )
  qq <- data.frame(quantile(xx1[[2]],c(0.25, 0.75), na.rm = T))
  Q1 <- qq[1, 1]
  Q3 <- qq[2, 1]
  
  outer_l_Q1 <- Q1 - 1.5 * (Q3 - Q1)
  outer_m_Q3 <- Q3 + 3 * (Q3 - Q1)
  outer_ll <- which(xx1$Norm < outer_l_Q1)
  outer_mm <- which(xx1$Norm > outer_m_Q3)
  
  # 重複なく昇順に行番号を抽出
  row_num_out <- unique(c(outer_ll, outer_mm)) %>% sort()
  
  # 外れ値の出力
  outer_outlier <- cbind.data.frame(Column[row_num_out,], row_number = row_num_out)
  Column_removeOutliers <- Column
  Column_removeOutliers[outer_outlier$row_number, 1] <- NA
  
  
  return(data.frame(Column_removeOutliers))
}

# 補完の関数化 ---------------------------------------------------------------------
# 必要なパッケージ
library(dplyr)
library(tseries)
library(forecast)
library(norm2)

impPrediction <- function(BivariateDataframe, season) {
  originalLabel <- BivariateDataframe[[1]]
  original_colname <- colnames(BivariateDataframe)
  dateList <- substr(originalLabel, 1, 10) %>% unique()
  MissingData <- BivariateDataframe[which(is.na(BivariateDataframe[[2]])), ]
  targetDate <- substr(MissingData[[1]], 1, 10) %>% unique() %>% as.Date()
  
  for (i in 4:length(dateList)) {
    if(i == 4) {
      
      theDate <- as.Date(dateList[i])
      endDate <- theDate - 1
      startDate <- endDate - 2
      
      dataset <- BivariateDataframe
      names(dataset) <- c("label", "value")
      trainingData <- dataset[1:(season*3), ]
      tsTrain <- ts(trainingData$value, start = 1, frequency = season)
      n_temp <- data.frame(trainingData)
      
      if(theDate %in% targetDate) {
        Model <- auto.arima(tsTrain, ic="aic", trace = F, stepwise = F, approximation = F, allowmean = F, allowdrift = F)
        Model.pred <- predict(Model, 24)
        pred <- Model.pred$pred
        
        Missing <- dataset %>% mutate(Date = substr(label, 1, 10)) %>% filter(Date == theDate)
        Miss_value <- Missing$value
        
        impX <- rep(NA, length(Miss_value))
        for (j in 1:length(Miss_value)) {
          if(is.na(Miss_value[j])) {
            impX[j] <- pred[j]
          } else {
            impX[j] <- Miss_value[j]
          }
        }
        values <- data.frame(label = Missing$label, value = impX)
        
        n_temp <- rbind(n_temp, values)
        theDate <- theDate + 1
        trainingData <- rbind(trainingData, values)
        trainingData <- trainingData[-(1:season),]
        
      } else {
        values <- dataset %>% mutate(Date = substr(dataset$label, 1, 10)) %>% 
          filter(Date == theDate)
        values <- data.frame(label = values$label, value = values$value)
        
        n_temp <- rbind(n_temp, values)
        theDate <- theDate + 1
        trainingData <- rbind(trainingData, values)
        trainingData <- trainingData[-(1:season),]
      }
    } 
    
    else if(theDate %in% targetDate) {
      tsTrain <- ts(trainingData$value, start = 1, frequency = season)
      Model <- auto.arima(tsTrain, ic="aic", trace = F, stepwise = F, approximation = F, allowmean = F, allowdrift = F)
      Model.pred <- predict(Model, 24)
      pred <- Model.pred$pred
      
      Missing <- dataset %>% mutate(Date = substr(dataset$label, 1, 10)) %>% 
        filter(Date == theDate)
      Miss_value <- Missing$value
      
      impX <- rep(NA, length(Miss_value))
      for (j in 1:length(Miss_value)) {
        if(is.na(Miss_value[j])) {
          impX[j] <- pred[j]
        } else {
          impX[j] <- Miss_value[j]
        }
      }
      values <- data.frame(label = Missing$label, value = impX)
      
      n_temp <- rbind(n_temp, values)
      theDate <- theDate + 1
      trainingData <- rbind(trainingData, values)
      trainingData <- trainingData[-(1:season),]
    }
    
    else {
      values <- dataset %>% mutate(Date = substr(dataset[[1]], 1, 10)) %>% 
        filter(Date == theDate)
      values <- data.frame(label = values$label, value = values$value)
      
      n_temp <- rbind(n_temp, values)
      theDate <- theDate + 1
      trainingData <- rbind(trainingData, values)
      trainingData <- trainingData[-(1:season),]
    }
    
  } 
  
  result <- n_temp
  names(result) <- original_colname
  
  return(result)
  
}
