##############################################################################################################################
#### 建物エネルギーデータ分析ツール server.R #################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(readr)
  library(tcltk)
  library(devtools)
  library(ggTimeSeries)
  library(plyr)
  library(data.table) 
  library(stats) 
  
}
# ブラウザでの立ち上げ
options(shiny.launch.browser = T)


## shinyサーバー ##
shinyServer(function(input, output, session){

  # トレンドグラフ -----------------------------------------------------------------
  # UIの選択によって、読み込むデータを変える
  passData <- reactive({
    if(!is.null(input$file)) {
      firstData <- read_csv(input$file$datapath)
      names(firstData)[1] <- "label"
    } else {
      firstData <- NULL
    }
    
    return(firstData)
  }) ### passDataの最終部分
  
  # カレンダーによる日付範囲の設定
  output$DateRange <- renderUI({
    dateRangeInput(inputId = "theRange", label = "日付範囲を指定してください",
                   start = substr(passData()$label[1], 1, 10),
                   end = substr(passData()$label[nrow(passData())], 1, 10),
                   format = "yyyy-mm-dd"
    )
  }) ### DateRangeの最終部分
  
  # カレンダーの範囲によって、変容するデータ
  passData2 <- reactive({
    # 日付ラベルを追加
    firstData <- passData() %>% mutate(date = substr(label, 1, 10))
    secondData <- firstData %>% filter(
      date >= input$theRange[1] & date <= input$theRange[2]
    ) %>% select(-c(date))
    
    # labelの型をPOSIXctに変換
    secondData$label <- as.POSIXct(secondData$label, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    return(secondData)
    
  }) ### passData2の最終部分
  
  
  output$selectDeps <- renderUI({
    # 列名labelは必要ないので除外
    Deplist = names(passData()[-1])
    
    selectInput(inputId = "theDeps", label = "トレンドグラフに描画する項目を指定してください（複数選択可）",
                Deplist, multiple = T, selected = Deplist[1])
  }) ### selectDepsの最終部分
  
  
  # 選択された部局のみ取り出す
  passData3 <- reactive({
    firstData <- passData2() %>% select(label, input$theDeps)
    
    return(firstData)
  }) ### passData3の最終部分
  
  # ggplot用にデータをgatherで整形しなおす
  passData4 <- reactive({
    firstData <- passData3() %>% tidyr::gather(input$theDeps, key = "Deps", value = "P_con")
    
    return(firstData)
  }) ### passData4の最終部分
  
  # データテーブルのアウトプット
  output$DataTable <- renderDataTable({
    if (!is.null(input$file)) {
      datatable(passData3(),
                options = list(
                  lengthMenu = c(10, 100, 1500),
                  pageLength = 100,
                  width = 1000,
                  scrollX = "200px",
                  scrollY = "700px",
                  scrollCollapse = T
                ))
    } else {
      print(NULL)
    }
    
    
  }) ### DataTableの最終部分
  
  
  output$target_cluster <- renderUI({
    # 列名labelは必要ないので除外
    Deplist = names(passData()[-1])
    
    selectInput(inputId = "target", label = "クラスタリングしたい項目を指定してください（複数選択不可）",
                Deplist, multiple = F, selected = Deplist[1])
  }) ### selectDepsの最終部分
  
  
  # 選択された部局のみ取り出す
  targetData <- reactive({
    firstData <- passData2() %>% select(label, input$target)
    
    return(firstData)
  }) ### passData3の最終部分
  
  # アイコン
  # 全学電力量の最大値をアイコンとして出力
  output$Max <- renderInfoBox({
    if (!is.null(input$file)) {
      infoBox("選択系列の最大値", max(targetData()[[2]], na.rm = T), color = "red")
    } else {
      infoBox("選択系列の最大値", NULL, color = "red")
    }
    
  }) ### Maxの最終部分
  
  # 全学電力量の最小値のアイコンとして出力
  output$Min <- renderInfoBox({
    if (!is.null(input$file)) {
      infoBox("選択系列の最小値", min(targetData()[[2]], na.rm = T), color = "blue")
    } else {
      infoBox("選択系列の最小値", NULL, color = "blue")
    }
    
  }) ### Minの最終部分
  
  # 全学電力量の平均電力をアイコンとして出力
  output$Mean <- renderInfoBox({
    if (!is.null(input$file)) {
      infoBox("選択系列の平均値", mean(targetData()[[2]], na.rm = T), color = "green")
    } else {
      infoBox("選択系列の平均値", NULL, color = "green")
    }
    
  }) ### Meanの最終部分
  
  ## トレンドグラフ ###
  output$trendGragh <- renderPlot({
    if (!is.null(input$file)) {
      validate(
        need(input$theDeps != "", "項目を選択してください")
      )
      
      ggplot(passData4(), aes(x = label, y = P_con, color = Deps)) + 
        geom_line() + ylim(input$RangeY[1], input$RangeY[2]) + xlab("時間") + ylab("電力消費量[kWhh]") + ggtitle("トレンドグラフ")
    } else {
      print(NULL)
    }
    
    
  }) ### trendGraghの最終部分
  

  # クラスターセンタープロット -----------------------------------------------------------
  
  
  # NAがあってはならない
  Data_qqq <- reactive({
    if (!is.null(input$file)) {
      x <- passData() %>% select(input$target) %>% data.frame()
      judge <- which(is.na(x))
      
      validate(
        need(length(judge) == 0, "データの中に欠損値が含まれているので計算できません")
      )
      
      tx <- passData() %>% select(label)
      tx <- strptime(unlist(tx), "%Y-%m-%d %H:%M:%S")
      time <- format(tx, "%H:%M:%S")
      date <- format(tx, "%Y-%m-%d")
      date.day <- levels(factor(date))
      hour <- levels(factor(time))
      lab.date <- list(date.day, hour)
      y <- matrix(x[1:nrow(x),], ncol=24, byrow=TRUE, dimnames = lab.date)
      
      inital.v <- apply(y,2, quantile, seq(0,1,1/6))
      
      kmean.y <- kmeans(y, inital.v[2:7,])
      centers <- kmean.y$centers
    } else {
      centers <- NULL
    }
    
    return(centers)
  }) ### Data_qqqの最終部分
  
  
  Data_qqq2 <- reactive({
    if (!is.null(input$file)) {
      tr <- t(Data_qqq())
      me <- melt(tr)
      mu<-mutate(me,cluster=paste0("cluster",me$Var2))
      
    } else {
      mu <- NULL
    }
    
    return(mu)
  }) ### Data_qqq2の最終部分
  
  # クラスタセンタープロット
  output$qqq <- renderPlot({
    if (!is.null(input$file)) {
      qqq <- ggplot(Data_qqq2(),aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("時間") + ylab("電力消費量[kWhh]") + ggtitle("クラスタセンター")
      
      print(qqq)
    } else {
      print(NULL)
    }
  }) ### qqqの最終部分
  
}) ###  shinyServerの最終部分
