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
      firstData <- read_csv("testData.csv")
      names(firstData)[1] <- "label"
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
    datatable(passData3(),
              options = list(
                lengthMenu = c(10, 100, 1500),
                pageLength = 100,
                width = 1000,
                scrollX = "200px",
                scrollY = "700px",
                scrollCollapse = T
              ))
  }) ### DataTableの最終部分
  
  # 全学電量データ
  testData <- read_csv("testData.csv")
  testData2 <- testData %>% mutate(Allcampus = 東キャンパス受電電力量 + 西キャンパス受電電力量)
  names(testData2)[1] <- "label"
  All <- reactive({
    # 日付ラベルを追加
    firstData <- testData2 %>% mutate(date = substr(label, 1, 10))
    secondData <- firstData %>% filter(
      date >= input$theRange[1] & date <= input$theRange[2]
    ) %>% select(-c(date))
    
    # labelの型をPOSIXctに変換
    secondData$label <- as.POSIXct(secondData$label, "%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    return(secondData)
    
  }) ### Allの最終部分
  
  output$target_cluster <- renderUI({
    # 列名labelは必要ないので除外
    Deplist = names(passData()[-1])
    
    selectInput(inputId = "target", label = "クラスタリングしたい項目を指定してください（複数選択不可）",
                Deplist, multiple = F, selected = Deplist[1])
  }) ### selectDepsの最終部分
  
  
  # アイコン
  # 全学電力量の最大値をアイコンとして出力
  output$Max <- renderInfoBox({
    
    infoBox("大学全体の最大電力[kW]", max(All()$Allcampus, na.rm = T), color = "red")
    
  }) ### Maxの最終部分
  
  # 全学電力量の最小値のアイコンとして出力
  output$Min <- renderInfoBox({
    
    infoBox("大学全体の最小電力[kW]", min(All()$Allcampus, na.rm = T), color = "blue")
    
  }) ### Minの最終部分
  
  # 全学電力量の平均電力をアイコンとして出力
  output$Mean <- renderInfoBox({
    
    infoBox("大学全体の平均電力消費[kW]", mean(All()$Allcampus, na.rm = T), color = "green")
    
  }) ### Meanの最終部分
  
  ## トレンドグラフ ###
  output$trendGragh <- renderPlot({
    
    ggplot(passData4(), aes(x = label, y = P_con, color = Deps)) + 
      geom_line() + ylim(input$RangeY[1], input$RangeY[2]) + xlab("時間") + ylab("電力消費量[kWh]") + ggtitle("トレンドグラフ")
    
  }) ### trendGraghの最終部分
  

  # クラスターセンタープロット -----------------------------------------------------------
  
  output$target_cluster <- renderUI({
    # 列名labelは必要ないので除外
    Deplist = names(testData2[-1])
    
    selectInput(inputId = "target", label = "クラスタリングしたい項目を指定してください（複数選択不可）",
                Deplist, multiple = F, selected = Deplist[1])
  }) ### selectDepsの最終部分
  
  # NAがあってはならない
  Data_qqq <- reactive({
    if (!is.null(input$file)) {
      x <- testData2 %>% select(input$target) %>% data.frame()
      
      tx <- testData2 %>% select(label)
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
      ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line()+xlab("time")
      ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+xlab("time")#to change the thickness of line
      ggplot(me,aes(x=Var1,y=value,group=Var2,colour=Var2))+geom_line(size=1.2)+
        xlab("time")+theme(axis.text.x = element_text(angle = 90, hjust = 1))#to rotate the x-axis by 90 degree
      #different colour ko lines athawa points haru plot garna cha vane yeso garne
      mu<-mutate(me,cluster=paste0("cluster",me$Var2))
      
    } else {
      mu <- NULL
    }
    
    return(mu)
  }) ### Data_qqq2の最終部分
  
  # Data_qqq2の出力
  output$Data_qqq <- renderDataTable({
    datatable(Data_qqq2())
  }) ### output$Data_qqqの最終部分
  
  output$qqq <- renderPlot({
    if (!is.null(input$file)) {
      qqq <- ggplot(Data_qqq2(),aes(x=Var1,y=value,group=Var2,color=cluster))+geom_line(size=1.2)+
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("時間") + ylab("電力消費量[kWh]") + ggtitle("クラスタセンター")
      
      print(qqq)
    } else {
      print(NULL)
    }
  }) ### qqqの最終部分
  
}) ###  shinyServerの最終部分
