##############################################################################################################################
#### 建物エネルギーデータ分析ツール ui.R #####################################################################################
##############################################################################################################################
# ライブラリ一覧
{
  library(shiny)
  library(shinydashboard)
}

### 構成要素 ###
# header #
header <- dashboardHeader(title = "建物エネルギーデータ分析ツール", titleWidth = 500) ### headerの最終部分

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("データセット[kW]", tabName = "table"),
    menuItem("トレンドグラフ", tabName = "trend", badgeLabel = "ロード遅め", badgeColor = "red"),
    menuItem("クラスタリング", tabName = "clustering")
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # 1行改行
  br(),
  
  # カレンダーの出力
  uiOutput("DateRange"),
  
  # 1行改行
  br(),
  
  # トレンドグラフに描画する項目選択
  uiOutput("selectDeps"),
  
  # 1行改行
  br(),
  
  # クラスタリングの対象とする項目の選択
  uiOutput("target_cluster"),
  
  # 1行改行
  br(),
  
  sliderInput(inputId = "RangeY", label = "Y軸（電力消費[kW]）の範囲をを指定してください",
              min = 0, max = 4000, value = c(0, 4000), step = 50)
  
) ### sidebarの最終部分

# body #
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "table",
            dataTableOutput("DataTable")),
    
    tabItem(tabName = "trend",
            shiny::fluidRow(
              infoBoxOutput(width = 3, "Max"),
              infoBoxOutput(width = 3, "Min"),
              infoBoxOutput(width = 3, "Mean"),
              
              # トレンドグラフの描画
              plotOutput("trendGragh")
            )),
    
    tabItem(tabName = "clustering",
            # クラスタセンターの描画
            plotOutput("qqq"),
            # カレンダープロットの描画
            plotOutput("CalenderPlot"),
            dataTableOutput("Data_calender")
            )
    
  )
) ### bodyの最終部分

## 組み立て ##
dashboardPage(header, sidebar, body)