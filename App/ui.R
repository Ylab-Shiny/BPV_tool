##############################################################################################################################
#### 建物エネルギーデータ分析ツール ui.R #####################################################################################
##############################################################################################################################

# パッケージ一覧 -----------------------------------------------------------------
library(shiny)
library(shinydashboard)

# Webページ構成要素 --------------------------------------------------------------
# header #
header <- dashboardHeader(title = "建物エネルギーデータ分析ツール", titleWidth = 500)

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("データセット[kWh]", tabName = "table"),
    menuItem("トレンドグラフ", tabName = "trend", badgeColor = "red"),
    menuItem("クラスタリング", tabName = "clustering")
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "csvファイルをアップロードしてください",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # カレンダーの出力
  uiOutput("DateRange"),
  
  # トレンドグラフに描画する項目選択
  uiOutput("selectDeps"),
  
  sliderInput(inputId = "RangeY", label = "Y軸（電力消費[kW]）の範囲をを指定してください",
              min = 0, max = 4000, value = c(0, 4000), step = 50),
  
  # クラスタリングの対象とする項目の選択
  uiOutput("target_cluster")
  
)

# body #
body <- dashboardBody(
  
  tabItems(
    tabItem(
      h1("ようこそ『建物エネルギーデータ分析ツール』へ"),
      tabName = "table",
      DT::dataTableOutput("DataTable")),
    
    tabItem(tabName = "trend",
            
            # トレンドグラフの描画
            plotOutput("trendGragh")
            ),
    
    tabItem(tabName = "clustering",
            
            # アイコン
            infoBoxOutput(width = 3, "Max"),
            infoBoxOutput(width = 3, "Min"),
            infoBoxOutput(width = 3, "Mean"),
            # クラスタセンターの描画
            plotOutput("qqq")
            )
    
  )
)


# 構成要素の組み立て ---------------------------------------------------------------
dashboardPage(header, sidebar, body)