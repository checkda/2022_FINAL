#  :pushpin: *2022년 12월 06일_기말고사_2022712258_서혜경*     

## Q1. Table 1 만들기(15점)
> 데이터(dt)의 연도 별 주어진 변수의 기술통계량을 Table로 나타내어라.  
> 단, 변수 Q_SMK_YN과 Q_HBV_AG는 범주형으로. <br/> 
> 주어진 변수 : “HGHT”, “WGHT”, “BMI”, “HDL”, “LDL”, “Q_SMK_YN”, “Q_HBV_AG” (총 7개) 

```R
# 필요한 library 및 데이터 불러오기
library(tableone);library(data.table);library(magrittr);library(survival);library(jstable)

url <- "https://raw.githubusercontent.com/jinseob2kim/lecture-snuhlab/master/data/example_g1e.csv"
dt <- fread(url, header=T)

# dt의 1~6행 살펴보기
head(dt)


# myVars : 주어진 변수 추출
myVars <- c("HGHT","WGHT","BMI","HDL","LDL", "Q_SMK_YN","Q_HBV_AG")

# catVars : 주어진 변수 중 범주형 변수(categorical variables) 추출
catVars <- c("Q_SMK_YN","Q_HBV_AG")

# 첫 번째 방법 : 범주형으로 미리 변환하지 않고 'factorVars'로 지정
CreateTableOne(vars= myVars, factorVars = catVars, strata= "EXMD_BZ_YYYY", data= dt)
```
<img width="404" alt="Q1_1" src="https://user-images.githubusercontent.com/112446994/205484766-ea890eee-d06d-42ab-b84c-f3857616fd29.png">

<img width="431" alt="Q1_2" src="https://user-images.githubusercontent.com/112446994/205484773-1462ab8e-1d1d-49f8-bab2-1cad0e45955f.png">


## Q2. 선형회귀, 로지스틱, 콕스생존분석 Table 만들기(15점)
### 2-1. 선형 회귀분석(Linear regression)(5점)
> survival 패키지에 내장되어 있는 대장암 데이터 ‘colon’ 사용 <br/> 
> time ~ rx + age + sex 선형회귀 실행 후 table로 나타내어라.

```R
# colon의 1~6행 살펴보기
head(colon)

res.reg <- glm(time ~ rx + age + sex, data = colon)
tb.reg <- glmshow.display(res.reg)     # 'jstable 패키지의 glmshow.display' 이용
knitr::kable(tb.reg$table, caption = tb.reg$first.line)
```
### 2-2. 로지스틱 회귀분석(Logistic regression)(5점)
> 마찬가지로 대장암 데이터 ‘colon’ 사용 <br/>
> status ~ rx + age + sex 로지스틱 회귀 실행 후 table로 나타내어라.

```R
res.logistic <- glm(status ~ rx + age + sex, data = colon, family = binomial)
tb.logistic <- glmshow.display(res.logistic)   # 'jstable 패키지의 glmshow.display' 이용
knitr::kable(tb.logistic$table, caption = tb.logistic$first.line)
```

![q2](q2.png)
### 2-3. 콕스 생존분석(Cox proportional hazard)(5점)
> Surv(time, status) ~ rx + age + sex 실행 후 table로 나태내어라.

```R
res.cox <- coxph(Surv(time, status) ~ rx + age + sex, data = colon, model = T)
tb.cox <- cox2.display(res.cox)   # 'jstable 패키지의 cox2.display' 이용
knitr::kable(tb.cox$table, caption = tb.cox$caption)
```

## Q3. 샤이니 웹 앱 만든 후 ShinyApps.io 배포하기(10점)
> 지난 주 강의 때 실습한 NBA 2018/19 시즌 스탯 혹은 직접 제작한 샤이니 웹 앱 제작하기 <br/>
> ShinyApps.io를 통해 배포한 후, 생성된 url도 함께 제출하시오.

```R
# 참고용 예시 코드

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

players <- read.csv("data/nba2018.csv")

ui <- fluidPage(
  titlePanel("NBA 2018/19 Player Stats"),
  sidebarLayout(
    sidebarPanel(
      "Exploring all player stats from the NBA 2018/19 season",
      h3("Filters"),
      sliderInput(
        inputId = "VORP",
        label = "Player VORP rating at least",
        min = -3, max = 10,
        value = 0
      ),
      selectInput(
        "Team", "Team",
        unique(players$Team),
        selected = "Golden State Warriors"
      )
    ),
    mainPanel(
      strong(
        "There are",
        textOutput("num_players", inline = TRUE),
        "players in the dataset"
      ),
      plotOutput("nba_plot"),
      DTOutput("players_data")
    )
  )
)

server <- function(input, output, session) {

  output$players_data <- renderDT({
    data <- players %>%
      filter(VORP >= input$VORP,
             Team %in% input$Team)

    data
  })

  output$num_players <- renderText({
    data <- players %>%
      filter(VORP >= input$VORP,
             Team %in% input$Team)

    nrow(data)
  })

  output$nba_plot <- renderPlot({
    data <- players %>%
      filter(VORP >= input$VORP,
             Team %in% input$Team)

    ggplot(data, aes(Salary)) +
      geom_histogram()
  })

}

shinyApp(ui, server)
```
![q3](q3.png)
