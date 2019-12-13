library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(scales)
library(leaflet)
library(shiny)
library(ggvis)

# radarchart
library(fmsb)
#remotes::install_github("gusef/d3Toolbox")
#require(d3Toolbox)


load("data/steam.tag.table.RData")
load("data/steam.tag_update.RData")

load("data/text_cor.RData")
load("data/word_cloud.RData")


#-----------------------------------------
# Ui
#-----------------------------------------
ui <- fluidPage(
    
    navbarPage("스팀", id = "nav",
               #-----------------------------------------
               # 통계요약 탭
               #-----------------------------------------
               tabPanel("통계요약",
                        
                        sidebarLayout(
                            #-----------------------------------------
                            # 통계요약 탭::input
                            #-----------------------------------------
                            sidebarPanel(
                                conditionalPanel(
                                    'input.mainplot1 === "그래프"',
                                    # 그래프
                                    selectInput(inputId = "plot_type",
                                                label = "보시고 싶은 정보",
                                                choices = c("년도별 출시 빈도" = "release_by_year",
                                                            "좋아요/싫어요" = "positive_negative",
                                                            "장르별 예상매출량" = "sales_by_year"
                                                )),
                                    
                                    # type1 
                                    conditionalPanel(
                                        "input.plot_type != 'positive_negative'",
                                        # 장르
                                        selectInput(inputId = "genres",
                                                    label = "장르 선택",
                                                    choices = c("Action", "RPG", "Indie",
                                                                "Strategy", "Simulation", "Casual",
                                                                "Sports", "Racing"),
                                                    multiple = T,
                                                    selected = c("Action", "Indie", "RPG", "Simulation")),
                                        # 날짜
                                        dateRangeInput(inputId = "DateRange",
                                                       label = "날짜",
                                                       start = "2006-01-01",
                                                       end = "2019-04-22"),
                                        # 랭킹 기준
                                        selectInput(inputId = "rank_criterion",
                                                    label = "랭킹 기준",
                                                    choices = c("인기순" = "total_vote",
                                                                "긍정 수" = "positive_ratings",
                                                                "부정 수" = "negative_ratings",
                                                                "예상 매출액" = "pred_sales")),
                                        # 상위
                                        sliderInput(inputId = "ranking",
                                                    label = "상위 몇 퍼?",
                                                    min = 0,
                                                    max = 100,
                                                    value = c(0,100))
                                    ),
                                    
                                    conditionalPanel(
                                        "input.plot_type == 'positive_negative'",
                                        # 장르
                                        checkboxGroupInput(inputId = "genres1",
                                                    label = "장르 선택",
                                                    choices = c("Action",  "RPG", "Indie",
                                                                "Strategy", "Simulation", "Casual",
                                                                "Sports", "Racing"),
                                                    selected = c("Action", "Indie", "RPG")),
                                        # 랭킹 기준
                                        selectInput(inputId = "rank_criterion1",
                                                    label = "랭킹 기준",
                                                    choices = c("인기순" = "total_vote",
                                                                "긍정 수" = "positive_ratings",
                                                                "부정 수" = "negative_ratings",
                                                                "예상 매출액" = "pred_sales")),
                                        # 상위
                                        sliderInput(inputId = "ranking1",
                                                    label = "상위 몇 퍼?",
                                                    min = 0,
                                                    max = 100,
                                                    value = c(45,50))
                                        )

                                    

                                ),
                                conditionalPanel(
                                    'input.mainplot1 === "GamePlot222"'
                                ),
                                
                                
                                width = 3
                            ),
                            #-----------------------------------------
                            # 통계요약 탭::output
                            #-----------------------------------------
                            mainPanel(
                                tabsetPanel(
                                    id = "mainplot1",
                                    tabPanel(
                                        "그래프" , 
                                        # conditionalPanel(
                                        #     "input.plot_type == 'release_by_year'",
                                        #     plotOutput(outputId = "GamePlot", width = "100%", height = 600)
                                        # ),
                                        # conditionalPanel(
                                        #     "input.plot_type == 'positive_negative'",
                                        #     ggvisOutput("p")
                                        # ),
                                        # conditionalPanel(
                                        #     "input.plot_type == 'sales_by_year'",
                                        #     plotOutput(outputId = "GamePlot", width = "100%", height = 600)
                                        # ),
                                        conditionalPanel(
                                            "input.plot_type == 'positive_negative'",
                                            ggvisOutput("p")
                                        ),
                                        # conditionalPanel(
                                        #     "input.plot_type != 'positive_negative'",
                                        #     plotOutput(outputId = "GamePlot", width = "100%", height = 600)
                                        # )
                                        plotOutput(outputId = "GamePlot", width = "100%", height = 600)
                                        # d3BarplotOutput(outputId = "GamePlot", width = "100%", height = 600)
                                    ),
                                    tabPanel(
                                        "표", DT::dataTableOutput(outputId = "GameData")
                                    )
                                ),
                                
                                width = 9
                            )
                        )
               ),
               
               #-----------------------------------------
               # 추천분석 탭
               #-----------------------------------------
               tabPanel("추천분석",
                        sidebarLayout(
                            #-----------------------------------------
                            # 추천분석 탭::input
                            #-----------------------------------------
                            sidebarPanel(
                                selectInput(inputId = "tag",
                                            label = "만들고 싶은 게임 성향(장르 포함)",
                                            choices = steam.tag[,-1] %>% colnames(),
                                            multiple = T)
                                
                            ),
                            
                            #-----------------------------------------
                            # 추천분석 탭::output
                            #-----------------------------------------
                            mainPanel(
                                verbatimTextOutput(outputId = "recommend_game_text"),
                                # includeHTML("test.html"),
                                plotOutput(outputId = "radar")
                                # DT::dataTableOutput(outputId = "recommend_game_data")
                            )
                            
                            
                            
                        )
               )
               
    )
)

#-----------------------------------------
# 서버
#-----------------------------------------
server <- function(input, output) {
    #-----------------------------------------
    # 데이터 필터링
    #-----------------------------------------
    sample_data = reactive({
        # 장르 필터
        filter_idx = ifelse(data.focus %>% select(input$genres) %>% rowSums == 0, FALSE, TRUE)
        
        # 시간 필터
        temp = data.focus[filter_idx,] %>% 
            filter(release_date >= input$DateRange[1], 
                   input$DateRange[2] >= release_date)
        
        # 랭크 기준 필터
        temp = temp %>% 
            mutate(rank = rank(temp %>% select_(input$rank_criterion) %>% -., ties.method = "min")) %>% 
            arrange(rank)
        
        
        # 랭크 순위 필터
        rank_threshold = input$ranking * (max(temp$rank) / 100)
        temp %>% filter(rank_threshold[1]  <= rank,
                        rank_threshold[2]  >= rank)
    })
    
    sample_data1 = reactive({
        # 장르 필터
        filter_idx = ifelse(data.focus %>% select(input$genres1) %>% rowSums == 0, FALSE, TRUE)
        
        
        # 랭크 기준 필터
        temp = data.focus[filter_idx,] %>% 
            mutate(rank = rank(data.focus[filter_idx,] %>% select_(input$rank_criterion1) %>% -., ties.method = "min")) %>% 
            arrange(rank)
        
        
        # 랭크 순위 필터
        rank_threshold = input$ranking1 * (max(temp$rank) / 100)
        temp %>% filter(rank_threshold[1]  <= rank,
                        rank_threshold[2]  >= rank)
    })
    
    
    #-----------------------------------------
    # 통계요약 테이블
    #-----------------------------------------
    output$GameData = DT::renderDataTable({
        sample_data() %>% select(name, release_date, developer, genres, steamspy_tags,
                                 positive_ratings, negative_ratings, average_playtime_h,
                                 owners, price)
    })
    
    
    
    
    #-----------------------------------------
    # 통계요약 테이블 그래프
    #-----------------------------------------
    output$GamePlot <- renderPlot({
        # 테마 및 plot 세팅 통일 (글씨 크기 등)
        theme_setting = theme(axis.text.x = element_text(face = "bold", size = 15),
                              axis.text = element_text(size = 15, face = "bold"),
                              axis.title = element_text(size = 20, face = "bold"),
                              legend.text = element_text(size = 13, face = "bold"),
                              legend.title = element_text(size = 15, face = "bold"))
        
        # 출시 빈도
        if (input$plot_type == "release_by_year"){
            sample_data() %>%
                # 집계
                group_by(year) %>%
                summarise(Indie = sum(Indie),
                          Strategy = sum(Strategy),
                          RPG = sum(RPG),
                          Simulation = sum(Simulation),
                          Casual = sum(Casual),
                          Sports = sum(Sports),
                          Racing = sum(Racing),
                          Action = sum(Action)) %>%
                # 데이터 변환
                gather(key = "genre", value = "value", -1) %>%
                # 장르 필터
                filter(genre %in% input$genres) %>%
                # 시각화
                ggplot( aes( x = factor(year),
                             y = value,
                             group = genre,
                             fill = genre)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label=ifelse(value>20, value,"")),
                          position = position_stack(vjust=0.5)) +
                # 커스텀
                labs(x="년도", y="출시 수(개)") +
                scale_y_continuous(labels = comma) +
                theme_setting
        }
        # 긍정/부정 비율
        else if (input$plot_type == "positive_negative"){
            games = sample_data1()
            
            # 데이터 비중
            games = games %>% mutate(genre = genres %>% str_split(";") %>% lapply(function(x){x[1]}) %>% unlist)
            # 데이터 필터
            games = games %>% filter(genre %in% input$genres1)
            games %>%
                ggvis(x = ~like_rate, y = ~total_like_unlike) %>%
                #scale_numeric("y", trans="log") %>%
                layer_points(size := 50, size.hover := 200,
                             fillOpacity := 0.2, fillOpacity.hover := 0.5, key := ~appid,
                             stroke = ~genre) %>%
                add_tooltip(point_tooltip, "hover") %>%
                add_axis("x", title = "긍정 리뷰 비율 (%)") %>%
                add_axis("y", title = "총 리뷰 수 (개)") %>%
                set_options(width = 700, height = 500) %>%
                scale_nominal("stroke",
                              range = c("#CC0000", "#aaa", "orange", "#009900", "#0000FF",
                                        "#9900CC", "#FF00FF", "#000000", "#330033")) %>%
                bind_shiny("p")
        }
        # 년도별 매출량
        else if (input$plot_type == "sales_by_year"){
            sample_data() %>%
                mutate(Action_sales = Action * pred_sales,
                       Indie_sales = Indie * pred_sales,
                       Strategy_sales = Strategy * pred_sales,
                       RPG_sales = RPG * pred_sales,
                       Simulation_sales = Simulation * pred_sales,
                       Casual_sales = Casual * pred_sales,
                       Sports_sales = Sports * pred_sales,
                       Racing_sales = Racing * pred_sales) %>%
                group_by(year) %>%
                summarise(Action = sum(Action_sales),
                          Indie = sum(Indie_sales),
                          Strategy = sum(Strategy_sales),
                          RPG = sum(RPG_sales),
                          Simulation = sum(Simulation_sales),
                          Casual = sum(Casual_sales),
                          Sports = sum(Sports_sales),
                          Racing = sum(Racing_sales)) %>%
                gather(key = "genre",
                       value = "value",-1) %>%
                filter(genre %in% input$genres) %>%
                # 단위 환산
                mutate(value = round(value/1000000,2)) %>%
                # 시각화
                ggplot(aes(x = factor(year),
                           y = value,
                           group = genre,
                           fill = genre)) +
                geom_bar(stat="identity") +
                geom_text(aes(label=ifelse(value>200, paste0("$",round(value,0)),"")),
                          position = position_stack(vjust=0.5)) +
                # 커스텀
                labs(x="년도", y="출시 수(백만 달러)") +
                scale_y_continuous(label = dollar) +
                theme(axis.text.x = element_text(face = "bold", size = 15),
                      axis.text = element_text(size = 15, face = "bold"),
                      axis.title = element_text(size = 20, face = "bold"),
                      legend.text = element_text(size = 13, face = "bold"),
                      legend.title = element_text(size = 15, face = "bold"))
        }
    })
    
    # Function for generating tooltip text
    point_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        if (is.null(x$appid)) return(NULL)
        
        # Pick out the game with this ID
        all_games <- isolate(sample_data())
        game <- all_games[all_games$appid == x$appid, ]
        
        paste0("<b>", game$name, "</b><br>",
               "출시일: ", game$release_date, "<br>",
               "긍정 리뷰 수: ", game$positive_ratings, "개 <br>",
               "부정 리뷰 수: ", game$negative_ratings, "개 <br>",
               "가격: $", format(game$price, big.mark = ",", scientific = FALSE)
        )
    }
    
    #-----------------------------------------
    # 추천분석 테이블 그래프
    #-----------------------------------------
    tag_data = reactive({
        if(!is.null(input$tag)){
            temp = steam.tag %>% 
                select_("appid", "input$tag") 
            temp = temp %>% mutate(total = temp[,-1] %>% rowSums(),
                                   mean = apply(temp[,-1],1,mean),
                                   var = apply(temp[,-1],1, var) )
            # 인기 순 # 내가 선택한 태그들이 골고루 들어있어야 함
            temp = temp %>% arrange(-mean,var,-total)
        }
        
    })
    
    output$recommend_game_text = renderPrint({
        top10 = tag_data()$appid %>% head(3)
        top10_name = data.focus %>% filter(appid %in% top10) %>% .$name %>% as.character()
        top10_name
    })
    
    
    output$radar = renderPlot({
        dd = tag_data() %>% select(-mean, -var, -total) %>% head(3)
        dd = as.data.frame(dd, row.names = dd$appid) # 행 이름으로 만들자
        dd = dd[,-1] # 행이름 변수 삭제
        
        dd = rbind(apply(dd,2,max), apply(dd,2,min), dd)
        
        # color
        colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
        colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
        
        # chart
        radarchart(dd, axistype = 1,
                   pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                   cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                   vlcex=0.8)
    })
    
    # 디버깅용
    output$recommend_game_data = DT::renderDataTable({
        tag_data()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
