# libs ----
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(mirt)
library(CTT)
library(randomNames)
library(echarts4r)

# data manipulation ----
# 95 items, 2460 respondents
items_full_tbl <- read_csv("lenguages_human_science_example.csv")

# only items
items_tbl <- items_full_tbl %>% 
  dplyr::select(starts_with("item"))

# only students
student_vect_ids <- items_full_tbl %>%
  pluck(2)

n_students <- length(student_vect_ids)

# given random names
set.seed(6232015)
gen_names <- function(n){
  names <- unique(randomNames(n=n, which.names = "first"))
  need <- n - length(names)
  while(need > 0){ 
    names <- unique(c(randomNames(n=need, which.names = "first"), names))
    need <- n - length(names)
  }
  return(names)
}
student_vect <- gen_names(n = n_students)

# activity code
activity_code <- items_full_tbl %>% 
  pluck(1)

# running 3PL model
threePL_model <- mirt(
  items_tbl, 
  1,          
  itemtype = '3PL')

# items name
items_name <- names(items_tbl)
items_name_tag <- append(items_name, "all")

# latent scores
latent_answers <- as.vector(fscores(threePL_model))

# IRT coefficients
irt_coefs <- coef(threePL_model, simplify = T, IRTpars =T)

# New Scores
new_score <- score.transform(
  latent_answers, mu.new = 500, sd.new = 100,
  normalize = FALSE
)

# data transformations
# items_name <- names(items_tbl)

# irt coefs tibble
irt_coefs_tbl <- irt_coefs$items %>% 
  as_tibble()

# max len of rows
max_n <- dim(items_tbl[][1])[1]

# putting together some informations
items_tbl["student_code"] <- student_vect
irt_coefs_tbl["item"] <- items_name

items_longer_format <- items_tbl %>% 
  pivot_longer(cols = -student_code, names_to = "items", values_to = "count")

scores_all_students <- new_score[1]$new.scores[1:max_n]

scores_tbl <- enframe(scores_all_students) %>% 
  rename(student_code = name,
         score = value) %>% 
  mutate(student_code = as_factor(student_vect))

# choosing the top n coef item
top_n_items_most_difficult <- irt_coefs_tbl %>% 
  select(b, item) %>% 
  arrange(desc(b)) %>% 
  slice_max(b, n = 10) %>% 
  pluck(2)

top_n_items_less_difficult <- irt_coefs_tbl %>% 
  select(b, item) %>% 
  arrange(desc(b)) %>% 
  slice_min(b, n = 6) %>% 
  pluck(2)

questions <- c("Quem acertou as mais difíceis?", "Quem errou as mais fáceis?")

# acertou as difíceis e as fáceis (somente uma possibilidade)
# items_longer_experimental <- items_longer_format %>% 
#   mutate(top_n_difficult = case_when(
#     (items %in% top_n_items_most_difficult) & (count == 1) ~ 1,
#     TRUE ~ 0
#   )) %>% 
#   mutate(top_n_easy = case_when(
#     (items %in% top_n_items_less_difficult) & (count == 1) ~ 1,
#     TRUE ~ 0
#   ))

# Quem acertou as mais difíceis?
# items_longer_experimental_dif <- items_longer_format %>% 
#   mutate(top_n_difficult = case_when(
#     (items %in% top_n_items_most_difficult) & (count == 1) ~ 1,
#     TRUE ~ 0
#   ))
# 
# # Quem errou as mais fáceis?
# items_longer_experimental_easy <- items_longer_format %>% 
#   mutate(top_n_easy = case_when(
#     (items %in% top_n_items_less_difficult) & (count == 1) ~ 1,
#     TRUE ~ 0
#   ))


# ui ----
ui <- navbarPage(
  title = "IRT analysis",
  inverse = FALSE,
  collapsible = TRUE,
  theme = shinytheme("flatly"),
  
  tabPanel(
    title = "Análise Técnica",
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput(
          "items",
          label = "Escolha um item",
          choices = items_name_tag
        )
      ),
      mainPanel(
        div(
          class = "container",
          plotOutput("iccs")
        ),
        div(
          class = "container",
          plotOutput("infotrace")
        )
      )
    )
  ),
  tabPanel(
    title = "Resultados",
    
    sidebarLayout(
      
      sidebarPanel(
        materialSwitch(
          inputId = "switch", 
          label = "Filtrar por aluno", 
          status = "success",
          value = FALSE
        ),
        uiOutput("switch_click"),
        selectInput(
          "item_performance",
          label = "Respondendo algumas perguntas",
          choices = questions
        ),
        actionButton(
          "results_run",
          label = "RUN",
          icon = icon("play")
        )
      ),
      mainPanel(
        div(
          class = "container",
          echarts4rOutput(
            "heatmap"
            ),
          style = "padding: 20px;"
        ),
        fluidRow(
          column(
            width = 6,
            plotOutput("by_score")
          ),
          column(
            width = 6,
            plotOutput("by_hits")
          )
        )
      )
    )
  )
)


# server ----
server <- function(input, output, session) {
  
  # outputs on technical part
  output$iccs <- renderPlot({
    
    item_selected = input$items
    
    if (item_selected == "all") {
      plot(threePL_model, type = 'trace')
    } else {
      item_selected_number = extract_numeric(input$items)
      itemfit(
        threePL_model, 
        group.bins=15,
        empirical.plot = item_selected_number,
        empirical.CI = .95,
        method = 'ML'
      )
    }
  })
  
  output$infotrace <- renderPlot({
    
    item_selected = input$items
    
    if (item_selected == "all") {
      plot(threePL_model, type = 'infotrace')
    } else {
      item_selected_number = extract_numeric(input$items)
      itemplot(threePL_model, item_selected_number, type = "info")
    }
  })
  
  # eventReactive to switch
  output$switch_click <- renderUI({
    print(input$switch)
    student_list = list()
    if (input$switch) {
      selectInput(
        "students",
        label = "Quais alunos você deseja visualizar?",
        choices = student_vect,
        multiple = TRUE
      )
    } else {
      numericInput(
        "student_n",
        label = "Quantos estudantes você quer visualizar no rank?",
        value = 10,
        min = 1,
        max = 1000
      )
    }
  })
  
  # outputs on results part
  btn_1_click <- eventReactive(
    eventExpr = input$results_run,
    valueExpr = {
      return(list(input$student_n, input$item_performance, input$students))
    },
    ignoreNULL = F
  )
    
  output$heatmap <- echarts4r::renderEcharts4r({
    
    values = btn_1_click()
    n = values[[1]][1]
    question = values[[2]][1]
    student_list = values[[3]]
    
    if (length(student_list) > 0) {
      top_n = scores_tbl %>% 
        filter(student_code %in% student_list) %>% 
        mutate(student_code = student_code %>% as_factor()) %>%
        pluck(1)
      
    } else {
      top_n = scores_tbl %>% 
        mutate(student_code = student_code %>% as_factor()) %>% 
        arrange(desc(score)) %>% 
        slice_max(score, n = n) %>% 
        pluck(1)
    }

    if (question == "Quem acertou as mais difíceis?") {
      
      items_longer_experimental <- items_longer_format %>% 
        mutate(top_n_difficult = case_when(
          (items %in% top_n_items_most_difficult) & (count == 1) ~ 1,
          TRUE ~ 0
        ))
      
      items_longer_experimental %>%
        filter(student_code %in% top_n) %>%
        e_charts(items) %>%
        e_heatmap(student_code, top_n_difficult, blurSize = 7, pointSize = 5) %>%
        e_visual_map(top_n_difficult, inRange = list(color = c("#081A40", "#F24C3D"))) %>%
        e_title(question) %>%
        e_tooltip(trigger = "item",
                  formatter = htmlwidgets::JS(
                    "function(params){
                return('<strong> Student Name: </strong>' + params.value[1] + '<br /> <strong>Item: </strong>' + params.value[0])
              }"
                  )) %>%
        e_legend(FALSE)
      
    } else { if (question == "Quem errou as mais fáceis?") {
      
      items_longer_experimental <- items_longer_format %>% 
        mutate(top_n_easy = case_when(
          (items %in% top_n_items_less_difficult) & (count == 1) ~ 1,
          TRUE ~ 0
        ))
      
      items_longer_experimental %>%
        filter(student_code %in% top_n) %>%
        e_charts(items) %>%
        e_heatmap(student_code, top_n_easy, blurSize = 7, pointSize = 5) %>%
        e_visual_map(top_n_easy, inRange = list(color = c("#081A40", "#F24C3D"))) %>%
        e_title(question) %>%
        e_tooltip(trigger = "item",
                  formatter = htmlwidgets::JS(
                    "function(params){
                return('<strong> Student Name: </strong>' + params.value[1] + '<br /> <strong>Item: </strong>' + params.value[0])
              }"
                  )) %>%
        e_legend(FALSE)
    }
    }
  })
  
  output$by_score <- renderPlot({
    
    values = btn_1_click()
    n = values[[1]][1]
    student_list = values[[3]]
    
    if (length(student_list) > 0) {
      scores_tbl %>% 
        mutate(student_code = student_code %>% as_factor()) %>% 
        arrange(desc(score)) %>% 
        filter(student_code %in% student_list) %>% 
        ggplot(aes(x = score,
                   y = student_code %>% fct_reorder(score),
                   fill = score)) + 
        geom_bar(stat = "identity") +
        geom_label(
          aes(label = score %>% round(digits = 2)),
          hjust = 1,
          nudge_x = -6,
          size = 4,
          fontface = "bold",
          family = "Fira Sans",
          fill = "white",
          label.size = 0
        ) +
        scale_fill_gradient(low = "#081A40", high = "#F24C3D") +
        labs(
          title = "Classificação dos estudantes por score",
          y = "Nome dos estudantes",
          x = "Scores"
        ) +
        guides(
          fill = F
        ) +
        theme_minimal()
      
    } else {
      scores_tbl %>% 
        mutate(student_code = student_code %>% as_factor()) %>% 
        arrange(desc(score)) %>% 
        slice_max(score, n = n) %>% 
        ggplot(aes(x = score,
                   y = student_code %>% fct_reorder(score),
                   fill = score)) + 
        geom_bar(stat = "identity") +
        geom_label(
          aes(label = score %>% round(digits = 2)),
          hjust = 1,
          nudge_x = -6,
          size = 4,
          fontface = "bold",
          family = "Fira Sans",
          fill = "white",
          label.size = 0
        ) +
        scale_fill_gradient(low = "#081A40", high = "#F24C3D") +
        labs(
          title = "Classificação dos estudantes por score",
          y = "Nome dos estudantes",
          x = "Scores"
        ) +
        guides(
          fill = F
        ) +
        theme_minimal()
    }
  },
  height = 600
  )
  
  output$by_hits <- renderPlot({
    
    values = btn_1_click()
    n = values[[1]][1]
    student_list = values[[3]]
    
    if (length(student_list) > 0) {
      items_longer_format %>% 
        # filter(student_code %in% c(1:max_n)) %>% 
        group_by(student_code) %>% 
        summarise(total_corrects = sum(count)) %>% 
        ungroup() %>%
        arrange(desc(total_corrects)) %>% 
        filter(student_code %in% student_list) %>% 
        mutate(student_code = as_factor(student_code)) %>% 
        ggplot(aes(x = total_corrects,
                   y = student_code %>% fct_reorder(total_corrects),
                   fill = total_corrects)) +
        geom_bar(stat = "identity") +
        geom_label(
          aes(label = total_corrects),
          hjust = 1,
          nudge_x = -1,
          size = 4,
          fontface = "bold",
          family = "Fira Sans",
          fill = "white",
          label.size = 0
        ) +
        scale_fill_gradient(low = "#081A40", high = "#F24C3D") +
        labs(
          x = "Quantidade de questões certas",
          y = "",
          title = "Classificação dos estudantes por questões certas"
        ) +
        guides(
          fill = F
        ) +
        theme_minimal()
    } else {
      items_longer_format %>% 
        # filter(student_code %in% c(1:max_n)) %>% 
        group_by(student_code) %>% 
        summarise(total_corrects = sum(count)) %>% 
        ungroup() %>%
        arrange(desc(total_corrects)) %>% 
        slice_max(total_corrects, n = n) %>%
        mutate(student_code = as_factor(student_code)) %>% 
        ggplot(aes(x = total_corrects,
                   y = student_code %>% fct_reorder(total_corrects),
                   fill = total_corrects)) +
        geom_bar(stat = "identity") +
        geom_label(
          aes(label = total_corrects),
          hjust = 1,
          nudge_x = -1,
          size = 4,
          fontface = "bold",
          family = "Fira Sans",
          fill = "white",
          label.size = 0
        ) +
        scale_fill_gradient(low = "#081A40", high = "#F24C3D") +
        labs(
          x = "Quantidade de questões certas",
          y = "",
          title = "Classificação dos estudantes por questões certas"
        ) +
        guides(
          fill = F
        ) +
        theme_minimal()
    }
    },
    height = 600
  )
}


# run-app ----
shinyApp(ui = ui, server = server)