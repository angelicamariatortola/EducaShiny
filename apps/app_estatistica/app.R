library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(rmarkdown)
library(callr)
library(shinyjs)


dados <- read_xlsx("Dados 1.xlsx")

names <- unique(dados$Exercicio)

ui <- fluidPage(
  titlePanel(h1(strong("Estatística Básica"), align = "center")),
  tabsetPanel(
    tabPanel(h5(strong("Interpretação de Gráficos")),
      sidebarLayout(
        mainPanel(
          useShinyjs(),
          passwordInput("senha", "Digite a senha:"),
          textOutput("mensagem"),
          selectInput("ENEM", "Escolha uma Prova:",
                      choices = names),
          selectInput("question", "Escolha uma Questão:",
                      choices = NULL),
          uiOutput("texto_selecionado"),
          uiOutput("Resposta1")
        ),
        sidebarPanel(
          uiOutput("botsel"),
          uiOutput("Resposta2")
        )
      )
    ),
    tabPanel(h5(strong("Medidas de Posição")),
             sidebarLayout(
               mainPanel(
                 uiOutput("MediaMedianaModaFor1")
               ),
               sidebarPanel(
                 uiOutput("MeddePos")
               )
             )
    )
)
)

server <- function(input, output) {

  
  #--------------------------------------------------------------------------------#
  #SEM FILTRO
  Ex1 <- reactive({
    req(input$ENEM)
    dados %>% filter(Exercicio ==  input$ENEM)
  })
  
  Ex <- reactive({
    req(input$question)
    Ex1() %>% filter(n == input$question)
  })
  #--------------------------------------------------------------------------------#
  
  observeEvent(Ex1(), {
    freezeReactiveValue(input, "question")
    number <- unique(Ex1()$n)
    updateSelectInput(inputId = "question", choices = number)
  })
  
  resp <- reactive(as.character(strsplit(Ex()$resp, "~")[[1]]))
  
  #ObserveEvent(input$question) Unificado
  observeEvent(input$question, {
    output$Resposta1 <- renderUI({
      fluidPage(
        p(),
      )
    })
    
    output$Resposta0 <- renderUI({
      fluidPage(
        p(),
      )
    })
    
    output$TabelaR1 <- renderUI({
      fluidPage(
        p(),
      )
    })
    
    output$TabelaR2 <- renderUI({
      fluidPage(
        p(),
      )
    })
    
    if (Ex()$nM == 1) {
      output$MediaMedianaModaFor1 <- renderUI({
        fluidPage(
          uiOutput("MediaMedianaModaFor")
        )
      }) 
      
      output$MeddePos <- renderUI({
        fluidPage(
          style = "display: flex; flex-direction: column;",
          actionButton("media", "Média"),
          actionButton("mediana", "Mediana"),
          actionButton("moda", "Moda"),
          selectInput("selecao_tabela", "",
                      choices = c("Tabela de Dados", "Tabela de Frequências")
          ),
          dataTableOutput("tabela_ymed")
        )
      })
    } else if (Ex()$nM == 0) {
      output$MeddePos <- renderUI({
        fluidPage(
        )
      })
      output$MediaMedianaModaFor1 <- renderUI({
        fluidPage(
        )
      }) 
    }
    
    output$botsel <- renderUI({
      senha_correta <- "2024!utfpr"
      
      observe({
      if (input$senha == senha_correta) {
        shinyjs::enable("resposta")
        output$mensagem <- renderText({"Senha correta. Você pode acessar."})
      } else {
        shinyjs::disable("resposta")
        output$mensagem <- renderText({"Senha incorreta. Tente novamente."})
      }
      })
      
      observeEvent(input$resposta, {
        output$mensagem <- renderText({"Botão clicado com sucesso!"})
      })
      
      R <- Ex()$R
      if (R == "0") {
        fluidPage(
          style = "display: flex; flex-direction: column;",
          actionButton("tabela_x", "Valores dos Pontos (x)"),
          actionButton("tabela_y", "Valores dos Pontos (y)"),
          actionButton("tabela_xy", "Valores dos Pontos (x,y)"),
          actionButton("agrupar_dados", "Agrupar Dados"),
          actionButton("amostra", "Tamanho da Amostra"),
          actionButton("resposta", "Resposta"),
          uiOutput("tabela_ou_freq"),
          uiOutput("tamanhoamostra")
        )
      } else if (R == "1" || R == "11" || R == "13" || R == "14" || R == "31" || R == "4" || R == "41" || R == "5" || R == "12") {
        fluidPage(
          style = "display: flex; flex-direction: column;",
          actionButton("tabela_x", "Valores dos Pontos (x)"),
          actionButton("tabela_y", "Valores dos Pontos (y)"),
          actionButton("tabela_xy", "Valores dos Pontos (x,y)"),
          actionButton("resposta", "Resposta", disabled = TRUE),
          uiOutput("tabela_ou_freq")
        )
      } else if (R == "2" || R == "51") {
        fluidPage(
          style = "display: flex; flex-direction: column;",
          actionButton("tabela_x", "Valores dos Pontos (x)"),
          actionButton("tabela_y", "Valores dos Pontos (y)"),
          actionButton("tabela_xy", "Valores dos Pontos (x,y)"),
          actionButton("agrupar_dados", "Agrupar Dados"),
          actionButton("resposta", "Resposta", disabled = TRUE),
          uiOutput("tabela_ou_freq")
        )
      }
    })
    
    output$texto_selecionado <- renderUI({
      fluidPage(
        h4(Ex()$titulo, align = "center"),
        p(Ex()$texto, style = "text-align: justify;"),
        plotOutput("grafico")
      )
    })
    
    observeEvent({
      input$question
    }, {
      
      #---------------------------------------------------------------#
      # Dataframe
      df <- reactive({
        
        tY <- Ex()$ty
        tX <- Ex()$tx
        
        if (tX == "1" && tY == "1") {
          X <- reactive(as.numeric(strsplit(Ex()$X, ",")[[1]]))
          Y <- reactive(as.numeric(strsplit(Ex()$Y, ",")[[1]]))
        } else if (tX == "1" && tY == "2") {
          X <- reactive(as.numeric(strsplit(Ex()$X, ",")[[1]]))
          Y <- reactive(as.character(strsplit(Ex()$Y, ",")[[1]]))
        } else if (tX == "2" && tY == "1") {
          Xval <- reactive(as.character(strsplit(Ex()$X, ",")[[1]]))
          X <- reactive(factor(Xval(), levels = Xval()))
          Y <- reactive(as.numeric(strsplit(Ex()$Y, ",")[[1]]))
        } else if (tX == "2" && tY == "2") {
          Xval <- reactive(as.character(strsplit(Ex()$X, ",")[[1]]))
          X <- reactive(factor(Xval(), levels = Xval()))
          Y <- reactive(as.character(strsplit(Ex()$Y, ",")[[1]]))
        }
        
        req(X(), Y())
        data.frame(X = X(), Y = Y())
      })
      
      #---------------------------------------------------------------#  
      
      #-----------------------------------------------------------------
      #respostas
      source('Respostas.R',local=TRUE, encoding = "UTF-8")
      #-----------------------------------------------------------------
      #tabelas valor
      source('Tabelas.R',local=TRUE, encoding = "UTF-8")
      #-----------------------------------------------------------------
      #média, mediana e moda #desvio padrão, variância e amplitude)
      source('Medidas_de_Posição.R',local=TRUE, encoding = "UTF-8")
      #-----------------------------------------------------------------
      #gráfico
      source('Gráficos.R',local=TRUE, encoding = "UTF-8")
      #-----------------------------------------------------------------
    })
    
  })
  
  
  observeEvent(input$resposta, {
    Rreativo <- reactive({Ex()$R})
    R <- Rreativo()
    output$Resposta1 <- renderUI({
      if (R == "2") {
        fluidPage(
          p(strong("--------------------------------------------RESPOSTA--------------------------------------------"), align = "center"),
          uiOutput("Resposta0"),
          plotOutput("graficoR"),
          p(strong("RESPOSTA:",resp()), align = "center")
        )
      } else if (R == "31" || R == "4" || R == "41" || R == "5" || R == "51") {
        fluidPage(
          p(strong("--------------------------------------------RESPOSTA--------------------------------------------"), align = "center"),
          uiOutput("Resposta0"),
          p(strong("RESPOSTA:",resp()), align = "center")
        )
      } else {
        fluidPage(
          p(strong("--------------------------------------------RESPOSTA--------------------------------------------"), align = "center"),
          plotOutput("graficoR"),
          p(strong("RESPOSTA:",resp()), align = "center")  
        )
      }
    })
    if (R == "2" || R == "5" || R == "51" || R == "13" || R == "31" || R == "4") {
      output$Resposta2 <- renderUI({
        fluidPage(
          p("")
        )
      })
    } else if (R == "11" || R == "12" || R == "14") {
      output$Resposta2 <- renderUI({
        fluidPage(
          uiOutput("TabelaR1"),
        )
      })
    } else {
      output$Resposta2 <- renderUI({
        fluidPage(
          uiOutput("TabelaR1"),
          uiOutput("TabelaR2"),
        )
      })
    }
  })
  
  rv <- reactiveValues(gg = NULL, tabela = NULL)
  
}

shinyApp(ui, server)