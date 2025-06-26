library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(bs4Dash)    # Para tabelas interativas
library(plotly)
library(readxl)
library(dplyr)
# library(rsconnect)

variaveis <- c("","altura máxima", "alcance", "tempo total","velocidade")
variaveisH <- c("","altura", "alcance", "velocidade")
tipos<- c("","aberto","fechado")
sv0<-c("","ângulo e altura","ângulo e alcance")
svH0 <- c("","velocidade e tempo","velocidade e altura")
svH1 <- c("","velocidade e tempo","velocidade e alcance")

# Aba de início
source("uis/ui_home.R", local = TRUE)

# Aba do teste diagnóstico
source("uis/ui_diag.R", local = TRUE)

# Aba de Revisão
source("uis/ui_rev.R", local = TRUE)

# Aba do lançamento horizontal
source("uis/ui_horiz.R", local = TRUE)

# # Aba do lançamento oblíquo
# source("uis/ui_obli.R", local = TRUE)


# União de todas as abas do app
ui <- dashboardPage(
  dashboardHeader(title = NULL),
  dashboardSidebar(disable = TRUE),  # Desativa o menu lateral padrão
  help = NULL,
  dark = NULL,
  
  dashboardBody(
  
  # configurações de CSS/html para estilizar mais a página
  tags$link(rel = "stylesheet", type = "text/css", href = "estilos_aba_main.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "estilos_aba_home.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "estilos_aba_diag.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "estilos_aba_rev.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "estilos_aba_horiz.css"),
  
  # includeCSS("www/estilos_aba_horiz.css"),
  
  # Importa a fonte do Google Fonts
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Chewy&display=swap", rel = "stylesheet")
  ),
  
  # Define as configurações do título do painel
  titlePanel(
    tags$h1("Aplicativo de Tipos de Lançamento", class = "titulo-principal")
  ),
  
  shinyFeedback::useShinyFeedback(),
  
  tabsetPanel(
    tabPanel("Início 🏠", ui_home),
    tabPanel("Teste Diagnóstico ✅", ui_diag),
    tabPanel("Revisão 📖", ui_rev),
    tabPanel("Lançamento horizontal 🎯", ui_horiz)
    # tabPanel("Lançamento Oblíquo 🚀", ui_obli)
      )
  )
)

server <- function(input,output,session){
  
  # Server da Aba do Teste Diagnóstico
  source("server_diag.R", local = TRUE)

  ## Server Aba de Revisão
  source("server_rev.R", local = TRUE)

  ## Server da Aba do Lançamento Horizontal
  source("server_horiz.R", local = TRUE)
  
  ## Server da Aba do Lançamento Obliquo
  # source("server_obli.R", local = TRUE)

}

shinyApp(ui,server)


