# Aba do lançamento horizontal
ui_horiz <- fluidPage(
  
  h1(""),
  tags$div(
    style = "display: flex; align-items: center;", 
    tags$label("Qual o tipo de exercício?", style = "margin-right: 10px; color:#003366; "),
    selectInput("tipoH", NULL, choices = tipos, selected = "")),
  fluidRow(
    column(8, 
           tags$div(
             style = "display: flex; align-items: center; ", 
             tags$label("Problema de:", style = "margin-right: 10px; color:#003366; "),
             selectInput("incognitaH", NULL, choices = variaveisH, selected = ""))
    ),
    column(4, uiOutput("exH"))
  ),
  sidebarLayout(
    mainPanel(
      h1(""),
      fluidRow(
        column(12, uiOutput("enunciadoH"))
      ),
      fluidRow(
        column(12, uiOutput("SegH"))
      ),
      fluidRow(
        style = "display: flex; justify-content: center; align-items: center;",
        
        column(8, uiOutput("parametrosH")),
        column(4, uiOutput("mostrarH"))
      ),
      h1(""),
      fluidRow(
        column(6, uiOutput("lancarH"))
      ),
      h1(""),
      plotlyOutput("movH")
    ),
    sidebarPanel(
      style = "background-color: #D1EEEE; padding: 20px; border-radius: 10px;",
      
      tags$h1(
        strong("Passo a Passo"), class = "titulo-secundario"),
      h1(""),
      uiOutput("b1H"),
      h1(""),
      uiOutput("t_eqH"),
      h1(""),
      fluidRow(
        column(6, uiOutput("resposta")),
        column(6, uiOutput("conf1"))
      ),
      uiOutput("r1"),
      h1(""),
      uiOutput("b2H"),
      h1(""),
      uiOutput("t_solH")
    )
  )
)
