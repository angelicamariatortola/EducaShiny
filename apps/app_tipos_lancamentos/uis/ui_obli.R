# Aba do lançamento oblíquo
ui_obli <- fluidPage(
  
  sidebarLayout(
    mainPanel(
      h1(""),
      tags$div(
        style = "display: flex; align-items: center;", 
        tags$label("Qual o tipo de exercício?", style = "margin-right: 10px; color:#003366; "),
        selectInput("tipo", NULL, choices = tipos, selected = "")),
      fluidRow(
        column(7, 
               tags$div(
                 style = "display: flex; align-items: center; ", 
                 tags$label("Problema de:", style = "margin-right: 10px; color:#003366; "),
                 selectInput("incognita", NULL, choices = variaveis, selected = ""))
        ),
        column(5, uiOutput("ex"))
      ),
      h1(""),
      fluidRow(
        column(12, uiOutput("enunciado"))
      ),
      fluidRow(
        column(12, uiOutput("Seg"))
      ),
      fluidRow(
        style = "display: flex; justify-content: center; align-items: center;",
        
        column(12, uiOutput("parametros"))
      ),
      h1(""),
      fluidRow(
        br(),
        column(6, uiOutput("mostrar")),
        column(6, uiOutput("lancar"))
      ),
      h1(""),
      plotlyOutput("mov")
    ),
    sidebarPanel(
      style = "background-color: #D1EEEE; padding: 20px; border-radius: 10px;",
      
      tags$h1(
        strong("Passo a Passo"), class = "titulo-secundario"),
      h1(""),
      uiOutput("b"), # botão de equação no arquivo equacoes.R
      h1(""),
      uiOutput("t_eq"), # equação detalhada no arquivo equacoes.R
      h1(""),
      fluidRow(
        column(6, uiOutput("respostaOb")), # campo para a resposta em resposta_ob.R
        br(),
        column(6, uiOutput("conf1Ob")) # botão de conferir a resposta em resposta_ob.R
      ),
      uiOutput("r1Ob"),
      h1(""),
      uiOutput("b2"),  # botão de conferir calculo em calculo.R
      h1(""),
      uiOutput("t_sol") # apresenta as soluções detalhadas. No arquivo calculo.R
      # h1(""),
      # passwordInput("password", "Acesso do professor:"),

    )
  )
)
