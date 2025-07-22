# Aba do teste diagnóstico

ui_diag <- dashboardBody(
  
  useShinyjs(),
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$div(
          HTML("<span style='font-size:20px; color:#003366; font-weight:bold;
               '>Escolha o Conteúdo para o Teste:</span>")
        ),
        selectInput(
          inputId = "tipo_teste",
          label = NULL,  # remove o label do selectInput
          choices = c("🎯 Lançamento Horizontal" = "horizontal",
                      "🚀 Lançamento Oblíquo"    = "obliquo"),
          selected = "horizontal",
          width = "400px"   # largura do select
        )
      )
    )
  ),
  
  fluidRow(
    style = "margin-top: 30px;",  # aumenta o espaço acima
    width = 12,
    uiOutput("conteudo_teste")  # Conteúdo renderizado dinamicamente
  )
)

