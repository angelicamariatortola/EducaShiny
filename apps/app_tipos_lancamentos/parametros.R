## Problema de Velocidade
# sv -> Quais variáveis usará no cálculo?
teste<- reactive({
  req(!is.na(input$sv))
  if(input$sv=="ângulo e alcance"){
    tagList(
      fluidRow(
        column(4,
               numericInput("theta", "O ângulo é:", 1, 89, value=0),
               textOutput("at")
        ),
        column(4,
               numericInput("s", "O alcance (m) é:", min = 0, max = 100,value=0),
               textOutput("as")
        ),
        column(4,
               numericInput("altura", "A altura inicial (m) é:", 1, 100, value=0),
               textOutput("alt")
        )
      )
)}
  else if(input$sv=="ângulo e altura"){
    tagList(
      fluidRow(
        column(4,
               numericInput("theta", "O ângulo é:", 1, 89, value=0),
               textOutput("at")
        ),
        column(4,
               numericInput("alt", "A altura máxima (m) é:", min = 0, max = 100,value=0),
               textOutput("aa")
        ),
        column(4,
               numericInput("altura", "A altura inicial (m) é:", 1, 100, value=0),
               textOutput("alt")
        ),
      )
    )}
})

output$parametros <- renderUI({
  if(input$incognita == ""){
    
  }
  else if(input$incognita == "velocidade"){ # se problema de velocidade
    teste()
    
  }
  else{  # se problema de altura maxima, altura ou tempo total:
    tagList(
      fluidRow(
        column(4,
               numericInput("theta", "O ângulo é:", 1, 89,value=0),
               textOutput("at")
        ),
        column(4,
               numericInput("v", "A velocidade (m/s) é:", min = 0, max = 100,value=0),
               textOutput("av")
        ),
        column(4,
               numericInput("altura", "A altura inicial (m) é:", 1, 100, value=0),
               textOutput("alt")
        )
      )
    )}
})


#Preenche os parâmetros quando necessário

output$mostrar <- renderUI({
  if(input$tipo=="aberto"){
  }
  else if(input$tipo=="fechado"){
    req(input$nex)
    actionButton("mos", "📌 Preencher dados")
  }
})

observeEvent(input$mos,{
  updateNumericInput(inputId = "theta", value = angulo())
  updateNumericInput(inputId = "s", value = alcance())
  updateNumericInput(inputId = "altura", value = altura_i())
  updateNumericInput(inputId = "alt", value = altura_max())
  updateNumericInput(inputId = "v", value = velocidade())
})

observeEvent(c(input$nex, input$tipo),{
  updateNumericInput(inputId = "theta", value = 0)
  updateNumericInput(inputId = "s", value = 0)
  updateNumericInput(inputId = "altura", value = 0)
  updateNumericInput(inputId = "alt", value = 0)
  updateNumericInput(inputId = "v", value = 0)
})


output$at <- renderText(aviso())
aviso <- reactive({
  req(!is.na(input$theta))
  if(input$tipo=="aberto"){
    if(input$theta <= 0){
      validate("O ângulo deve ser maior que zero")
    } 
  }
  else{
    if(input$theta != angulo())
      validate("Confira o ângulo do enunciado")
  }
})

output$as <- renderText(avisos())
avisos <- reactive({
  req(!is.na(input$s))
  if(input$tipo=="aberto"){
    if(input$s <= 0){
      validate("o alcance deve ser maior que zero")
    } 
  }else{
    if(input$s != alcance()){
      validate("Confira o alcance do enunciado")
    }
  }
})

output$alt <- renderText(avisoalt())
avisoalt <- reactive({
  req(!is.na(input$altura))
  if(input$tipo=="fechado"){
    if(input$altura != altura_i()){
      validate("Confira a altura do enunciado")
    }
  }
})

output$aa <- renderText(avisoa())
avisoa <- reactive({
  req(!is.na(input$alt))
  if(input$tipo=="aberto"){
    if(input$alt <= 0){
      validate("A altura deve ser maior que zero")
    } 
  }else{
    if(input$alt != altura_max()){
      validate("Confira a altura enunciado")
    }
  }
})

output$av <- renderText(avisov())
avisov <- reactive({
  req(!is.na(input$v))
  if(input$tipo=="aberto"){
    if(input$v <= 0){
      validate("A velocidade deve ser maior que zero")
    }
  }else{
    if(input$v != velocidade()){
      validate("Confira a velocidade do enunciado")
    }
  }
})
