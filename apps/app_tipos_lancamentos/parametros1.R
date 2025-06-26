pa1<-reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  }
  else if(input$sH=="velocidade e tempo"){
    tagList(
      column(6,
             numericInput("vH", "A velocidade (m/s) é:", min = 0, 
                          max = 700,value=0),
             textOutput("avH")
      ),
      column(6,
             numericInput("tempoH", "O tempo total (s) é:",
                          1, 700, value=0),
             textOutput("atH")
      )
    )
  }
  else{
    tagList(
      column(6,
             numericInput("vH", "A velocidade (m/s) é:", min = 0, 
                          max = 700,value=0),
             textOutput("avH")
      ),
      column(6,
             numericInput("alturaH", "A altura inicial (m) é:", 
                          1, 700, value=0),
             textOutput("aaH")
      )
    )
  }
})

pa2<-reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  }
  else if(input$sH=="velocidade e tempo"){
    tagList(
      column(6,
             numericInput("vH", "A velocidade (m/s) é:", 
                          min = 0, max = 700,value=0),
             textOutput("avH")
      ),
      column(6,
             numericInput("tempoH", "O tempo total (s) é:", 
                          1, 700, value=0),
             textOutput("atH")
      )
    )
  }
  else {
    tagList(
      column(6,
             numericInput("vH", "A velocidade (m/s) é:", 
                          min = 0, max = 700,value=0),
             textOutput("avH")
      ),
      column(6,
             numericInput("alcanceH", "O alcance (m) é:", 
                          1, 700, value=0),
             textOutput("alH")
      )
    )
  }
})

output$parametrosH <- renderUI({
  if(input$incognitaH == ""){
    
  }
  else if(input$incognitaH=="velocidade"){
    tagList(
      column(6,
             numericInput("alcanceH", "O alcance (m) é:", min = 0, max = 700,
                          value=0),
             textOutput("alH")
      ),
      column(6,
             numericInput("alturaH", "A altura inicial (m) é:", 1, 700,
                          value=0),
             textOutput("aaH")
      )
    )
  }
  else if(input$incognitaH=="alcance"){
    pa1()
  }
  else if(input$incognitaH=="altura"){
    pa2()
  }
})

#Preenche os parâmetros quando necessário
output$mostrarH <- renderUI({
  if(input$tipoH=="aberto"){
    
  }
  else if(input$tipoH=="fechado"){
    if(input$incognitaH == "velocidade"){
      req(input$nexH)
      actionButton("mosH", "📌 Preencher dados")
    }
    else{
      req(input$sH)
      actionButton("mosH", "📌 Preencher dados")
    }
  }
})


observeEvent(input$mosH,{
  updateNumericInput(inputId = "alturaH", value = alturaH())
  updateNumericInput(inputId = "vH", value = velocidadeH())
  updateNumericInput(inputId = "alcanceH", value = alcanceH())
  updateNumericInput(inputId = "tempoH", value = tempoH())
})

observeEvent(c(input$nexH,input$tipoH),{
  updateNumericInput(inputId = "alturaH", value = 0)
  updateNumericInput(inputId = "vH", value = 0)
  updateNumericInput(inputId = "alcanceH", value = 0)
  updateNumericInput(inputId = "tempoH", value =0)
})

output$atH <- renderText(avisotH())
avisotH <- reactive({
  req(!is.na(input$tempoH))
  if(input$tipoH=="aberto"){
    if(input$tempoH <= 0){
      validate("o tempo deve ser maior que zero")
    } 
  }
  else{
    if(input$tempoH != tempoH())
      validate("Confira o tempo do enunciado")
  }
})

output$alH <- renderText(avisosH())
avisosH <- reactive({
  req(!is.na(input$alcanceH))
  if(input$tipoH=="aberto"){
    if(input$alcanceH <= 0){
      validate("o alcance deve ser maior que zero")
    } 
  }else{
    if(input$alcanceH != alcanceH()){
      validate("Confira o alcance do enunciado")
    }
  }
})

output$aaH <- renderText(avisoH())
avisoH <- reactive({
  req(!is.na(input$alturaH))
  if(input$tipoH=="aberto"){
    if(input$alturaH <= 0){
      validate("A altura deve ser maior que zero")
    } 
  }else{
    if(input$alturaH != alturaH()){
      validate("Confira a altura do enunciado")
    }
  }
})

output$avH <- renderText(avisovH())
avisovH <- reactive({
  req(!is.na(input$vH))
  if(input$tipoH=="aberto"){
    if(input$vH <= 0){
      validate("A velocidade deve ser maior que zero")
    }
  }else{
    if(input$vH != velocidadeH()){
      validate("Confira a velocidade do enunciado")
    }
  }
})