
## Respostas do lançamento Obliquo
## Valores das respostas corretas
resposta1_ob <- reactive ({
  if(input$incognita == "altura máxima") {
    round(altura_maxima(), digits = 1)
  }
  else if(input$incognita == "alcance") {
    round(distancia_maxima(), digits = 1)
  }
  else if(input$incognita == "tempo total") {
    round(Tt(), digits = 1)
  }
  else if(input$incognita == "velocidade") {
    round(v(), digits = 1)
  }
})

## Após clicar no botão ele retorna o feedback
output$r1Ob <- renderUI({
  req(input$conferir1Ob)
  HTML(feedback1_ob())
})

## Feedback que confere a resposta do estudante (resp1Ob) com a resposta correta (resposta1)
feedback1_ob <- reactive({
  req(input$resp1Ob)
  req(resposta1_ob())
  if(input$resp1Ob==resposta1_ob()){
    return(paste("<span style='color:green;'>", "Parabéns! Continue assim.😊"))
  }
  else{
    return(paste("<span style='color:red;'>",
                 "Continue praticando! Revise a teoria na aba de Revisão e tente novamente.🌟"))
  }
})

## Botão de Conferir
output$conf1Ob <- renderUI({
  req(input$resp1Ob)
  tags$div(
    br(),
    actionButton("conferir1Ob", "📌 Conferir Resposta"))
})


## Criando os inputs numericos para a resposta, levando em conta as exigencias de cada exercicio
# Se for problema de velocidade
a1v <- reactive({
  req(!is.na(input$sv))
  if(input$sv==""){
    
  } else if(input$sv=="ângulo e alcance"){
    req(input$theta != 0); req(input$s!= 0)
    # para mostrar o campo "Resposta", exige que angulo e alcance sejam diferentes de zero
    numericInput("resp1Ob", label = "Resposta:", value = NULL, width = "100%")
  } else if(input$sv=="ângulo e altura"){
    req(input$theta != 0); req(input$alt!= 0)
    # para mostrar o campo "Resposta", exige que angulo e altura maxima sejam diferentes de zero
    numericInput("resp1Ob", label = "Resposta:", value = NULL, width = "100%")
  }
})

output$respostaOb <- renderUI({
  if(input$incognita==""){
    
  } else if(input$incognita=="velocidade"){ # se problema de velocidade
    a1v()
  } else { # se problema de altura maxima, altura ou tempo total:
    req(input$theta != 0); req(input$v!= 0)
    # para mostrar o campo "Resposta", 
    # exige que angulo e velocidade sejam diferentes de zero (para calcular a formula)
    numericInput("resp1Ob", label = "Resposta:", value = NULL, width = "80%")
  }
  
})

observeEvent(c(input$nex,input$incognita,input$tipo),{
  updateNumericInput(session, "resp1Ob", value = NULL)
})