
## Respostas do lançamento Horizontal
resposta1 <- reactive ({
  if(input$incognitaH == "altura") {
    round(atH(), digits = 1)
  }
  else if(input$incognitaH == "alcance") {
    round(alH(), digits = 1)
  }
  else if(input$incognitaH == "velocidade") {
    round(v1(), digits = 1)
  } 
})

v1 <- reactive(input$alcanceH / sqrt(input$alturaH*2 / -gravidade))

## Após clicar no botão ele retorna o feedback
output$r1 <- renderUI({
  req(input$conferir1)
  HTML(feedback1())
  })

# observeEvent(input$incognitaH, { # só para visualizar o resultado
#   print(resposta1())
# })

feedback1 <- reactive({
  req(input$resp1)
  req(resposta1())
  if(input$resp1==resposta1()){
    return(paste("<span style='color:green;'>", "Parabéns! Continue assim.😊")) 
  }
  else{
    return(paste("<span style='color:red;'>",
                 "Continue praticando! Revise a teoria na aba de Revisão e tente novamente.🌟")) 
  }
})

## Criando o botão de Conferir
output$conf1 <- renderUI({
  req(input$resp1)
  tags$div(
    br(),
    actionButton("conferir1", "📌 Conferir Resposta"))
})

## Criando os inputs numericos para a resposta, levando em conta as exigencias de cada exercicio
## Se for Problema de Alcance
a1 <- reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(input$vH != 0); req(input$tempoH!= 0) 
    # para mostrar o campo "Resposta", exige que velocidade e tempo sejam diferentes de zero,
    # ou seja, que o usuario preencha corretamente os inputs
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  } else if(input$sH=="velocidade e altura"){
    req(input$vH != 0); req(input$alturaH!= 0)   
    # para mostrar o campo "Resposta", exige que velocidade e altura sejam diferentes de zero
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  }
})

## Se for Problema de Altura
a2 <- reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(input$vH != 0); req(input$tempoH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  } else if(input$sH=="velocidade e alcance"){
    req(input$vH != 0); req(input$alcanceH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  }
})

## Só aparece o campo para inserir a reposta se as condições forem satisfeitas
output$resposta <- renderUI({
  if(input$incognitaH==""){
    
  } else if(input$incognitaH=="alcance"){
    a1()
  } else if(input$incognitaH=="altura"){
    a2()
  } else { # Se for problema de velocidade
    req(input$alcanceH != 0); req(input$alturaH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "80%")
  }
  
})

observeEvent(c(input$nexH,input$incognitaH,input$tipoH),{
  updateNumericInput(session, "resp1", value = NULL)
})