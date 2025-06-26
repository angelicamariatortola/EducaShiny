
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

output$r1 <- renderUI({
  req(input$conferir1)
  HTML(feedback1())
  })

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

output$conf1 <- renderUI({
  req(input$resp1)
  actionButton("conferir1", "📌 Conferir Resposta")
})

a1 <- reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(input$vH != 0)
    req(input$tempoH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  } else if(input$sH=="velocidade e altura"){
    req(input$vH != 0)
    req(input$alturaH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  }
})

a2 <- reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(input$vH != 0)
    req(input$tempoH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  } else if(input$sH=="velocidade e alcance"){
    req(input$vH != 0)
    req(input$alcanceH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "100%")
  }
})

output$resposta <- renderUI({
  if(input$incognitaH==""){
    
  } else if(input$incognitaH=="alcance"){
    a1()
  } else if(input$incognitaH=="altura"){
    a2()
  } else {
    req(input$alcanceH != 0)
    req(input$alturaH!= 0)   
    numericInput("resp1", label = "Resposta:", value = NULL, width = "80%")
  }
  
})

observeEvent(c(input$nexH,input$incognitaH,input$tipoH),{
  updateNumericInput(session, "resp1", value = NULL)
})