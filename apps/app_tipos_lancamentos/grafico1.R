gravidade <- -10

#altura(
atH <- reactive({ 
  req(input$incognitaH)
  req(input$sH)
  if(input$incognitaH=="altura"){
    if(input$sH=="velocidade e tempo"){
      input$tempoH^2*(-gravidade) / (2)
    } else {
      tH()^2*(-gravidade) / 2
    }
  }
  else if(input$incognitaH=="alcance"){
    if(input$sH=="velocidade e tempo"){
      input$tempoH^2*(-gravidade) / (2)
    } else {
      input$alturaH
    } 
  }
  else{
    input$alturaH
  }
})

#alcance
alH <- reactive({ input$vH * tH() })

#velocidade
velH <- reactive({
  if(input$incognitaH=="velocidade"){
    input$alcanceH / sqrt(input$alturaH*2 / -gravidade)
  }
  else{
    input$vH
  }
})

#tempo
th1<-reactive({
  req(!is.na(input$sH))
  if (input$sH == "velocidade e tempo") {
    input$tempoH
  } else {
    input$alcanceH / input$vH
  }
})

th2<-reactive({
  req(!is.na(input$sH))
  if (input$sH == "velocidade e tempo") {
    input$tempoH
  } else {
    sqrt((2 * input$alturaH) / (-gravidade))
  }
})

tH <- reactive({
  if (input$incognitaH == "altura") {
    th1()
  } else if (input$incognitaH == "alcance") {
    th2()
  } else if (input$incognitaH == "velocidade") {
    sqrt(input$alturaH*2 / -gravidade)
  }
})


tempH <- reactive({
  req(!is.na(tH()))
  seq(0, to = tH(), by = 0.05)
})

l1 <- reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(input$vH != 0)
    req(input$tempoH!= 0)   
    actionButton("grafH", "🚀 Lançar!")
  } else if(input$sH=="velocidade e altura"){
    req(input$vH != 0)
    req(input$alturaH!= 0)   
    actionButton("grafH", "🚀 Lançar!")
  }
})

l2 <- reactive({
  req(!is.na(input$sH))
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(input$vH != 0)
    req(input$tempoH!= 0)   
    actionButton("grafH", "🚀 Lançar!")
  } else if(input$sH=="velocidade e alcance"){
    req(input$vH != 0)
    req(input$alcanceH!= 0)   
    actionButton("grafH", "🚀 Lançar!")
  }
})

output$lancarH <- renderUI({
  if(input$incognitaH==""){
    
  } else if(input$incognitaH=="alcance"){
    l1()
  } else if(input$incognitaH=="altura"){
    l2()
  } else {
    req(input$alcanceH != 0)
    req(input$alturaH!= 0)   
    actionButton("grafH", "🚀 Lançar!")
  }
  
})

# cálcula o step do gráfico 
stepH <- reactiveVal(NULL)

observeEvent(c(input$grafH,input$vH),
             {step_valueH <- 1
             stepH(step_valueH)
             })

observe({
  invalidateLater(250, session) 
  req(input$grafH)
  isolate({
    if (!is.null(stepH()) && stepH() < length(tempH())) {
      stepH(stepH() + 1)
    }
  })
})

# plota o gráfico     
limH <- reactive({atH()+10}) 

posicao_xH <- reactive({ velH() * tempH() })
posicao_yH <- reactive({ atH() + ((gravidade) * (tempH()^2)) / 2})

pH <- reactiveValues(plotH = NULL)

output$movH <- renderPlotly({
  pH$plotH
})

observeEvent(input$grafH, {
  pH$plotH <- plot_ly() %>%
    layout(
      xaxis = list(title = "Posição Horizontal", range = c(0, limH())),
      yaxis = list(title = "Posição Vertical", range = c(0, limH())))  %>%
    add_markers(x = ~posicao_xH()[1:stepH()],
                y = ~posicao_yH()[1:stepH()],
                mode = 'markers')%>% 
    config(displayModeBar = F) %>% 
    layout(xaxis=list(fixedrange=TRUE)) %>% 
    layout(yaxis=list(fixedrange=TRUE))
})

observeEvent(c(input$nexH,input$incognitaH,input$sH,input$vH,input$alturaH,input$alcanceH,input$tempoH), {
  pH$plotH <- NULL
})