gravidade <- -10

t_subida <- reactive({
  req(!is.na(vel()))
  (vel()*sin(angulo_rad()))/-gravidade})

Tt <- reactive({
  if(input$altura==0){
    (2*t_subida())
  }
  else {
    t_subida() + (sqrt(2*(-gravidade)*altura_maxima())/(-gravidade))
  }
})

teste2<- reactive(({
  req(!is.na(input$sv))
  if(input$sv==""){
    
  } else if(input$sv=="ângulo e altura"){
    sqrt((input$alt*2*(-gravidade)))/(sin(angulo_rad()))
  } else if(input$sv=="ângulo e alcance"){
    sqrt((input$s * (-gravidade)) / (sin(2 * angulo_rad())))
  }
}))

vel <- reactive({
  req(input$incognita)
  if(input$incognita=="velocidade"){
  teste2()
  }
  else{
    input$v
  }
})

tempo <- reactive({
  req(!is.na(Tt()))
  if(vel()>100){
    seq(0, to = Tt(), by = 0.5)
  }
  else{
    seq(0, to = Tt(), by = 0.05)
  }
})


altura_maxima <- reactive({
  req(input$altura) # impede que input$altura seja NULL
  if(input$incognita=="velocidade"){
     if(input$sv =="ângulo e altura"){
      altura_max()
    } else{
      if(input$altura==0){
        (((vel())^2) * ((sin(angulo_rad()))^2)) / (2 * (-gravidade))
      } else{
        input$altura+(((vel())^2) * ((sin(angulo_rad()))^2)) / (2 * (-gravidade))
      }
    }
  } else{
    if(input$altura==0){
      (((vel())^2) * ((sin(angulo_rad()))^2)) / (2 * (-gravidade))
    } else{
      input$altura+(((vel())^2) * ((sin(angulo_rad()))^2)) / (2 * (-gravidade))
    }
  }

})

distancia_maxima <- reactive({ (((vel())^2) * (sin(2 * angulo_rad()))) / (-gravidade) })

l <- reactive({
  req(!is.na(input$sv))
  if(input$sv==""){
    
  } else if(input$sv=="ângulo e altura"){
    req(input$theta != 0)
    req(input$alt!= 0)   
    actionButton("graf", "🚀 Lançar!", class = "btn-info")
  } else if(input$sv=="ângulo e alcance"){
    req(input$theta != 0)
    req(input$s!= 0)   
    actionButton("graf", "🚀 Lançar!", class = "btn-info")
  }
})

output$lancar <- renderUI({
  if(input$incognita==""){
  
  } else if(input$incognita=="velocidade"){
    l()
  } else {
    req(input$theta != 0)
    req(input$v!= 0)   
    actionButton("graf", "🚀 Lançar!", class = "btn-info")
  }

})

# cálcula o step do gráfico 
step <- reactiveVal(NULL)

observeEvent(c(input$graf,input$v,input$theta,input$s),
             {step_value <- 1
             step(step_value)
             })

observe({
  invalidateLater(250, session) 
  req(input$graf)
  isolate({
    if (!is.null(step()) && step() < length(tempo())) {
      step(step() + 1)
    }
  })
})

# plota o gráfico     
lim_x <- reactive({((vel())) * (sin(pi/2)) * Tt() }) 

angulo_rad <- reactive({ input$theta * pi / 180 })
posicao_x <- reactive({ vel() * cos(angulo_rad()) * tempo() })
posicao_y <- reactive({ input$altura + (vel() * sin(angulo_rad()) * tempo() + (gravidade * (tempo()^2)) / 2) })

p <- reactiveValues(plot = NULL)

output$mov <- renderPlotly({
  p$plot
})


observeEvent(input$graf, {
  v_init <- vel() 
  angulo <- angulo_rad() 
  escala <- 0.5 
  
  x_end <- v_init * cos(angulo) * escala
  y_end <- input$altura + v_init * sin(angulo) * escala
  
  p$plot <- plot_ly() %>%
    layout(
      xaxis = list(title = "Posição Horizontal (m)", range = c(0, lim_x())),
      yaxis = list(title = "Posição Vertical (m)", range = c(0, lim_x()))) %>%
    add_markers(x = ~posicao_x()[1:step()],
                y = ~posicao_y()[1:step()],
                mode = 'markers') %>%
    add_segments(x = 0, y = input$altura, xend = x_end, yend = y_end, 
                 line = list(color = 'red', width = 2)) %>%
    config(displayModeBar = F) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>%
    layout(yaxis=list(fixedrange=TRUE)) %>%
    layout(showlegend = FALSE)
})

observeEvent(input$theta,{
  req(!is.na(input$theta))
    angulo <- angulo_rad() 
    
    x_end <-  cos(angulo)
    y_end <-  sin(angulo) 
    
    p$plot <- plot_ly() %>%
      layout(
        xaxis = list(title = "Posição Horizontal (m)", range = c(0, 5)),
        yaxis = list(title = "Posição Vertical (m)", range = c(0, 5))) %>%
      add_segments(x = 0, y = input$altura, xend = x_end, yend = y_end, 
                   line = list(color = 'red', width = 2)) %>%
      config(displayModeBar = F) %>%
      layout(xaxis=list(fixedrange=TRUE)) %>%
      layout(yaxis=list(fixedrange=TRUE))
})

observeEvent(input$altura,{
  req(!is.na(input$altura))
  req(!is.na(vel()))
    v_init <- vel() 
    angulo <- angulo_rad() 
    escala <- 0.5 
    
    if(v_init == 0){
      
      x_end <- cos(angulo) * escala
      y_end <- input$altura +  sin(angulo) * escala
      
      lim <- input$altura + 5 
      
      p$plot <- plot_ly() %>%
        layout(
          xaxis = list(title = "Posição Horizontal (m)", range = c(0, lim)),
          yaxis = list(title = "Posição Vertical (m)", range = c(0, lim))) %>%
        add_segments(x = 0, y = input$altura, xend = x_end, yend = y_end, 
                     line = list(color = 'red', width = 2)) %>%
        config(displayModeBar = F) %>%
        layout(xaxis=list(fixedrange=TRUE)) %>%
        layout(yaxis=list(fixedrange=TRUE)) 
      
    }else{
      x_end <- v_init * cos(angulo) * escala
      y_end <- input$altura + v_init * sin(angulo) * escala
      
      p$plot <- plot_ly() %>%
        layout(
          xaxis = list(title = "Posição Horizontal (m)", range = c(0, lim_x())),
          yaxis = list(title = "Posição Vertical (m)", range = c(0, lim_x()))) %>%
        add_segments(x = 0, y = input$altura, xend = x_end, yend = y_end, 
                     line = list(color = 'red', width = 2)) %>%
        config(displayModeBar = F) %>%
        layout(xaxis=list(fixedrange=TRUE)) %>%
        layout(yaxis=list(fixedrange=TRUE))
    }
})


observeEvent(c(input$nex,input$incognita,input$sv, 
               input$v,input$altura,input$alt,input$s,input$theta), {
  p$plot <- NULL
})