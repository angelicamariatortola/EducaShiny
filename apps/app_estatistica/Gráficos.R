
output$grafico <- renderPlot({
  G <- Ex()$grafico
  y_min <- min(Ex()$y_min)
  #if (Ex()$tx == "2") {
  #x_min <- min(df()$Xvar)  
  #} else if (Ex()$tx == "1") {
  #x_min <- min(Ex()$x_min)  
  #}
  x_min <- min(Ex()$x_min)  
  req(df())
  req(dfa())
  #observeEvent({
  #  input$question 
  #  input$ENEM           
  #            }, {
  s <- selected_freq()
  s1 <- selected_xy()
  s2 <- selected_x()
  s3 <- selected_y()
  #})
  
  #TIPOS DE GRÁFICOS
  #Os gráficos são definidos por G (Gráficos na Aba de Exercícios) e R (Gráficos na Aba de Resposta). Os G são divididos em dois:
  # Aqueles que começam com 1 - Gráficos de Linhas
  # 10 - Não possui as Linhas Conectando os Pontos
  # 11 - Não possui as linhas Conectando os Pontos e possui labels numéricos nos Pontos
  # 12 - Não possui os labels em y
  # 13 - Normal (Possui labels em X e Y, e Linhas conectando os Pontos)
  # Aqueles que começam com 2 - Gráficos de Barras
  # 21 - Normal (Possui Labels em X e Y)
  # 22 - Gráfico "Deitado"
  
  if (G == 11 || G == 12 || G == 1 || G == 10 || G == 13) {
    gg <- ggplot(data = df(), aes(x = X, y = Y, group = 1)) +
      labs(x = Ex()$eixo_x, y = Ex()$eixo_y,
           title = Ex()$titulo)
    
  } else if (G == 2 || G == 22 || G == 21) {
    gg <- ggplot(data = df(), aes(x = X, y = Y)) +
      geom_col(position = "dodge") +
      labs(title = Ex()$titulo,
           x = Ex()$eixo_x,
           y = Ex()$eixo_y)
    
  #} else if (G == 3) {
  #  gg <- ggplot(df3(),aes(x = X, y = Y, Color = Type)) +
  #    labs(x = Ex()$eixo_x, y = Ex()$eixo_y,
  #        title = Ex()$titulo)  
  } else if (G == 4) {
    gg <- ggplot(df(),aes(x = X)) +
      geom_histogram(binwidth = 60) +
      labs(x = Ex()$eixo_x, y = Ex()$eixo_y,
           title = Ex()$titulo)  
  }
  
  
  
  if (G == 22) {
    gg = gg + coord_flip() 
  } else if (G == 12) {
    gg = gg + theme(axis.text.y = element_blank())
  } else if (G == 1 || G == 11 || G == 2 || G == 3) {
    gg = gg + geom_text(aes(x = X, y = Y, label = Y), vjust = -0.8)
  }
  
  if (G == 1 || G == 12 || G == 13 || G == 3) {
    gg = gg + geom_line(size = 1.5)
  }
  
  if (G == 11 || G == 12 || G == 1 || G == 10 || G == 13 || G == 3) {
    gg = gg + geom_point(aes(x = X, y = Y), size = 3)
  }
  
  if (Ex()$tx == "1") {
    gg = gg + scale_x_continuous(breaks = df()$X)
  } else if (Ex()$tx == "2") {
    gg = gg
  }
  
  
  
  x_line <- reactive(as.numeric(strsplit(Ex()$x_line, ",")[[1]]))
  y_line <- reactive(as.numeric(strsplit(Ex()$y_line, ",")[[1]]))
  LINE <- Ex()$line
  lab_plot <- strsplit(Ex()$labels_plot, ",")[[1]]
  LAB <- Ex()$labelsfactor
  GRID <- Ex()$grid_type
  
  
  
if (LINE == 0) {
  gg = gg + scale_y_continuous(limits = c(Ex()$y_min, Ex()$y_max)) 
} else if (LINE == 1) {
  gg = gg + scale_y_continuous(limits = c(Ex()$y_min, Ex()$y_max)) + scale_x_continuous(breaks = x_line(),limits = c(Ex()$x_min, Ex()$x_max))
} else if (LINE == 2) {
  gg = gg + scale_y_continuous(breaks = y_line(), limits = c(Ex()$y_min, Ex()$y_max))
} else {
  gg = gg + scale_y_continuous(breaks = y_line(), limits = c(Ex()$y_min, Ex()$y_max)) + scale_x_continuous(breaks = x_line(),limits = c(Ex()$x_min, Ex()$x_max))
}
  
  
  if (LAB == 0) {
    ##
  } else {
    gg <- gg + geom_text(aes(label = lab_plot),vjust = -1, hjust = 0.5 ,check_overlap = TRUE)
  }
  
  
  if (GRID == 1) {
    gg = gg + theme(panel.grid.major.x = element_line(color = "grey70"), panel.grid.major.y = element_line(color = "grey70"))
  } else if (GRID == 0) {
    gg = gg + theme(panel.grid = element_blank())
  } else if (GRID == 11) {
    gg = gg + theme(panel.grid.major.x = element_line(color = "grey70",linetype="dashed"), panel.grid.major.y = element_line(color = "grey70",linetype="dashed"))
  } else if (GRID == 2) {
    gg = gg + theme(panel.grid.major.y = element_line(color = "grey70"))
  } else if (GRID == 3) {
    gg = gg + theme(panel.grid.major.x = element_line(color = "grey70"))
  }
  
  
  #if (G == 3) {
  #  gg = gg + 
  #    geom_point(data = df3()[s1, ],
  #               color = "red", size = 4) +
  #    geom_point(data = df3()[s2, ],
  #               color = "red", size = 4) +
  #    geom_point(data = df3()[s3, ],
  #               color = "red", size = 4) +
  #    geom_point(data = df3()[df3()$Y %in% dfa2$Var1[s], ],
  #               color = "blue", size = 4) +
  #    geom_segment(data = df3()[s1, ],
  #                 aes(x = X, xend = X, y = y_min, yend = Y),
  #                 color = "purple", size = 1.5, linetype = "dashed") +
  #   geom_segment(data = df3()[s1, ],
  #                 aes(x = x_min, xend = X, y = Y, yend = Y),
  #                 color = "purple", size = 1.5, linetype = "dashed") +
  #    geom_segment(data = df3()[s2, ],
  #                 aes(x = X, xend = X, y = y_min, yend = Y),
  #                 color = "purple", size = 1.5, linetype = "dashed") +
  #    geom_segment(data = df3()[s3, ],
  #                 aes(x = x_min, xend = X, y = Y, yend = Y),
  #                 color = "purple", size = 1.5, linetype = "dashed")
  #}
    gg = gg +
      geom_segment(data = df()[s1, ],
                   aes(x = X, xend = X, y = y_min, yend = Y),
                   color = "purple", size = 1.5, linetype = "dashed") +
      geom_segment(data = df()[s1, ],
                   aes(x = x_min, xend = X, y = Y, yend = Y),
                   color = "purple", size = 1.5, linetype = "dashed") +
      geom_segment(data = df()[s2, ],
                   aes(x = X, xend = X, y = y_min, yend = Y),
                   color = "purple", size = 1.5, linetype = "dashed") +
      geom_segment(data = df()[s3, ],
                   aes(x = x_min, xend = X, y = Y, yend = Y),
                   color = "purple", size = 1.5, linetype = "dashed") +
      geom_point(data = df()[df()$Y %in% dfa()$Var1[s], ],
                 color = "blue", size = 4) +
      geom_point(data = df()[s1, ],
                 color = "red", size = 4) +
      geom_point(data = df()[s2, ],
                 color = "red", size = 4) +
      geom_point(data = df()[s3, ],
                 color = "red", size = 4)
  
  gg = gg + theme(axis.text=element_text(size=14, face = "bold"))
  
  #if (Ex()$x_max == 0) {
  #  gg = gg
  #} else {
  #  gg = gg + scale_x_continuous(limits = c(Ex()$x_min, Ex()$_max))
  #}
  rv$gg <- gg 
  rv$gg
})