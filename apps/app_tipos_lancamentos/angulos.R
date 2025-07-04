output$b <- renderUI({
  if(input$incognita==""){
    
  } else {
    actionButton("tabela", "Ângulos Notáveis")
  }
  
})


observeEvent(c(input$nex,input$incognita,input$tipo,input$sv),{
  output$tab <- renderUI({})
})


observeEvent(input$tabela,{
  output$tab <- renderUI({
  img(src="angulos.jpg", height = "200px", width = "300px")
  })
})
