output$b1H <- renderUI({
  
  if(input$incognitaH==""){
    
  }
  else{
    actionButton("eqH", "📚 Equação")
  }
})

eH<- reactiveValues(equaçãoH=NULL)

eH1<- reactiveValues(equH=0)

observeEvent(input$eqH,{
  eH1$equH <- eH1$equH+1
})

observeEvent(c(input$nexH,input$incognitaH,input$tipoH),{
  eH1$equH <- 0
  eH$equaçãoH<- NULL
})

observeEvent(input$eqH,{
  if(eH1$equH %% 2 == 0){
    eH$equaçãoH<- NULL
  }
  else{
    eH$equaçãoH <- isolate(
      if(input$incognitaH == "altura") {
        if(input$sH == "velocidade e tempo"){
          tagList(
            p(withMathJax("\\(y = \\dfrac{g \\times t^2}{2}\\)"), align="center"),
            p("y - altura máxima"),
            p("g - gravidade"),
            p("t - tempo")
          )
        }
        else {
          tagList(
            p(withMathJax("\\(t = \\dfrac{x}{v}\\)"), align="center"),
            p("t - tempo"),
            p("x - alcance"),
            p("v - velocidade"),
            p(withMathJax("\\(y = \\dfrac{g \\times t^2}{2}\\)"), align="center"),
            p("y - altura máxima"),
            p("g - gravidade")
          )
        }
        
        
      }   
      else if(input$incognitaH == "alcance") {
        if(input$sH == "velocidade e tempo"){
          tagList(
            p(withMathJax("\\(x = v_{0} \\times {t}\\)"), align="center"),
            p("x - alcance"),
            p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
            p("t - tempo")
          )
        }
        else {
          tagList(
            p(withMathJax("\\(t = \\sqrt{\\dfrac{2 \\times y}{g}}\\)"), align="center"),
            p("t - tempo"),
            p("y - altura máxima"),
            p("g - gravidade"),
            p(withMathJax("\\(x = v_{0} \\times {t}\\)"), align="center"),
            p("x - alcance"),
            p("v - velocidade")
          )
        }
      }
      else if(input$incognitaH == "velocidade") {
        tagList(
          p(withMathJax("\\(t = \\sqrt{\\dfrac{2 \\times y}{g}}\\)"), align="center"),
          p("t - tempo"),
          p("y - altura máxima"),
          p("g - gravidade"),
          p(withMathJax("\\(v_{0} = \\dfrac{x}{t}\\)"), align="center"),
          p("x - alcance"),
          p("v - velocidade")
        )
      }
    )}
})

output$t_eqH <- renderUI({
  eH$equaçãoH
})  
