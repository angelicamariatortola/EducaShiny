# seleciona o exercício e extrai dados__________________________________________
dados <- read_xlsx("lancamento obliquo.xlsx")  

dados1 <- reactive({
  req(input$incognita)
  if (input$incognita == "altura máxima") {
    filter(dados, pergunta == "altura max")
  } else if (input$incognita == "alcance") {
    filter(dados, pergunta == "alcance")
  } else if (input$incognita == "tempo total") {
    filter(dados, pergunta == "tempo total")
  }else if (input$incognita == "velocidade") {
    filter(dados, pergunta == "vel")
  } else {
    dados 
  }
})

output$Seg <- renderUI({
  if(input$incognita=="velocidade"){
    tagList(
      selectInput("sv","Quais variáveis usará no cálculo?", choices = sv0),
      textOutput("aseg")
  )}
})

output$aseg<- renderText(ff())

ff<-reactive({
  if(input$tipo=="fechado"){
    if(input$incognita=="velocidade"){
      if(input$sv=="ângulo e alcance"){
        if(alcance()==0){
          validate("O enunciado disponibiliza o ângulo e altura")
        }
      } else if (input$sv=="ângulo e altura"){
        if(altura_max()==0){
          validate("O enunciado disponibiliza o ângulo e alcance")
        }
      }
    }
  }else {
    
  }
})

num_ex <- reactive({
  nrow(dados1())
})

output$ex <- renderUI({
  req(input$tipo=="fechado")
  selectInput("nex","exercício:", choices = seq_len(num_ex()))
})

enunciado <- reactive(dados1()[input$nex,"enunciado"])  
pergunta <- reactive(dados1()[input$nex,"pergunta"])
angulo <- reactive(as.numeric(dados1()[input$nex,"angulo"]))
velocidade <- reactive(as.numeric(dados1()[input$nex,"velocidade"]))
alcance <- reactive(as.numeric(dados1()[input$nex,"alcance"]))
altura_i <- reactive(as.numeric(dados1()[input$nex,"altura inicial"]))
altura_max <- reactive(as.numeric(dados1()[input$nex,"altura max"]))
segv <- reactive(as.character(dados1[input$nex,"variavel"]))

output$enunciado <- renderUI({
  if(input$tipo=="aberto"){
    
  }
  else if(input$tipo=="fechado"){
    p(enunciado(),style = "text-align: justify;")
  }
})