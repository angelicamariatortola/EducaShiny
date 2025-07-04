# Carregar pacotes
library(shiny)
library(plotly)
library(numbers)
library(DT)

#função que realiza a subtração de um vetor
subtract_vec <- function(s){ #s é um vetor numérico
  subtract_val <- s[1]
  for(i in 2:length(s)){
    subtract_val <- subtract_val - s[i]
  }
  return(subtract_val)
}
#---------------------------------------------------------------------------------------------------------------- 
ui1 <- sidebarLayout(
  sidebarPanel(
    #Painel com os inputs
    numericInput("num_elements", "Escolha em quantas partes quer dividir cada círculo", value = 3, min = 2, max = 10),
    numericInput("num_charts", "Número de Gráficos:", value = 2, min = 1, max = 6),
    selectInput("operator", h4("Operador Matemático"), choices = c("+","-", "*")),#, "/")),
    verbatimTextOutput("numerador"),
    h3("Resultado da operação"),
    withMathJax(
      uiOutput("fractionsOutput")
    ),
    actionButton("show_answer_result", "Mostrar resposta"),
    h3("Fração reduzida"),
    withMathJax(
      uiOutput("fractionsOutput_redux")
    ),
    actionButton("show_answer_redux", "Mostrar resposta"),
    h3("Fração mista"),
    withMathJax(
      uiOutput("fractionsOutput_mixed")
    ),
    actionButton("show_answer_mixed", "Mostrar resposta"),
    actionButton("show_answer_graph", "Mostrar gráficos")
  ),
  mainPanel(
    #Painel com os resultados
    fluidRow(
    column(4,
           uiOutput("charts")
    ),
    column(4,
#           conditionalPanel(
#             condition = "input.show_answer % 2 == 1",
             uiOutput("chart_result")
#           )
    ),
    column(2,
#           conditionalPanel(
#             condition = "input.show_answer % 2 == 1",
             uiOutput("chart_result2")
#           )
    )
  ),
  )
)
#----------------------------------------------------------------------------------------------------------------
ui2 <- sidebarLayout(
  sidebarPanel(
    #Painel com os inputs
    numericInput("elements1", "Divisões do primeiro gráfico:", value = 2, min = 2, max = 10),
    numericInput("elements2", "Divisões do segundo gráfico:", value = 2, min = 2, max = 10),
    selectInput("operator_dif", h4("Operador Matemático"), choices = c("+","-", "*")),#, "/")),
    h3("Resultado da operação"),
    withMathJax(
      uiOutput("fractionsOutput_dif")
    ),
    actionButton("show_answer_result_dif", "Mostrar resposta"),
    h3("Fração reduzida"),
    withMathJax(
      uiOutput("fractionsOutput_redux_dif")
    ),
    actionButton("show_answer_redux_dif", "Mostrar resposta"),
    h3("Fração mista"),
    withMathJax(
      uiOutput("fractionsOutput_mixed_dif")
    ),
    actionButton("show_answer_mixed_dif", "Mostrar resposta"),
    actionButton("show_answer_graph_dif", "Mostrar gráficos"),
    actionButton("show_tables", "Mostrar tabuadas")
  ),
  mainPanel(
  #Painel com os resultados    
    column(3,
           uiOutput("charts_dif"),
    ),
    column(3,
             uiOutput("chart_result_dif")
    ),
    column(3,
             uiOutput("chart_result2_dif")
    ),
    column(1,
           #mostra as tabuadas
             textOutput("TabuadaA"),
             DTOutput("table1"),
             textOutput("TabuadaB"),
             DTOutput("table2")
           )
  )
  
)

# Interface do usuário
ui <- fluidPage(

# Título da aplicação
titlePanel("Operações com frações"),

tabsetPanel(

  tabPanel("Denominadores Iguais", ui1
           ),
 
  tabPanel("Denominadores Diferentes", ui2
  )
)
)

#-----------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  render_answer_result <- reactiveVal(FALSE) #variável que detemina se a resposta será mostrada
  observeEvent(input$show_answer_result, {   #mostra a resposta ao clicar no botão
    render_answer_result(TRUE)
  })
  render_answer_redux <- reactiveVal(FALSE) #variável que detemina se a fração reduzida será mostrada
  observeEvent(input$show_answer_redux, {   #mostra a resposta ao clicar no botão
    render_answer_redux(TRUE)
  })
  render_answer_mixed <- reactiveVal(FALSE) #variável que detemina se a fração mista será mostrada
  observeEvent(input$show_answer_mixed, {   #mostra a resposta ao clicar no botão
    render_answer_mixed(TRUE)
  })
  render_answer_graph <- reactiveVal(FALSE) #variável que detemina se o gráfico será mostrado
  observeEvent(input$show_answer_graph, {   #mostra o gráfico ao clicar no botão
    render_answer_graph(TRUE)
  })

  
  
  observeEvent({ #observa os eventos que apagam a resposta
    input$num_elements
    input$num_charts
    input$operator
    numerator_original()
  }, {
    render_answer_result(FALSE) #apaga as respostas
    render_answer_redux(FALSE)
    render_answer_mixed(FALSE)
    render_answer_graph(FALSE)
  })
  #resetar o título dos gráficos
  observeEvent({ #observa os eventos que apagam a resposta
    input$num_elements
    input$num_charts
  },{
    numerator_original(rep(0,input$num_charts))
  })
  
  num_elements <- reactive(input$num_elements) #determina o número de divisões reativamente
  num_charts <- reactive(input$num_charts)     #determina o número de gráficos reativamente
  
  #cria um dataframe para os gráficos de input
  data <- reactive({
    data.frame(
      labels = LETTERS[1:num_elements()],
      values = rep(1, num_elements()),  # Todos os valores são iguais para que os setores tenham o mesmo tamanho
      colors = rep("white", num_elements())  # Inicialmente, todas as cores são brancas
    )
  })

  section_colors <- reactiveVal()  # Variável reativa para armazenar as cores dos setores de cada gráfico
  
  #inicializa as cores dos gráficos
  observe({
    colors <- vector("list", num_charts())
    for (i in 1:num_charts()) {
      colors[[i]] <- rep("white", num_elements())
    }
    section_colors(colors)
  })
  
  # Renderizar os gráficos de setores dinamicamente
  output$charts <- renderUI({
    chart_outputs <- lapply(1:num_charts(), function(i) {
      plotlyOutput(paste0("pieChart", i))
    })
  })
  
  # Renderizar cada gráfico de setores usando plotly
  observe({
    lapply(1:input$num_charts, function(i){
      local({
        my_i <- i
        output[[paste0("pieChart", my_i)]] <- renderPlotly({
          plot_data <- data()
          plot_data$colors <- section_colors()[[my_i]]
          plot_ly(data(), labels = ~labels, values = ~values, textposition = "none", hoverinfo ="none",
                  marker = list(colors = plot_data$colors,line = list(color = 'black', width = 2)),source = paste0("source", my_i) , type = 'pie') %>%
            layout(title = paste(numerator_original()[i],'/',num_elements()), showlegend = FALSE)
        })
      })
    })
  })
  
  numerator_original <- reactiveVal(NULL) #vetor com a quantidade de seções selecionadas de cada gráfico
  observe({numerator_original(rep(0,input$num_charts))}) #tamanho do vetor é determinado pelo número de gráficos
  

  # Atualizar a variável reativa quando uma seção de qualquer gráfico é clicada
  lapply(1:10, function(i) {
    observeEvent(event_data("plotly_click", source = paste0("source", i), priority = "event"), {
      event_data <- event_data("plotly_click", source = paste0("source", i), priority = "event")
      if (!is.null(event_data)) {
        chart_id <- i
        index <- event_data$pointNumber + 1
        current_colors <- section_colors()
        current_colors[[chart_id]][index] <- if (current_colors[[chart_id]][index] == "white") "red" else "white"
        section_colors(current_colors)
        
        
        #Soma o número de seções vermelhas para determinar o numerador 
        num_red_sections <- sapply(current_colors, function(colors) sum(colors == "red"))
        numerator_original(num_red_sections)

        

      }
    })
  })
  #declara todas as variáveis que estarão dentro dos observe
  redux_num <- reactiveVal(NULL) #numerador da fração reduzida
  redux_den <- reactiveVal(NULL) #denominador da fração reduzida
  redux <- reactiveVal(NULL) #resultado da fração reduzida
  inteiro <- reactiveVal(0) #valor do número inteiro da fração mista
  mixed_frac <- reactiveVal(NULL) #resultado da fração mista
  graph_num <- reactiveVal(NULL) #numerador do gráfico de resultado
  result <- reactiveVal(NULL) #resultado da operação inicial

  
  observe({
    denominator_vec <- reactiveVal( #cria um vetor com o número de divisões com o tamanho do número de gráficos
      rep(num_elements(),input$num_charts)
    )
    vec <- numerator_original()
    #transforma o vetor das seleções em uma fração
    fractions <- sapply(1:length(vec), function(i) {
      paste0("\\frac{", vec[i], "}{", denominator_vec()[i], "}")
    })
    operador <- input$operator
    latex_code <- paste(fractions, collapse = operador) #cria uma representação da expressão das frações para mostrar em latex
    
    #realiza as operações de acordo com o operador selecionado
    if(operador == "+"){
      soma <- reactiveVal(
        sum(numerator_original())
      )
      result_num <-reactiveVal(soma())
      result_den <-reactiveVal(num_elements())
    }
    else if(operador == "-"){
      subtracao <- reactiveVal(
        subtract_vec(numerator_original())
      )
      result_num <-reactiveVal(subtracao())
      result_den <-reactiveVal(num_elements())
    }
    else if(operador == "*"){
      num_prod <- reactiveVal(
        prod(numerator_original())
      )
      den_prod <- reactiveVal(
        prod(denominator_vec())
      )
      result_num <-reactiveVal(num_prod())
      result_den <-reactiveVal(den_prod())
    }
    else if(operador == "/"){
      temp1 <-  numerator_original()
      temp1[1] <- input$num_elements
      result_den<-reactiveVal(
        prod(temp1))
      
      temp2 <- denominator_vec()
      temp2[1] <- numerator_original()[1]
      result_num <- reactiveVal(
        prod(temp2))
    }
    result(paste0(latex_code,"=","\\frac{",  result_num(), "}{",  result_den(), "}")) #armazena o resultado sem simplificação
    
    #Calcula a simplificação da fração
    mdc <- GCD(result_num(),result_den())
    redux_num(result_num()/mdc)
    redux_den(result_den()/mdc)

    if(mdc != 1){
      redux({paste0("","\\frac{",  redux_num(), "}{",  redux_den(), "}")})
    }else{
      redux(paste0("","\\frac{",  redux_num(), "}{",  redux_den(), "}"))
    }

    #Calcula a fração mista
    if(abs(redux_num())>redux_den()){
      inteiro(trunc(redux_num()/redux_den()))
      mixed_num <- redux_num()%%redux_den()
      
      if(inteiro()>=0){
        mixed_frac({paste0("=",abs(inteiro()),"+","\\frac{",  mixed_num, "}{",  redux_den(), "}")})
        graph_num(mixed_num)
      }
      else{
        mixed_frac({paste0("= -(",abs(inteiro()),"+","\\frac{",  abs(mixed_num), "}{",  redux_den(), "}",")")})
        graph_num(mixed_num*(-1))
      }
      
    }
    else{
      inteiro(0)
      mixed_frac(paste0(""))
      graph_num(redux_num())
    }
  })
  
  #mostra a equação
  output$fractionsOutput <- renderUI({
    if(render_answer_result()){
      p(withMathJax(sprintf("$$ %s $$",result())))
    }
  })
  
  #Mostra o resultado reduzido
  output$fractionsOutput_redux <- renderUI({
    if(render_answer_redux()){
    p(withMathJax(sprintf("$$ %s $$",redux())))
    }
  })
  
  #Mostra o resultado misto
  output$fractionsOutput_mixed <- renderUI({
    if(render_answer_mixed()){
    p(withMathJax(sprintf("$$ %s $$",mixed_frac())))
    }
  })
  
  #cria o dataframe do gráfico de resposta
  observe({
#  observeEvent(input$show_answer, {
    data_r <- reactive({
      #avalia se o numerador é positivo
      if(graph_num()>=0){
        values_aux = rep(1, redux_den()) 
        colors_aux = c(rep('red',graph_num()),rep('white',redux_den()-graph_num()))
      }
      else{
        values_aux = rep(1, redux_den())  
        colors_aux = c(rep('blue',abs(graph_num())),rep('white',redux_den()-abs(graph_num())))
      }
      
      data.frame(
        labels = LETTERS[1:redux_den()],
        values = values_aux,
        colors = colors_aux
      )
      
    })
    plot_data_r <- data_r()
    print(inteiro())
    #cria o dataframe dos gráficos dos números inteiros caso o resultado seja fração mista
    data_r2 <- reactive({
      if(inteiro()>=1){
        colors_aux2 = c('red')
      }
      else{
        colors_aux2 = c('blue')
      }
      data.frame(
        labels = LETTERS[1],
        values = c(1),  
        colors = colors_aux2
      )
    })
    plot_data_r2 <- data_r2()
    
  #plota os gráficos de resposta
    output$chart_result <- renderUI({
      if(render_answer_graph()){
      result_graph <- renderPlotly({
        plot_ly(data_r(), labels = ~labels, values = ~values, textposition = "none", hoverinfo ="none",
                marker = list(colors = plot_data_r$colors,line = list(color = 'black', width = 2)) , type = 'pie') %>%
          layout(title = paste(graph_num(),'/',redux_den()), showlegend = FALSE)
      })
      }
    })
    #plota os gráficos dos valores inteiros
#      if(inteiro()!= 0){
      output$chart_result2 <- renderUI({
        if(render_answer_graph()&&inteiro()!= 0){
        result_graph2 <- lapply(1:abs(inteiro()), function(i) {
          plotlyOutput(paste0("inteiros", i))
        })
      }
      })
      
      
      #contrói cada gráfico de setores usando plotly
      observe({
        if(abs(inteiro())>=1){
        lapply(1:abs(inteiro()), function(i){
          local({
            my_i <- i
            output[[paste0("inteiros", my_i)]] <- renderPlotly({
              plot_ly(data_r2(), labels = ~labels, values = ~values, textposition = "none", hoverinfo ="none",
                      marker = list(colors = plot_data_r2$colors,line = list(color = 'black', width = 2)) , type = 'pie') %>%
                layout(title = paste(''), showlegend = FALSE)
            })
          })
        })
      }
      })
#      }
  })
  
#-----------------------------------------------------------------------------------------------------------------------
# Denominadores Diferentes
  
  render_answer_result_dif <- reactiveVal(FALSE) #variável que detemina se a resposta será mostrada
  observeEvent(input$show_answer_result_dif, {   #mostra a resposta ao clicar no botão
    render_answer_result_dif(TRUE)
  })
  render_answer_redux_dif <- reactiveVal(FALSE) #variável que detemina se a fração reduzida será mostrada
  observeEvent(input$show_answer_redux_dif, {   #mostra a resposta ao clicar no botão
    render_answer_redux_dif(TRUE)
  })
  render_answer_mixed_dif <- reactiveVal(FALSE) #variável que detemina se a fração mista será mostrada
  observeEvent(input$show_answer_mixed_dif, {   #mostra a resposta ao clicar no botão
    render_answer_mixed_dif(TRUE)
  })
  render_answer_graph_dif <- reactiveVal(FALSE) #variável que detemina se o gráfico será mostrado
  observeEvent(input$show_answer_graph_dif, {   #mostra a resposta ao clicar no botão
    render_answer_graph_dif(TRUE)
  })
  render_tables <- reactiveVal(FALSE) #variável que detemina se as tabuadas serão mostradas
  observeEvent(input$show_tables, {   #mostra as tabuadas ao clicar no botão
    render_tables(TRUE)
  })
  
  observeEvent({ #observa os eventos que apagam a resposta
    input$elements1
    input$elements2
    input$operator_dif
    numerator_original_dif()
  }, {
    render_answer_result_dif(FALSE) #apaga as respostas
    render_answer_redux_dif(FALSE)
    render_answer_mixed_dif(FALSE)
    render_answer_graph_dif(FALSE)
    render_tables(FALSE)
  })
  #reseta o título dos gráficos
  observeEvent({ #observa os eventos que apagam a resposta
    input$elements1
    input$elements2
  },{
    numerator_original_dif(c(0,0))
  })
  
  section_colors_dif <- reactiveVal() #declara a variável que determina as cores dos gráficos de input
  
  observe({
#  observeEvent(input$generate, {
    # Obter o tamanho dos vetores do input do usuário
    denominator1 <- input$elements1
    denominator2 <- input$elements2
    denominators <- c(denominator1, denominator2)
    
    # Função para gerar o dataframe dos gráficos de input
    generate_df <- function(num_el) {
      labels_ <- LETTERS[1:num_el]
      values_ <- rep(1, num_el)
      colors_ <- rep("white", num_el)
      
      df <- data.frame(
        labels = labels_,
        values = values_,
        colors = colors_
      )
      return(df)
    }
    
    # Gerar os dataframes
    df1 <- generate_df(denominator1)
    df2 <- generate_df(denominator2)
    
    
    # Atualizar cores
    cores <- list()
    for (i in 1:2) {
      cores[[i]] <- rep('white', times = denominators[i])
    }
    section_colors_dif(cores)
    
    # Renderizar o dataframe
    output$dataframe <- renderTable({
      df1
    })
    
    # Renderizar os gráficos
    output$charts_dif <- renderUI({
      chart_outputs_dif <- lapply(1:2, function(i){
        plotlyOutput(paste0("pieChart_", i))
      })
    })
    #constrói os gráficos
    lapply(1:2, function(i){
      output[[paste0("pieChart_", i)]] <- renderPlotly({
        plot_data <- if (i == 1) df1 else df2
        plot_data$colors <- section_colors_dif()[[i]]
        plot_ly(plot_data, labels = ~labels, values = ~values, textposition = "none", hoverinfo = "none",
                marker = list(colors = plot_data$colors, line = list(color = 'black', width = 2)),source = paste0("source_", i)  ,type = 'pie') %>%
          layout(title = paste(numerator_original_dif()[i],'/',denominators[i]) , showlegend = FALSE)
        #      event_register(p, 'plotly_click')
      })
    })
    
  })
  
  numerator_original_dif <- reactiveVal(NULL) #vetor com a quantidade de seções selecionadas de cada gráfico
  denominators_dif <- reactiveVal(NULL)
  observe({numerator_original_dif(rep(0,2))}) #inicializa o vetor com tamanho 2 pois são apenas 2 gráficos
  
  #registra quais seções foram clicadas
  lapply(1:2, function(i) {
    observeEvent(event_data("plotly_click", source = paste0("source_", i), priority = "event"), {
      event_data <- event_data("plotly_click", source = paste0("source_", i), priority = "event")
      if (!is.null(event_data)) {
        chart_id <- i
        index <- event_data$pointNumber + 1
        current_colors <- section_colors_dif()
        current_colors[[chart_id]][index] <- if (current_colors[[chart_id]][index] == "white") "red" else "white"
        section_colors_dif(current_colors)
        
        
        #Soma o número de seções vermelhas para determinar o numerador 
        num_red_sections <- sapply(current_colors, function(cores) sum(cores == "red"))
        numerator_original_dif(num_red_sections)
        #        print(numerator_original_dif())
      }
    })
  })
  #declara todas as variáveis que estarão dentro dos observe
  redux_num_dif <- reactiveVal(NULL)   #numerador da fração reduzida
  redux_den_dif <- reactiveVal(NULL)   #denominador da fração reduzida
  redux_dif <- reactiveVal(NULL)       #resultado da fração reduzida
  inteiro_dif <- reactiveVal(0)        #valor do número inteiro da fração mista
  mixed_frac_dif <- reactiveVal(NULL)  #resultado da fração mista
  graph_num_dif <- reactiveVal(NULL)   #numerador do gráfico de resultado
  result_dif <- reactiveVal(NULL)      #resultado da operação inicial

  observe({
    denominators_dif(c(input$elements1, input$elements2)) #cria um vetor com os denominadores das frações

    vec <- numerator_original_dif()
    #transforma o vetor das seleções em uma fração
    fractions_dif <- sapply(1:length(vec), function(i) {
      paste0("\\frac{", vec[i], "}{", denominators_dif()[i], "}")
    })
    operador_dif <- input$operator_dif
    latex_code_dif <- paste(fractions_dif, collapse = operador_dif) #cria uma representação da expressão das frações para mostrar em latex
    
    #realiza as operações de acordo com o operador selecionado
    if(operador_dif == "+"){
      resultado_denominador_dif <- mLCM(denominators_dif())
      resultado_numerador_dif <- sum(vec*(resultado_denominador_dif/denominators_dif()))
      
      result_num_dif <-reactiveVal(resultado_numerador_dif)
      result_den_dif <-reactiveVal(resultado_denominador_dif)
    }
    else if(operador_dif == "-"){
      resultado_denominador_dif <- mLCM(denominators_dif())
      resultado_numerador_dif <- subtract_vec(vec*(resultado_denominador_dif/denominators_dif()))
      
      result_num_dif <-reactiveVal(resultado_numerador_dif)
      result_den_dif <-reactiveVal(resultado_denominador_dif)
    }
    else if(operador_dif == "*"){
      num_prod <- reactiveVal(
        prod(numerator_original_dif())
      )
      den_prod <- reactiveVal(
        prod(denominators_dif())
      )
      result_num_dif <-reactiveVal(num_prod())
      result_den_dif <-reactiveVal(den_prod())
    }
    else if(operador_dif == "/"){
      temp1 <-  numerator_original_dif()
      temp1[1] <- input$num_elements
      result_den_dif<-reactiveVal(
        prod(temp1))
      
      temp2 <- denominators_dif()
      temp2[1] <- numerator_original_dif()[1]
      result_num_dif <- reactiveVal(
        prod(temp2))
    }
    result_dif(paste0(latex_code_dif,"=","\\frac{",  result_num_dif(), "}{",  result_den_dif(), "}"))
    
    #Calcula a simplificação da fração
    mdc_dif <- GCD(result_num_dif(),result_den_dif())
    redux_num_dif(result_num_dif()/mdc_dif)
    redux_den_dif(result_den_dif()/mdc_dif)
    
    if(mdc_dif != 1){
      redux_dif({paste0("=","\\frac{",  redux_num_dif(), "}{",  redux_den_dif(), "}")})
    }else{
      redux_dif(paste0("","\\frac{",  redux_num_dif(), "}{",  redux_den_dif(), "}"))
    }
    
    #Calcula a fração mista
    if(abs(redux_num_dif())>redux_den_dif()){
      inteiro_dif(trunc(redux_num_dif()/redux_den_dif()))
      mixed_num_dif <- redux_num_dif()%%redux_den_dif()
      
      if(inteiro_dif()>=0){
        mixed_frac_dif({paste0("=",abs(inteiro_dif()),"+","\\frac{",  mixed_num_dif, "}{",  redux_den_dif(), "}")})
        graph_num_dif(mixed_num_dif)
      }
      else{
        mixed_frac_dif({paste0("= -(",abs(inteiro_dif()),"+","\\frac{",  mixed_num_dif, "}{",  redux_den_dif(), "}",")")})
        graph_num_dif(mixed_num_dif*(-1))
      }
    }
    else{
      inteiro_dif(0)
      mixed_frac_dif(paste0(""))
      graph_num_dif(redux_num_dif())
    }
    
  })
  
  #mostra a equação
  output$fractionsOutput_dif <- renderUI({
    if(render_answer_result_dif()){
      p(withMathJax(sprintf("$$ %s $$",result_dif())))
    }
  })
  
  #Mostra o resultado reduzido
  output$fractionsOutput_redux_dif <- renderUI({
    if(render_answer_redux_dif()){
      p(withMathJax(sprintf("$$ %s $$",redux_dif())))
    }
  })
  
  #Mostra o resultado misto
  output$fractionsOutput_mixed_dif <- renderUI({
    if(render_answer_mixed_dif()){
      p(withMathJax(sprintf("$$ %s $$",mixed_frac_dif())))
    }
  })

  observe({
    data_r <- reactive({
      #avalia se o numerador é positivo
      if(graph_num_dif()>=0){
        values_aux = rep(1, redux_den_dif()) 
        colors_aux = c(rep('red',graph_num_dif()),rep('white',redux_den_dif()-graph_num_dif()))
      }
      else{
        values_aux = rep(1, redux_den_dif())  
        colors_aux = c(rep('blue',abs(graph_num_dif())),rep('white',redux_den_dif()-abs(graph_num_dif())))
      }
      
      data.frame(
        labels = LETTERS[1:redux_den_dif()],
        values = values_aux,
        colors = colors_aux
      )
      
    })
    plot_data_r <- data_r()
    
    data_r2 <- reactive({
        if(inteiro_dif()>=1){
          colors_aux2 = c('red')
        }
        else{
          colors_aux2 = c('blue')
        }
      data.frame(
        labels = LETTERS[1],
        values = c(1),  
        colors = colors_aux2
      )
    })
    plot_data_r2 <- data_r2()
    
    #plota os gráficos de resposta
    output$chart_result_dif <- renderUI({
      if(render_answer_graph_dif()){
      result_graph <- renderPlotly({
        plot_ly(data_r(), labels = ~labels, values = ~values, textposition = "none", hoverinfo ="none",
                marker = list(colors = plot_data_r$colors,line = list(color = 'black', width = 2)) , type = 'pie') %>%
          layout(title = paste(graph_num_dif(),'/',redux_den_dif()), showlegend = FALSE)
      })
    }
    })
    
    #plota os gráficos dos valores inteiros

      output$chart_result2_dif <- renderUI({
        if(render_answer_graph_dif()&&inteiro_dif()!=0){
        result_graph2 <- lapply(1:abs(inteiro_dif()), function(i) {
          plotlyOutput(paste0("inteiros", i))
        })
        }
      })
      
      # Renderizar cada gráfico de setores usando plotly
      observe({
        if(abs(inteiro_dif())>=1){
        lapply(1:abs(inteiro_dif()), function(i){
          local({
            my_i <- i
            output[[paste0("inteiros", my_i)]] <- renderPlotly({
              plot_ly(data_r2(), labels = ~labels, values = ~values, textposition = "none", hoverinfo ="none",
                      marker = list(colors = plot_data_r2$colors,line = list(color = 'black', width = 2)) , type = 'pie') %>%
                layout(title = paste(''), showlegend = FALSE)
            })
          })
        })
        }
      })
    
  })

  
  
  #---------------------------------------------------------------------------------------------------------
  #mostra as tabuadas
  observe({
 # observeEvent(input$generate_table, {
    denominator1 <- input$elements1
    denominator2 <- input$elements2
    
    gerar_tabuada <- function(num_div) {
      
      Multiplicando <- 1:10
      Multiplicador <- num_div
      Resultado <- num_div * 1:10
      
      tabuada <- data.frame(
        Multiplicando = Multiplicando,
        x = "X",
        Multiplicador = Multiplicador,
        igual = "=",
        Resultado = Resultado
      )
      return(tabuada)
    }
    
    tabuada1 <- gerar_tabuada(denominator1)
    tabuada2 <- gerar_tabuada(denominator2)
    
    # Encontrar linhas com resultados iguais
    highlighted_rows <- reactive({
      common_results <- intersect(tabuada1$Resultado, tabuada2$Resultado)
      tabuada1_highlight <- tabuada1[tabuada1$Resultado %in% common_results, ]
      tabuada2_highlight <- tabuada2[tabuada2$Resultado %in% common_results, ]
      
      list(
        tabuada1 = tabuada1_highlight,
        tabuada2 = tabuada2_highlight
      )
    })
    
    output$TabuadaA <- renderText({
      if(render_tables()){
        paste0("Tabuada do ", input$elements1)
      }
    })
    output$TabuadaB <- renderText({
      if(render_tables()){
        paste0("Tabuada do ", input$elements2)
      }
    })
    
    
    output$table1 <- renderDT({
      if(render_tables()){
      datatable(tabuada1, options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE, colnames = NULL) %>%
        formatStyle('Resultado', target = 'row',
                    backgroundColor = styleEqual(highlighted_rows()$tabuada1$Resultado, rep('yellow', nrow(highlighted_rows()$tabuada1))))
      }
    })
    
    output$table2 <- renderDT({
      if(render_tables()){
      datatable(tabuada2, options = list(pageLength = 10, dom = 't', ordering = FALSE), rownames = FALSE, colnames = NULL) %>%
        formatStyle('Resultado', target = 'row',
                    backgroundColor = styleEqual(highlighted_rows()$tabuada2$Resultado, rep('yellow', nrow(highlighted_rows()$tabuada2))))
      }
    })
  })
}
#-----------------------------------------------------------------------------------------------------------------------------------
shinyApp(ui, server)
