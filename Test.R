################################################################################
# Libbrerie per i grafici ed le analisi esplorative
library(shiny)
library(shinythemes)
library(htmltools)
library(ggplot2)
library(data.table)
library(RColorBrewer)
options(scipen=1000)
################################################################################

################################################################################
#
# Imbellettare
#
################################################################################
not_sel <- "Non Selezionata"


################################################################################
#
# SEZIOPNE UI 
#
################################################################################

ui <- fluidPage(
  navbarPage("Esempio di cose da fare:",
             theme = shinytheme('united'),
             tabPanel("Caricamento",
                      titlePanel("Caricamento Dati"),
                      # Inputs per il Sidebar panel ----
                      sidebarLayout(
                        sidebarPanel(
                          # Input: Selezione file ----
                          fileInput("file1", "Scegli il file CSV",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          # Linea orizzontale ----
                          hr(),
                          # Input: Checkbox per l'intestazione del file ----
                          checkboxInput("header", "Intestazione", TRUE),
                          # Input: Select separator ----
                          radioButtons("sep", "Separatore",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          # Linea orizzontale ----
                          hr(),
                          # Input: Selezione del numero di linee da visualizzare ----
                          radioButtons("disp", "Mostra",
                                       choices = c(Head = "Inizio",
                                                   All = "Tutto"),
                                       selected = "Inizio"),
                          # Input: Selezione del tipo di virgolette  ----
                          radioButtons("quote", "Virgolette",
                                       choices = c("Nessune" = "",
                                                   "Doppie" = '"',
                                                   "Singole" = "'"),
                                       selected = '')),
                        # Pannello principale ----
                        mainPanel(
                          # Output: Statistiche descrittive dei dati caricati ----
                          h3("Statistiche derscrittive dei dati caricati:"),
                          hr(),
                          verbatimTextOutput("summary"),
                          h3("Esempio di dati caricati:"),
                          hr(),
                          tableOutput("contents")
                        ))),
             # Sezione statistiche univariate ----
             tabPanel("Statistiche Univariate",
                      titlePanel("Univariate!"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("variableUNI", "Selezione variabile:",
                                      choices = colnames(data))), #c(not_sel)))
                        mainPanel(
                          h3("Statistiche descrittive:"),
                          hr(),
                          textOutput("Fattore"),
                          verbatimTextOutput("Stat_Descrittive"),
                          h3("Grafico descrittivo"),
                          hr(),
                          br(),
                          plotOutput("Stat_UniPlot"),
                          hr(),
                          h3("Scarica le statistiche descrittive: ",
                          downloadButton("Report01", label="Genera report"))
                        ))),
             tabPanel("Statistiche Bivariate",
                       titlePanel("Bivariate!"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("variableBI01", "Selezione prima variabile:",
                                       choices = c(not_sel)),
                         selectInput("variableBI02", "Selezione seconda variabile:",
                                     choices = c(not_sel))),
                         mainPanel(
                           h3("Statistiche descrittive"),
                           hr(),
                           p("Le variabili categoriche vengono trattate come numeriche!."),
                           verbatimTextOutput("Stat_DescrittiveBi"),
                           hr(),
                           h3("Correlazione"),
                           verbatimTextOutput("CorrelazioneBi"),
                           h3("Grafico"),
                           hr(),
                           plotOutput("Stat_BiPlot", brush = brushOpts("plot_brush",resetOnNew=T))
                           #plotOutput("ggplot", brush = "plot_brush"),
                           #verbatimTextOutput("Selezione")
                         ))
             ),
             #            plotOutput("ggplot", brush = brushOpts("plot_brush",resetOnNew=T)),
             # Sezione Commenti ----
             tabPanel("Lo sapevate?",
                      titlePanel("Lo sapevate?"),
                      h2("Creato con R Shiny e RStudio"),
                      br(),
                      p("Questa e' ancora una brutta prova di quello che si puo' fare ed ha una reattivita' ed interattivita' molto limitata, ma resta un inizio!"),
                      br(),
                      p("Aggiungo del testo per mostrare un minimo di formattazione:"),
                      h3("Lucrezio", align = "center"),
                      p("Lucrezio, nel scrivere il", em("\"de rerum natura\" ", style = "font-family: 'times'; font-si16pt"), ", si era posto l\'obbiettivo di liberare gli uomini dagli affanni e dai problemi esistenziali attraverso la promozione della", strong("filosofia epicurea."))
             ) # Fine tabPanel
  ) # Fine navbarPage
) # Fine fluidPage

################################################################################
#
# SEZIONE SERVER 
#
################################################################################

server <- function(input, output,session) {
  reactive_data <- reactive({
    req(input$file1)
    print(input$file1$datapath)
    data <- fread(input$file1$datapath,
                  header = input$header,
                  sep = input$sep,
                  quote = input$quote,
                  stringsAsFactors = FALSE)
    data$GASDAY <- as.POSIXct(strptime(data$GASDAY, format="%Y-%m-%d", tz= "UTC"))
    data$YEAR <- as.factor(data$YEAR)
    data$MONTH <- as.factor(data$MONTH)
    data$DD <- as.factor(data$DD)
    return(data)
  })

  # preview
  output$contents <- renderTable({
    # Carico i dati
    data <- reactive_data()
    
    if(input$disp == "Inizio") {
      return(head(data))
    }
    else {
      return(data)
    }
  })

  # Statistiche riassuntive
  output$summary <- renderPrint({
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
    }else{
      summary(data)
    }
  })

  # Statistiche descrittive univariate
  output$Stat_Descrittive <- renderPrint({
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
    }else{
      updateSelectInput(session, "variableUNI",
                        choices = colnames(data),
                        selected = input$variableUNI)
      summary(data[[input$variableUNI]])
    }
  })

  # Statistiche descrittive bivariate
  output$Stat_DescrittiveBi <- renderPrint({
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
    }else{
      updateSelectInput(session, "variableBI01",
                        choices = colnames(data),
                        selected = input$variableBI01)
      updateSelectInput(session, "variableBI02",
                        choices = colnames(data),
                        selected = input$variableBI02)
      TMP <- cbind(data[[input$variableBI01]], data[[input$variableBI02]])
      colnames(TMP) <- c(colnames(data)[colnames(data) == input$variableBI01],
                         colnames(data)[colnames(data) == input$variableBI02])
      summary(TMP)
    }
  })

  
  # Correlazione fra due variabili numeriche
  output$CorrelazioneBi <- renderPrint({
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
      # paste("Nessuna variabile selezionata.")
    }else{
      updateSelectInput(session, "variableBI01",
                        choices = colnames(data),
                        selected = input$variableBI01)
      updateSelectInput(session, "variableBI02",
                        choices = colnames(data),
                        selected = input$variableBI02)
      if(!is.factor(data[[input$variableBI01]]) & !is.factor(data[[input$variableBI02]])) {
        Risultato <- data.frame(Correlazione = c(
          cor(x=data[[input$variableBI01]], y=data[[input$variableBI02]], use = "pairwise.complete.obs"),
          cor(x=data[[input$variableBI01]], y=data[[input$variableBI02]], use = "pairwise.complete.obs", method = "kendall"),
          cor(x=data[[input$variableBI01]], y=data[[input$variableBI02]], use = "pairwise.complete.obs", method = "spearman"))
          )
        rownames(Risultato) <- c("Pearson", "Kendall", "Spearman")
        print(Risultato)
      } else {
        return(NULL)
      }
    }
  })

  # Grafico Univariato: istograma o diagramma a barre
  output$Stat_UniPlot <- renderPlot({
    
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
      
    }else{
      # Carico dati e seleziono variabile
      updateSelectInput(session, "variableUNI",
                        choices = colnames(data),
                        selected = input$variableUNI)
      if(is.factor(data[[input$variableUNI]])) {
        data2 <- as.data.frame(table(data[[input$variableUNI]]))
        colnames(data2) <- c("Livelli", "Freq")
        ggplot(data2, aes(x=Livelli, y=Freq)) +  geom_bar(stat = "identity")
      }
      else ggplot(data, aes_string(x=colnames(data)[colnames(data) == input$variableUNI]))+
        geom_histogram(na.rm = TRUE, fill=I("blue"), col=I("red"))
    }})

  # Controllo sul tipo di variabile letta
  output$Fattore <- renderText({
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
      paste("Nessuna variabile selezionata.")
    }else{
      updateSelectInput(session, "variableUNI",
                        choices = colnames(data),
                        selected = input$variableUNI)
      if(is.factor(data[[input$variableUNI]])) 
        paste("La variabile e' di tipo categorico.") 
      else paste("La variabile e' di tipo numerico.")
    }
  })

  # Grafico Bivariato: diagramma di dispersione
  output$Stat_BiPlot <- renderPlot({
    
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
      
    }else{
      # Carico dati e seleziono variabile
      updateSelectInput(session, "variableBI01",
                        choices = colnames(data),
                        selected = input$variableBI01)
      updateSelectInput(session, "variableBI02",
                        choices = colnames(data),
                        selected = input$variableBI02)
    if(!is.factor(data[[input$variableBI01]]) & !is.factor(data[[input$variableBI02]])) {
      ggplot(data, aes_string(x=colnames(data)[colnames(data) == input$variableBI01],
                              y=colnames(data)[colnames(data) == input$variableBI02])) +
        geom_point() 
    } else {
      return(NULL)
    }
    }
  })

  # Controllo sul tipo di variabile letta
  output$Fattore <- renderText({
    # Carico i dati
    data <- reactive_data()
    if(is.null(data)){
      return(NULL)
      paste("Nessuna variabile selezionata.")
    }else{
      updateSelectInput(session, "variableUNI",
                        choices = colnames(data),
                        selected = input$variableUNI)
      if(is.factor(data[[input$variableUNI]])) 
        paste("La variabile e' di tipo categorico.") 
      else paste("La variabile e' di tipo numerico.")
    }
  })

  # Report01 dati Univariati
  output$Report01 <- downloadHandler(
    filename <- function(){paste0("Report.html")},
    content = function(file) {
      params <- list(DatiRMD = reactive_data(),
                     DatiReportRMD = input$variableUNI)
          id <- showNotification(
            "Rendering report...",
            duration = NULL,
            closeButton = FALSE
          )
      on.exit(removeNotification(id), add = TRUE)
      rmarkdown::render("report.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

} # end server

# Esecuzione dell'applicazione
shinyApp(ui = ui, server = server)
