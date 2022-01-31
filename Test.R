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
                          plotOutput("Stat_UniPlot")
                        ))),
             # tabPanel("Statistiche Bivariate",
             #          titlePanel("Bivariate!"),
             #          sidebarLayout(
             #            sidebarPanel(
             #              selectInput("variableBI01", "Selezione prima variabile:",
             #                          choices = c(not_sel)),
             #            selectInput("variableBI02", "Selezione seconda variabile:",
             #                        choices = c(not_sel))),
             #            mainPanel(
             #              h3(textOutput("Statistiche descrittive")),
             #              h3(textOutput("Grafico")),
             #              br()
             #              #plotOutput("ggplot", brush = brushOpts("plot_brush",resetOnNew=T)),
             #              #plotOutput("ggplot", brush = "plot_brush"),
             #              #verbatimTextOutput("Selezione")
             #            ))),
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

  # Statistiche descrittive
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
} # end server

# Esecuzione dell'applicazione
shinyApp(ui = ui, server = server)
