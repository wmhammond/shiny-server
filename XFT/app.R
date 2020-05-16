library(shiny)
library(DT)
library(ggplot2)
#library(data.table)
# which fields get saved 
fieldsAll <- c("group", "family", "genus", "species", "subspecies","P50", "P12", "P88", "porosity", "conduit.density")
# which fields are mandatory
fieldsMandatory <- c("group", "family", "species", "P50")
filelength<-length(list.files("./responses"))
# add an asterisk to an input label
labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}
# get current Epoch time
epochTime <- function() {
    return(as.integer(Sys.time()))
}
# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
    format(Sys.time(), "%Y%m%d-%H%M%OS")
}
# save the results to a file
saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
}
# load all responses into a data.frame
loadData <- function() {
    files <- list.files(file.path(responsesDir), full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    #data <- dplyr::rbind_all(data)
    data <- do.call(rbind, data)
    data
}
# directory where responses get stored
responsesDir <- file.path("responses")
databaseDir <- file.path("database")
#databaseFile <-"database.csv"
#database_df <- read.csv(file=file.path(databaseDir, databaseFile))
database_df <-readRDS("database.RDS")
databaseFilename <- "XFT_cleaned_databse.csv"
# CSS to use in the app
appCSS <-
    ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
# adminUsers <- c("admin", "prof")

# info for sharing this app on facebook/twitter
share <- list(
    title = "Xylem Functional Traits Database",
    url = "http://wmhammond.shinyapps.io/XFT",
    image = "",
    description = "Xylem Functional Traits database web portal",
    twitter_user = "wmhammond"
)

shinyApp(
    ui = shinyUI(fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        title = "Xylem Functional Traits Database",
        titlePanel(h1("Xylem Functional Traits Database"),
                   h4("Data submission and access portal")),
        tabsetPanel(
          tabPanel("Home Page",
                   mainPanel(tags$h4("Welcome to the Xylem Functional Traits Database portal")),
                     img(src='Picture1.png',height=728,width=829),
                             includeHTML("welcome.html")),
          tabPanel("Trait Definitions",
                   mainPanel(tags$h4("Trait definitions"),
                             br(),
                             tags$h6("...coming soon..."))),
            tabPanel("Submit Data",
            sidebarLayout(
                sidebarPanel(
                    id = "form",
                    selectInput("group", labelMandatory("Group"), c("Angiosperm", "Gymnosperm")),
                    textInput("family", labelMandatory("Family")),
                    textInput("genus", labelMandatory("Genus")),
                    textInput("species", labelMandatory("Species")),
                    textInput("P50", labelMandatory("P50")),
                    textInput("subspecies", "Subspecies"),
                    checkboxInput("hydraulictraits", "Hydraulic Traits",FALSE),
                    uiOutput("conditionalInput"),
                    uiOutput("conditionalInput2"),
                    checkboxInput("anatomicaltraits", "Anatomical Traits", FALSE),
                    uiOutput("conditionalAnatomical1"),
                    uiOutput("conditionalAnatomical2"),
                    actionButton("submit", "Submit", class = "btn-primary"),
                    shinyjs::hidden(
                        span(id = "submit_msg", "Submitting..."),
                        div(id = "error",
                            div(br(), tags$b("Error: "), span(id = "error_msg"))
                        )
                    ),

                    # column(6,
                    #        uiOutput("dataTableContainer")
                    # )
                ),
                mainPanel(
                    tableOutput('dataTableContainer'),
                    shinyjs::hidden(
                      div(
                        id = "thankyou_msg",
                        h3("Thanks, your response was submitted successfully!"),
                        actionLink("submit_another", "Submit another response"),
                      )
                    )
                )
            )
            # tabPanel("Entered data"),
            # pageWithSidebar(
            #     headerPanel("Data pending cleaning"),
            #     sidebarPanel(
            #         
            #     ),
            #     mainPanel = ("dataTableContainer")
            # )
            # 
        # tabPanel("Submitted"),
        #   sidebarLayout(
        #     sidebarPanel(
        #       id="thanks",
        #       shinyjs::hidden(
        #         div(
        #           id = "thankyou_msg",
        #           h3("Thanks, your response was submitted successfully!"),
        #           actionLink("submit_another", "Submit another response"),
        #         )
        #       )
        #     ),
        #     mainPanel(
        #       shinyjs::hidden(
        #         div(
        #           id = "thankyou_msg",
        #           h3("Thanks, your response was submitted successfully!"),
        #           actionLink("submit_another", "Submit another response"),
        #         )
        #       )
        #       
        #     )
        #   )
            ),
        tabPanel("Database",
                 # sidebarLayout(
                 #   sidebarPanel(
                 #     id = "database",
                 #     textInput("testinput", "Test Input")),
                   mainPanel(width="auto",
                     tableOutput("databasePanel"),
                     tableOutput("database")
                   )),
        tabPanel("Explore",
                 sidebarLayout(
                   sidebarPanel(
                     id = "explore",
                     selectInput("x", "X Variable", names(database_df)),
                     selectInput("y", "Y Variable", names(database_df))
                     ),
                   mainPanel(
                     tags$h4("Interactive Plots [!under construction!]"),
                     plotOutput("plot1")
                   ))),
        tabPanel("Citation",
                 mainPanel(tags$h4("Citation information for the database:"),
                           br(),
                           tags$h6("Choat B., Jansen S., Brodribb T.J., Cochard H., Delzon S., Bhaskar R., Bucci S., Feild T.S., Gleason S.M., Hacke U.G., Jacobsen A.L., Lens F., Maherali H., Martinez-Vilalta J., Mayr S., Mencuccini M., Mitchell P.J., Nardini A., Pittermann J., Pratt R.B., Sperry J.S., Westoby M., Wright I.J., Zanne A. (2012) Global convergence in the vulnerability of forests to drought. Nature 491: 752-755. ")))
        
        ))),
    
    
    server = function(input, output, session) {
        filtered_df<-reactive(database_df)
        # Enable the Submit button when all mandatory fields are filled out
        observe({
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        # Gather all the form inputs (and add timestamp)
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
        })    
        
        # When the Submit button is clicked, submit the response
        observeEvent(input$submit, {
            
            # User-experience stuff
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            # Save the data (show an error message in case of error)
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
               #shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
                shinyjs::hide("form")
                shinyjs::hide("dataTableContainer")
                shinyjs::show("thanks")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
            output$responsesTable <- DT::renderDataTable({
              data <- loadData()
              data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
              DT::datatable(
                data,
                rownames = FALSE,
                options = list(searching = FALSE, lengthChange = FALSE)
              )
            })
            
            
        })
        
        # submit another response
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
            shinyjs::show("dataTableContainer")
            #shinyjs::js$refresh() ##will refresh entire page, disconnects server.
        })
        
          # render the admin panel
        output$dataTableContainer <- renderUI({
            # if (!isAdmin()) return()
            # 
            div(
                id = "dataTable",
                h2("Previous submissions"),
                downloadButton("downloadBtn", "Download responses"), br(), br(),
                DT::dataTableOutput("responsesTable"), br(),
            )
        })
        output$databasePanel <- renderUI({
          div(id = "databaseTable",
              h2("XFT Cleaned Database"),
              downloadButton("DownloadBtn2", "Download Entire Database"),
              # downloadButton("DownloadBtn3", "Download Filtered Database"), br(),
              DT::renderDataTable(database_df,
                                  extensions = c("Buttons","Scroller","FixedColumns"),
                                  options=list(deferRender = TRUE,
                                               scrollY = 600,
                                               scrollX= TRUE,
                                               scroller = TRUE,
                                               # columnDefs = list(
                                               #   list(targets = c(0,1,6:19,21:200), visible = FALSE)),
                                              lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                              pageLength = 15,
                                              dom ="Bfrtip",
                                              fixedColumns=TRUE,
                                               buttons=list("colvis",list(extend="csv", 
                                                                 text="Download Filtered Database",
                                                                 filename=paste('XFT_Filtered_', Sys.Date(), sep = '')),
                                                            list(
                                                              extend = "collection",
                                                              text = 'Show All',
                                                              action = DT::JS("function ( e, dt, node, config ) {
                                    dt.page.len(-1);
                                    dt.ajax.reload();
                                }")))),
                                  server=TRUE, 
                                  class = 'white-space: nowrap',
                                  filter = "top",
                                  rownames=FALSE), br(),
          )
        })
        #hydraulic traits
        output$conditionalInput <- renderUI({
          if(input$hydraulictraits){
            textInput("P12", "P12 value:")
          }
        })
        output$conditionalInput2 <- renderUI({
          if(input$hydraulictraits){
            textInput("P88", "P88 value:")
          }
        })
        #anatomical traits
        output$conditionalAnatomical1 <- renderUI({
          if(input$anatomicaltraits){
            selectInput("porosity", "Select Porosity:",
                        c("","diffuse-porosity", "ring-porosity", "semi-ring-porosity"))
          }
        })
        output$conditionalAnatomical2 <- renderUI({
          if(input$anatomicaltraits){
            textInput("conduit.density", "Conduit density per mm^2:")
          }
        })
        
        # # determine if current user is admin
        # isAdmin <- reactive({
        #     is.null(session$user) || session$user %in% adminUsers
        # })
        

        # Show the responses in the admin table
        output$responsesTable <- DT::renderDataTable({
            data <- loadData()
            data$timestamp <- as.POSIXct(data$timestamp, origin="1970-01-01")
            DT::datatable(
                data,
                rownames = FALSE,
                options = list(searching = FALSE, lengthChange = FALSE)
            )
        })
        
        
        # Allow user to download responses
        output$downloadBtn <- downloadHandler(
            filename = function() { 
                sprintf("XFT_download_%s.csv", humanTime())
            },
            content = function(file) {
                write.csv(loadData(), file, row.names = FALSE)
            }
        )
        ### download button for complete database
        output$DownloadBtn2 <- downloadHandler(
          filename = function() { 
            sprintf("XFT_full_database_download_%s.csv", humanTime())
          },
          content = function(databaseFilename) {
            write.csv(database_df, databaseFilename , row.names = FALSE)
          }
        )
        # output$DownloadBtn3 <- downloadHandler(
        #   filename = function() { 
        #     sprintf("XFT_filtered_database_download_%s.csv", humanTime())
        #   },
        #   content = function(databaseFilename) {
        #     write.csv(database_df[input[["database_rows_all"]], ], databaseFilename , row.names = FALSE)
        #   }
        # )

        output$plot1 <- renderPlot({
          ggplot(database_df, aes(x=input$x, group = input$x)) + geom_histogram(stat="count", position = "dodge")
        })
        
    }
)
