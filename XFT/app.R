library(shiny)

# which fields get saved 
fieldsAll <- c("group", "family", "genus", "species", "subspecies","P50", "P12", "P88")

# which fields are mandatory
fieldsMandatory <- c("group", "family", "species", "P50")

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
adminUsers <- c("admin", "prof")

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
            tabPanel("Submit Data"),
            sidebarLayout(
                sidebarPanel(
                    id = "form",
                    selectInput("group", labelMandatory("Group"), c("Angiosperm", "Gymnosperm")),
                    textInput("family", labelMandatory("Family")),
                    textInput("genus", labelMandatory("Genus")),
                    textInput("species", labelMandatory("Species")),
                    textInput("P50", labelMandatory("P50")),
                    textInput("subspecies", "Subspecies"),
                    actionButton("submit", "Submit", class = "btn-primary"),
                    shinyjs::hidden(
                        span(id = "submit_msg", "Submitting..."),
                        div(id = "error",
                            div(br(), tags$b("Error: "), span(id = "error_msg"))
                        )
                    )
                ),
                mainPanel(
                    tableOutput('dataTableContainer')
                ),
                #        shinyjs::hidden(
                #            div(
                #                id = "thankyou_msg",
                #                h3("Thanks, your response was submitted successfully!"),
                #                actionLink("submit_another", "Submit another response")
                #            )
                #        ),
                # column(6,
                #        uiOutput("dataTableContainer")
                # )
            ),
            tabPanel("Entered data"),
            pageWithSidebar(
                headerPanel("Data pending cleaning"),
                sidebarPanel(
                    actionButton("submit_another", "Submit another response")
                ),
                mainPanel = ("dataTableContainer")
            )
            
            
        ))),
    server = function(input, output, session) {
        
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
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })
        
        # submit another response
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
        
        # render the admin panel
        output$dataTableContainer <- renderUI({
            if (!isAdmin()) return()
            
            div(
                id = "dataTable",
                h2("Previous submissions"),
                downloadButton("downloadBtn", "Download responses"), br(), br(),
                DT::dataTableOutput("responsesTable"), br(),
            )
        })
        
        # determine if current user is admin
        isAdmin <- reactive({
            is.null(session$user) || session$user %in% adminUsers
        })    
        
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
                sprintf("mimic-google-form_%s.csv", humanTime())
            },
            content = function(file) {
                write.csv(loadData(), file, row.names = FALSE)
            }
        )    
    }
)
