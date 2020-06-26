# Libs
library("shiny")
library("tidyverse")

# Data
dta_mstr <- rownames_to_column(.data = mtcars)

# Drop down selection choices
# Add regex to include 'all'
selection_choices <- as.list(append(".*", dta_mstr$rowname))
names(selection_choices) <- c("All", dta_mstr$rowname)

does_not_contain <- negate(contains)

ui <- fluidPage(titlePanel("Cars"),

                sidebarLayout(sidebarPanel(
                    selectInput(
                        inputId = "chosenRow",
                        label = "Model",
                        choices = selection_choices
                    )
                ),

                mainPanel(
                    h3(textOutput("selectedVar")),
                    dataTableOutput("filteredTable")
                )))

server <- function(input, output) {
    output$filteredTable <- renderDataTable({
        filter(dta_mstr, grepl(input$chosenRow, rowname)) %>%
            pivot_longer(cols = c(everything(), -rowname)) %>%
            {if (input$chosenRow != ".*") select(., -rowname) else .}

    })


    output$selectedVar <- renderText({
        paste("Selection:", input$chosenRow)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
