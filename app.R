#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # tags$div(HTML("
    #             ")),
    # Application title
    sidebarLayout(
        sidebarPanel(
            h2("Inputs"),
            
            numericInput("RI",
                         h4("Flood recurrence interval [yrs]:"),
                         min = 1,
                         max = 1000,
                         value = 100),
            
            numericInput("n_yr",
                         h4("Number of years to simulate:"),
                         min = 1,
                         max = 1000,
                         value = 100),
            
            actionButton("sim_flood", strong("Simulate Floods"), style = "background-color: #a6bddb"),
            
            width = 3
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h1("Flood Frequency Simulation"),
            
            h4("This app helps visualize the probabilities associated with flood events of different recurrence intervals.
               Users specify a recurrence interval and the number of years over which to examine flooding. The tool creates
               a random simulation showing the number of floods of that recurrence interval over the number of years specfied by the user.
               Addtionally, 100 such simulations are run to show the range in the possible number of observed floods over this time
               period. These results are shown in the histogram below, along with a comparison of the theoretical 'risk' and
               estimate of 'risk' based on these simulations."),
            
            br(),
            br(),
            #plotOutput("watershed", width = "600px"),
            
            #plotOutput("UH", width = "600px")
            plotOutput("flood_sim", height = "200px"),
            
            br(),
            br(),
            
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("flood_hist"), h3(htmlOutput("flood_calc"))),
            
            br(),
            br(),
            
            h4(a(href = "https://github.com/rodlammers/Flood_Frequency", target="_blank", "Code"), "created by Rod Lammers (rodlammers@gmail.com)."),
            
            width = 9
        ),
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$sim_flood, {
        
        floods <- sample(x = c(0, 1), size = input$n_yr, replace = TRUE, prob = c(1 - 1 / input$RI, 1 / input$RI))
        
        palette(c("white", "red"))
        
        floods_list <- list()
        
        for (i in 1:100){
            floods_list[[i]] <- sample(x = c(0, 1), size = input$n_yr, replace = TRUE, prob = c(1 - 1 / input$RI, 1 / input$RI))
        }
        
        n_floods <- sapply(floods_list, sum)
        
        output$flood_sim <- renderPlot({
            ann_chance <- round(1 / input$RI * 100, 2)
            par(mar = c(5, 0, 2, 0))
            barplot(rep(1, input$n_yr), col = floods + 1, yaxt = "n", main = paste0("Years with floods (", ann_chance, "% annual chance)"), cex.main = 1.5)
            legend("bottom", legend = c("No Flood", "Flood"), fill = 1:2, horiz = TRUE, bty = "n", cex = 1.5, inset = c(0, -0.4), xpd = NA)

        })
        
        
        output$flood_hist <- renderPlot({
            
            hist(n_floods, breaks = -1:max(n_floods), xaxt = "n", las = 1, xlab = paste("Number of floods over", input$n_yr, "years"),
                 ylab = "Frequency", main = "Histogram of 100 flood simulations", cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5,
                 col = "gray80")
            axis(side = 1, at = 0:max(n_floods) - 0.5, labels = 0:max(n_floods), cex.axis = 1.5)
            box()
           
        })
        
        output$flood_calc <- renderUI({
            # HTML(paste(paste0("Theoretical prob: ", round(100 * (1 - (1 - 1 / input$RI) ^ input$n_yr), 1), "%"),
            # paste0("Simulated prob: ", round(sum(n_floods > 0) / length(n_floods) * 100, 1), "%"), sep = "<br/>"))
                withMathJax(helpText(""),
                            helpText("Flood Risk Calculation (probability of at least one flood): $$Risk = 1 - (1 - 1 / RI) ^ {n_{years}}$$"),
                            helpText(paste0("Theoretical risk: ", round(100 * (1 - (1 - 1 / input$RI) ^ input$n_yr), 1), "%")),
                            helpText(paste0("Simulated risk: ", round(sum(n_floods > 0) / length(n_floods) * 100, 1), "%")))

            
            
        })

    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
