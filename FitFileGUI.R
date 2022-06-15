

library(shiny)
library(leaflet)
library(shinyFiles)

source("FitFileFunctions.R")

ui <- fluidPage(

	fileInput('infile', 'Select input FIT file', multiple=F, accept='fit',
			  buttonLabel = "Open", placeholder = "No file selected"),
	plotOutput("fitPlot"),
	leafletOutput("fitMap"),
	downloadButton("downloadData", "Download")
)

server <- function(input, output, session){

	observeEvent(input$infile, {

		fitData = FitToDataframe(input$infile$datapath)
		output$fitPlot = renderPlot({
			FitFilePlots(fitData)
		})
		output$fitMap = renderLeaflet({
			FitFileMap(fitData)
		})
		output$downloadData = downloadHandler(
			filename = function() {
				paste0(gsub(pattern = "\\.fit$", "", input$datapath), '.csv')
			},
			content = function(file) {
				write.csv(fitData, file)
			}
		)
	})
}

shinyApp(ui, server)

