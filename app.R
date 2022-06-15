
# deployed on shiny.io as sensingsystems.shiny.io
# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='sensingsystems',
# 						  token='12CFA953868D48CBDE2482D6323862B2',
# 						  secret='x2PpXiMWAZpY/xRqg86pYSsvRVdlD79cNsdPoQSh')
# rsconnect::deployApp('.')

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
				# gsub("fit", "csv", input$infile$datapath)
				'download.csv'
			},
			content = function(file) {
				write.csv(fitData, file)
			}
		)
	})
}

shinyApp(ui, server)

