library(bubbles)
library(dplyr)
library(shinySignals)

function(input, output, session) {
  
  # Connect to data source
  sock <- socketConnection("rstudio.com", 6789, blocking = FALSE, open = "r")
  # Clean up when session is over
  session$onSessionEnded(function() {
    close(sock)
  })
  
  # Returns new lines
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(sock)
  })
  
  # Parses newLines() into data frame
  newData <- reactive({
    if (length(newLines()) == 0)
      return()
    read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
      col.names = c("date", "time", "size", "r_version", "r_arch",
        "r_os", "package", "version", "country", "ip_id"
      )
    )
  })
  
  # Accumulates newData results over time
  pkgData <- shinySignals::reducePast(newData, rbind, NULL)

  output$bubbles <- renderBubbles({
    if (is.null(pkgData()))
      return(NULL)
    
    df <- pkgData() %>%
      select_(label = input$by) %>%
      group_by(label) %>%
      tally() %>%
      arrange(match(label, pkgData()[[input$by]]))
    
    value <- switch(input$scale,
      area = sqrt(df$n),
      radius = df$n
    )
    
    bubbles(value = value, label = df$label,
      tooltip = sprintf("%s: %d", df$label, df$n)
    )
  })
}
