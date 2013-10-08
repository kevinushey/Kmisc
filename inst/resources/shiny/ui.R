library(shiny)

shinyUI(bootstrapPage(
  includeCSS("www/css/jquery-ui.css"),  
  
  includeScript("www/js/jquery.js"),
  includeScript("www/js/jquery-ui.js"),
  includeScript("www/js/jquery.dialogextend.min.js")
  
))
