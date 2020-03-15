library(shiny)

ui<-fluidPage(titlePanel("Association Rules Mining"),
              
              sidebarPanel(
                
                sliderInput('sup', "Support", min = 0.01, max = 1, value = 0.02, step = 0.005),
                sliderInput('conf', 'Confidence', min = 0.01, max =1, value = 0.7, step = 0.005),
                sliderInput('len', 'Minimum Rule Length', min = 1, max =15, value = 3, step = 1),
                sliderInput('mlen', 'Maximum Rule Length', min = 1, max =15, value = 7, step = 1),
                selectInput('attr','Attrition',choices=c('Attrition=Yes','Attrition=No')),
                selectInput('sort','Sort By',choices = c('lift','support','confidence'))
              ),
              
              mainPanel(
                tabsetPanel(
                            tabPanel('Rules for Attrition', verbatimTextOutput("rules")),
                            tabPanel('Parallel Co-Ordinate Plot',plotOutput('pplot')),
                            tabPanel('Scatter Plot',plotOutput('splot'))
                           )
              ))
    
server<-function(input,output){
  library(arules)
  library(arulesViz)
  ruleset <- reactive(
    {
      
      ruleset <- apriori(data=ar, 
                       parameter=list (supp= as.numeric(input$sup),conf = as.numeric(input$conf) , minlen= as.numeric(input$len)+1, maxlen = as.numeric(input$mlen)),appearance = list(default='lhs',rhs=input$attr),control=list(verbose = FALSE))
    }
  )
  
  output$pplot <- renderPlot(
                      {
                                ruleset <-ruleset()
                                validate(need(length(ruleset)!=0, "Please set low support values "))
                                p <- plot(head(ruleset,5),method = "paracoord")
                                print (p)
                      },
                      height = 500)
  
  output$splot <- renderPlot(
                      {
                                ruleset <-ruleset()
                                validate(need(length(ruleset)!=0, "Please set low support values "))
                                s <- plot(ruleset,jitter=0)
                                print (s)
                      },
                      height = 500)
  
  output$rules <- renderPrint( 
                      {
                       ruleset <- ruleset()
                       validate(need(length(ruleset)!=0, "Please set low support values "))
                       inspect(head(sort(ruleset, decreasing = TRUE, na.last = NA, by = input$sort),5))
                  
                      })
}
  
shinyApp(ui=ui,server=server)