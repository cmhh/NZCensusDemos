#' Display Census data as a bar chart and table.
#'
#' Simple shiny application to display distribution of count data.
#'
#' @author Chris Hansen
#'
#' @export
#'
#' @examples
#' \dontrun{
#' }
distributiondemo <- function(){

   shinyApp(

      ui = fluidPage(
         titlePanel("Distributions..."),
         sidebarLayout(
            sidebarPanel(width=3,
               selectInput("type", "select dataset:",
                  choices=c("dwelling"="dwelling", "household"="household", "family"="family", "individual"="individual")),
               uiOutput("subtypecontainer"),
               uiOutput("variablecontainer"),
               selectInput("esttype", "estimate type:",
                  choices=c("count"="count", "percentage (%)"="percentage"),
                  selected="percentage")
            ),
            mainPanel(width=9,
               tabsetPanel(
                  tabPanel("Plot", plotOutput("plot", height="100%")),
                  tabPanel("Data", div(DT::dataTableOutput("tab")), style="padding: 10px;"),
                  tags$head(includeCSS(paste0(path.package("NZCensusDemos"), "/www/styles.css")))
               )
            )
         )
      ),
      server = function(input, output){

         output$subtypecontainer <- renderUI({
            req(input$type)
            try({
               x <- datadictionary[type==input$type, unique(subtype)]
               choices <- as.list(x)
               names(choices) <- x
               selectInput("subtype", "choose a subtype:", choices=choices, selected=choices[[1]])
            }, silent=TRUE)
         })

         output$variablecontainer <- renderUI({
            req(input$type, input$subtype)
            try({
               x <- datadictionary[type==input$type&subtype==input$subtype, unique(variable)]
               choices <- as.list(x)
               names(choices) <- x
               selectInput("variable", "choose a variable:", choices=choices, selected=choices[[1]])
            }, silent=TRUE)
         })

         data <- reactive({
            req(input$type, input$subtype, input$variable)
            vars <- datadictionary[type==input$type&subtype==input$subtype&variable==input$variable, .(name, outcome)]
            if (nrow(vars)>1) vars <- vars[!grepl("^(Total)|(Median)|(Mean).*$", outcome, perl=TRUE)]
            if (input$type=="dwelling")
               res <- dwelling[geography=="RC"&code!='99', c("year", vars$name), with=FALSE]
            else if (input$type=="family")
               res <- family[geography=="RC"&code!='99', c("year", vars$name), with=FALSE]
            else if (input$type=="household")
               res <- household[geography=="RC"&code!='99', c("year", vars$name), with=FALSE]
            else if (input$type=="individual")
               res <- individual[geography=="RC"&code!='99', c("year", vars$name), with=FALSE]
            e <- paste0("sum(", vars$name, ")")
            names(e) <- vars$name
            res <- group_by(res, year) %>% summarise_(.dots=e)
            res <- melt(res, id.vars="year", value.name="count")
            res$variable <- factor(res$variable, vars$name, vars$outcome, ordered=TRUE)
            res <- res %>% merge(group_by(res, year) %>% summarise(total=sum(count, na.rm=TRUE)), by="year")
            res[,percentage:=round(count/total*100, 1)]
            res[, .(year, variable, count, percentage)]
            setorder(res, year, variable)
            res
         })

         output$plot <- renderPlot({
            req(input$type, input$subtype, input$variable, input$esttype)
            if (!is.null(data())){
               suppressWarnings({
                  if (input$esttype=="count") p <- ggplot(data=data(), aes(variable, count))
                  else if (input$esttype=="percentage") p <- ggplot(data=data(), aes(variable, percentage))
                  p + geom_bar(stat="identity", fill=rgb(0.2,0.4,0.4)) +
                     facet_grid(year~.) + coord_flip() +
                     theme(axis.title.y=element_blank())
               })
            }
         })

         output$tab <- DT::renderDataTable({
            data.table(data())
         }, escape=FALSE, rownames=FALSE, options=list(scrollY=FALSE, scrollX=FALSE, paging=FALSE))

      }, onStart=NULL, options=list(), uiPattern="/")
}
