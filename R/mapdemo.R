#' Display Census data on an interactive map.
#'
#' Simple shiny application to display Census data on a map.
#'
#' @author Chris Hansen
#'
#' @export
#'
#' @examples
#' \dontrun{
#' }
mapdemo <- function(){

   shinyApp(
      ui = fluidPage(
         titlePanel("Mapping Census..."),
         sidebarLayout(
            sidebarPanel(width=3,
               selectInput("geo", "select geography:",
                  choices=c("Area Unit"="AU", "Ward"="Ward",
                            "Territorial Authority"="TA",
                            "Regional Council"="RC"),
                  selected="RC"),
               selectInput("type", "select dataset:",
                  choices=c("dwelling"="dwelling", "household"="household", "family"="family", "individual"="individual")),
               uiOutput("subtypecontainer"),
               uiOutput("variablecontainer"),
               uiOutput("outcomecontainer"),
               selectInput("year", "Census year:", choices=c("2001"=2001, "2006"=2006, "2013"=2013), selected=2013)
            ),
            mainPanel(width=9,
               leafletOutput("map", height = "100%"),
               absolutePanel(actionButton("reset", "reset extent"), bottom="20px", left="30px"),
               tags$head(includeCSS(paste0(path.package("NZCensusDemos"), "/www/styles.css")))
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

         output$outcomecontainer <- renderUI({
            req(input$type, input$subtype, input$variable)

            try({
               x <- datadictionary[type==input$type&subtype==input$subtype&variable==input$variable]
               x <- x[!grepl("^(Total)|(Median)|(Mean).*$", outcome, perl=TRUE), unique(outcome)]
               choices <- as.list(x)
               names(choices) <- x
               selectInput("outcome", "choose an outcome:", choices=choices, selected=choices[[1]])
            }, silent=TRUE)
         })

         mapdata <- reactive({
            req(input$geo, input$type, input$subtype, input$variable, input$outcome, input$year)
            try({
               u <- datadictionary[type==input$type&subtype==input$subtype&variable==input$variable]
               u <- u[!grepl("^(Total)|(Median)|(Mean).*$", outcome, perl=TRUE)]
               v <- u[outcome==input$outcome,name]
               u <- u[,name]
               d1 <- get(input$type)[geography==input$geo&year==input$year, c("code", u), with=FALSE]
               d1 <- melt(d1, "code")
               d1 <- group_by(d1, code) %>% summarise(total=sum(value))
               d2 <- get(input$type)[geography==input$geo&year==input$year, .(code, get(v))]
               setnames(d2, colnames(d2)[2], "value")
               d  <- merge(d1, d2, by="code")[, p:=round(value/total * 100, 1)]
               d
            }, silent=TRUE)
         })

         output$map <- renderLeaflet({
            leaflet() %>%
               fitBounds(lng1=166.42615, lat1=-47.28999, lng2=178.57724, lat2=-34.39263) %>%
               addTiles()
         })

         observe({
            input$reset
            leafletProxy("map") %>%
               fitBounds(lng1=166.42615, lat1=-47.28999, lng2=178.57724, lat2=-34.39263)
         })

         observe({
            req(input$geo, input$type, input$subtype, input$variable, input$outcome, mapdata())

            leafletProxy("map") %>% removeControl("legend") %>% clearGroup("choropleth")

            try({
               if (input$geo=="RC") ds <- "REGC2013"
               else if (input$geo=="TA") ds <- "TA2013"
               else if (input$geo=="Ward") ds <- "WARD2013"
               else if (input$geo=="AU") ds <- "AU2013"
               d <- copy(mapdata())
               setnames(d, "code", ds)
               d <- sp::merge(get(ds), d, by=ds, all=FALSE)
               p <- colorNumeric(palette="Blues", domain=d@data$p)
               leafletProxy("map") %>%
                  addPolygons(data=d,
                     color="#222222", fillColor=~p(p),
                     opacity=1,  fillOpacity=0.9, weight=2,
                     popup=~paste0("<b>", d@data[,paste0(ds,"_name")],
                                   "</b><br>", as.character(p), "%"),
                     group="choropleth") %>%
                  addLegend("bottomright", pal=p, values=d@data$p, title = "%", layerId="legend")
            }, silent=FALSE)
         })
      }
   )
}
