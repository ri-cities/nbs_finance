library(shiny)
library(ggplot2)  # used by plotly
library(stats)    # used by plotly
library(graphics) # used by plotly
library(readxl)
library(plotly)
library(leaflet)

# navbarPage("Nature-positive climate risk transfer & financing instruments", id="main",
#            tabPanel("Map", leafletOutput("inventorymap", height=1000)),
#            tabPanel("Data", DT::dataTableOutput("data")),
#            #tabPanel("Instruments", DT::dataTableOutput("instruments_sunburst")), # update output
#            #tabPanel("Ecosystems", DT::dataTableOutput("ecosystems_sunburst")), # update output
#            tabPanel("Information",includeMarkdown("readme.md"))
#            )

navbarPage("Nature-positive climate risk transfer & financing instruments", id = "main",
           
           tabPanel("Map", 
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("geo_scale", "Select geographic scale:",
                                    choices = c("local (city, neighborhood)",
                                                "subnational (province, district)",
                                                "landscape (e.g. river basin, coastline)",
                                                "national", 
                                                "supranational"),
                                    selected = "local (city, neighborhood)"),
                        width = 3
                      ),
                      mainPanel(
                        # Text above map
                        h4("Inventory of nature-positive climate risk transfer and financing instruments"), 
                        p("This interactive map displays pojects across different geographic scales. 
                          Select a geographic scale on the left to explore the projects."),
                        leafletOutput("inventorymap", height = 1000),
                        width = 9
                      )
                    )
           ),
           
           # tabPanel("Data", DT::dataTableOutput("data")),
           # include filter option
           tabPanel("Financial instruments", 
                    h4("Nature-positive climate risk transfer and financing instruments (CRTFIs) by instrument category"), 
                    p("The panel presents the distribution of nature-positive CRTFI (outer circle), categorized by instrument type (inner circle). The share of each instrument in the outer circle reflects its prevalence in the instrument category. One project can involve multiple CRTFIs. The same project may be counted multiple times if it appears in multiple publications. Sample size: projects in academic publications (n = 104) and non-academic publications (n = 209)."), 
                    plotlyOutput("instruments_sunburst", height = "800px")),
           tabPanel("Ecosystems", 
                    h4("Ecosystem types by ecosystem category"), 
                    p("The panel illustrates the distribution of ecosystem types supported by nature-positive climate risk transfer and financing instruments (outer circle), categorized by broader ecosystem categories (inner circle), that benefit from a nature-positive climate risk transfer and financing instrument identified in the review. One project can support multiple ecosystems. The same project may be counted multiple times if it appears in multiple publications. Sample size: projects in academic publications (n = 104) and non-academic publications (n = 209)."), 
                    plotlyOutput("ecosystems_sunburst", height = "800px")),
           # insert Figures Panel
           # include filter option
           tabPanel("Information", includeMarkdown("readme.md")), 
           # tabPanel("Status", textOutput("status"))
  )
