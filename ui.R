library(shiny)

shinyUI(
    navbarPage(
        "Query Tool 2",
        tabPanel(
            "Databases",
            fluidPage(
                # ew.
                ## tags$head(
                ##     tags$style(HTML("body { background-image: url('great_dane_exchange_background.jpg') }"))),
            titlePanel(h1(strong("ProjPet Databases"))),
            sidebarLayout(
                sidebarPanel(
                    h4(strong("Data sources:")),
                    uiOutput("databases"),
                    uiOutput("tables"),
                    br(),
                    h4(strong("Filters:")),
                    uiOutput("filters"),
                    downloadButton('downloadData', 'Download CSV')
                ),
                mainPanel(
                    h4(strong(textOutput("text1"))),
                    br(),
                    tableOutput("mainTable")
                )
            )
        )),
        tabPanel(
            "Signature Time Series",
            fluidPage(
                titlePanel(h1(strong("Petition Data"))),
                sidebarLayout(
                    sidebarPanel(
                        textInput("petID", "Petition ID:"),
                        radioButtons("dateType", label = "File dates:",
                                     choices = c("First 30 days", "custom")),
                        dateRangeInput("dateRange", label = "Dates:"),
                        actionButton("tsbutton", "Get time series"),
                        br(), br(),
                        downloadButton('downloadData2', 'Download CSV')
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Plot", plotOutput("plot")),
                            tabPanel(
                                "Table",
                                h4(strong(textOutput("text2"))),
                                br(),
                                tableOutput("petitionTable")
                            ) # end of tab panel
                        ) # end of tabset panel
                    ) # end of mainPanel
                ) # end of sidebarLayout
            ) # end of fluidPage
        ), # end of tabpanel
        tabPanel(
            "Signature Geography",
            fluidPage(
                titlePanel(h1(strong("Petition Data"))),
                sidebarLayout(
                    sidebarPanel(
                        textInput("geopetID", "Petition ID:"),
                        radioButtons("geoType", label = "Level:",
                                     choices = c("State",
                                         "ZIP Code")),
                        actionButton("geobutton", "Get Data"),
                        br(), br(),
                        actionButton("mapbutton",
                                     "Get Map (can take a few minutes)"),
                        br(), br(),
                        downloadButton('geoDownload', 'Download CSV')
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Plot", plotOutput("map")),
                            tabPanel(
                                "Table",
                                h4(strong(textOutput("geoText"))),
                                br(),
                                tableOutput("geoTable")
                            ) # end of tab panel
                        ) # end of tabset panel
                    ) # end of mainPanel
                ) # end of sidebarLayout
            ) # end of fluidPage
        ) # end of tabpanel
    )
)
