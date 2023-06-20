options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output, session) {
  useShinyjs()

  sel <- reactiveValues(
    OM=NULL,          # input file
    Ind=NULL,        # Sheet names
    tail=NULL,       # FPI summary tab
    alp=NULL,        # FPI output-table
    yind=NULL,   # FPI input-table
  )

  # dynamic plot dimensions
  #window_dims <- reactive(input$dimension)

  # Log stuff
  #USERID <-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  #SessionID <- paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  #output$SessionID<-renderText(SessionID)

}

