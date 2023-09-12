options(shiny.maxRequestSize=1000*1024^2)


server <- function(input, output, session) {
  useShinyjs()

  sel <- reactiveValues(obj=NULL,
  #setwd("C:/Github/ECP/inst/shiny_apps/ECP"); obj  = readRDS("./data/ECP_obj_SOO.rda"); sel <- list(obj=obj,
                        OMind = 1,
                        Iind=1,
                        yind=1:8,
                        powind=1,
                        alp=0.05,
                        nFac=2,
                        Factor_Labels=c("Factor 1","Factor 2"),
                        Labels=list(c("F1 Lev 1","F1 Lev 2"),c("F2 Lev 1","F2 Lev 2")),
                        tail=FALSE)


  obj  = readRDS("./data/ECP_obj_final.rda")
  # sel = list(obj=obj)

  FiltersServer('filters', sel)
  VizServer('Viz',sel)
  CorServer('Cor',sel)
  PowServer('Pow',sel)

  sel$obj = obj
  sel$nFac = ncol(obj$OM_Design)
  sel$Factor_Labels = colnames(obj$OM_Design)
  sel$Labels = as.list(apply(obj$OM_Design,2,unique))

   #Refresh_UI<-function(){
    #lapply(1:4, function(i) { # should be nFac
     # updateCheckboxGroupInput(paste0("Fil_SN_1_",i),inline=T,obj$OM$Factor_Labels[i],choiceNames=obj$OM$Labels[[i]],selected=1:length(obj$OM$Labels[[i]]),choiceValues=1:length(obj$OM$Labels[[i]]))
    #})
    #updateCheckboxGroupInput("Fil_MP_T",label=NULL,inline=T,choices=obj$MP$Labels,selected=obj$MP$Labels)
    #updateCheckboxGroupInput("Fil_PM_Det_T",label="Deterministic",inline=T,choices=obj$Perf$Det$Codes,selected=obj$Perf$Det$Codes)
    #updateCheckboxGroupInput("Fil_PM_Stoch_T",label="Stochastic",inline=T,choices=obj$Perf$Stoch$Codes,selected=obj$Perf$Stoch$Codes)
    #updateCheckboxGroupInput("Fil_PM_Proj_T",label="Projected",inline=T,choices=obj$Perf$Proj$Codes,selected=obj$Perf$Proj$Codes)
  #}


  # dynamic plot dimensions
  #window_dims <- reactive(input$dimension)

  # Log stuff
  #USERID <-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  #SessionID <- paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  #output$SessionID<-renderText(SessionID)

}

