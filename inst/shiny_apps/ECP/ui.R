HelpUI <- function(id, label="settings") {

  ns <- NS(id)
  tagList(
    fluidRow(
      p('help')

    )
  )
}

SettingsUI <- function(id, label="settings") {

  ns <- NS(id)
  tagList(
    fluidRow(
      p('Settings')

    )
  )
}



# -- header ----
header <- shinydashboardPlus::dashboardHeader(
  #leftUi = tagList(
  #  dropdownButton(
   #   label = "Help",
    #  icon = icon("info"),
     # status = "primary",
    #  circle = FALSE,
    #  uiOutput("language")
    #)),
  controlbarIcon=shiny::icon('gears')
)


# -- rhs controlbar ----
controlbar <- dashboardControlbar(overlay = FALSE, width=450,skin='light', collapsed = TRUE,
                                  FiltersUI('filters')

)

# -- lhs sidebar ----
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(id='sidebar',
    menuItem("Data Projection", tabName = "Viz", icon = icon("stats",lib="glyphicon")),
    menuItem("Data Correlation", tabName = "Cor", icon = icon("xmark")),
    menuItem("Power Analysis", tabName = "Pow", icon = icon("chart-line"))
  )
)


# -- body ----
body <- dashboardBody(height = 800,
  tags$head(
    includeScript(path = "www/js/js4checkbox.js"),
    includeScript(path = "www/js/index.js"),
    tags$link(rel='stylesheet', type='text/css', href='styles.css'),
    tags$link(href="fa/css/all.css", rel="stylesheet"), # font-awesome
    tags$link(rel="shortcut icon", href="favicon.ico"),

    tags$style(HTML("#SessionID{font-size:12px;}")),
    tags$style(HTML("/* https://fonts.google.com/?preview.text=SLICK&preview.text_type=custom */
        @import url('//fonts.googleapis.com/css?family=Cairo|Cabin:400,700');
        /* Font of SLICK title */
      ")),
    tags$script(
      'var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
      dimension[0] = window.innerWidth;
      dimension[1] = window.innerHeight;
      Shiny.onInputChange("dimension", dimension);
    });
    '),
    tags$script("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")

  ),
  tabItems(
    tabItem(tabName = "Viz",
            Viz_UI('Viz')
    ),
    tabItem(tabName = "Cor",
            Cor_UI('Cor')
    ),
    tabItem(tabName = "Pow",
           Pow_UI('Pow')
    )
  )
)


# -- page ----

dashboardPage(
  skin = "blue-light",
  header=header,
  sidebar=sidebar,
  body=body,
  controlbar=controlbar,
  title='ECP',
  dashboardFooter(left =  paste0("ECP version:", packageVersion('ECP')),
                  right = h6("Copyright", HTML("&#169;"), tags$a(href='https://bluematterscience.com/',
                                                                 target="_blank", paste("Blue Matter Science Ltd.", format(Sys.Date(), "%Y")))))
)


