

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    title = "IPedia",
    tags$head(tags$link(rel="shortcut icon", href="Favicon-48x48.ico")),
    useShinyjs(),
    
    useShinydashboardPlus(),
    includeScript(path = "www/myscript.js"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    ),
    fixedPanel(
        top = "0px",
        width="auto",
        right="105px",
        align="left",
        style="z-index:10000; margin-top:14px;",
        textOutput("logo_user_name")
    ),
    fixedPanel(
        top = "0px",
        width="auto",
        right="30px",
        align="right",
        style="z-index:10000; margin-top:-13px; text-align:right;",
        actionButton("login_buttn", "Login", class="btn")
    ),
    
    ##Beitrag Button im Hintergrund 
    fixedPanel(
       
        bottom = "3%",
        left = "35%",
        width="30%",
        right="35%",
    
            align="center",
        style="z-index:10000;", 
     
        shinyjs::hidden(
        actionButton("beitrag_erstellen", label = "Beitrag erstellen")),
    ),
    
    navbarPage(
        
       div(style="margin-top:-34px;",tags$img(src='Logo.svg',height='91',width='91')),
        position = "fixed-top",
        collapsible = FALSE,
        fluid = TRUE,
                   #tabPanel("Home",
                            
                            fluidRow(
                              div(id="padding_anpassen",
                                column(12, align="center", shinyjs::hidden(
                                    div(id= "filter", style="width:100%; margin-bottom:35px; margin-top:75px; background-color:white; border-radius:2px", #box-shadow: 0 1.5px 2px rgba(0,0,0,0.4);
                                     
                                          div(style="text-align: left; padding-left:10px; padding-top:5px; height:30px; font-size:10)0% ",
                                              prettyToggle(inputId = "toggle_filter_kachel", 
                                                    value = FALSE,

                                                    label_on = "Filter anzeigen", icon_on = icon("chevron-circle-down", "fas-3x"), status_on = "info",
                                                    status_off = "warning", label_off = "Filter ausblenden", icon_off = icon("chevron-circle-up", "fas-3x"))),
                                                                                hidden(div(id = "filter_kachel", style="width:100%;",
                                                                                           
                                        #   prettyToggle shiny 
                                        # label_on = "Filter ausblenden", if("Filter ausblenden" == TRUE){ tags$img(src='arrow_drop_down-24-px.svg',height='40',width='40')
                                        # else{label_off = "Filter anzeigen" tags$img(src='arrow_drop_down-24-px.svg',height='40',width='40')})
                                        # hidden(div(id="filter_kachel", style="width:100%;",
                                        #

                                                  hr(height=10),
                                                  fluidRow(
                                                      column(3, align="left",
                                                             selectizeInput("semester_filter", "Semester", choices=na.omit(filter_options$semester), selected=NULL, multiple=TRUE)
                                                      ),
                                                      column(3, align="left",
                                                             selectizeInput("kurs_filter", "Kurse", choices=na.omit(filter_options$kurs), selected=NULL, multiple=TRUE)  
                                                      ),
                                                      column(3, align="left",
                                                             selectizeInput("prof_filter", "Profs & Co.", choices=na.omit(filter_options$prof), selected=NULL, multiple=TRUE) 
                                                      ),
                                                      column(3, align="left",
                                                             selectizeInput("aktivitaten_filter", "Aktivitaten", choices=na.omit(filter_options$aktivitaten), selected=NULL, multiple=TRUE)  
                                                      )
                                                  )
                                                  ))
                                    ) )
                                       # boxPlus(
                                       #     title = "Add Filter:", 
                                       #     closable = FALSE, 
                                       #     width = 12,
                                       #     status="primary",
                                       #     solidHeader = FALSE, 
                                       #     collapsible = TRUE,
                                       #     collapsed = TRUE,
                                       #     numericInput("card_count","Anzahl Cards", value = 1),
                                       #     selectizeInput("kurs_filter", "Kurse", choices=filter_options$kurs, selected=NULL, multiple=TRUE)
                                       # )
                                )),
                                uiOutput("card_grid")
                            )
                   )
                  # tabPanel("Summary",
                            
                   #),
                   
                  # navbarMenu("More",
                              #tabPanel("Table",
                                       
                             # ),
                             # tabPanel("About",
                                       
                                       
                             # )
                   #)
#)
))
