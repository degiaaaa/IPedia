get_login_server <- function(session, input, output, lokal){
  

  
  
  
  observeEvent(input$login_buttn,{
    showModal(modalDialog(
      footer=NULL,
      easyClose = TRUE,
      #fade = TRUE,
      id = "loginpage", style = "width:100%; max-width: 100%; margin: 0 auto; padding: 0px;",
          wellPanel(
            tags$h2("LOG IN",id="log_uberschrift", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
            
            shinyjs::hidden(
              tags$h2("LOG IN",id="log_uberschrift", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;")),
            
            shinyjs::hidden(
              tags$h2("REGISTRIEREN",id="register_uberschrift", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;")),
            
            hide("log_uberschrift"),
            show("register_uberschrift"),
            
            textInput("anmelden_user", placeholder="Benutzername", label = tagList(icon("user"), "Benutzername")),
            passwordInput("anmelden_pw", placeholder="Passwort", label = tagList(icon("unlock-alt"), "Passwort")),
            br(),
            div(
              style = "text-align: center;",
              actionButton("anmelden", "SIGN IN"),#, style = "color: white; background-color:#3c8dbc;
                                       #padding: 10px 15px; width: 150px; cursor: pointer;
                                       #font-size: 18px; font-weight: 600;"),
              shinyjs::hidden(
                actionButton("registrieren", "REGISTER")), #, style = "color: white; background-color:#3c8dbc;
                                      # padding: 10px 15px; width: 150px; cursor: pointer;
                                       #font-size: 18px; font-weight: 600;")),
              
              shinyjs::hidden(
                div(id = "nomatch",
                    tags$p("Oops! falsches Passwort oder falscher Benutzername",
                           style = "color: red; font-weight: 600; 
                                                  padding-top: 5px;font-size:16px;", 
                           class = "text-center"))),
              shinyjs::hidden(
                div(id = "nomatch_register",
                    tags$p("Oops! Benutzername gibts schon",
                           style = "color: red; font-weight: 600; 
                                                  padding-top: 5px;font-size:16px;", 
                           class = "text-center"))),
              shinyjs::hidden(
                div(id = "passwortkurz",
                    tags$p("Oops! Passwort zu kurz. Versuche ein längeres",
                           style = "color: red; font-weight: 600; 
                                                  padding-top: 5px;font-size:16px;", 
                           class = "text-center"))),
              
              shinyjs::hidden(
                div(id = "match",
                    tags$p("Erfolgreich angemeldet",
                           style = "color: green; font-weight: 600; 
                                                  padding-top: 5px;font-size:16px;", 
                           class = "text-center"))),
              br(),
              hr(),
              div(
                style = "text-align: center;",
                
                actionLink("registrieren_link","Noch nicht bei IPedia? Hier registrieren", style ="color: blue"),
                
                hidden(actionLink("anmelden_link","Hier anmelden", style ="color: blue"))
                
              )
              
            ))
      
    ))
  })
  
  observeEvent(input$anmelden,{
    user <- input$anmelden_user
    pw <- input$anmelden_pw
   
    if(length(which(user_data$user == user & user_data$pw == pw))>0){
      lokal$user <- user #speichern, dass user mit dem nemen sich angemeldet hat
      #alert("Sie wurden erforgreich eingelogt")
      show("match")
      delay(600, {
        removeModal()
      })
      
    }else{
      # alert("Fehler!!!")
      show("nomatch")
     delay(600,{ hide("nomatch")})
    }
    
  })
  
  observeEvent(input$registrieren,{
    
    user <- input$anmelden_user
    pw <- input$anmelden_pw
    
    
    if(nchar(pw) < 6){
      show("passwortkurz")
      delay(600, {
        hide("passwortkurz")
      })
    }else{
      if(length(which(user_data$user == user))==0){ #prüfe ob der user existiert, wenn länge gleich null, dann neu speichern
        lokal$user <- user #speichern, dass user mit dem nemen sich angemeldet hat
        user_data <<- rbind(user_data, c(user, pw, NA)) 
        save(user_data, file="Daten/user_data.rdata")
        show("match")
        delay(400, {
          removeModal()
        })
        
      }else{
        show("nomatch_register")
        delay(600, {
          hide("nomatch_register")
        })
      }
    }
    
    
  })
  
  
  
  output$logo_user_name <- renderText({
    req(lokal$user)
    user_aktuell <- lokal$user
   paste("Hallo", user_aktuell)
  })
  
  observeEvent(input$registrieren_link, {
    hide("log_uberschrift")
    show("register_uberschrift")
    show("registrieren")
    hide("anmelden")
    hide("registrieren_link")
    show("anmelden_link")
    hide("nomatch")
  })
  
  observeEvent(input$anmelden_link, {
    show("log_uberschrift")
    hide("register_uberschrift")
    hide("registrieren")
    show("anmelden")
    show("registrieren_link")
    hide("anmelden_link")
    hide("passwortkurz")
  })
  
  
}