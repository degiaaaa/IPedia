get_card_server <- function(session, input, output, lokal){
  
  card_data_reac <- reactiveVal({
    card_data 
  })
  
  observe({
    card_data <- card_data_reac()
    save(card_data, file="Daten/card_data.rdata")
  })
 
   
  card_data_filtered <- reactive({
    if(is.null(lokal$user)){ 
      return("erst_anmelden")
      
    }else{
      #{if(!is.null(input$semester_filter)){filter(., grepl(semester, semester, fixed = TRUE))}else{.}} %>% 
      # card_temp <- card_data_reac() %>% 
      #   {if(!is.null(input$kurs_filter)){filter(., kurs %in% input$kurs_filter)}else{.}} %>% 
      #   {if(!is.null(input$prof_filter)){filter(., profs %in% input$prof_filter)}else{.}} %>% 
      #   {if(!is.null(input$aktivitaten_filter)){filter(., aktivitaten %in% input$aktivitaten_filter)}else{.}}
      
      card_temp <- card_data_reac() 
        
      if(!is.null(input$semester_filter)){
        y <- rep(FALSE, nrow(card_temp))
        useless <- sapply(input$semester_filter, function(x){ y <<- y | grepl(x, card_temp$semester, fixed = TRUE)})
        card_temp <- card_temp[y,]
      }
    
      
      if(!is.null(input$kurs_filter)){
        y <- rep(FALSE, nrow(card_temp))
        useless <- sapply(input$kurs_filter, function(x){ y <<- y | grepl(x, card_temp$kurs, fixed = TRUE)})
        card_temp <- card_temp[y,]
      }
      
      if(!is.null(input$aktivitaten_filter)){
        y <- rep(FALSE, nrow(card_temp))
        useless <- sapply(input$aktivitaten_filter, function(x){ y <<- y | grepl(x, card_temp$aktivitaten, fixed = TRUE)})
        card_temp <- card_temp[y,]
      }
      #sapply
      #grepl filter: schaue ob element x (z.B. Asta) in card_temp$aktivitaten 
      if(!is.null(input$prof_filter)){
        y <- rep(FALSE, nrow(card_temp))
        useless <- sapply(input$prof_filter, function(x){ y <<- y | grepl(x, card_temp$profs, fixed = TRUE)})
        card_temp <- card_temp[y,]
      }
      card_temp
    }
   
  })
  
  observeEvent(input$close, {
    removeModal()})

  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.pdf', sep='')
  #   },
  #   
  #   content = function(con) {
  #     write.pdf(data, con)
  #   }
  # )
  
  # 
  # output$downloadData <- downloadHandler(
  #   filename = "your-pdf-name.pdf",
  #   content = function(file) {
  #     file.copy("www/teste.pdf", file)
  #   }
  # )
  
  # output$download <- renderUI({
  #   
  #     downloadButton('OutputFile', 'Herunterladen' , icon = icon("fa fa-download"))
  #   
  # })
  # 
  # output$OutputFile <- downloadHandler(
  #   
  #   
  #   filename = function() {
  #     paste("OutputFile", "_",Sys.time(),".pdf",sep="")
  #   },
  #   
  #   content = function(file) {
  #     
  #     
  #     write.pdf(my_function(file1$datapath), file, sep = ",",
  #               row.names = FALSE)
  #   }
  # )



  observeEvent(input$toggle_filter_kachel,{
    if(input$toggle_filter_kachel){
      hide(id="filter_kachel")
      
    }else{
      show(id="filter_kachel")
    }
  })
  
  
  # output$post_big_downloader <- downloadHandler(
  #   
  #   # filename = function() {
  #   #   inner_card_data[x,"dokumente"]
  #   # },
  #   # 
  #   # content = function(file) {
  #   #   showNotification(file)
  #   #   file.copy(inner_card_data[x,"dokumente"],file)
  #   # }
  #   filename = "your-pdf-name.pdf",
  #   content = function(file) {
  #     file.copy(paste0("www/",card_data[24,"dokumente"]), file)
  #   }
  # )
  
  # output[["post_big_downloader"]] <- downloadHandler(
  #     filename = "your-pdf-name.pdf",
  #     content = function(file) {
  #       file.copy(paste0("www/",card_data[24,"dokumente"]), file)
  #     }
  # )
  
  output$card_grid <- renderUI({

    inner_card_data <- card_data_filtered()
    

    if(is.null(nrow(inner_card_data)) && inner_card_data == "erst_anmelden"){
      hide("filter")
      hide("beitrag_erstellen")
      div(id = "LandingPage", style = "margin-left = 0px",
      tags$img(src='LP ohne Navbar.svg'))
     # tags$img(src='Logo.svg',height='92',width='92')
      #div(id = "LandingPage",tags$img(src='LP ohne Navbar@2x.png',height='100',width='100'))
      ###div(style="margin-top:-10px;",tags$img(src='Logo.svg',height='40',width='40')),
      
       }else if(nrow(inner_card_data)==0){
         
         
      show("filter")
      show("beitrag_erstellen")
      div(id = "OopsPage",
          tags$img(src='Oops!.svg'))
  
      }else{
      show("filter")
      show("beitrag_erstellen")
      
      lapply(session$userData$observerList, function(x){x$destroy()})
      lapply(session$userData$observerList_kommentar, function(x){x$destroy()}) # https://shiny.rstudio.com/reference/shiny/1.5.0/MockShinySession.html
      lapply(session$userData$observerList_downloader, function(x){x$destroy()})

      ui_list <- lapply(1:nrow(inner_card_data), function(x) {
        kommentare_list <- list()
        kommentare_list[[1]] <- tags$hr()
        kommendar_add <- list()
        if(!is.na(inner_card_data[x,"kommentare"])){
          temp <- c(unlist(strsplit(inner_card_data[x,"kommentare"], "I%X%I")))
          
          for(i in 1:length(temp)){
            kommentar <- strsplit(temp[[i]], "I%Y%I")
            kommentare_list[[i+1]] <- div(style="padding-left:10px; padding-right:10px; word-break:break-word;", userPost(
              
              src = if(!is.na(user_data[which(user_data$user==kommentar[[1]][1]),"bild"])){user_data[which(user_data$user==kommentar[[1]][1]),"bild"]}else{NULL},
              collapsible = FALSE,
              author = kommentar[[1]][1],
              description = kommentar[[1]][2],
              kommentar[[1]][3]
            ), hr())
            
          }
        }
        kommendar_add[[1]] <- div(style="", 
                                  fluidRow(
                                    column(9,
                                           textAreaInput(paste0("kommentar_textarea",x),"Schreibe einen Kommentar:", width = "100%")
                                    ),
                                    column(3,
                                           actionButton(paste0("kommentar_verfasst",x),"Kommentieren", style="margin-left:-14px; margin-top:28%;")
                                    )
                             )
                                  
        )
        
        
        session$userData$observerList_kommentar[[x]] <<- observeEvent(input[[paste0("kommentar_verfasst",x)]],{
          req(input[[paste0("kommentar_textarea",x)]])
          text <- input[[paste0("kommentar_textarea",x)]]
          isolate({
            card_data_temp <- card_data_reac()
            if(!is.na(card_data_temp[x,"kommentare"])){
              card_data_temp[x,"kommentare"] <- paste0(card_data_temp[x,"kommentare"], lokal$user, "I%Y%I", Sys.Date(), "I%Y%I", text, "I%X%I")
            }else{
              card_data_temp[x,"kommentare"] <- paste0(lokal$user, "I%Y%I", Sys.Date(), "I%Y%I", text, "I%X%I")
            }
            card_data_reac(card_data_temp)
            kommentare_list[[length(kommentare_list)+1]] <- div(style="padding-left:10px; padding-right:10px; word-break:break-word;",
              
              userPost(
              src = if(!is.na(user_data[which(user_data$user==lokal$user),"bild"])){user_data[which(user_data$user==lokal$user),"bild"]}else{NULL},
              collapsible = FALSE,
              author = lokal$user,
              description = Sys.Date(),
              text
            ))
            
      
            delay(200,{
              html(id=paste0("post_big_",x),asis = TRUE, add = FALSE,
                   #as.character(create_card_big(x, inner_card_data[x,], user_data[which(user_data$user==inner_card_data[x,"user"]),], kommentare_list, div=FALSE))
                   #html(selector = paste0("post_big_",x,":last-child"), "")#,
                   showModal(modalDialog(
                     #title = "Somewhat important message",
                     size = "l",
                     easyClose = TRUE,
                     footer = NULL,
                     create_card_big(x, inner_card_data[x,], user_data[which(user_data$user==inner_card_data[x,"user"]),], kommentare_list, kommendar_add, div=FALSE)
                   ))
              )
            })
            
          })
        }, ignoreInit = TRUE)
        
        
        # session$userData$observerList_downloader[[x]] <<- observeEvent(input[[paste0("post_big_downloader_",x)]],{
        #   showNotification(inner_card_data[x,"dokumente"])
        #   #output[[plotname]]
        # })
        
        # output[[paste0("post_big_downloader_",x)]] <- downloadHandler(
        # 
        #   # filename = function() {
        #   #   inner_card_data[x,"dokumente"]
        #   # },
        #   # 
        #   # content = function(file) {
        #   #   showNotification(file)
        #   #   file.copy(inner_card_data[x,"dokumente"],file)
        #   # }
        #   filename = "your-pdf-name.pdf",
        #   content = function(file) {
        #     file.copy(inner_card_data[x,"dokumente"], file)
        #   }
        # )
        
        # output[["post_big_downloader"]] <- downloadHandler(
        # 
        #   # filename = function() {
        #   #   inner_card_data[x,"dokumente"]
        #   # },
        #   #
        #   # content = function(file) {
        #   #   showNotification(file)
        #   #   file.copy(inner_card_data[x,"dokumente"],file)
        #   # }
        #   filename = "your-pdf-name.pdf",
        #   content = function(file) {
        #     file.copy(inner_card_data[24,"dokumente"], file)
        #   }
        # )
        
        output[[paste0("post_big_downloader_",x)]] <- downloadHandler(
          filename = "your-pdf-name.pdf",
          content = function(file) {
            file.copy(paste0("www/",card_data[x,"dokumente"]), file)
          }
        )
        
        delay(500,{
          session$userData$observerList[[x]] <<- observe({
            onclick(paste0("post_",x),{
              showModal(modalDialog(
                #title = "Somewhat important message",
                size = "l",
                easyClose = TRUE,
                footer = NULL,
                create_card_big(x, inner_card_data[x,], user_data[which(user_data$user==inner_card_data[x,"user"]),], kommentare_list, kommendar_add)
              ))
            })
          })
        })
        create_card(paste0("post_",x), inner_card_data[x,], user_data[which(user_data$user==inner_card_data[x,"user"]),])
      })
     # save(ui_list, file="temp.rdata")
      
      tagList(div(id="container_my_fluid", class="my_fluid_cols", rev(ui_list)))
      
    }

  })
  
  
}

