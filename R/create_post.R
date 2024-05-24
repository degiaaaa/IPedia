get_create_post <- function(session, input, output, lokal) {
  observeEvent(input$beitrag_erstellen, {
    showModal(
      modalDialog(id="create_post",
        footer = NULL,
        easyClose = TRUE,
        
        
        div(
          id = "neu_beitrag",
          style = " max-width: 110%;",
          #padding: 0px;
        # wellPanel(
            tags$h2(
              "Neuen Beitrag erstellen",
              class = "text-center",
              actionButton("close"," ", icon= icon("fas fa-times")), 
            )
          ),

          #padding-top: 0
          
          div(
            style = "margin: inherit;",
            textInput("post_title", label = "Titel", placeholder = " Titel eingeben: z.B. 'Mein Erfahrungsbericht in MMK'")
          ),
          
        
        div(
          style = "margin: inherit;",
          fileInput(
            inputId = "post_bild",
            label = "Titelbild .jpg mit dem Maß 755 x 425",
            multiple = TRUE,
            buttonLabel = "Durchsuchen",
            placeholder = "Noch kein Bild ausgesucht."
          ),
          mainPanel(textOutput("pictureNames")),
        ),
        
          
          div(
            style = "margin: inherit;",
            fileInput(
              inputId = "post_dokumente",
              label = " Nur ein PDF-Dokument (optional)",
              multiple = TRUE,
              buttonLabel = "Durchsuchen",
              placeholder = " Noch kein Dokument ausgesucht."
            ),
            mainPanel(textOutput("fileNames")),
          ),
        
        

          
          
          # div(tags$Files, style="margin:22px, margin-left: auto"),
          
          
        

        div(
          id = "post_text_div_outter",
          textAreaInput("post_text", label = "Text", placeholder = " Textbeitrag eingeben."),
          
        ),
        
        #div(id= "filter_inner_card", style="width:100%; padding:5px; margin-bottom:20px; margin-top:60px; background-color:white;border-radius:2px", # ; box-shadow: 0 1.5px 2px rgba(0,0,0,0.4)
        # fluidRow(column (3,)
        div(
          
          selectizeInput(
            "semester_card",
            "Semester",
            choices = na.omit(filter_options$semester),
            selected = NULL,
            multiple = TRUE
          ),
          
        ),
        
        div(
          selectizeInput(
            "kurs_card",
            "Kurse",
            choices = na.omit(filter_options$kurs),
            selected = NULL,
            multiple = TRUE
          ),
          
        ),
        div(
          
          selectizeInput(
            "prof_card",
            "Profs & Co.",
            choices = na.omit(filter_options$prof),
            selected = NULL,
            multiple = TRUE
          ),
          
        ),
        div(
          selectizeInput(
            "aktivitaten_card",
            "Aktivitaten",
            choices = na.omit(filter_options$aktivitaten),
            selected = NULL,
            multiple = TRUE
          ),
          
          # )
          # )
        ),
        div(
          style = " margin:auto",
          
          actionButton("beitrag_hochladen", label = "Beitrag hochladen"),
          
      ),
      
      
      
      shinyjs::hidden(div(
        id = "TextfeldLeer",
        tags$p(
          "Oops! Der Titel oder der Text darf nicht leer sein.",
          style = "color: red; font-weight: 600;
                                                  padding-top: 5px;font-size:16px;",
          class = "text-center"
        )
      )),
      
      
      
      shinyjs::hidden(div(
        id = "gespeichert",
        tags$p(
          "Dein Beitrag wird gepostet.",
          style = "color: green; font-weight: 600;
                                                  padding-top: 5px;font-size:16px;",
          class = "text-center"
        )
      )),
      
      
    )
    
    )
  })
  
  # observeEvent(input$card_title,{
  #   title <- input$card_title
  #
  # })

  
  
  observeEvent(input$beitrag_hochladen, {
    
    date <- as.character(Sys.Date())
    
    user_aktuell <- lokal$user
    
    semester_card <- input$semester_card
    aktivitaten_card <- input$aktivitaten_card
    prof_card <- input$prof_card
    kurs_card <- input$kurs_card
    post_text <- input$post_text
    post_title <- input$post_title
    post_dokumente <- input$post_dokumente
    post_bild <- input$post_bild
    
    #save(semester_card, aktivitaten_card, prof_card, kurs_card, post_text, post_title, post_dokumente, post_bild, file="maximal_post.rdata")
    
    
    
    
    ##Filter
    
    semester1 <-
      if (is.null(semester_card)) {
        NA
      } else{
        paste0(semester_card, collapse="_")
      }
    
    aktivitaten1 <-
      if (is.null(aktivitaten_card)) {
        NA
      } else{
        paste0(aktivitaten_card, collapse="_")
      }
    
    profs1 <- if (is.null(prof_card)) {
      NA
    } else{
      paste0(prof_card, collapse="_")
    }
    kurs1 <- if (is.null(kurs_card)) {
      NA
    } else{
      paste0(kurs_card, collapse="_")
    }

    text1 <- if (is.null(post_text)) {
      NA
    } else{
      paste0(post_text, collapse="_")
    }

    titel1 <- if (is.null(post_title)) {
      NA
    } else{
      paste0(post_title, collapse="_")
    }

    dokumente1 <-
      if (is.null(post_dokumente)) {
        NA
      } else{
        post_dokumente
      }
    if (length(dokumente1) > 1 && !is.na(dokumente1)) {
      src <-
        paste0("user_documents/",
               user_aktuell,
               round(runif(1, 1, 1000)),
               dokumente1$name)
      file.copy(dokumente1$datapath, paste0("www/", src))
      dokumente1 <- src
    }

    post_bild1 <-
      if (!is.null(post_bild )) {
        post_bild
      } else{
        NA
      }
    if (length(post_bild1) > 1 && !is.na(post_bild1)) {
      src <-
        paste0("user_documents/",
               user_aktuell,
               round(runif(1, 1, 1000)),
               post_bild1$name)
      file.copy(post_bild1$datapath, paste0("www/", src))
      post_bild1 <- src
    }



    row_to_add <-
      c(
        user_aktuell,
        date,
        semester1,
        kurs1,
        aktivitaten1,
        profs1,
        dokumente1,
        post_bild1,
        text1,
        NA,
        titel1
      )
    #save(card_data, file="temp.rdata")


    if (nchar(text1) < 4 || nchar(titel1) < 4) {
      show("TextfeldLeer")
      hide("gespeichert")

      delay(3000, {
        hide("TextfeldLeer")
      })

    } else{
      card_data <<- rbind(card_data, row_to_add)
      save(card_data, file = "Daten/card_data.rdata")

      show("gespeichert")
      hide("TextfeldLeer")
      #delay(3000, {
        removeModal()
      #})
    }
    
    
    
    
  })
  
  
  
}



# card_data_reac <- reactiveVal({
#   card_data
# })
#
# observe({
#   card_data <- card_data_reac()
#   save(card_data, file="Daten/card_data.rdata")
# })


# observeEvent(input$neuer_beitrag_hochladen,{
#   "holaaa"
# })

#Falls in card_data was kaputt geht: copy : > card_data["user"] <- c("oxel","peter")
# > card_data["bild"] <- c("oxel_94f52d9f65c9499d7bd971bc9cebea1av1_max_755x425_b3535db83dc50e27c1bb1392364c95a2.jpg", NA)
# > card_data["profs"] <- c("Stefan Pfeffer", "NA")
# > card_data["aktivitaten"] <- c("Party","Sportreferate")
# > card_data["semester"] <- c("5","2")
# > card_data <- card_data[-(3:6),]
# > save(card_data, file="Daten/card_data.rdata")
# > View(card_data)
# > runApp()



#> View(card_data)


#card_data$user <- paste0("user_documents/",card_data$bild) 
#colnames(card_data)
#colnames(card_data)[11] <- "titel"

# > card_data <- openxlsx::read.xlsx("Daten/Beispielbeitraege.xlsx")
# > card_data <- cbind(card_data[,1:9],kommentare=NA,card_data[,10])
# > colnames(card_data)[11] <- "titel"
# > card_data$bild <- paste0("user_documents/",card_data$bild) 
# > save(card_data, file="Daten/card_data.rdata")
# > runApp()

# card_data <- card_data %>% mutate(titel = c(NA,NA))
# save(card_data, file="Daten/card_data.rdata")
#View(card_data)
