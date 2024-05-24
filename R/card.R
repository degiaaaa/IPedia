create_card <- function(set_id, card_data_row, user_row){
  
  #column(12, align="center",
    div(id=set_id, style="padding:0px; box-shadow: 0 1.5px 2
        px rgba(0,0,0,0.4); border-radius:2px; margin-bottom:10px; background-color: white;",
        
       userPost(
           collapsible = FALSE,
           
           src = if(!is.na(user_row$bild)){user_row$bild}else{NULL},
           author = div(div(style="text-align:left; word-break: break-word;",card_data_row$titel), div(style="color:#999; font-size:13px;",card_data_row$user)),
           description = paste0("Erstellt am ",card_data_row$datum),
           userPostMedia(src = if(!is.na(card_data_row$bild)){card_data_row$bild}else{NULL}),
           div(style="padding-left:10px; padding-right:10px; padding-bottom:10px; word-break:break-word;", if(nchar(card_data_row$text)>300){paste0(substr(card_data_row$text,1,300)," ...")}else{card_data_row$text}),
       )
    )
  #)
}

create_card_big <- function(id, card_data_row, user_row, kommentare_list, kommendar_add, div=TRUE){
  if(div==TRUE){
    div(id=paste0("post_big_",id), style = "min-height:100vh ",
       
        userPost(
          actionButton("close"," ", icon= icon("fas fa-times")),
          collapsible = FALSE,
          src = if(!is.na(user_row$bild)){user_row$bild}else{NULL},
          author = div(div(style="text-align:left; word-break: break-word;",card_data_row$titel), div(style="color:#999; font-size:13px;", card_data_row$user)),
          description = paste0("Erstellt am ",card_data_row$datum),
          userPostMedia(src = if(!is.na(card_data_row$bild)){card_data_row$bild}else{NULL}),
          div(style="padding-left:10px; padding-right:10px; word-break:break-word;",card_data_row$text),
          
          if(!is.na(card_data_row$dokumente))
          { 
            tags$hr()
            #downloadButton
            downloadButton(paste0("post_big_downloader_",id), paste0(" Anhang Herunterladen") , icon = icon("fa fa-download"))
          },
          
          
          # if(!is.null(card_data_row$dokumente) || !is.na(card_data_row$dokumente)){
          # HTML("<hr>")
          # downloadButton('downloadData', 'Download')},
          
          
          hr(),
          kommendar_add,
          rev(kommentare_list),
          
         
        )
        
    )
  }else{
        userPost(
          collapsible = FALSE,
          actionButton("close"," ", icon= icon("fas fa-times")), 
          
          src = if(!is.na(user_row$bild)){user_row$bild}else{NULL},
          author = div(div(style="text-align:left; word-break: break-word;",card_data_row$titel), div(style="color:#999; font-size:13px;", card_data_row$user)),
          description = paste0("Erstellt am ",card_data_row$datum),
          userPostMedia(src = if(!is.na(card_data_row$bild)){card_data_row$bild}else{NULL}),
          div(style="padding-left:10px; padding-right:10px; word-break:break-word;", card_data_row$text),
         
          # if(!is.na(card_data_row$dokumente))
          #    { tags$hr()
          #   
          #      uiOutput("download")},
         
          
          # if(!is.null(card_data_row$dokumente) || !is.na(card_data$dokumente)){
          #   HTML("<hr>")
          #   downloadButton('downloadData', 'Download')},
          
          hr(),
          kommendar_add,
          rev(kommentare_list)
        )
  }
}

