


shinyServer(function(session, input, output) {
    

    lokal <- reactiveValues(user=NULL)
    
   # html(selector = ".nav.navbar-nav", add = TRUE, html="<hr id='nav_slide_hover'>")
    
    get_login_server(session, input, output, lokal)
    
    get_card_server(session, input, output, lokal)

    get_create_post(session, input, output, lokal)

})
