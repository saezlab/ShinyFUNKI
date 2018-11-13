setBookmarkExclude(c("upload_bookmark", "dorothea_bookmark"))

observeEvent(input$upload_bookmark, {
  session$doBookmark()
})

observeEvent(input$dorothea_bookmark, {
  session$doBookmark()
})