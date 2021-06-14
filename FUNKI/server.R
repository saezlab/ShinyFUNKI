# SERVER
server = function(input, output, session) {
  source("sub/02_server_upload.R", local=T)
  source("sub/03_server_dorothea.R", local=T)
  source("sub/04_server_progeny.R", local=T)
  source("sub/05_server_kinact.R", local=T)
  source("sub/06_server_carnival.R", local=T)
  source("sub/07_server_cosmos.R", local=T)
}
