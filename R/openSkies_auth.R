getTrinoToken <- function(username, password) {
  request = POST("https://auth.opensky-network.org/auth/realms/opensky-network/protocol/openid-connect/token",
                 body=list(client_id="trino-client", grant_type="password", username=username, password=password), encode="form")
  token <- content(request)$access_token
  return(token)
}

getTrinoConnection <- function(username, token) {
    set_config(
        add_headers(Authorization = paste("Bearer", token)),
        override = TRUE
        )
    con <- dbConnect(
        drv = RPresto::Presto(),
        host = "https://trino.opensky-network.org",
        port = 443,
        user = username,
        catalog = "minio",
        schema = "osky",
        use.trino.headers=TRUE)
    set_config(config())
    return(con)
}

getTrinoAuth <- function(username=NULL, password=NULL, client_id=NULL, client_secret=NULL){
  token <- getTrinoToken(username = username, password = password, client_id = client_id, client_secret = client_secret)
    set_config(
        add_headers(Authorization = paste("Bearer", token)),
        override = TRUE
        )
    con <- dbConnect(
        drv = RPresto::Presto(),
        host = "https://trino.opensky-network.org",
        port = 443,
        user = username,
        catalog = "minio",
        schema = "osky",
        use.trino.headers=TRUE)
    set_config(config())
  return(connection)
}