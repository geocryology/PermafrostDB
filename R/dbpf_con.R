# =============================================================================
#'
#' @title dbpf_con
#'
#' @description Returns connection to permafrost database
#'
#' @param user character, User name
#' @param passwd  character, Password
#' @param host character, Location of database server, This is usually an IP address or a URL.
#' @param port character, port on which to make request.  Typically "5432" for postgres databases.
#' @param database character, "observations" or for testDB:"obs_dev"
#'
#' @return Returns a DB connection object.
#'
#' @details Function establishes a connection to the data base and returns a
#'          connection object for easy use. When finished, make sure to close the connection
#'          object (see examples).
#'
#'          As of \code{PermafrostDB v1.0.0}, Database credentials and host location must now be explicitly provided.
#'              Default connection information is no longer provided. This is done as a
#'              security consideration. However, to retain the convenience of older version of this function,
#'              you can create a text file called \code{permafrostdb.config} that provides the
#'              necessary information.
#'
#'              The location of the \code{permafrostdb.config} file is determined by the R function \code{path.expand("~")}.
#'              On linux computers, this usually corresponds to \code{/home/username}. On Windows machines, this is usually either
#'              \code{C:/Users/yourUserName} or \code{C:/Users/yourUserName/Documents} (RStudio). To be sure, run the command
#'              \code{path.expand("~")} in your R terminal of choice.
#'
#'              The file should be structured like a csv table, and should use quotes around the parameters:
#'
#'              \code{user,passwd,host,port}
#'
#'              \code{"your_username","your_pasword","your_db_host","your_db_port"}
#'
#'              an example might look like:
#'
#'              \code{user,passwd,host,port}
#'
#'              \code{"readonly","fjZ$fwg?H9(<jDsd","256.245.11.15","5432"}
#'
#'
#'
#' @export
#' @examples
#' #example of using specific user name and password
#' \donttest{
#' con <- dbpf_con("stephan", "password", "192.168.2.25", "5432")
#' dbDisconnect(con)
#'
#' # get connection object (using default values from a permafrostdb.config file)
#' con <- dbpf_con()
#' loc <- dbGetQuery(con, "SELECT * FROM locations")
#' dbDisconnect(con)         # close connection when done
#' }
#'
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_con <- function(user, passwd, host, port="5432", database="observations")
{
  # Check credentials
  if (missing(user) || missing(passwd) || missing(host)){

    config.file <- get.config()

    tryCatch({
                    credential <- read.csv(config.file, stringsAsFactors = FALSE, nrows=2)
                    user <- credential$user
                    passwd <- credential$passwd
                    host <- credential$host
                    port <- credential$port
                    database <- credential$database
    },
    error = function(e) {
      message(e)
      config.error()
      return
    })
  }

  # Load PGSQL requirements
  require(DBI)

  # enforce use of UTC
  Sys.setenv(TZ="UTC")

  # DB parameters
  pgDBConDetails <- c(database, host, port, user, passwd)

  # Initiate connection, return connection
  pgDBCon <- DBI::dbConnect(RPostgres::Postgres(), dbname = pgDBConDetails[1], host = pgDBConDetails[2], port = pgDBConDetails[3], user = pgDBConDetails[4], password = pgDBConDetails[5], sslmode="require")

  # check that observations table exists
  DBI::dbExistsTable(pgDBCon, database)

  #TRUE, return connection
  return(pgDBCon)
}


#' @title SSH database connection
#' @description Connect to database through an SSH tunnel
#' @param ssh_user Username with which to connect to remote host using SSH
#' @param ssh_host Address of remote host from which request will be made (e.g. '206.12.93.23' or 'data.myserver.com')
#' @param ssh_keyfile (optional) private key to log onto remote host (must be in OpenSSH format - *.pem). You must either provide
#' a passwordless keyfile, or set up a keyserver to handle the keyfile password. There will be no password prompt.
#' @param user Username for database. Passed to \code{\link{dbpf_con}}
#' @param passwd Password for database. Passed to \code{\link{dbpf_con}}
#' @param host Address for database server.
#' @param ssh_port Port on which to connect using ssh
#' @param port Port on which database server is (default based on \code{\link{dbpf_con}})
#' @param local_port Local port to use for tunnel.
#' @param database Name of database (default based on \code{\link{dbpf_con}})
#' @export
#' @details  Acts as a wrapper combining \code{\link{dbpf_con}} and \code{\link{create_tunnel}}.
#' Refer to those functions for more information. To auto-connect, add the following parameter keywords to the database configuration file
#' described in  \code{\link{dbpf_con}}.
#'
#'
#'              \code{user,passwd,host,port,ssh_user,ssh_host,ssh_port,ssh_keyfile,local_port}
#'
#'              \code{"readonly","fjZfwg?H9jDsd","256.245.11.15","5432","user01","125.25.1.35","5432","/home/user/.ssh/keyfile.pem","5555"}
dbpf_tunnel <- function(ssh_user, ssh_host, ssh_keyfile,
                        user, passwd, host, ssh_port='22',
                        port="5432", local_port='5555', database="observations"){

  if (missing(user) || missing(passwd) || missing(host) || missing(ssh_user) || missing(ssh_host)){
    config.file <- get.config()

    tryCatch({
      credential <- read.csv(config.file, stringsAsFactors = FALSE, nrows=2)
      user <- credential$user
      passwd <- credential$passwd
      host <- credential$host
      port <- ifelse(is.null(credential$port), port, credential$port)
      database <- ifelse(is.null(credential$database), database, credential$database)
      ssh_host <- credential$ssh_host
      ssh_port <- ifelse(is.null(credential$ssh_port), ssh_port, credential$ssh_port)
      ssh_keyfile <- credential$ssh_keyfile
      local_port <- ifelse(is.null(credential$local_port), local_port, credential$local_port)


    },
    error = function(e) {
      message(e)
      config.error()
      return
    })
  }

  if (ssh_host == host){
    host <- "127.0.0.1"
  }

  pid <- create_tunnel(ssh_user=ssh_user,
                       ssh_host=ssh_host,
                       ssh_keyfile=ssh_keyfile,
                       db_host=host,
                       db_port=port,
                       local_port=local_port,
                       ssh_port=ssh_port)

  # print("Waiting for tunnel")

  con <- dbpf_con(user=user,
                  passwd=passwd,
                  host="127.0.0.1",
                  port=as.character(local_port),
                  database=database)
  return(con)
}


#' @title Create SSH tunnel
#' @description Create an SSH tunnel in the background
#' @param ssh_user username with which to connect to remote host
#' @param ssh_host address of remote host from which request will be made (e.g. ip address)
#' @param ssh_keyfile (optional) path to privatekey file to log onto remote host
#' (must be in OpenSSH *.pem format - see details for more information)
#' @param db_host character, address to database host
#' @param db_port int, final destination port for requests on target machine
#' @param local_port int, port on localhost through which to access remote host
#' @param ssh_port int, ssh connection port of remote host (usually 22)
#' @details the SSH tunnel makes requests to the target from a remote host. A local port is used
#' to redirect requests through the tunnel.
#' Keyfiles must be in OpenSSH format. Google has ample information on how to convert from ppk to pem files,
#' @return int process id
#' @export
create_tunnel <- function(ssh_user, ssh_host, ssh_keyfile, db_host, db_port, local_port=5555, ssh_port=22){

  if (!require(ssh)){
    install.packages('ssh')
  }

  dbpf_close_tunnel()

  errlog <- tempfile("dbpf_err", fileext = ".txt")
  outlog <- tempfile("dbpf_out", fileext = ".txt")


  cmd <- paste0("ssh::ssh_tunnel(ssh::ssh_connect(",
                str_glue("host = '{ssh_user}@{ssh_host}:{ssh_port}', "),
                str_glue("keyfile='{gsub( '\\\\\\\\', '/', ssh_keyfile)}'),"),
                str_glue("port = {local_port}, target = '{db_host}:{db_port}')"))

  pid <- sys::r_background(
    std_out = FALSE,
    std_err = errlog,
    args = c("-e", cmd)
  )
  #
  # pid <- sys::exec_background(cmd='ssh',
  #                     std_out = FALSE,
  #                     std_err = errlog,
  #                     args = c("-L",
  #                              stringr::str_glue("127.0.0.1:{local_port}:{db_host}:{db_port}"),
  #                              stringr::str_glue("{ssh_user}@{ssh_host}"),
  #                              "-i",
  #                              gsub( "\\\\", "/", ssh_keyfile))
  # )

  # Sys.sleep(2)
  # if (any(grepl("Connection closed", readLines(errlog)))){
  #   stop("could not connect")
  # }

  assign("dbpf.tunnel.pid", pid, envir=.GlobalEnv)
  print(stringr::str_glue("Attempting tunnel to database on {db_host}:{db_port} on local port {local_port} through {ssh_host}. Running as process {pid}"))

  return(dbpf.tunnel.pid)
}

#' @title Close database ssh tunnel if it exists
#' @description closes database ssh tunnel
#' @details Only works in the context of the R session used to create the tunnel.
#' @export
dbpf_close_tunnel <- function(){
  tryCatch({tools::pskill(dbpf.tunnel.pid)   # kill existing tunnel if it exists
    message("Closing existing tunnel")},
    error = function(e) {})
}


#' @title Get path to configuration file
#' @noRd
get.config <- function(){
  path.expand(file.path("~", "permafrostdb.config"))
}

config.error <- function(){
  message(paste0("\n\n\nDatabase credentials not provided or configuration file not set up properly. ",
                 "You must supply a username, password, hostname and port to create a database connection. ",
                 "\nAlternatively, create a configuration file called '", get.config(), "' to save a 'default' connection. ",
                 "This file must have the following structure:"))
  message("\n\nuser,passwd,host,port")
  message('"your_username,"your_pasword","your_db_host","your_db_port"\n\n')
}


