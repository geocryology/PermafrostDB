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
#' con <- dbpf_con("stephan", "password", "192.168.2.25", "5432")
#' dbDisconnect(con)
#' 
#' # get connection object (using default values from a permafrostdb.config file)
#' con <- dbpf_con()  
#' loc <- dbGetQuery(con, "SELECT * FROM locations")
#' dbDisconnect(con)         # close connection when done 
#' 
#' @author Stephan Gruber <stephan.gruber@@carleton.ca>
# =============================================================================

dbpf_con <- function(user, passwd, host, port="5432")
{
  # Check credentials
  if (missing(user) || missing(passwd) || missing(host)){
    
    config.file <- path.expand(file.path("~", "permafrostdb.config"))
    
    tryCatch({
                    credential <- read.csv(config.file, stringsAsFactors = FALSE, nrows=2)
                    user <- credential$user
                    passwd <- credential$passwd
                    host <- credential$host
                    port <- credential$port
    },
    error = function(e) {
      message(e)
      message(paste0("\n\n\nDatabase credentials not provided or configuration file not set up properly. ",
                 "You must supply a username, password, hostname and port to create a database connection. ",
                 "\nAlternatively, create a configuration file called '",config.file,"' to save a 'default' connection. ",
                 "This file must have the following structure:"))
      message("\n\nuser,passwd,host,port")
      message('"your_username,"your_pasword","your_db_host","your_db_port"\n\n')
      
      return
    })
  }
  
  # Load PGSQL requirements
  require(DBI) 
 
  # enforce use of UTC
  Sys.setenv(TZ="UTC")
  
  # DB parameters
  pgDBConDetails <- c("observations",host, port, user, passwd)
  
  # Initiate connection, return connection
  pgDBCon <- dbConnect(RPostgres::Postgres(), dbname = pgDBConDetails[1], host = pgDBConDetails[2], port = pgDBConDetails[3], user = pgDBConDetails[4], password = pgDBConDetails[5], sslmode="require")
  
  # check that observations table exists
  DBI::dbExistsTable(pgDBCon, "observations")
  
  #TRUE, return connection
  return(pgDBCon)
}
