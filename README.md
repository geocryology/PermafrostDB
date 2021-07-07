# PermafrostDB
R package that provides convenience functions for interacting with a permafrost database that implements the [COLDASS schema](https://github.com/geocryology/COLDASS). This library provides wrappers to query, import and export data.

## Installation

### Option 1: Install from source
This requires that you have git installed on your computer.

```bash
git pull https://github.com/geocryology/PermafrostDB
cd PermafrostDB
Rscript -e "devtools::install()"
```

### Option 2: Installing with `install_github`
If you don't have git installed on your computer, you can use a function in the `devtools` package that downloads and installs it for you. If you use this from R-studio, you may need to update your other packages beforehand.

```r
library(devtools)
install_github("geocryology/PermafrostDB", ref="main")
```

## Usage

### The databse connection object
Most functions in `PermafrostDB` require a database connection object. This is created using the `dbpf_con()` function. You need to provide a username and password, as well as the database host and port number:

```R
con <- dbpf_con(user="groucho",
                passwd="swordfish",
                host="206.145.32.44",
                port="5432")
```

You can set up default connection settings by creating a file called `permafrostdb.config` and saving it in your home directory. Your home directory is determined using R's `path.expand()` function, and may vary depending on which R environment you use (e.g. on Windows, R Studio sets it to `C:/Users/Username/Documents` whereas base R sets it to `C:/Users/Username`). To determine the right location for the file, run `path.expand("~")` from your R terminal of choice.

Your file should contain the following three lines, with the information on the second line changed to correspond with your database. The third line is blank and is only there to signal the end of the file.

```
user,passwd,host,port
"groucho","swordfish","206.145.32.44","5432"

```
#### Secure access
Because Postgres is not designed to be exposed to the wider internet, you may be restricted in the locations or IP addresses from which you are able to access the database. PermafrostDB provides the function `dbpf_tunnel` as a way to connect to the database securely using [SSH tunneling](https://i.stack.imgur.com/a28N8.png). Note that the database credentials remain the same as for `dbpf_con` but you additionally provide SSH credentials to a location that is able to access the database directly.

```R
con <- dbpf_tunnel(ssh_user = 'user01', ssh_host='206.12.93.23', ssh_port = '22', ssh_keyfile = "C:/Users/me/sshkey.pem",
                   user = 'groucho', passwd = 'swordfish', host = '206.145.32.44', port = '5432')
```

### Accessing the database
First, create a database connection object as illustrated above.

#### Listing all locations
Get a list of all locations in the database with `dbpf_locations(con)`

#### Access temperature records
Access temperature records using `dbpf_observations_agg(con, location, period)`.
