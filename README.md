# PermafrostDB
R package that provides convenience functions for interacting with a permafrost database that implements the [schema described in SensorDB](https://github.com/geocryology/sensorDb#schema). This library provides wrappers to query, import and export data.

## Installation

### Option 1: Install from source
This requires that you have git installed on your computer.

```bash
git clone https://github.com/geocryology/PermafrostDB
cd PermafrostDB
Rscript -e "devtools::install()"
```

### Option 2: Installing with `install_github`
If you don't have git installed on your computer, you can use a function in the `devtools` package that downloads and installs it for you. If you use this from R-studio, you may need to update your other packages beforehand.

```R
library(devtools)
install_github("geocryology/PermafrostDB", ref="main")
```

## Usage
Functions in the `PermafrostDB` library are documented with docstrings. You can access the documentation for a function by running `help(function_name)` in R. Below are some examples of the most commonly used functions.

### Connecting to the database
Most functions in `PermafrostDB` require a database connection object. This is created using the `dbpf_con()` function. You need to provide a username and password, as well as the database host, name, and port number (The credentials below are examples - contact your 'database person' to get the correct credentials for you.):

```R
con <- dbpf_con(user="groucho",
                passwd="swordfish",
                host="206.145.32.44",
                port="5432",
                database="observations")
```

You can set up default connection settings by creating a file called `permafrostdb.config` and saving it in your home directory. Your home directory is determined using R's `path.expand()` function, and may vary depending on which R environment you use (e.g. on Windows, R Studio sets it to `C:/Users/Username/Documents` whereas base R sets it to `C:/Users/Username`). To determine the right location for the file, run `path.expand("~")` from your R terminal of choice.

Your file should contain the following three lines, with the information on the second line changed to correspond with your database. The third line is blank and is only there to signal the end of the file (The credentials below are examples - contact your 'database person' to get the correct credentials for you.).

```
user,passwd,host,port,database
"groucho","swordfish","206.145.32.44","5432","observations"

```
#### Secure access
Because Postgres is not designed to be exposed to the wider internet, you may be restricted in the locations or IP addresses from which you are able to access the database. PermafrostDB provides the function `dbpf_tunnel` as a way to connect to the database securely using [SSH tunneling](https://i.stack.imgur.com/a28N8.png). Note that the database credentials remain the same as for `dbpf_con` but you additionally provide SSH credentials to a location that is able to access the database directly (The credentials below are examples - contact your 'database person' to get the correct credentials for you.).

```R
con <- dbpf_tunnel(ssh_user='user01', ssh_host='206.12.93.23', ssh_port='22',
                   ssh_keyfile="C:/Users/me/sshkey.pem", local_port='5555',
                   user='groucho', passwd='swordfish', host='206.145.32.44',
                   port='5432', database='observations')
```

### Interacting with the database
First, create a database connection object, `con`, as illustrated above.

#### Listing all locations
Get a list of all locations in the database with `dbpf_locations`

```R
locations <- dbpf_locations(con)
> tail(locations[c("name", "lon", "lat")])

              name        lon      lat
684 NGO-RC-174 -109.5571 64.65801
685 NGO-RC-175 -109.4402 64.62102
686 NGO-RC-033 -109.8557 64.34058
687 NGO-RC-149 -110.5478 64.62925
688 NGO-RC-157 -110.4697 64.85303
689 NGO-RC-169 -110.4619 64.76850
```
#### Access time series observations
The easiest way to access time series records, such as temperature from thermistors, is with the `dbpf_observations_agg` function, which aggregates observations at a site for a given averaging period.

```R
> obs <- dbpf_observations_agg(con, "NGO-RC-169", period=24)
> head(obs)
    loc_name height    agg_avg agg_cnt       time
1 NGO-RC-169   -0.5 -0.1768801      16 2015-07-04
2 NGO-RC-169   -0.5 -0.1885972      24 2015-07-05
3 NGO-RC-169   -0.5 -0.2016046      24 2015-07-06
4 NGO-RC-169   -0.5 -0.2328397      24 2015-07-07
5 NGO-RC-169   -0.5 -0.2582143      24 2015-07-08
6 NGO-RC-169   -0.5 -0.2640735      24 2015-07-09
```

The returned dataframe contains the following columns:
- `loc_name`: The name of the location
- `height`: The height of the sensor
- `agg_avg`: The average value of the observations within the aggregation period
- `agg_cnt`: The number of observations that were averaged
- `time`: The time of the observations

#### Access manual observations
Use the `dbpf_sensors` function to get a list of valid 'sensors' (i.e. manual observations). Then use the `dbpf_observations` function to get the actual observations.

```R
> sens <- dbpf_sensors(con)
> head(sens$label)
[1] "service_need"                "drainage_tendency_estimated" "LAI"                        
[4] "landscape_feature"           "slope_angle"                 "slope_aspect" 

```R
> obs <- dbpf_manual_obs_by_location(con, "NGO-DD-2010", "ice_visual_perc")
> obs[,c('height_max_metres', 'height_min_metres', 'numeric_value')]
   height_max_metres height_min_metres numeric_value
1               0.00             -0.30           NaN
2              -0.30             -0.48           NaN
3              -0.48             -0.64           NaN
4              -0.64             -1.20            55
5              -1.20             -1.22            30

```
#### View tables 
You can view the tables in the database with the `dbpf_tables` function. This can be useful if you want to build your own SQL queries or develop new functions.

```R
> head(dbpf_tables(con))
 [1] "imports"                "locations"              "sensors"                "logs"                  
 [5] "observations"           "device_sensor_profiles" "devices"               "devices_locations"
 [9] "devices_sensors"        "geography_columns"      "geometry_columns"      

```

table columns can then be viewed with the `dbpf_table_columns` function:

```R
> dbpf_table_columns(con, "locations")
 [1] "id"                  "name"                "coordinates"         "elevation_in_metres"
 [5] "comment"             "record_observations" "accuracy_in_metres" 
```

#### Import and Export
Without going into too much detail, you can import and export data using the `dbpf_*_add` and `dbpf_export_*` functions. 

The import functions take a dataframe and a connection object, and add the data to the database. Each import function has a `mode` parameter that can be set either to `test` or `insert`. In `test` mode, the data is not actually added to the database, but the function checks that no duplicate records exist. The `insert` mode will also perform these checks but will also add the data to the database.


```R
> con    <- dbpf_con()
> devices_locations <- data.frame(serialnumber="E50DBD",
                                  comment="example",
                                  sitename="field_site_1",
                                  time = lubridate::now())
> dbpf_devices_locations_add(con, devices_locations, mode="test")
  serialnumber comment     sitename                time   dev   loc   dup inserted                strtime
1       E50DBD example field_site_1 2024-02-14 18:47:07 FALSE FALSE FALSE    FALSE 2024-02-14 18:47:07+00
> 
```

The export functions take a connection object and a filename, and write the data to the file in a variety of different formats.

```R
dbpf_export_nc_erddap(con, 
                      location_name ="NGO-DD-2011",
                      file_name = "~/output/ngo-dd-2011.nc" )

dbpf_export_csv_GTNP(con, 
                     "NGO-DD-2011",
                     "C:/Users/Nick/Downloads",
                     freq='daily')
```
