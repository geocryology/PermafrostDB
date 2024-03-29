% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbpf_devices_sensors_add.R
\name{dbpf_devices_sensors_add}
\alias{dbpf_devices_sensors_add}
\title{Adds new devices_sensors to database}
\usage{
dbpf_devices_sensors_add(con, dev_sen, mode = "test")
}
\arguments{
\item{con}{DB connection as returned by dbpf_con(). For insert priviledges
specific user name and password need to be supplied.}

\item{dev_sen}{Data frame with these columns (type):
oldSerial: logger being removed from thermistor chain
newSerial: logger being newly attatched to thermistor chain
comment
time (POSIXct, UTC)}

\item{mode}{Can be 'test' (default) or
'insert' (requires login with insert previledges)}
}
\value{
dev_sen dataframe with pass / fail result and message.
}
\description{
Inserts a data frame of new devices_sensors into database.
}
\details{
Devices_sensors indicate the time when a device was connected
         to a specific string of sensors. Until assigned to a new string of
         sensors, the device is then interpreted to be at that sensor.
         Insertion is done one row at a time and feedback is provided.
         The default mode (mode='test') only tests the data to be
         inserted. Insert mode (mode='insert') requires a database
         connection generated with a login that has insert priviledge.
         In Test Mode, duplicate rows in the input data are identified
         and sensors and devices are tested for existance in the DB.
         The input data frame is retuned with three new columns containing
         test results.
}
\examples{
\dontrun{
con    <- dbpf_con()
devices_sensors <- data.frame(oldSerial='E53128',
                                 newSerial='E53339',
                                 time=2021-08-19 17:20,
                                 comment="")
dbpf_devices_sensors_add(con, devices_sensors, mode="test")
dbDisconnect(con)
}
}
\author{
Hannah Macdonell <hannah.macdonell@carleton.ca>
}
