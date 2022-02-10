# =============================================================================
#'
#' @title Plot horizon
#'
#' @description Plot horizon at a location
#' 
#' @details Make a plot
#'
#' @param con Database connection object, as returned by \code{\link{dbpf_con}}
#' 
#' @param location location name used to obtain location ID [character]
#' 
#' @export
#'                  
#' @author Nick Brown <nick.brown@@carleton.ca>
# =============================================================================
dbpf_horizon_plot <- function(con, location){
  df <- dbpf_horizon_get(con, location)
  par(mfrow=c(1,2))
  plot_horizon_1(df$azimuth, df$horizon)
  plot_horizon_2(df$azimuth, df$horizon)
}

# plotting parameters
gridcol = 'lightgrey'
gridstyle = 2
skycol = '#87ceeb'
terraincol = 'grey'


plot_horizon_1 <- function(az, hor){
  xlim <- c(-180, 180)
  ylim <- c(0, 90)
  xlab <- "Azimuth (\U00B0)"
  ylab <- "Horizon Angle (\U00B0)"
  plot(x=az, 
       y=hor, 
       xlim=xlim, 
       ylim=ylim, 
       xlab=xlab,
       ylab=ylab,
       type='l',
       yaxs='i',
       yaxt='n',
       xaxs='i',
       xaxt='n')
  xlabels = seq(-360, 360*2, 60) 
  ylabels = seq(0,90,10)
  axis(side=2, at=ylabels, labels=as.character(ylabels), las=2, tick=F)
  axis(side=1, at=xlabels, labels=as.character(xlabels%%360))
  axis(side=3, at=seq(-360, 360*2-90, 90), labels=rep(c("N", "E", "S", "W"),3), tick = F)
  
  rect(-360, 0, 2*360, 90, col=skycol)
  polygon(c(-360,az-360,az, az+360,2*360), c(0,hor,hor,hor,0), col=terraincol)
  lapply(seq(10,80,10), function(x) abline(h=x, lty=gridstyle, col=gridcol))
  lapply(seq(-180,360,90), function(x) abline(v=x, lty=gridstyle, col=gridcol))
}

plot_horizon_2 <- function(az, hor){
  coords <- horiz_to_carte(az, hor)
  par(pty="s")
  xlab = ""
  ylab = ""
  plot(coords$x,
       coords$y, 
       xlim=c(-1,1),
       ylim=c(-1,1),
       xlab=xlab,
       ylab=ylab,
       type='l',
       xaxt='n', 
       yaxt='n',
       bty='n')
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
       col = "white", bty='n', lty=0)
  bkg_coords <- horiz_to_carte(seq(0,360,1), rep(0,361))
  polygon(bkg_coords$x,
          bkg_coords$y,
          col=terraincol,
          bty='n')
  polygon(coords$x,
          coords$y,
          col=skycol,
          bty='n')
  spacing=15
  gridlines = c(seq(0, 180, spacing))
  if (90 %in% gridlines){
    abline(v=0, lty=gridstyle, col=gridcol)
    gridlines <- gridlines[gridlines != 90]
  }
  lapply(gridlines, function(x) abline(b=tan(2*pi*x/360),
                                        a=0, lty=gridstyle, col=gridcol))
  axis(side=1, at=0, labels="S", las=1, tick=F, font = 2)
  axis(side=2, at=0, labels="W", las=1, tick=F, font = 2)
  axis(side=3, at=0, labels="N", las=1, tick=F, font = 2)
  axis(side=4, at=0, labels="E", las=1, tick=F, font = 2)
  
}



horiz_to_carte <- function(azimuth, horizon){
  azimuth[horizon > 90] <- azimuth[horizon > 90] + 180
  horizon[horizon > 90] <- 180 - horizon[horizon > 90]
  azimuth <- azimuth %% 360
  r <- azimuth * 0 + 1  # assume unit radius
  theta <- 90 - azimuth 
  phi <- 90 - horizon
  coords <- sphr_to_carte(theta, phi, r)
  return(coords)  
}

sphr_to_carte <- function(theta, phi, r){
  theta = theta %% 360
 # if  all((0 <= phi) * (phi < 180)):
 #   raise ValueError("phi must be between 0 and 180 degrees")
  
  x = r*sin(radians(phi))*cos(radians(theta))
  y = r*sin(radians(phi))*sin(radians(theta))
  z = r*cos(radians(phi))
  coords = data.frame(x=x,y=y,z=z)
  return(coords)
}

radians <- function(degrees){
  degrees * 2 * pi /360
}


