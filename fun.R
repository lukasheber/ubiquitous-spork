# Definir funções; execute esse bloco de codigo sem modificar nada
mcwd.f <- function(x)
{
  x.temp = numeric(1)
  for(i in 1:12)
  {
    x.temp[i+1] = x[i]-100+x.temp[i]
    x.temp = ifelse(x.temp > 0,0,x.temp)
  }
  min(x.temp, na.rm=T)
}

mcwd.f2 = function(x){ #https://github.com/celsohlsj/RasterMCWD/blob/master/MCWD.R
  x <- x - 100
  result= as.numeric(x)
  for(i in 1:length(result)){
    wdn = result[i]
    wdn1 = result[i-1]
    
    if(i==1){
      if(wdn>0){ result[i]=0}
      else{result[i]=wdn}
    }
    
    if(i!=1){
      cwd = wdn1+wdn
      if( cwd < 0){ result[i]=cwd}
      else{result[i]=0}
    }
  }
min(result, na.rm=T)
}

mcwd.f3 <- function(x){
  data.table(tem = temp, grp = temp <= 100)[, sum(tem - 100), by=.(grp, rleid(grp))][,3] %>% min()
}

raster.TRMM <- function(x) {
  nc <- ncdf4::nc_open(x)
  nc.long.TRMM <- ncdf4::ncvar_get(nc,nc$dim[[1]])
  nc.lat.TRMM <- ncdf4::ncvar_get(nc,nc$dim[[2]])
  data <-ncdf4::ncvar_get(nc,'precipitation')
  data <-data[nrow(data):1,]
  
  TRMM <- raster::raster(x = as.matrix(data), xmn = nc.long.TRMM[1], xmx = nc.long.TRMM[NROW(nc.long.TRMM)], ymn = nc.lat.TRMM[1], ymx = nc.lat.TRMM[NROW(nc.lat.TRMM)], crs = sp::CRS('+proj=longlat +datum=WGS84'))
  return(TRMM)
}

stack.TRMM <- function(y) {
  raster.all <- map(y, ~map2((pull(.x, file.name)), (pull(.x, n.days)), function(.x, .y){raster.TRMM(.x)*24*.y}))
  stal <- map(raster.all, function(x){stack(x)})
  return(stal)
}

# Obter caminho dos arquivos de precipitação (.nc4) e escolher o periodo considerado. numero refere-se ao ultimo mês de um periodo de 12 meses; i.e. se oct-sept = 9; ou, jan-feb = 0 ou qualquer valor entre 0 e 12.
dir.TRMM <- function(inicio.periodo){
  f <- data.frame(file.name = list.files(pattern ="*.nc4", recursive = TRUE))
  f <- mutate(f, date = ymd(str_extract(file.name, "[0-9]{8}")))
  f$n.date <- f$date %m-% months(inicio.periodo)
  f$n.days <- days_in_month(f$date)
  n <- f %>% group_by(n.date = year(n.date)) %>% nest() %>% pull(n.date)
  f <-f %>% group_by(n.date = year(n.date)) %>% group_split()
  names(f) <- n
  return(f) 
}

delta.anom.mcwd <- function(mcwd.data = NULL, hh = c("delta", "anomalia"))
{
  if (hh == "delta") {
    g <- map(mcwd.data, function(x) {
      x - mean.mcwd
    })
  } else{
    g <- map(mcwd.data, function(x) {
      (x - mean.mcwd) / sd.mcwd
    })
  }
  return(g)
}