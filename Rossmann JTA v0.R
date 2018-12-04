library(data.table)
library(ggplot2)

#####################
#  Config
#####################

input <- 'D:/Proyectos/ML/kaggle/Rossmann/input_original/'
output <- 'D:/Proyectos/ML/kaggle/Rossmann/output/'

#####################
#  Importamos
#####################

tr <- fread(paste0(input,'train.csv'))
ts <- fread(paste0(input,'test.csv'))
store <- fread(paste0(input,'store.csv'))

#####################
# EDA básico
#####################
str(tr)
str(ts)
str(store)

tr[,Date:=as.Date(Date)]
ts[,Date:=as.Date(Date)]

print(tr)
print(ts)
print(store)

summary(tr)
summary(ts)
summary(store)

# Registros por tienda
tr[,.(RegistrosPorTienda=.N,minDate=min(Date),maxDate=max(Date)),
   by=Store][,.(Tiendas=.N, minDate=unique(minDate), maxDate=unique(maxDate),
                dias=unique(maxDate)-unique(minDate)+1),by=RegistrosPorTienda]

ts[,.(RegistrosPorTienda=.N,minDate=min(Date),maxDate=max(Date)),
   by=Store][,.(Tiendas=.N, minDate=unique(minDate), maxDate=unique(maxDate),
                dias=unique(maxDate)-unique(minDate)+1),by=RegistrosPorTienda]

tiendas_tr <- unique(tr$Store)
tiendas_ts <- unique(ts$Store)
tiendas_tr_y_ts <- tiendas_tr[tiendas_tr%in%tiendas_ts]
tiendas_tr_y_no_ts <- tiendas_tr[!tiendas_tr%in%tiendas_ts]


# Visualizamos gap
ggplot(data=tr[,.N,by=Date][order(Date)], aes(x=Date,y=N))+geom_point()
tr[,.N,by=Date][order(Date)][N<1115]

tiendas_rehabilitadas = tr[,.N,by=Store][N==758,Store]


# Comprobamos que si la tienda esta cerrada, las ventas son 0
tr[Open==0,.N,by=Sales]

# Comprobamos que conocemos simepre si la tienda esta abierta Open=1 o cerrada Open=0
tr[is.na(Open),.N]
ts[is.na(Open),.N]
tiendasOpenNA=unique(ts[is.na(Open),Store])
ts[Store%in%tiendasOpenNA & is.na(Open)]
ts[!Store%in%tiendasOpenNA & Date>='2015-09-05' & Date<='2015-09-17',.N,by=.(Date,Open)]
ts[Store%in%tiendasOpenNA & is.na(Open),Open:=1]

#####################
# Solucion básica 1
#####################
# Si la tienda está cerrada, Sales = 0
# Si la tienda está abierta, Sales = MEDIA de las ventas de cada tienda por DayOfWeek

ventas_medias_tienda_diaSemana = tr[Open==1,.(Sales=mean(Sales)), by=.(Store,DayOfWeek)][order(Store,DayOfWeek)]

submission1 = rbindlist(list(
  ts[Open==0,.(Id,Sales=0)],
  ventas_medias_tienda_diaSemana[ ts[Open!=0,.(Id,Store,DayOfWeek)], on=c(Store='Store', DayOfWeek='DayOfWeek')][,.(Id,Sales)]
), use.names = T, fill=T)[order(Id)]

filename <- paste0(output,'submission_0_media_DOW_',gsub(':','_',substr(Sys.time(),1,16)),'.csv')
fwrite(x=submission1, file=filename, row.names=F, quote=FALSE,na=NA)

# Private_Score = 0.24104 (Public_Score = 0.18969), Posición = 2801/3303, Top 85%

# Compruebo si Public/Private LeaderBoard se ha dividido temporalmente
round(nrow(ts)*39/100,0)
ts[order(Date)][16024,Date]
Ids_primer_39 = ts[Date<="2015-08-19",Id]
submission1[!Id%in%Ids_primer_39,Sales:=100]
filename <- paste0(output,'submission_0a_probando_LB_',gsub(':','_',substr(Sys.time(),1,16)),'.csv')
fwrite(x=submission1, file=filename, row.names=F, quote=FALSE,na=NA)

# Private_Score = 0.98267 (Public_Score = 0.18969)



#####################
# Solucion básica 2
#####################
# Si la tienda está cerrada, Sales = 0
# Si la tienda está abierta, Sales = MEDIA de las ventas de cada tienda por DayOfWeek, Promo, StateHoliday, SchoolHoliday
# Si la tienda está abierta, Sales = MEDIA de las ventas de cada tienda por DayOfWeek (Para clusters test que no están en train)

clusters_tr = tr[,.(casos_tr=.N),by=.(Store, DayOfWeek, Promo, StateHoliday, SchoolHoliday)]
clusters_ts = ts[,.(casos_ts=.N),by=.(Store, DayOfWeek, Promo, StateHoliday, SchoolHoliday)]
# clusters en test y train
clusters_tr[clusters_ts, on=c('Store', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday')][!is.na(casos_tr)]
# clusters en test y no en train
clusters_tr[clusters_ts, on=c('Store', 'DayOfWeek', 'Promo', 'StateHoliday', 'SchoolHoliday')][is.na(casos_tr)]


ventas_medias_cluster = tr[Open==1,.(Sales=mean(Sales)), by=.(Store,DayOfWeek, Promo, StateHoliday, SchoolHoliday)][order(Store,DayOfWeek)]

submission2 = rbindlist(list(
  ts[Open==0,.(Id,Sales=0)],
  ventas_medias_cluster[ ts[Open!=0 | is.na(Open),.(Id,Store,DayOfWeek, Promo, StateHoliday, SchoolHoliday)],
                                  on=c(Store='Store', DayOfWeek='DayOfWeek', Promo='Promo', StateHoliday='StateHoliday', SchoolHoliday='SchoolHoliday')][,.(Id,Sales)]
), use.names = T, fill=T)[order(Id)]

# Añado las ventas de submission1
submission2 = submission1[,.(Id,Sales1=Sales)][submission2, on=c('Id')]

# En caso de que Sales==NA, me quedo con Sales1
submission2[is.na(Sales),Sales:=Sales1]
submission2[,Sales1:=NULL]

filename <- paste0(output,'submission_0b_',gsub(':','_',substr(Sys.time(),1,16)),'.csv')
fwrite(x=submission2, file=filename, row.names=F, quote=FALSE,na=NA)

# Private_Score = 0.16360 (Public_Score = 0.14937), Posición = 2477/3303, Top 75%


