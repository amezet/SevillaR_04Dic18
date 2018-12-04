##########################################################################
# Solucion aplicando Machine Learning: Problema Supervisado de Regresión
##########################################################################

#--------------------
# Librerias
#--------------------
library(data.table)
library(ggplot2)
library(xgboost)

#--------------------
# Configuracion
#--------------------
TRAIN_completo=F

#--------------------
# EDA básico
#--------------------

print(tr)
print(ts)

# La variable Customers está en TRAIN y no test: la eliminamos
tr[,Customers:=NULL]

#-------------------------
# Fusionamos Train y Test
#-------------------------

# Unimos Train y Test, y añadimos información de las tiendas
all <- rbindlist(list(tr,ts),use.names=T,fill=T)
all <- store[all,on=c('Store')]

str(all)
print(all)

# Fechas
all[,Date:=as.Date(Date)]

# ¿Cuantos dias tiene el train?¿Entre qué fechas?
all[!is.na(Sales),.(minFecha=min(Date),maxFecha=max(Date),dias=.N),by=Store][,.(tiendas=.N),by=.(minFecha,maxFecha,dias)]
# El TRAIN comprende del 2013-01-01 al 2015-07-31

# ¿Cuantos dias tiene el test?¿Entre qué fechas?
all[is.na(Sales),.(minFecha=min(Date),maxFecha=max(Date),dias=.N),by=Store][,.(tiendas=.N),by=.(minFecha,maxFecha,dias)]
# El test dura 48 dias del 2015-08-01 al 2015-09-17


#-----------------------------------------
# Esquema de validación / crossvalidación
#-----------------------------------------

# Validación temporal (la distribución del val similar al test, de 48 días, los 48 más recientes del TRAIN)
test_max_date = '2015-09-17'
test_min_date = '2015-08-01'

val_max_date = '2015-07-31' # test_min_date -  1 dias
val_min_date = '2015-06-14' # test_min_date - 48 dias

train_max_date = '2015-06-13' # val_min_date -  1 dias
train_min_date = '2013-01-01'

# Val con las mismas tiendas que test (856)

#--------------------
# Metrica evaluación
#--------------------

print(ggplot(tr[Sales>0], aes(x=Sales)) + geom_density())
print(ggplot(tr[Sales>0], aes(x=log(Sales+1))) + geom_density())

RMSPE_log<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMSPE", value = err))
}

RMSPE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-as.numeric(labels)
  epreds<-as.numeric(preds)
  err <- sqrt(mean((1-epreds/elab)^2))
  return(list(metric = "RMSPE", value = err))
}


#------------------------
# Ingeniería de variables
#------------------------

# Generación variables basicas de fecha
#--------------------------------------
all[,dia:=mday(Date)]
all[,sem:=week(Date)]
all[,mes:=month(Date)]
all[,anio:=year(Date)]
print(all)


# Variables serie temporal
#--------------------------------------
st <- all[,.(Date,Store,Sales,DayOfWeek,StateHoliday=ifelse(StateHoliday=='0',0,1),SchoolHoliday,Open,Promo)][order(Store,Date)]
print(st)



# --------------------------------------------------------
# Grupos DayOfWeek,StateHoliday,SchoolHoliday,Open,Promo
# --------------------------------------------------------
grupos <- na.omit(st[,.N,by=.(DayOfWeek,StateHoliday,SchoolHoliday,Open,Promo)])
st_grupo <- data.table()
var_grupos_names <- c('Store','Date',
                      'Mean_Sales_Grupo_DSSOP_lag',
                      'Median_Sales_Grupo_DSSOP_lag',
                      'Sd_Sales_Grupo_DSSOP_lag',
                      'Min_Sales_Grupo_DSSOP_lag',
                      'Max_Sales_Grupo_DSSOP_lag')

for(i in 1:nrow(grupos)){
  
  sta <- st[DayOfWeek==grupos[i,DayOfWeek] &
              StateHoliday==grupos[i,StateHoliday] &
              SchoolHoliday==grupos[i,SchoolHoliday] &
              Open==grupos[i,Open] &
              Promo==grupos[i,Promo],]
  
  tiendas_minimo_dos_casos <- sta[,.N,by=Store][N>=2,Store]
  
  if(length(tiendas_minimo_dos_casos)>=1){
    
    sta <- sta[Store%in%tiendas_minimo_dos_casos]
    
    sta <- sta[order(Store,Date)]
    
    varnames <- c()
    for(j in 1:20){
      varlag <- paste0('Sales_Grupo_DSSOP_lag_',j)
      varnames <- c(varnames, varlag)
      sta[,datelag:=shift(Date, n=j, type='lag'), by=.(Store)]
      sta[,(varlag):=ifelse( !is.na(datelag) & as.integer(Date-datelag)<49, NA, shift(Sales, n=j, type='lag')), by=.(Store)]
    }
    
    
    sta[,Mean_Sales_Grupo_DSSOP_lag:=rowMeans(.SD,na.rm=T),.SDcols=varnames]
    sta <- sta[!is.na(Mean_Sales_Grupo_DSSOP_lag)]
    sta[,Median_Sales_Grupo_DSSOP_lag:=apply(.SD, 1, median, na.rm=T),.SDcols=varnames]
    sta[,Sd_Sales_Grupo_DSSOP_lag:=apply(.SD, 1, sd, na.rm=T),.SDcols=varnames]
    sta[,Min_Sales_Grupo_DSSOP_lag:=apply(.SD, 1, min, na.rm=T),.SDcols=varnames]
    sta[,Max_Sales_Grupo_DSSOP_lag:=apply(.SD, 1, max, na.rm=T),.SDcols=varnames]
    
    st_grupo <- rbindlist(list(
      st_grupo,
      sta[,.SD,.SDcols=var_grupos_names]
    ),use.names = T, fill=T)
    st_grupo <- st_grupo[order(Store,Date)]
    
  }
}

all <- st_grupo[all,on=c('Store','Date')][order(Store,Date)]



# ------------------------------
# Grupos DayOfWeek,Open,Promo
# ------------------------------

grupos <- na.omit(st[,.N,by=.(DayOfWeek,Open,Promo)])
st_grupo <- data.table()
var_grupos_names <- c('Store','Date',
                      'Mean_Sales_Grupo_DOP_lag',
                      'Median_Sales_Grupo_DOP_lag',
                      'Sd_Sales_Grupo_DOP_lag',
                      'Min_Sales_Grupo_DOP_lag',
                      'Max_Sales_Grupo_DOP_lag')

for(i in 1:nrow(grupos)){
  
  sta <- st[DayOfWeek==grupos[i,DayOfWeek] &
              Open==grupos[i,Open] &
              Promo==grupos[i,Promo],]
  
  tiendas_minimo_dos_casos <- sta[,.N,by=Store][N>=2,Store]
  
  if(length(tiendas_minimo_dos_casos)>=1){
    
    sta <- sta[Store%in%tiendas_minimo_dos_casos]
    
    sta <- sta[order(Store,Date)]
    
    varnames <- c()
    for(j in 1:20){
      varlag <- paste0('Sales_Grupo_DOP_lag_',j)
      varnames <- c(varnames, varlag)
      sta[,datelag:=shift(Date, n=j, type='lag'), by=.(Store)]
      sta[,(varlag):=ifelse( !is.na(datelag) & as.integer(Date-datelag)<49, NA, shift(Sales, n=j, type='lag')), by=.(Store)]
    }
    
    sta[,Mean_Sales_Grupo_DOP_lag:=rowMeans(.SD,na.rm=T),.SDcols=varnames]
    sta <- sta[!is.na(Mean_Sales_Grupo_DOP_lag)]
    sta[,Median_Sales_Grupo_DOP_lag:=apply(.SD, 1, median, na.rm=T),.SDcols=varnames]
    sta[,Sd_Sales_Grupo_DOP_lag:=apply(.SD, 1, sd, na.rm=T),.SDcols=varnames]
    sta[,Min_Sales_Grupo_DOP_lag:=apply(.SD, 1, min, na.rm=T),.SDcols=varnames]
    sta[,Max_Sales_Grupo_DOP_lag:=apply(.SD, 1, max, na.rm=T),.SDcols=varnames]
    
    st_grupo <- rbindlist(list(
      st_grupo,
      sta[,.SD,.SDcols=var_grupos_names]
    ),use.names = T, fill=T)
    st_grupo <- st_grupo[order(Store,Date)]
    
  }
}

all <- st_grupo[all,on=c('Store','Date')][order(Store,Date)]


# Vistazo a curvas
# g<-ggplot(st[Store==920,.(Date,Sales)], aes(x=Date,y=Sales))
# g<-g+geom_line(colour='darkgreen')
# g<-g+geom_line(aes(y=Mean_Sales_Grupo_DSSOP_lag),colour='red')
# print(g)
# View(all[Store==2,.(Date,Sales,Mean_Sales_Grupo_DSSOP_lag)])

#---------------------------------------

# Rellenamos NA las fechas sin valor
fechas1 = sort(unique(st[Store%in%tiendas_tr_y_ts]$Date))
tiendas1 = sort(unique(st[Store%in%tiendas_tr_y_ts]$Store))
tiendas_fechas1 <- data.table(expand.grid(fechas1,tiendas1))

fechas2 = sort(unique(st[Store%in%tiendas_tr_y_no_ts]$Date))
tiendas2 = sort(unique(st[Store%in%tiendas_tr_y_no_ts]$Store))
tiendas_fechas2 <- data.table(expand.grid(fechas2,tiendas2))

tiendas_fechas <- rbindlist(list(tiendas_fechas1,tiendas_fechas2))

setnames(tiendas_fechas,colnames(tiendas_fechas),c('Date','Store'))
st <- st[tiendas_fechas,on=c('Date','Store')]
st <- st[order(Store,Date)]

# Variable ventas el mismo día de la semana: 7, 8, ..., 14 semanas atras

varnames <- c()
for(j in 7:14){
  varname <- paste0('Sales_DOW_lag_',j,'w')
  varnames <- c(varnames, varname)
  st[,(varname) := shift(Sales, n=7*j, type='lag'), by=.(Store)]
}

vars_4_DOW <- c('Sales_DOW_lag_7w','Sales_DOW_lag_8w','Sales_DOW_lag_9w','Sales_DOW_lag_10w')
st[,Sales_mean_4_DOW:=rowMeans(.SD,na.rm=T),.SDcols=vars_4_DOW]
st[,Sales_sd_4_DOW:=apply(.SD, 1, sd, na.rm=T),.SDcols=vars_4_DOW]
st[,Sales_min_4_DOW:=pmin(Sales_DOW_lag_7w,Sales_DOW_lag_8w,Sales_DOW_lag_9w,Sales_DOW_lag_10w)]
st[,Sales_max_4_DOW:=pmax(Sales_DOW_lag_7w,Sales_DOW_lag_8w,Sales_DOW_lag_9w,Sales_DOW_lag_10w)]


st[,Sales_mean_8_DOW:=rowMeans(.SD,na.rm=T),.SDcols=varnames]
st[,Sales_sd_8_DOW:=apply(.SD, 1, sd, na.rm=T),.SDcols=varnames]
st[,Sales_min_8_DOW:=pmin(Sales_DOW_lag_7w,Sales_DOW_lag_8w,Sales_DOW_lag_9w,Sales_DOW_lag_10w,Sales_DOW_lag_11w,Sales_DOW_lag_12w,Sales_DOW_lag_13w,Sales_DOW_lag_14w)]
st[,Sales_max_8_DOW:=pmax(Sales_DOW_lag_7w,Sales_DOW_lag_8w,Sales_DOW_lag_9w,Sales_DOW_lag_10w,Sales_DOW_lag_11w,Sales_DOW_lag_12w,Sales_DOW_lag_13w,Sales_DOW_lag_14w)]


# Variable días hasta/desde Navidad

st[,Navidad := as.Date(paste0(year(Date),'-12-25'))]
st[,NavidadAnterior := as.Date(paste0(year(Date)-1,'-12-25'))]
st[,NavidadProxima := as.Date(paste0(year(Date)+1,'-12-25'))]

st[,DiasParaNavidad:=ifelse(Date<=Navidad,Navidad-Date,NavidadProxima-Date)]
st[,DiasDesdeNavidad:=ifelse(Date<=Navidad,Date-NavidadAnterior,Date-Navidad)]

st[,Navidad := NULL]
st[,NavidadAnterior := NULL]
st[,NavidadProxima := NULL]



# Ventas últimas 4 semanas

varnames <- c()
for(j in 49:(49+7*4-1)){
  varname <- paste0('Sales_lag_',j,'w')
  varnames <- c(varnames, varname)
  st[,(varname) := shift(Sales, n=j, type='lag'), by=.(Store)]
}
st[,Sales_mean_1m:=rowMeans(.SD,na.rm=T),.SDcols=varnames]
st[,Sales_sd_1m:=apply(.SD, 1, sd, na.rm=T),.SDcols=varnames]
st[,Sales_min_1m:=apply(.SD, 1, min, na.rm=T),.SDcols=varnames]
st[,Sales_max_1m:=apply(.SD, 1, max, na.rm=T),.SDcols=varnames]

# Ventas últimas 8 semanas

varnames2 <- varnames
for(j in (49+7*4):(49+7*8-1)){
  varname <- paste0('Sales_lag_',j,'w')
  varnames2 <- c(varnames2, varname)
  st[,(varname) := shift(Sales, n=j, type='lag'), by=.(Store)]
}
st[,Sales_mean_2m:=rowMeans(.SD,na.rm=T),.SDcols=varnames2]
st[,Sales_sd_2m:=apply(.SD, 1, sd, na.rm=T),.SDcols=varnames2]
st[,Sales_min_2m:=apply(.SD, 1, min, na.rm=T),.SDcols=varnames2]
st[,Sales_max_2m:=apply(.SD, 1, max, na.rm=T),.SDcols=varnames2]
st<-st[,.SD,.SDcols=!varnames2]

all <- st[,.(Store,Date,
             DiasParaNavidad, DiasDesdeNavidad,
             Sales_mean_4_DOW,Sales_sd_4_DOW,Sales_min_4_DOW,Sales_max_4_DOW,
             Sales_mean_8_DOW,Sales_sd_8_DOW,Sales_min_8_DOW,Sales_max_8_DOW,
             Sales_DOW_lag_7w,Sales_DOW_lag_8w,Sales_DOW_lag_9w,Sales_DOW_lag_10w,Sales_DOW_lag_11w,Sales_DOW_lag_12w,Sales_DOW_lag_13w,Sales_DOW_lag_14w,
             Sales_mean_1m, Sales_sd_1m, Sales_min_1m, Sales_max_1m,
             Sales_mean_2m, Sales_sd_2m, Sales_min_2m, Sales_max_2m)][all,on=c('Store','Date')]

all[Sales_mean_2m>0,ratio_mean_1m_2m:=Sales_mean_1m/Sales_mean_2m]
all[,dif_mean_1m_2m:=Sales_mean_1m - Sales_mean_2m]
all[Sales_mean_2m>0,ratio_mean_8DOW_2m:=Sales_mean_8_DOW/Sales_mean_2m]
all[Sales_mean_2m>0,ratio_DOWlag7w_mean2m:=Sales_DOW_lag_7w/Sales_mean_2m]
all[Sales_mean_8_DOW>0,ratio_DOWlag7w_8DOW:=Sales_DOW_lag_7w/Sales_mean_8_DOW]




# Variables Categoricas

for(f in names(all)){
  if(class(all[[f]])=='character'){
    print(f)
    print(all[,.(casos=.N,mediaVentas=mean(Sales,na.rm=T),sdVentas=sd(Sales,na.rm=T)),by=.(dataset=ifelse(!is.na(Sales),'train','test'),var=get(f))][order(-dataset,var)])
  }
}

# Label encoding
all <- all[,.(meanSales=mean(Sales,na.rm=T)),by=.(StoreType)][order(meanSales)][,.(StoreType,StoreType_LE=.I)][all,on=c('StoreType')]
all <- all[,.(meanSales=mean(Sales,na.rm=T)),by=.(Assortment)][order(meanSales)][,.(Assortment,Assortment_LE=.I)][all,on=c('Assortment')]
all <- all[,.(meanSales=mean(Sales,na.rm=T)),by=.(StateHoliday)][order(meanSales)][,.(StateHoliday,StateHoliday_LE=.I)][all,on=c('StateHoliday')]
all[,StoreType:=NULL]
all[,Assortment:=NULL]
all[,StateHoliday_01:=ifelse(StateHoliday=='0',0,1)]
all[,StateHoliday:=NULL]


#------------------------
# Nos quedamos con aquellas observaciones para las que teníamos al menos 14 semanas previas
#------------------------
all <- all[Date>=as.Date(train_min_date)+7*14]



#------------------------
# Promo
#------------------------

print(unique(all$PromoInterval))
all[PromoInterval=="Jan,Apr,Jul,Oct" & mes%in%c(1,4,7,10),enPromo:=1]
all[PromoInterval=="Feb,May,Aug,Nov" & mes%in%c(2,5,8,11),enPromo:=1]
all[PromoInterval=="Mar,Jun,Sept,Dec" & mes%in%c(3,6,9,12),enPromo:=1]
all[is.na(enPromo),enPromo:=0]

# Meses que lleva la competencia abierta
all[,competitionOpen:= 12 * (anio - CompetitionOpenSinceYear) + (mes - CompetitionOpenSinceMonth)]
all[is.na(competitionOpen),competitionOpen:=0]

# Meses que lleva la promocion abierta
all[,promoOpen:= 12 * (anio - Promo2SinceYear) + (sem - Promo2SinceWeek) / 4]
all[is.na(Promo2SinceYear),promoOpen:=0]
all[promoOpen<0,promoOpen:=0]

    
# Label encoding PromoInterval
all[,.(meanSales=mean(Sales,na.rm=T)),by=.(PromoInterval)][order(meanSales)][,.(PromoInterval,PromoInterval_LE=.I)][all,on=c('PromoInterval')]
all[,PromoInterval:=NULL]

# Tiendas remodeladas
tiendas_no_remodeladas <- all[Date=='2014-07-01',Store]
all[,tienda_con_remodelacion:=ifelse(Store%in%tiendas_no_remodeladas,0,1)]
all[tienda_con_remodelacion==1 & Date<'2014-07-01',DiasParaRemodelacion:=as.integer(as.Date('2014-07-01')-Date)]
all[tienda_con_remodelacion==1 & Date>'2014-12-31',DiasDesdeRemodelacion:=as.integer(Date-as.Date('2014-12-31'))]


no_incluir <- c('CompetitionOpenSinceMonth','Promo2SinceWeek')
all <- all[,.SD,.SDcols=colnames(all)[!colnames(all) %in% no_incluir]]


#-------------------------------
# Datasets train, val, test
#-------------------------------

# Separamos TRAIN y test en all
TRAIN <- all[!is.na(Sales) & Sales>0] # Vamos a entrenar con aquellas tiendas con ventas
test <- all[is.na(Sales)]

# Dividimos TRAIN en train y val
train <- TRAIN[Date <  val_min_date]
# Val con las mismas tiendas que test (mimetizamos el test)
val   <- TRAIN[Date >= val_min_date & Store %in% tiendas_ts]


# Eliminamos campo Store
TRAIN[,Store:=NULL]
train[,Store:=NULL]
val[,Store:=NULL]
test[,Store:=NULL]

# Eliminamos campo fecha
TRAIN[,Date:=NULL]
train[,Date:=NULL]
val[,Date:=NULL]
test[,Date:=NULL]

# Guardamos Id_test
Id_test_open   = test[Open==1 | is.na(Open),Id]
Id_test_closed = test[Open==0,Id]

# Eliminamos Id
TRAIN[,Id:=NULL]
train[,Id:=NULL]
val[,Id:=NULL]
test[,Id:=NULL]

# Guardamos Sales
TRAINSales <- log(TRAIN$Sales+1)
trainSales <- log(train$Sales+1)
valSales <- log(val$Sales+1)

# Eliminamos campo Sales
TRAIN[,Sales:=NULL]
train[,Sales:=NULL]
val[,Sales:=NULL]
test[,Sales:=NULL]

# Garantizamos el mismo orden de columnas en train y test
setcolorder(test,colnames(train))

# Un vistazo
print(train)
print(val)
print(test)


#-------------------------------
# Modelo XGBoost
#-------------------------------

# DMatrix
dtrn <- xgb.DMatrix(data=as.matrix(train),label=trainSales, missing=NA)
dval <- xgb.DMatrix(data=as.matrix(val),label=valSales, missing=NA)

# Entrenamos modelo XGBoost
param <- list(booster = "gbtree"
              ,tree_method = 'hist'
              ,nthread = 4
              ,objective = 'reg:linear'
              ,eval_metric = RMSPE_log # 
              ,max_depth = 10 
              ,eta = 0.01 
              ,subsample = 0.8 
              ,colsample_bytree = 0.7 
)

iteraciones <- 5
best_iterations <- c()
for(i in 1:iteraciones){
  set.seed(i) # para que sea reproducible
  model <- xgb.train(data=dtrn
                     ,params = param
                     ,watchlist = list(train = dtrn, eval = dval)
                     ,nrounds = 4000
                     ,verbose = 1
                     ,early_stopping_rounds = 100
                     ,print_every_n = 20
                     ,maximize = FALSE)
  
  #print(xgb.importance(feature_names=colnames(train), model=model))
  
  best_iterations <- c(best_iterations, model$best_iteration)
  
  dtest <- xgb.DMatrix(data=as.matrix(test[Open==1 | is.na(Open)]), missing=NA)
  predSales <- predict(model, dtest, ntreelimit = model$best_iteration)
  predSales <- exp(as.numeric(predSales))-1
  
  submission3 = rbindlist(list(
    data.table(Id=Id_test_closed, Sales=0),
    data.table(Id=Id_test_open, Sales=predSales)
  ), use.names = T, fill = T)[order(Id)]
  
  filename <- paste0(output,'submission_2_',gsub(':','_',substr(Sys.time(),1,16)),'.csv')
  fwrite(x=submission3, file=filename, row.names=F, quote=FALSE,na=NA)
  
}

# RESULTADOS

# Stopping. Best iteration:
## [604]	train-RMSPE:0.140146	eval-RMSPE:0.122456
# 0.11296
# 0.11009
# Posicion 80/3303 (2,4%)

# submission_ensemble_bagging_5_seeds
# 0.11294
# 0.10983
# Puesto 80/3303 (2,4%)


###### Entrenamos con TRAIN #######

if(TRAIN_completo){
  
  dTRN <- xgb.DMatrix(data=as.matrix(TRAIN),label=TRAINSales, missing=NA)
  iteraciones <- 5
  
  for(i in 1:iteraciones){

    set.seed(i) # Nuevo seed para que sea reproducible
    model_TRAIN <- xgb.train(data=dTRN
                             ,params = param
                             ,nrounds = (round(best_iterations[i]*1.05, 0))
                             ,verbose = 1
                             ,print_every_n = 20
                             ,maximize = FALSE)
    
    predSales <- predict(model_TRAIN, dtest)
    predSales <- exp(as.numeric(predSales))-1
    
    submission4 = rbindlist(list(
      data.table(Id=Id_test_closed, Sales=0),
      data.table(Id=Id_test_open, Sales=predSales)
    ), use.names = T, fill = T)[order(Id)]
    
    filename <- paste0(output,'submission_2_TRAIN_',gsub(':','_',substr(Sys.time(),1,16)),'.csv')
    fwrite(x=submission4, file=filename, row.names=F, quote=FALSE,na=NA)
  }
}

# RESULTADOS
# Primera iteracion
# 0.11406
# 0.10793

# Ensemble 5 seeds


