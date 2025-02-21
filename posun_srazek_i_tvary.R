library(tidyr)
library(tools)
library(dplyr)
library(ggplot2)
library(zoo)
library(gridExtra)
library(psych)
library(gdata)
library(tibble)
library(data.table)

#setwd("s:/_PROJEKTY/2019_StrazkyII/01_reseni_projektu/02_povodi/5km_povodi/_R")
setwd("d:/2_granty_projekty/2_Bezici/2020_PPZ_Zavlahy/01_reseni_projektu/09_malapovodi/srazky2022")
#setwd('/home/mha/Dropbox/hanel/projekty/2020 TACR Zavlahy/6h/')
#setwd('/home/mha/ownCloud/Shared/SC_malapovo/posuny_srazek/PKII/')

rainDTA = read.csv2(file = "srazky_malapovo2022.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".", sep = ",") 
#nacteni dat
#upov_all_data_45 = read.csv2(file = "upov_all_data_45.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
# data pro upovy, ale nevim proc jsme to cht?li scen?r 4.5
#upov_all_data_85 = read.csv2(file = "upov_all_data_85.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
# data pro upovy, ale nevim proc jsme to cht?li scen?r 8.5

pov_all_data_45 = read.csv2(file = "pov_all_data_45.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
#data pro mala povodi - ty nejvetsi
pov_all_data_45$KSC = 4.5
pov_all_data_85 = read.csv2(file = "pov_all_data_85.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
##data pro mala povodi - ty nejvetsi
pov_all_data_85$KSC = 8.5

pov_all_data = rbind (pov_all_data_45, pov_all_data_85)

#Import T_lag - ta je v jinym souboru, pro to takto
pov_stat = read.csv2(file = "nejvetsi_all.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".", sep =  ",", fileEncoding="latin1")
vyber = c("size_ID", "Tlag")
pov_stat = select(pov_stat, vyber)

xDTA = merge(pov_all_data, pov_stat, by = "size_ID")
xDTA = merge(xDTA, rainDTA, by = "size_ID")

nn = colnames(xDTA)
write.csv(nn, 'xDTAcolnames.txt')

selCL = as.character(read.csv(file = "sel_col.csv", header = FALSE, stringsAsFactors=FALSE, dec = ".", sep = ";"))
xDTA = select(xDTA, selCL)

renCL = as.character(read.csv(file = "sel_col_ren.csv", header = FALSE, stringsAsFactors=FALSE, dec = ".", sep = ";"))
colnames(xDTA) = renCL

#####vypocet CN
xDTA$A = 25.4*(1000/xDTA$CN_mean - 10)
xDTA$Ia = 0.2*xDTA$A

CNcomputeHe = function(A, Ia, Rainmm)
  {ifelse(Rainmm > Ia, ((Rainmm - Ia)^2)/(Rainmm - Ia + A), 0)
  
  }  
colnames(xDTA)
rain_sel = colnames(xDTA)[c(16:19,87:150)] #152
#name = paste("xHe00P", substr(rain_sel[2], 5, 20), sep = "_")
#xDTA[name] = CNcomputeHe(A = xDTA$A, Ia = xDTA$Ia,Rainmm = xDTA$nav_100yr_rok)


for (i in rain_sel) {
    print(i)
    name = paste("He00P", substr(i, 5, 20), sep = "_")
    #print (name)
    xDTA[name] = CNcomputeHe(A = xDTA$A, Ia = xDTA$Ia, Rainmm = xDTA[,i]) 
  }

#####TVARY#####

tvary = read.csv2(file = "tvary.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
tvary_cum = read.csv2(file = "tvary_cum.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")

Tp = as.data.frame(bDTA[,c("size_ID", "KSC", "Tlag")])
for (i in 1:72){
  a = i*5
  c = i +3
  Tp$c = a/2 + Tp$Tlag
  names(Tp)[ncol(Tp)] = a
}

Tp = Tp[,4:75]


hyetogram = function (data, Tp, Ia, A, rain, rainshape, area)
      {
      smaz = ncol(data)
      data2 = data
      nc=ncol(data) + 1
      a = 1
      for (i in rainshape){
        c = (a*5)
        mm = rain*i
        cHe = ifelse(mm > Ia, ((mm - Ia)^2)/(mm - Ia + A), 0)
        print(c)
        maxQ = (cHe*area)/(4.806*Tp[,a])
        #print (outdata[,nc])
        data = cbind(data, cHe)
        data2 = cbind(data2, maxQ)
        names(data)[ncol(data)] = c
        a = a+1
        nc = nc+1}
        he = data[,smaz:ncol(data)]
        maxQ = data2[,smaz:ncol(data2)]
        data1 = cbind(data, data2)
        return(data1)
      
      }

xx = c("Tlag", "Ia", "A", "nav_100yr_rok", "Shape_Area")
sel = bDTA[,xx]

zeby2 = hyetogram(data = sel, Tp = Tp, Ia = sel$Ia, A = sel$A, rain = sel$nav_100yr_rok, rainshape = tvary_cum$B/100, sel$Shape_Area)
zeby = zeby2[,6:77]

xx = as.numeric(colnames(Tp))
xxx = xx/60


tTP = Tp[1,]
tQ = zeby[1,]
a = NCOL(tTP)
b =  NCOL(tQ)

qMax = as.data.frame(matrix(nrow = b,ncol = a))
qMax <- qMax %>% replace(is.na(.), 0)
a = 1
for (i in 1:NCOL(tTP)){
  print(c(i,a))
  
  for (j in a:NCOL(tQ)){
    j = a
    t = j/12
    Tpi = as.numeric(tTP[j])
    Qmj = as.numeric(tQ[j])
    
    cc = (t/(Tpi*exp(1-t/Tpi))^3.7)*Qmj
    if (cc > 0) {print(c(i, j, a, cc))}
    qMax[i,j] = cc
    #print(c(a, i, j))
    
  }
  a = a + 1
}


qMax$sum = rowSums(qMax)

plot (qMax$sum)


cc = aa/(bb*exp(1-a/bb))^3.7
hydrogram = function(maxQ, he, Tp)
  {
  timemin = as.numeric(colnames(Tp))
  timeh = timemin/60
  
  }




ttt = as.data.frame(bDTA[,c("size_ID", "KSC")])
a = 1
for (i in tvary_cum$A){
    c = (a*5)
  outdata = sel$nav_100yr_rok*i
  ttt = cbind (ttt, outdata)
  names(ttt)[ncol(ttt)] = c
  a = a+1}

#####vypocetmaxQ
xDTA$Tp = xDTA$Tlag + 180/2
#he_sel = colnames(xDTA)[253:253] #319
for (i in 249:316) {
  print(i)
  cname = colnames(xDTA)[i]
  name = paste("q", substr(cname, 6, 20), sep = "")
  print (name)
  xDTA[,i+69] = xDTA[,i]*xDTA$Shape_Area/1000000/(4.806*xDTA$Tp)
  colnames(xDTA)[i+69] = name
  }

#### tady to ukladam
write.csv(xDTA, 'xDTA.csv')

# a pak nacitam
xDTA = read.csv2(file = "xDTA.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".", sep =  ",")


bDTA = xDTA

#vypocet objemu odtoku podle zastoupeni tvaru

rep = c("002", "003", "005", "010", "020", "030", "050", "100")

He_shp = as.data.frame(bDTA[,c("size_ID", "KSC",  )])
for (i in rep) {
  print (i)
  seln = names(bDTA)[grepl(i, names(bDTA))]
  shp = seln[grepl("shp", seln)]
  Hex = seln[grepl("He", seln)]
  for(x in shp){
    for (y in Hex){
      name = paste(x, y, sep = "_")
      He_shp[name] = bDTA[x]*bDTA[y]*0.01
      
      }
    
    }
  
  }


rname = c("pA_", "pB_", "pC_", "pD_", "pE_", "pF_")
seln = names(He_shp)[grepl("rok", names(He_shp))]
He_rok = select(He_shp, seln)
He_rokABCDEF = as.data.frame(bDTA[,c("size_ID", "KSC")])
for (i in rname) {
  print (i)
  seln = names(He_rok)[grepl(i, names(He_rok))]
  tmp = select(He_rok, seln)
  He_rokABCDEF = cbind.data.frame(He_rokABCDEF, tmp)
  
}

rname = c("002", "005", "010", "020", "050", "100")
for (i in rname) {
  print (i)
  seln = names(He_rokABCDEF)[grepl(i, names(He_rokABCDEF))]
  tmp = select(He_rokABCDEF, seln)
  xsum = paste("sum", i, sep="_")
  tmp[xsum] = rowSums(tmp)
  He_rokABCDEF = cbind.data.frame(He_rokABCDEF, tmp[xsum])
  
}


#vypocet max prutoku podle zastoupeni tvaru

q_shp = as.data.frame(bDTA[,c("size_ID", "KSC", "Tlag", "Tp")])
for (i in rep) {
  print (i)
  seln = names(bDTA)[grepl(i, names(bDTA))]
  shp = seln[grepl("shp", seln)]
  Qex = seln[grepl("q_", seln)]
  for(x in shp){
    for (y in Qex){
      name = paste(x, y, sep = "_")
      q_shp[name] = bDTA[x]*bDTA[y]*0.01
      
    }
    
  }
  
}

rname = c("pA_", "pB_", "pC_", "pD_", "pE_", "pF_")
seln = names(q_shp)[grepl("rok", names(q_shp))]
q_rok = select(q_shp, seln)
q_rokABCDEF = as.data.frame(bDTA[,c("size_ID", "KSC")])
for (i in rname) {
  print (i)
  seln = names(q_rok)[grepl(i, names(q_rok))]
  tmp = select(q_rok, seln)
  q_rokABCDEF = cbind.data.frame(q_rokABCDEF, tmp)
    
  }

rname = c("002", "005", "010", "020", "050", "100")
for (i in rname) {
  print (i)
  seln = names(q_rokABCDEF)[grepl(i, names(q_rokABCDEF))]
  tmp = select(q_rokABCDEF, seln)
  xsum = paste("sum", i, sep="_")
  tmp[xsum] = rowSums(tmp)
  q_rokABCDEF = cbind.data.frame(q_rokABCDEF, tmp[xsum])
  
}

q_rokABCDEF$sum =   

seln = names(He_shp)[grepl("rok", names(He_shp))]
selnb = grepl("100", seln)
seln = names(seln)[grepl("100", seln)]

##############
#CO JE DAL JE K NICEMU
##############

###45 - neum?m prom?nejma, tak je to pro 85 zvl᚝
ttemp = colnames(pov_all_data_45)
ttemp1 = ttemp[97:100]
ttemp2 = ttemp[129:156]
HeN = append(ttemp1, ttemp2)


pov_all_data_45T = merge(x = pov_all_data_45, y = pov_stat, by = "size_ID")
pov_all_data_45T$Tp = pov_all_data_45T$Tlag + 180/2

mtemp1 = pov_all_data_45T[,97:100]
mtemp2 = pov_all_data_45T[,129:156]

subHe = cbind(mtemp1, mtemp2)#, mtemp3)
ccopy = subHe
Q_He = cbind(subHe, ccopy)
NsubHe = colnames(Q_He)

for (i in 33:64) {
  qx = paste("Q", NsubHe[i], sep = "")
  print(qx)
  print(i)
  colnames(Q_He)[i] =  qx
  Q_He[,i] = Q_He[,i-32]*pov_all_data_45T$plocha/1000000/(4.806*pov_all_data_45T$Tp)
}

pov_all_data_45T = cbind(pov_all_data_45T,Q_He[,33:64])

#to same pro 85

pov_all_data_85T = merge(x = pov_all_data_85, y = pov_stat, by = "size_ID")
pov_all_data_85T$Tp = pov_all_data_85T$Tlag + 180/2

mtemp1 = pov_all_data_85T[,97:100]
mtemp2 = pov_all_data_85T[,129:156]

subHe = cbind(mtemp1, mtemp2)#, mtemp3)
ccopy = subHe
Q_He = cbind(subHe, ccopy)
NsubHe = colnames(Q_He)

for (i in 33:64) {
  qx = paste("Q", NsubHe[i], sep = "")
  print(qx)
  print(i)
  colnames(Q_He)[i] =  qx
  Q_He[,i] = Q_He[,(i-32)]*pov_all_data_85T$plocha/1000000/(4.806*pov_all_data_85T$Tp)
}

pov_all_data_85T = cbind(pov_all_data_85T,Q_He[,33:64])



pov_all_data_45 = data.table(pov_all_data_45T, RCP = 'rcp45')
pov_all_data_85 = data.table(pov_all_data_85T, RCP = 'rcp85')

# spojeny dataset - pro me vychozi bod
dta = rbind(pov_all_data_45, pov_all_data_85)
names(dta)

# prejmenuj srazky z kontrolniho obdobi dle konvence pouzite pro He a QHe
p0 = names(dta)[grepl('yr_6h', names(dta))]
setnames(dta, p0, gsub('P', 'PP_0P', p0) %>% gsub('h', 'H', .) %>% paste0(., '_0000') %>% gsub('_m', '', .) %>% gsub('20yr', '020yr', .) %>% gsub('10yr', '010yr', .) %>% gsub('2yr', '002yr', .))

# prejmenuj srazky z scen obdobi dle konvence pouzite pro He a QHe
p = names(dta)[grepl('dP|_MEAN_', names(dta))]
p = p[!grepl('He|QHe|odtok', p)]
setnames(dta, p, paste0('PP_', p)%>% gsub('20yr', '020yr', .) %>% gsub('10yr', '010yr', .) %>% gsub('2yr', '002yr', .)%>% gsub('Q10', 'Q010', .)%>% gsub('Q25', 'Q025', .)%>% gsub('Q5', 'Q005', .)%>% gsub('Q75', 'Q075', .)%>% gsub('Q90', 'Q090', .)%>% gsub('Q95', 'Q095', .))
