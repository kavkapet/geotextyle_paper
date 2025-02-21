library(tidyr)
library(tools)
library(dplyr)
library(ggplot2)
library(zoo)
library(gridExtra)
library(psych)
library(gdata)
library(tibble)

#setwd("s:/_PROJEKTY/2019_StrazkyII/01_reseni_projektu/02_povodi/5km_povodi/_R")
setwd("d:/2_granty_projekty/2_Bezici/2020_PPZ_Zavlahy/01_reseni_projektu/09_malapovodi/")
#setwd('/home/mha/Dropbox/hanel/projekty/2020 TACR Zavlahy/6h/')
#setwd('/home/mha/ownCloud/Shared/SC_malapovo/posuny_srazek/PKII/')

require(data.table)

#nacteni dat
#upov_all_data_45 = read.csv2(file = "upov_all_data_45.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
# data pro upovy, ale nevim proc jsme to cht?li scen?r 4.5
#upov_all_data_85 = read.csv2(file = "upov_all_data_85.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
# data pro upovy, ale nevim proc jsme to cht?li scen?r 8.5

pov_all_data_45 = read.csv2(file = "pov_all_data_45.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
#data pro mala povodi - ty nejvetsi

pov_all_data_85 = read.csv2(file = "pov_all_data_85.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".")
##data pro mala povodi - ty nejvetsi

#Import T_lag - ta je v jinym souboru, pro to takto
pov_stat = read.csv2(file = "nejvetsi_all.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".", sep =  ",", fileEncoding="latin1")
vyber = c("size_ID", "Tlag")
pov_stat = select(pov_stat, vyber)

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

# vyber nazvy sloupcu s PP, He, QHe
p = names(dta)[grepl('PP_', names(dta))]
he = names(dta)[grepl('He|_MEAN_', names(dta))]
he = he[!grepl('QHe|pomer', he)]
qhe = names(dta)[grepl('QHe|_MEAN_', names(dta))]

# vyber nazvy ostatnich promennych, ktere se budou pak pouzivat - pokud se bude menit, je potreba zmenit i na radku DTA = cdta[, ] a cDTA = dcast(DTA ... a oso = dcast(oso) a oso = oso[, .(

vyb = c('size_ID', 'RCP', 'UPOV_ID')
vdta = dta[, c(vyb, p, he, qhe), with = FALSE]
# hod promenne ze sloucu do radku a rozkoduj nazvy 
mdta = melt(vdta, id.vars = vyb)
mdta[, c('var', 'per', 'TT', 'stat') := tstrsplit(variable, '_', keep = c(1, 2, 3, 5))]

# odtokovy soucinitel
oso = mdta[var %in% c('PP', 'He')]
oso[, variable:=NULL]
oso = dcast(oso, size_ID + RCP + UPOV_ID + per + TT + stat ~ var, value.var = 'value')
oso[, C := He/PP]
oso = oso[, .(size_ID, RCP, UPOV_ID, variable = paste('C', per, TT, '6H', stat, sep = '_'), value = C, var = 'C', per, TT, stat)]

# spoj, vyber kontrolni a scen obdobi, propoj a spocitej absolutni a relativni zmeny
mdta = rbind(mdta, oso)
cdta = mdta[per == '0P' & RCP == 'rcp45']
mdta1 = mdta[per != '0P']
setnames(cdta, 'value', 'cvalue')
DTA = cdta[, -c("per", "RCP", "variable", "stat"), with = FALSE][mdta, on = c('size_ID', 'UPOV_ID', 'var', 'TT'), allow = FALSE]
DTA[, a := value - cvalue]
DTA[, r := value / cvalue]
DTADTA[is.nan(r), r:=0]

# vyhod radky, pro ktere neni spoctena zmena srazek (asi nejaka mala povodi na hranicich, celkem 369 povodi)
DTA = DTA[value>=0]

# rozhod do sloupcu
cDTA = dcast(DTA[, .(size_ID, UPOV_ID, var, TT, RCP, value, stat, a, r)], size_ID + UPOV_ID + TT + RCP + stat ~ var, value.var = list('a', 'r'))
cDTA[, size := tstrsplit(size_ID, '_', keep = 1)]

cor(cDTA[stat=='MEAN' & is.finite(r_C * r_QHe * r_PP) & RCP=='rcp85' & TT == '002yr', 6:13], use = "pairwise.complete.obs")

# uloz
fwrite(cDTA, 'zmeny2.csv')
saveRDS(cDTA, 'zmeny.rds')
write_fst(cDTA, 'zmeny.fst', compress = 100)




require(ggplot2)

ggplot(cDTA[stat == 'MEAN' & size == '050']) + geom_point(aes(x = a_PP, y = a_He)) + facet_grid(TT ~ RCP, scale= 'free') + geom_abline(intercept=0, slope = 1) #+ coord_equal() + xlim(c(1, 6))+ylim(c(1, 6))


ggplot(cDTA[stat == 'MEAN' & size == '050']) + geom_density(aes(x = a_He, fill = RCP), alpha = .5) + facet_grid(TT~.)+xlim(c(1, 3))


data_all = read.csv2(file = "zmeny.csv", header = TRUE, stringsAsFactors=FALSE, dec = ".", sep = ",")


#data_all[(which(data_all$size_ID == "040_5404")),]
xxx = paste("MEAN", c("002", "005", "010", "020", "050", "100"),sep = "_")
xxx = append(xxx, "size_ID")
jenmean = pov_all_data_45[,xxx]
print(jenmean[(which(jenmean$size_ID == "040_5404")),])

data_all_sub = subset(data_all, RCP == "rcp45")# & stat == "MEAN")# & TT == "002yr")# & stat == "MEAN")
data_all_sub <- transform(data_all_sub, stat = ifelse(stat == "MEAN", "Q050", stat))

 # + geom_abline(intercept=0, slope = 1, color = "blue")

plot_pair = ggplot(data_all_sub,  aes(y = a_QHe, x = a_PP, colour = stat))
# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")

plot(plot_pair + geom_point())#+ facet_grid(  ~ size,  scale= 'free'))# + scale_y_continuous(limits = c(0.2, 8)))
plot(plot_pair + geom_point()+ facet_grid( TT ~ stat,  scale= 'free')) + geom_smooth(method=lm, se=FALSE, col='red', size=2)# + scale_y_continuous(limits = c(0.2, 8)))
plot(plot_pair + geom_point()+ facet_grid( TT ~ stat,  scale= 'free')) # + scale_y_continuous(limits = c(0.2, 8)))
# MH -  tyhle density grafy mi nějak nejdou
plot(plot_pair + stat_density_2d(aes(fill = ..level.., alpha = ..level.., bins = 15))) + facet_grid( TT ~ stat,  scale= 'free')


all_plot = c("a_PP", "a_He", "a_QHe", "a_C", "r_PP", "r_He", "r_QHe", "r_C")
#all_plot = c("r_He")

# na velikosti povodi nezalezi
plot_4a = ggplot(data_all_sub,  aes(y = a_He, x = a_PP, colour = size)) + geom_violin() + facet_grid(  ~ size,  scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_4a)# + scale_y_continuous(limits = c(0.2, 8)))

plot_4r = ggplot(data_all_sub,  aes(y = r_He, x = r_PP)) + geom_violin () + facet_grid( stat ~ size,  scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_4r) +  scale_y_continuous(limits = c(0, 10))



 + stat_density_2d(aes(fill = ..level.., alpha = ..level.., bins = 15))# + 
scale_x_continuous(limits = c(0, 25)) + scale_y_continuous(limits = c(0, 10))# + facet_grid( stat ~ TT,  scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_4)


plot_4 = ggplot(data_all_sub,  aes(y = a_He, x = a_PP, colour = TT)) + stat_density_2d(aes(fill = ..level.., geom = "raster"))# + 
#scale_x_continuous(limits = c(0, 25)) + scale_y_continuous(limits = c(0, 10))# + facet_grid( stat ~ TT,  scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_4 + facet_grid(stat ~ TT, scale = "free"))




xcochci = data_all_sub[,"r_He"]
ycochci = data_all_sub[,"a_PP"]

j =  1
for (i in all_plot){
  
  
  
  print(i)
  ycochci = data_all_sub[,i]
  pov_stat = (ggplot (data = data_all_sub, mapping = aes(y = ycochci, colour = stat))
              #+ geom_point(colour = "#CCCCCC", size = 1.0 )
              + geom_boxplot()
              #+ facet_grid()
              
              #, scales = "free_y")
              #+ geom_smooth(show.legend = TRUE, se = TRUE)
              +ylab(i) + scale_y_continuous(limits = c(0, 10))
              
              
  )
  j = j+1
  
  plot(pov_stat)
  ggsave(paste(i,"ylim10.png"))
}





j =  1
for (i in all_plot){
  
  
  
  print(i)
  ycochci = data_all_sub[,i]
  xcochci = data_all_sub[,"a_PP"]
  
  pov_stat = (ggplot (data = data_all_sub, mapping = aes(x = xcochci, y = ycochci, colour = stat))
              #+ geom_point(colour = "#CCCCCC", size = 1.0 )
              + geom_boxplot()
              #+ facet_grid()
              
              #, scales = "free_y")
              #+ geom_smooth(show.legend = TRUE, se = TRUE)
              +ylab(i)  + xlab("a_PP")
              
              
  )
  j = j+1
  
  plot(pov_stat)
  ggsave(paste(i,"ylim10.png"))
}






plot_4 = ggplot(data_all_sub,  aes(y = a_He, x = a_PP, colour = TT)) + stat_density_2d(aes(fill = ..level.., alpha = ..level.., bins = 15))# + 
  scale_x_continuous(limits = c(0, 25)) + scale_y_continuous(limits = c(0, 10))# + facet_grid( stat ~ TT,  scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_4)


plot_5 = ggplot(data_all_sub,  aes(y = a_C, x = a_PP, colour = TT)) + stat_boxplot() + facet_grid( ~ TT)# + geom_abline(intercept=0, slope = 1, color = "red") #+ geom_density(aes(x = a_PP, fill = TT))facet_grid( TT ~ stat, scale= 'free')# + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_5)


plot_geom2 = ggplot(data_all_sub) + stat_density_2d_filled(aes(y = r_He, x = r_PP, fill = ..level..))# + facet_grid(TT ~ stat, scale= 'free') + geom_abline(intercept=0, slope = 1, color = "blue")
plot(plot_geom2)

###cor(dta[stat=='MEAN' & is.finite(r_C * r_QHe * r_PP) & RCP=='rcp85' & TT == '002yr', 6:13], use = "pairwise.complete.obs")

#dopocteni relativni zmeny odtoku_ ku srazce

dTA = readRDS('zmeny.rds')
dTA[, dPQ := a_He/a_PP]
#vsechno do radku
ddTA =  dcast(dTA, size_ID ~ RCP + TT + stat, value.var = c("a_C", "a_He", "a_PP", "a_QHe", "r_C", "r_He", "r_PP", "r_QHe", "dPQ"))

#vyber jen zmen
sdTA =  dcast(dTA, size_ID ~ RCP + TT + stat, value.var = c("a_He", "a_PP", "a_QHe","dPQ"))

melt.data.table()

saveRDS(sdTA, 'zmeny-dPQ.rds')
fwrite(sdTA, 'zmeny-dPQ.csv', sep = ';')
probDTAdc =  dcast(xxx, strid_pixel ~ repet , value.var = c("A", "B", "C", "D", "E", "F"))


xxx = read.csv('zmeny-dPQ.csv')

