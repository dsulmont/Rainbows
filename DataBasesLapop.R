
load("lapop_al.rdata")

library(car)

relig.dem <- recode(lapop_al$q3c, "1=1; 2=2; 3=3; 4=4; 5:6=2; 7=3;10=3; 12=2; 1501=3; 2701=3;
                    2702=3; 11=4")

relig.dem[lapop_al$wave < 2010 & lapop_al$q3 == 1] <- 1
relig.dem[lapop_al$wave < 2010 & lapop_al$q3 %in% c(2,5,6)] <- 2
relig.dem[lapop_al$wave < 2010 & lapop_al$q3 %in% c(3,7)] <- 3
relig.dem[lapop_al$wave < 2010 & lapop_al$q3 == 4] <- 4

relig.dem[lapop_al$q3_06 == 1] <- 1
relig.dem[lapop_al$q3_06 %in% c(2,5)] <- 2
relig.dem[lapop_al$q3_06 %in% c(3,7,6)] <- 3
relig.dem[lapop_al$q3_06 == 4] <- 4
relig.dem[lapop_al$q3_06 == 8] <- NA


table(relig.dem, lapop_al$wave)
table(relig.dem, lapop_al$pais, lapop_al$wave)

lapop_al$relig.dem <- relig.dem


library(haven)
Argentina_10 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Argentina_10.sav")
Argentina_12 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Argentina_12.sav")
Argentina_14 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Argentina_14.sav")
Argentina_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Argentina_17.dta")


Bolivia_10 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Bolivia_10.sav")
Bolivia_12 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Bolivia_12.sav")
Bolivia_14 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Bolivia_14.sav")
Bolivia_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Bolivia_17.dta")

Brazil_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Brazil_16.dta")

Chile_10 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Chile_10.sav")
Chile_12 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Chile_12.sav")
Chile_14 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Chile_14.sav")
Chile_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Chile_17.dta")

Colombia_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Colombia_16.dta")

CostaRica_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/CostaRica_16.dta")

DomRep_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/DomRep_16.dta")

Ecuador_14 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Ecuador_14.sav")
Ecuador_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Ecuador_16.dta")

ElSalvador_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/ElSalvador_17.dta")

Guatemala_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Guatemala_17.dta")

Honduras_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Honduras_16.dta")

Mexico_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Mexico_17.dta")

Nicaragua_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/NIcaragua_16.dta")

Panama_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Panama_16.dta")

Paraguay_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Paraguay_16.dta")

Peru_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Peru_17.dta")

Uruguay_17 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Uruguay_17.dta")

Venezuela_10 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Venezuela_10.sav")
Venezuela_12 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Venezuela_12.sav")
Venezuela_14 <- read_sav("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Venezuela_14.sav")
Venezuela_16 <- read_dta("~/Dropbox/Proyectos/WVS_Rainbows/Data Bases/Lapop/Paises/Venezuela_16.dta")


Argentina_10$ecivil <- Argentina_10$q11
Argentina_12$ecivil <- Argentina_12$q11
Argentina_14$ecivil <- Argentina_14$q11n
Argentina_17$ecivil <- Argentina_17$q11n

Bolivia_10$ecivil <- Bolivia_10$q11
Bolivia_12$ecivil <- Bolivia_12$q11
Bolivia_14$ecivil <- Bolivia_14$q11n
Bolivia_17$ecivil <- Bolivia_17$q11n

Brazil_16$ecivil <- Brazil_16$q11n

Chile_10$ecivil <- Chile_10$q11
Chile_12$ecivil <- Chile_12$q11
Chile_14$ecivil <- Chile_14$q11n
Chile_17$ecivil <- Chile_17$q11n

Colombia_16$ecivil <- Colombia_16$q11n

CostaRica_16$ecivil <- CostaRica_16$q11n

DomRep_16$ecivil <- DomRep_16$q11n

Ecuador_14$ecivil <- Ecuador_14$q11n
Ecuador_16$ecivil <- Ecuador_16$q11n

ElSalvador_17$ecivil <- ElSalvador_17$q11n

Guatemala_17$ecivil <- Guatemala_17$q11n

Honduras_16$ecivil <- Honduras_16$q11n

Mexico_17$ecivil <- Mexico_17$q11n

Nicaragua_16$ecivil <- Nicaragua_16$q11n

Panama_16$ecivil <- Panama_16$q11n

Paraguay_16$ecivil <- Paraguay_16$q11n

Peru_17$ecivil <- Peru_17$q11n

Uruguay_17$ecivil <- Uruguay_17$q11n

Venezuela_10$ecivil <- Venezuela_10$q11
Venezuela_12$ecivil <- Venezuela_12$q11
Venezuela_14$ecivil <- Venezuela_14$q11n
Venezuela_16$ecivil <- Venezuela_16$q11n

lapop_al$ecivil <- c()


lapop_al$ecivil <- lapop_al$q11
lapop_al$ecivil <- ifelse(lapop_al$year == 2014, lapop_al$q11n, lapop_al$ecivil)

table(lapop_al$ecivil, lapop_al$year)
table(lapop_al$q11n, lapop_al$year)
table(lapop_al$pais, lapop_al$year)

var.list.lap <- c("pais", "year", "q1", "q2", "ed","ecivil", "d5", "d6", "q5a", "q5b", "relig.dem")
var.list <- c("pais", "year", "q1", "q2", "ed","ecivil", "d5", "d6", "q5a", "q5b", "q3c")


lapop_al2 <- lapop_al[var.list.lap]

names(lapop_al2)

Argentina_14$year <- c(2014)
Argentina_14$q5a <- c(NA)
Argentina_17$year <- c(2017)

Argentina_10b <- Argentina_10[var.list]
Argentina_12b <- Argentina_12[var.list]
Argentina_14b <- Argentina_14[var.list]
Argentina_17b <- Argentina_17[var.list]

Bolivia_10$year <- c(2010)
Bolivia_14$year <- c(2014)
Bolivia_14$q5a <- c(NA)
Bolivia_17$year <- c(2017)

Bolivia_10b <- Bolivia_10[var.list]
Bolivia_12b <- Bolivia_12[var.list]
Bolivia_14b <- Bolivia_14[var.list]
Bolivia_17b <- Bolivia_17[var.list]

Brazil_16$year <- c(2017)
Brazil_16b <- Brazil_16[var.list]


Chile_10$year <- c(2010)
Chile_14$year <- c(2014)
Chile_14$q5a <- c(NA)
Chile_17$year <- c(2017)

Chile_10b <- Chile_10[var.list]
Chile_12b <- Chile_12[var.list]
Chile_14b <- Chile_14[var.list]
Chile_17b <- Chile_17[var.list]

Colombia_16$year <- c(2016)
Colombia_16b <- Colombia_16[var.list]

CostaRica_16$year <- c(2016)
CostaRica_16b <- CostaRica_16[var.list]

DomRep_16$year <- c(2016)
DomRep_16b <- DomRep_16[var.list]

Ecuador_14$year <- c(2014)
Ecuador_14$q5a <- c(NA)
Ecuador_16$year <- c(2016)

Ecuador_14b <- Ecuador_14[var.list]
Ecuador_16b <- Ecuador_16[var.list]

ElSalvador_17$year <- c(2016)
ElSalvador_17b <- ElSalvador_17[var.list]

Guatemala_17$year <- c(2017)
Guatemala_17b <- Guatemala_17[var.list]

Honduras_16$year <- c(2016)
Honduras_16b <- Honduras_16[var.list]

Mexico_17$year <- c(2017)
Mexico_17b <- Mexico_17[var.list]

Nicaragua_16$year <- c(2016)
Nicaragua_16b <- Nicaragua_16[var.list]

Panama_16$year <- c(2017)
Panama_16b <- Panama_16[var.list]

Paraguay_16$year <- c(2016)
Paraguay_16b <- Paraguay_16[var.list]

Peru_17$year <- c(2017)
Peru_17b <- Peru_17[var.list]

Uruguay_17$year <- c(2017)
Uruguay_17b <- Uruguay_17[var.list]

Venezuela_10$year <- c(2010)
Venezuela_14$year <- c(2014)
Venezuela_14$q5a <- c(NA)
Venezuela_16$year <- c(2016)

Venezuela_10b <- Venezuela_10[var.list]
Venezuela_12b <- Venezuela_12[var.list]
Venezuela_14b <- Venezuela_14[var.list]
Venezuela_16b <- Venezuela_16[var.list]

df.lapop.0 <- rbind(Argentina_10b, Argentina_12b, Argentina_14b, Argentina_17b,
                  Bolivia_10b, Bolivia_12b, Bolivia_14b, Bolivia_17b, Brazil_16b, Chile_10b,
                  Chile_12b, Chile_14b, Chile_17b, Colombia_16b, CostaRica_16b, DomRep_16b,
                  Ecuador_14b, Ecuador_16b, ElSalvador_17b, Guatemala_17b, Honduras_16b, 
                  Mexico_17b, Nicaragua_16b, Panama_16b, Peru_17b, Paraguay_16b, Uruguay_17b,
                  Venezuela_10b, Venezuela_12b, Venezuela_14b, Venezuela_16b)

names(df.lapop.0)

df.lapop.0$relig.dem <- recode(df.lapop.0$q3c, "1=1; 2=2; 3=3; 4=4; 5:6=2; 7=3;10=3; 12=2; 1501=3; 2701=3;
                    2702=3; 11=4; 77=3")

table(df.lapop.0$pais, df.lapop.0$q3c, df.lapop.0$year)

df.lapop.01 <- df.lapop.0[var.list.lap]


names(df.lapop.01)
names(lapop_al2)

df.lapop <- rbind(lapop_al2, df.lapop.01)

table(df.lapop$pais, df.lapop$year)

save(df.lapop, file = "Lapop_Rainbows.rdata")
