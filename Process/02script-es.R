# Code: Spanish graphs ----------------------------------------------------
# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, ggplot2, lubridate, openxlsx)


# 2. Load data base -------------------------------------------------------
unzip(zipfile = "Input/dataverse_files.zip", exdir = "Input")
ohl<-readWorkbook("Input/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)

# 3. Process --------------------------------------------------------------
ohl$ciuur2<-as.factor(ohl$ciuur2)
ohl$ciuur3<-as.factor(ohl$ciuur3)
ohl$leg<-as.factor(ohl$leg)

ohl<-ohl %>% mutate(sector=case_when(ciuur2==1 ~ "A Agricultura",
                                     ciuur2==2 ~ "B Minería",
                                     ciuur2==3 ~ "C Industria Manufacturera",
                                     ciuur2==4 ~ "D-E Electricidad, Agua y servicios sanitarios",
                                     ciuur2==5 ~ "F Construcción",
                                     ciuur2==6 ~ "G-I Comercio",
                                     ciuur2==7 ~ "H-J Transporte y telecomunicaciones",
                                     ciuur2==8 ~ "L-K Bancos y servicios financieros",
                                     ciuur2==9 ~  "O Gobierno central, regional y municipal",
                                     ciuur2==10 ~ "P Educación (privada, pública y municipalizada)",
                                     ciuur2==11 ~ "Q Salud (privada, pública y municipalizada)",
                                     ciuur2==12 ~ "Q Servicios sociales",
                                     ciuur2==13 ~ "Otras actividades",
                                     ciuur2==14 ~ "Huelgas Nacionales",
                                     is.na(ciuur2)&ciuur4==1 ~ "A Agricultura",
                                     is.na(ciuur2)&ciuur4==2  ~ "B Minería",
                                     is.na(ciuur2)&ciuur4==3  ~ "C Industria Manufacturera",
                                     is.na(ciuur2)&ciuur4==4  ~ "D-E Electricidad, Agua y servicios sanitarios",
                                     is.na(ciuur2)&ciuur4==5  ~ "D-E Electricidad, Agua y servicios sanitarios",
                                     is.na(ciuur2)&ciuur4==6  ~ "F Construcción",
                                     is.na(ciuur2)&ciuur4==7  ~ "G-I Comercio",
                                     is.na(ciuur2)&ciuur4==8  ~ "H-J Transporte y telecomunicaciones",
                                     is.na(ciuur2)&ciuur4==9  ~ "G-I Comercio",
                                     is.na(ciuur2)&ciuur4==10 ~ "H-J Transporte y telecomunicaciones",
                                     is.na(ciuur2)&ciuur4==11 ~ "L-K Bancos y servicios financieros",
                                     is.na(ciuur2)&ciuur4==12 ~ "L-K Bancos y servicios financieros",
                                     is.na(ciuur2)&ciuur4==13 ~ "M Profesionales, científicos y técnicos",
                                     is.na(ciuur2)&ciuur4==14 ~ "N Servicios administrativos",
                                     is.na(ciuur2)&ciuur4==15 ~ "O Gobierno central, regional y municipal",
                                     is.na(ciuur2)&ciuur4==16 ~ "P Educación (privada, pública y municipalizada)",
                                     is.na(ciuur2)&ciuur4==17 ~ "Q Salud (privada, pública y municipalizada)",
                                     is.na(ciuur2)&ciuur4==18 ~ "R Artísticas, entretenimiento y redcreativas",
                                     is.na(ciuur2)&ciuur4==19 ~ "Otras actividades",
                                     is.na(ciuur2)&ciuur4==20 ~ "Otras actividades",
                                     is.na(ciuur2)&ciuur4==21 ~ "Otras actividades"))

ohl %>% subset(is.na(ciuur2)) %>% select(yr,ciuur2,ciuur3,ciuur4,sector)


# worker-lostday ----------------------------------------------------------
theme_set(theme_classic() + theme(text = element_text(size = 14),
                                  legend.position="bottom"))

ohl$dhtp <- as.numeric(ohl$dhtp)
options(scipen = 999)

# Tradicional -----------
ohl %>% filter(!is.na(sector), sector %in% c("A Agricultura",         
                                             "B Minería",
                                             "C Industria Manufacturera",
                                             "D-E Electricidad, Agua y servicios sanitarios",
                                             "F Construcción",
                                             "H-J Transporrte y telecomuniciones")) %>% 
  group_by(sector,leg, yr) %>% 
  summarise(dhtp = sum(dhtp, na.rm = T)) %>% 
  ggplot(aes( x = yr, y =dhtp, fill = leg, color = leg)) +
  facet_wrap(~ sector) +
  geom_line(size = 1)  +
  labs(title = "Día-trabajador perdido por huelga por sector y legalidad (1980-2018)",
       x="Año",
       y = "Día-trabajador perdido por huelga",
       caption = "")  +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Extralegal"))

ggsave(plot = last_plot(),
       filename = "Output/graph/worker-lostday-tradicional-es.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Services --------------
# N y M sin n
ohl %>% filter(!is.na(sector), sector %in% c("G-I Comercio",         
                                             "L-K Bancos y servicios financieros",
                                             "Q Salud (privada, pública y municipalizada)",
                                             "Q Salud (privada, pública y municipalizada)")) %>% 
  group_by(sector,leg, yr) %>% 
  summarise(dhtp = sum(dhtp, na.rm = T)) %>% 
  ggplot(aes( x = yr, y =dhtp, fill = leg, color = leg)) +
  facet_wrap(~ sector) +
  geom_line(size = 1)  +
  labs(title = "Día-trabajador perdido por huelga por sector y legalidad (1980-2018)",
       x="Año",
       y = "Día-trabajador perdido por huelga",
       caption = "")  +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Extralegal"))

ggsave(plot = last_plot(),
       filename = "Output/graph/worker-lostday-services-es.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)
