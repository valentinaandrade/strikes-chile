# Code: Spanish graphs ----------------------------------------------------
# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, ggplot2, lubridate, openxlsx)


# 2. Load data base -------------------------------------------------------
unzip(zipfile = "Input/dataverse_files.zip", exdir = "Input")
ohl<-readWorkbook("Input/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)


# 3. Setup -----------------------------------------------------------------

theme_set(theme_classic() + theme(text = element_text(size = 14),
                                  legend.position="bottom"))

options(scipen = 999)

# 4. Process --------------------------------------------------------------
ohl$ciuur2<-as.factor(ohl$ciuur2)
ohl$ciuur3<-as.factor(ohl$ciuur3)
ohl$leg<-as.factor(ohl$leg)

ohl<-ohl %>% mutate(sector=case_when(ciuur2==1 ~ "A Agriculture",
                                     ciuur2==2 ~ "B Mining",
                                     ciuur2==3 ~ "C Manufacturing industry",
                                     ciuur2==4 ~ "D-E Electricity, Water and Sanitary Services",
                                     ciuur2==5 ~ "F Construction",
                                     ciuur2==6 ~ "G-I Commerce",
                                     ciuur2==7 ~ "H-J Transportation and Communication",
                                     ciuur2==8 ~ "L-K Banks and Financial Services",
                                     ciuur2==9 ~  "O Central, Regional and Municipal Government",
                                     ciuur2==10 ~ "P Education (private, public and municipalized)",
                                     ciuur2==11 ~ "Q Health (private, public and municipalized)",
                                     ciuur2==12 ~ "Q Social and Personal Services",
                                     ciuur2==13 ~ "Unknown or Other Activities",
                                     ciuur2==14 ~ "National Strikes",
                                     is.na(ciuur2)&ciuur4==1 ~ "A Agriculture",
                                     is.na(ciuur2)&ciuur4==2  ~ "B Mining",
                                     is.na(ciuur2)&ciuur4==3  ~ "C Manufacturing industry",
                                     is.na(ciuur2)&ciuur4==4  ~ "D-E Electricity, Water and Sanitary Services",
                                     is.na(ciuur2)&ciuur4==5  ~ "D-E Electricity, Water and Sanitary Services",
                                     is.na(ciuur2)&ciuur4==6  ~ "F Construction",
                                     is.na(ciuur2)&ciuur4==7  ~ "G-I Commerce",
                                     is.na(ciuur2)&ciuur4==8  ~ "H-J Transportation and Communication",
                                     is.na(ciuur2)&ciuur4==9  ~ "G-I Commerce",
                                     is.na(ciuur2)&ciuur4==10 ~ "H-J Transportation and Communication",
                                     is.na(ciuur2)&ciuur4==11 ~ "L-K Banks and Financial Services",
                                     is.na(ciuur2)&ciuur4==12 ~ "L-K Banks and Financial Services",
                                     is.na(ciuur2)&ciuur4==13 ~ "M Professional, scientific and technical activities",
                                     is.na(ciuur2)&ciuur4==14 ~ "N Activities of administrative and support services",
                                     is.na(ciuur2)&ciuur4==15 ~ "O Central, Regional and Municipal Government",
                                     is.na(ciuur2)&ciuur4==16 ~ "P Education (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==17 ~ "Q Health (private, public and municipalized)",
                                     is.na(ciuur2)&ciuur4==18 ~ "R Artistic, entertainment and recreational activities",
                                     is.na(ciuur2)&ciuur4==19 ~ "Unknown or Other Activities",
                                     is.na(ciuur2)&ciuur4==20 ~ "Unknown or Other Activities",
                                     is.na(ciuur2)&ciuur4==21 ~ "Unknown or Other Activities"))

ohl %>% subset(is.na(ciuur2)) %>% select(yr,ciuur2,ciuur3,ciuur4,sector)


# worker-lostday ----------------------------------------------------------
ohl$dhtp <- as.numeric(ohl$dhtp)

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
  labs(title = "",
       x="",
       y = "",
       caption = "")  +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Extralegal"))

ggsave(plot = last_plot(),
       filename = "Output/graph/worker-lostday-tradicional-es-paper.png",
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
  labs(title = "",
       x="",
       y = "",
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

# Educacion --------------
# P
ohl %>% filter(!is.na(sector), sector %in% c("P Education (private, public and municipalized)")) %>% 
  group_by(sector,leg, yr) %>% 
  summarise(dhtp = sum(dhtp, na.rm = T)) %>% 
  ggplot(aes( x = yr, y =dhtp, fill = leg, color = leg)) +
  facet_wrap(~ sector) +
  geom_line(size = 1)  +
  labs(title = "",
       x="",
       y = "",
       caption = "")  +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Extralegal"))

ggsave(plot = last_plot(),
       filename = "Output/graph/worker-lostday-educ-en.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)
