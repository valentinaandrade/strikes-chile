#----1. Paquetes y base de datos----
library(dplyr)
library(car)
library(summarytools)
library(ggplot2)
library(magrittr)
library(tidyverse) 

library(ggplot2)
library(lubridate)
library(ggpubr)
library(openxlsx)

unzip(zipfile = "Input/dataverse_files.zip", exdir = "Input")

ohl<-readWorkbook("Input/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)


## Procesamiento
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



#### Numero de huelgas en general

ohl %>% subset(inicio>"2010-01-01") %>% group_by(yr,mes) %>% tally() %>% mutate(t=paste(yr,"-",mes,"-","01",sep = ""),t=ymd(t)) %>% 
  ggplot(aes( x = t, y =n)) +
  geom_line() + theme_bw()

ohl %>% group_by(yr) %>% tally() %>% 
  ggplot(aes( x = yr, y =n)) +
  geom_line() + theme_bw()


ohl %>% group_by(yr,sector,leg) %>% subset(!is.na(sector)) %>% tally() %>% 
  ggplot(aes( x = yr, y =n, fill = leg, color = leg)) + facet_wrap(~sector) +
  geom_line() + theme_bw() + 
  labs(title = "Número de huelgas en Chile (1980-2018) según sector económico y legalidad",
       x="Tiempo",
       y = "Número de huelgas",
       caption = "Fuente: Elaboración propia en base a OHL (2010-2018)") + theme(legend.position="bottom") +
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="#e42829","2"="#0c4b8f"),
                     labels = c("Legal","Extralegal"))
  
ggsave(
  plot = last_plot(),
  filename = "../Output/graph/Número de huelgas en Chile (1980-2018) según sector económico y legalidad.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)


## Número de trabajadores
table(ohl$sector)
ohl$leg<-as.factor(ohl$leg)
ohl$tc<-as.numeric(ohl$tc)
ohl %>% group_by(yr,sector,leg) %>% subset(!is.na(sector)) %>% summarise(tc=sum(tc,na.rm = TRUE)) %>% 
  subset(sector %in% c("C Manufacturing industry",
                       "D-E Electricity, Water and Sanitary Services","F Construction",
                       "G-I Commerce","L-K Banks and Financial Services")) %>% 
  ggplot(aes( x = yr, y =tc, fill = leg, color = leg)) + facet_wrap(~sector) +
  geom_line() + theme_bw() + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(title = "Número de trabajadores en huelgas en Chile (1980-2018) según sector económico y legalidad",
       subtitle = "Solo manufactura, electricidad, comercio y banca",
       x="Tiempo",
       y = "Número de trabajadores",
       caption = "Fuente: Elaboración propia en base a OHL (2010-2018)") + theme(legend.position="bottom") +
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="#e42829","2"="#0c4b8f"),
                     labels = c("Legal","Extralegal"))

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/Número de trabajadores en huelgas en Chile (1980-2018). Solo manufactura, electricidad, comercio y banca.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)

table(ohl$sector)
ohl %>% group_by(yr,sector,leg) %>% subset(!is.na(sector)) %>% summarise(tc=sum(tc,na.rm = TRUE)) %>% 
  subset(sector %in% c("A Agriculture",         
                       "B Mining",
                       "P Education (private, public and municipalized)",
                       "Q Health (private, public and municipalized)",
                       "Q Social and Personal Services")) %>% 
  ggplot(aes( x = yr, y =tc, fill = leg, color = leg)) + facet_wrap(~sector) +
  geom_line() + theme_bw() + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(title = "Número de trabajadores en huelgas en Chile (1980-2018) según sector económico y legalidad",
       subtitle = "Solo agricultura, minería, educación, salud y servicios sociales",
       x="Tiempo",
       y = "Número de trabajadores",
       caption = "Fuente: Elaboración propia en base a OHL (2010-2018)") + theme(legend.position="bottom") +
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="#e42829","2"="#0c4b8f"),
                     labels = c("Legal","Extralegal"))

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/Número de trabajadores en huelgas en Chile (1980-2018). Solo agricultura, minería, sector público, educación, salud y servicios sociales.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)


table(ohl$sector)
ohl %>% group_by(yr,sector,leg) %>% subset(!is.na(sector)) %>% summarise(tc=sum(tc,na.rm = TRUE)) %>% 
  subset(sector %in% c("O Central, Regional and Municipal Government",
                       "National Strikes")) %>% 
  ggplot(aes( x = yr, y =tc, fill = leg, color = leg)) + facet_wrap(~sector) +
  geom_line() + theme_bw() + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  labs(title = "Número de trabajadores en huelgas en Chile (1980-2018) según sector económico y legalidad",
       subtitle = "Nacionales y sector público",
       x="Tiempo",
       y = "Número de trabajadores",
       caption = "Fuente: Elaboración propia en base a OHL (2010-2018)") + theme(legend.position="bottom") +
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="#e42829","2"="#0c4b8f"),
                     labels = c("Legal","Extralegal"))

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/Número de trabajadores en huelgas en Chile (1980-2018). Nacionales y sector público.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)


## comercio y manuf NT
g1<-ohl %>% subset(sector%in% c("C Manufacturing industry","G-I Commerce")) %>% group_by(yr,sector,leg)  %>% 
  summarise(tc=sum(tc,na.rm = TRUE)) %>% 
  ggplot(aes( x = yr, y =tc, fill = leg, color = leg)) + facet_wrap(~sector) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+
  geom_line() + theme_bw() + 
  labs(title = "Número de trabajadores",
       x="Tiempo",
       y = "Número de trabajadores") +
  theme(legend.position="bottom") + 
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="purple1","2"="red1"),
                     labels = c("Legal","Extralegal")) 

## comercio y manuf Tally
g2<-ohl %>% subset(sector%in% c("C Manufacturing industry","G-I Commerce")) %>% group_by(yr,sector,leg)  %>% 
  tally() %>% 
  ggplot(aes( x = yr, y =n, fill = leg, color = leg)) + facet_wrap(~sector) +
  geom_line() + theme_bw() + 
  labs(title = "Número de huelgas",
       x="Tiempo",
       y = "Número de huelgas")  +
  theme(legend.position="bottom") + 
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="purple1","2"="red1"),
                     labels = c("Legal","Extralegal"))

g3<-ggarrange(g1,g2)

annotate_figure(g3, top=text_grob("Número de huelgas y de trabajadores comprometidos en estas (1980-2018) en comercio e industria manufacturera"),
                bottom = text_grob("Fuente: OHL (2010-2018)", color = "blue",
                                       hjust = 1, x = 1, face = "italic", size = 10))

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/Número de trabajadores y de huelgas en Chile (1980-2018). Comercio y manufactura.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 15
)



### tácticas en comercio

## Se recarga la base como texto

a<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(tactica1) %>% tally() %>% rename(tactica=tactica1,t1=n)
b<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(tactica2) %>% tally() %>% rename(tactica=tactica2,t2=n)
c<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(tactica3) %>% tally() %>% rename(tactica=tactica3,t3=n)
d<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(tactica4) %>% tally() %>% rename(tactica=tactica4,t4=n)
e<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(tactica5) %>% tally() %>% rename(tactica=tactica5,t5=n)

a<-Reduce(function(...) merge(..., all=T), list(a,b,c,d,e))
a[is.na(a)] = 0

a<-a %>% mutate(t2=case_when(tactica==0 ~ 0, TRUE ~ t2),
                t3=case_when(tactica==0 ~ 0, TRUE ~ t3),
                t4=case_when(tactica==0 ~ 0, TRUE ~ t4),
                t5=case_when(tactica==0 ~ 0, TRUE ~ t5))

a<-a %>% mutate(t=t1+t2+t3+t4+t5)

a<-a %>% mutate(tactica=case_when(tactica==1 ~ "March.",
                                  tactica==2 ~ "Demonstration or mobilization in a place other than the company.",
                                  tactica==3 ~ "Caravan (cars in a row).",
                                  tactica==4 ~ "Picketing or peaceful demonstration with posters outside the company or institution.",
                                  tactica==5 ~ "Vigil.",
                                  tactica==6 ~ "Weapons collection.",
                                  tactica==7 ~ "Money collection",
                                  tactica==8 ~ "Act with speakers, possibly with amplification equipment and platforms or scenarios.",
                                  tactica==9 ~ "Chorus, chants, slogans uttered by protesters.",
                                  tactica==10~ "Called not to buy certain products or services, not pay debts, not abide laws, etc.",
                                  tactica==11~ "Press conference, public statement or letters to authorities.",
                                  tactica==12~ "Assembly, discussion or open and collective debate among the participants.",
                                  tactica==13~ "Prayers, sermons, display of religious symbols.",
                                  tactica==14~ "Remembrance or homage to dates, events, groups or individuals .",
                                  tactica==15~ "Artistic performance on behalf of the protesters themselves.",
                                  tactica==16~ "Recreational and/or artistic activity.",
                                  tactica==17~ "Non-artistic symbolic activities that require prior coordination between several participants.",
                                  tactica==18~ "Massive Resignation.",
                                  tactica==19~ "Occupation of buildings outside the company.",
                                  tactica==20~ "Occupation of public spaces with tents or camps.",
                                  tactica==21~ "Cuts or routes, streets, roads, ports, bridges or access blocks.",
                                  tactica==22~ "Interruption of meetings of political authorities or other elites.",
                                  tactica==23~ "Dangerous or self-destructive acts as a way of attracting attention.",
                                  tactica==24~ "Hunger strikes.",
                                  tactica==25~ "Set fire to vehicles and buildings.",
                                  tactica==26~ "Destruction of public property (signage, traffic lights) or private (excluding pillage).",
                                  tactica==27~ "Use of explosive devices, Molotov cocktails, firearms, etc.",
                                  tactica==28~ "Assaults between protesters.",
                                  tactica==29~ "Attacks to the police, security guards or armed forces.",
                                  tactica==30~ "Looting of furniture or property owned by the company.",
                                  tactica==31~ "Attack on anti-protesters.",
                                  tactica==32~ "Attack to bystanders or third parties not involved.",
                                  tactica==33~ "Taking of hostages or kidnappings.",
                                  tactica==34~ "Judicial tactics (complaint in Labor Inspection, go to court).",
                                  tactica==35~ "Ethical shifts.",
                                  tactica==36~ "Silence or concerted noises and concentrated in a short time.",
                                  tactica==37~ "External pickets.",
                                  tactica==38~ "Main, immediate or direct boycott.",
                                  tactica==39~ "Sit-down strike, white strike or unwilling work.",
                                  tactica==40~ "Threat strike.",
                                  tactica==41~ "Occupation or takeover of production means of the company.",
                                  tactica==42~ "Traditional, indefinite or fixed time strike.",
                                  tactica==43~ "Illegal work stoppage in the framework of a legal collective bargaining.",
                                  tactica==44~ "Smart strike (for teachers’ strikes, for example, doing classes only the first hours of each day).",
                                  tactica==45~ "Soup-kitchen.",
                                  tactica==46~ "Begin strike before the legal period.",
                                  tactica==47~ "Audiovisual propaganda.",
                                  tactica==48~ "Solidarity tactics.",
                                  tactica==49~ "Delays at the start of the day with protest reasons.",
                                  TRUE ~ "no tactic mentioned"))

a %>% ggplot(aes(x=reorder(tactica,t), y=t)) +
  geom_bar(stat="identity", fill="#3399FF", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(t, big.mark = ",", scientific = FALSE)), hjust=-0.5, colour = "black", size=3.0) +
  labs(title="Tácticas utilizadas en las huelgas del comercio desde el año 2010 al 2018",
       x="Tácticas", 
       y = "Huelgas")

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/tácticas en comercio.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 15
)


### demandas

a<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(dem1) %>% tally() %>% rename(demanda=dem1,d1=n)
b<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(dem2) %>% tally() %>% rename(demanda=dem2,d2=n)
c<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(dem3) %>% tally() %>% rename(demanda=dem3,d3=n)
d<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(dem4) %>% tally() %>% rename(demanda=dem4,d4=n)
e<-ohl %>% subset(sector%in% c("G-I Commerce") & yr>=2010) %>% group_by(dem5) %>% tally() %>% rename(demanda=dem5,d5=n)

a<-Reduce(function(...) merge(..., all=T), list(a,b,c,d,e))
a[is.na(a)] = 0

a<-a %>% mutate(d2=case_when(demanda==0 ~ 0, TRUE ~ d2),
                d3=case_when(demanda==0 ~ 0, TRUE ~ d3),
                d4=case_when(demanda==0 ~ 0, TRUE ~ d4),
                d5=case_when(demanda==0 ~ 0, TRUE ~ d5))

a<-a %>% mutate(d=d1+d2+d3+d4+d5)

a$demanda<-as.character(a$demanda)
a<-a %>% mutate(demanda=case_when(demanda==1  ~ "Related to remuneration.",
                               demanda==2  ~ "Dismissals and transfers.",
                               demanda==3  ~ "Solidarity with peers of one's own or another company and/or guilds.",
                               demanda==4  ~ "Solidarity with superiors or the company.",
                               demanda==5  ~ "General conditions of work, hygiene and safety.",
                               demanda==6  ~ "Working day.",
                               demanda==7  ~ "Protest for the management of the company and/or for the way of organizing production",
                               demanda==8  ~ "Protest for the infrastructure and/or lack of supplies /personnel.",
                               demanda==9  ~ "Anti-union practices.",
                               demanda==10 ~ "Bad treatment of superiors and/or dismissal of superiors.",
                               demanda==11 ~ "Failure to comply with agreements.",
                               demanda==12 ~ "Concerning negotiation procedures.",
                               demanda==13 ~ "Protest against laws or bills.",
                               demanda==14 ~ "Reasons unrelated to the company.",
                               demanda==15 ~ "Support to national, regional, provincial, and communal authorities.",
                               demanda==16 ~ "Protest for change in dependence or ownership of the company or institution.",
                               demanda==17 ~ "Homologation of salary and/or labor conditions of outsourced workers with respect to those permanent." ,
                               demanda==18 ~ "Homologation of conditions and/or work by gender or between colleagues.",
                               demanda==19 ~ "Protest for the performance of union leaders and/or union administration.",
                               demanda==20 ~ "Other causes.",
                               demanda==21 ~ "Contract related to salary structure.",
                               TRUE ~ "no demand mentioned"))


a %>% ggplot(aes(x=reorder(demanda,d), y=d)) +
  geom_bar(stat="identity", fill="#3399FF", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(d, big.mark = ",", scientific = FALSE)), hjust=-0.5, colour = "black", size=3.0) +
  labs(title="Demandas de las huelgas del comercio desde el año 2010 al 2018",
       x="Demandas", 
       y = "Huelgas")

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/demandas en comercio.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 15
)



###  Correlación
ohl %>% subset(sector%in% c("G-I Commerce") & tc<2500) %>% 
  ggplot(aes(yr,tc)) + geom_point(aes(colour = factor(leg)),na.rm = TRUE) +
  stat_cor(method = "pearson") + 
  theme_bw() + 
  geom_smooth(se = FALSE, method = lm) + 
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="black","2"="red1"),
                     labels = c("Legal","Extralegal")) + 
  labs(title = "Relación entre año de la huelga y trabajadores comprometidos en estas (1980-2018)",
       subtitle = "Se excluyen dos huelgas con más de 2.500 trabajadores de los años 2014 y 2016",
       x="Tiempo",
       y = "Trabajadores comprometidos",
       caption = "Elaboración propia en base a OHL")  +
  theme(legend.position="bottom") 

ggsave(
  plot = last_plot(),
  filename = "../Output/graph/Correlación TC y año de la huelga.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 20
)




ohl %>% subset(sector%in% c("G-I Commerce") & tc<2500) %>% 
  ggplot(aes(yr,duracion)) + geom_point(aes(colour = factor(leg)),na.rm = TRUE) +
  stat_cor(method = "pearson") + 
  theme_bw() + 
  geom_smooth(se = FALSE, method = lm) + 
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="black","2"="red1"),
                     labels = c("Legal","Extralegal")) + 
  labs(caption = "Elaboración propia en base a OHL")  +
  theme(legend.position="bottom") 

names(ohl)

ohl %>% subset(sector%in% c("G-I Commerce") & tc<2500) %>% 
  ggplot(aes(yr,tactica1)) + geom_point(aes(colour = factor(leg)),na.rm = TRUE) +
  stat_cor(method = "pearson") + 
  theme_bw() + 
  geom_smooth(se = FALSE, method = lm) + 
  scale_color_manual(name="Legalidad de la huelga",values = c("1"="black","2"="red1"),
                     labels = c("Legal","Extralegal")) + 
  labs(caption = "Elaboración propia en base a OHL")  +
  theme(legend.position="bottom") 


