# Code: Spanish graphs ----------------------------------------------------
# 1. Load packages --------------------------------------------------------
pacman::p_load(tidyverse, ggplot2, lubridate, openxlsx)

# 2. Load data base -------------------------------------------------------
ohl_19 <- readWorkbook("input/data/Base OHL 2010-2019.xlsx", detectDates = T)
ohl_18 <-readWorkbook("input/data/Labor_Strikes_Dataset_1979_2018_Public.xlsx", detectDates=TRUE)

ohl <- ohl_18 %>%
  plyr::rbind.fill(ohl_19) %>%
  mutate(id = seq(3390, 15886, by = 1),
         folio = if_else(is.na(folio), id, folio)) %>% 
  distinct(folio, .keep_all = T)

# 3. Setup -----------------------------------------------------------------
theme_set(theme_minimal() + theme(text = element_text(size = 14),
                                  legend.position="bottom"))
options(scipen = 999)

# 4. Process --------------------------------------------------------------
# Class
ohl <- ohl %>% 
  mutate_at(vars(ciuur2,ciuur3,leg), funs(as.factor(.))) %>% 
  mutate(dhtp = as.numeric(dhtp))

# Recode
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
                                     is.na(ciuur2)&ciuur4==21 ~ "Unknown or Other Activities"),
                    general_strike = if_else(sector == "National Strikes","National Strikes", "Firm or sector strike"))

ohl %>% subset(is.na(ciuur2)) %>% select(yr,ciuur2,ciuur3,ciuur4,sector)


# Tradicional -----------
ohl %>% filter(!is.na(sector), sector %in% c("A Agriculture",         
                                             "B Mining",
                                             "C Manufacturing industry",
                                             "D-E Electricity, Water and Sanitary Services",
                                             "F Construction",
                                             "H-J Transportation and Communication")) %>% 
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
       filename = "output/graph/worker-lostday-tradicional-en.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Services --------------
# N y M sin n
ohl %>% filter(!is.na(sector), sector %in% c("G-I Commerce",         
                                             "L-K Banks and Financial Services",
                                             "Q Health (private, public and municipalized)")) %>% 
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
       filename = "output/graph/worker-lostday-services-en.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Educacion --------------
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
       filename = "output/graph/worker-lostday-educ-en.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# National -----------
ohl %>% 
  filter(!is.na(sector), sector %in% c("National Strikes")) %>% 
  group_by(sector,leg, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes( x = yr, y = n)) +
  geom_point(aes(size = n), color = "brown1")  +
  scale_size(range = c(3,11)) +
  scale_y_continuous(limits = c(0,7)) +
  labs(title = "National strikes in Chile (1989-2019)",
       x="",
       y = "Number of strikes",
       caption = "Source: Strike repository Andrade&Ratto (2021)")+
  guides(size = F)

ggsave(plot = last_plot(),
       filename = "output/graph/worker-lostday-nationa-2l.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


# National -----------
ohl %>% 
  filter(!is.na(sector), sector %in% c("National Strikes")) %>%
  group_by(sector,leg, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes( x = yr, y = n)) +
  geom_line(size = 1.3, color = "brown1")  +
  scale_y_continuous(limits = c(0,7)) +
  labs(title = "National strikes in Chile (1989-2019)",
       x="",
       y = "Number of strikes",
       caption = "Source: Strike repository Andrade&Ratto (2021)")

ggsave(plot = last_plot(),
       filename = "output/graph/worker-lostday-national.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# National -----------
ohl %>% 
  filter(!is.na(sector), general_strike %in% c("Firm or sector strike")) %>%
  group_by(general_strike,leg, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes( x = yr, y = n, color = leg)) +
  geom_line(size = 1.3)   +
  geom_vline(xintercept = c(1989, 1997,2002, 2003, 2008, 2015, 2016, 2018, 2019),
             color = "gray30", size = 1,  linetype = "longdash") +
  geom_label(aes(x = 1989, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 1997, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 2002, label = "2 National strike", y = 160),color = "black", size = 4) +
  geom_label(aes(x = 2003, label = "7 National strike", y = 200),color = "black", size = 4) +
  geom_label(aes(x = 2008, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 2015, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 2016, label = "3 National strike", y = 170),color = "black", size = 4) +
  geom_label(aes(x = 2018, label = "7 National strike", y = 230),color = "black", size = 4) +
  geom_label(aes(x = 2019, label = "6 National strike", y = 200),color = "black", size = 4) +
      labs(title = "Figure 2. Evolution  of number in strikes in Chile  by legacy (1989-2019)",
       x="",
       y = "Number of strikes",
       caption = "Source: Strike repository Andrade&Ratto (2021)") +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Lawless")) +
  scale_x_continuous(breaks = c(1979,1983,1986,1989,1991,1994,1997,2000,2003,2006,2008,
                                2011,2016,2019))


ggsave(plot = last_plot(),
       filename = "output/graph/strikes-evolution-national.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

## Final

ohl %>% 
  filter(!is.na(sector)) %>%
  group_by(leg, yr) %>% 
  summarise(dhtp = sum(dhtp, na.rm = T)) %>% 
  ggplot(aes( x = yr, y = dhtp, color = leg)) +
  geom_line(size = 1.3)   +
  geom_vline(xintercept = c(1989, 1997,2002, 2003, 2008, 2015, 2016, 2018, 2019),
             color = "gray30", size = 1,  linetype = "longdash") +
  geom_label(aes(x = 1989, label = "1 National strike", y = 5400000),color = "black", size = 4) +
  geom_label(aes(x = 1997, label = "1 National strike", y = 5400000),color = "black", size = 4) +
  geom_label(aes(x = 2002, label = "2 National strike", y = 6000000),color = "black", size = 4) +
  geom_label(aes(x = 2003, label = "7 National strike", y = 9000000),color = "black", size = 4) +
  geom_label(aes(x = 2008, label = "1 National strike", y = 5400000),color = "black", size = 4) +
  geom_label(aes(x = 2015, label = "1 National strike", y = 5400000),color = "black", size = 4) +
  geom_label(aes(x = 2016, label = "3 National strike", y = 6500000),color = "black", size = 4) +
  geom_label(aes(x = 2018, label = "7 National strike", y = 9000000),color = "black", size = 4) +
  geom_label(aes(x = 2019, label = "6 National strike", y = 8000000),color = "black", size = 4) +
  labs(title = "Figure 3. Evolution of worker lost on day in strikes in Chile by legacy (1989-2019)",
       x="",
       y = "Worker lost on day",
       caption = "Source: Strike repository Andrade&Ratto (2021)") +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Lawless")) +
  scale_x_continuous(breaks = c(1979,1983,1986,1989,1991,1994,1997,2000,2003,2006,2008,
                                2011,2016,2019))

ggsave(plot = last_plot(),
       filename = "output/graph/strikes-evolution-workerlost-national.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


# National and social -----------
ohl %>% 
  filter(!is.na(sector), general_strike %in% c("Firm or sector strike")) %>%
  group_by(general_strike,leg, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes( x = yr, y = n, color = leg)) +
  geom_line(size = 1.3)   +
  geom_vline(xintercept = c(1989, 1997,2002, 2003, 2008, 2015, 2016, 2018, 2019),
             color = "black", size = 1,  linetype = "longdash") +
  geom_vline(xintercept = c(2001, 2006,2011, 2016, 2018),
             color = "gray50", size = 1,  linetype = "dashed") +
  geom_label(aes(x = 1989, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 1997, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 2001, label = "Mochilazo", y = 55),color = "gray30", size = 3) +
  geom_label(aes(x = 2002, label = "2 National strikes", y = 160),color = "black", size = 4) +
  geom_label(aes(x = 2003, label = "7 National strikes", y = 200),color = "black", size = 4) +
  geom_label(aes(x = 2006, label = "Penguins'\nRevolution", y = 60),color = "gray30", size = 3) +
  geom_label(aes(x = 2008, label = "1 National strike", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 2011, label = "Chilean Student\nprotest (2011)", y = 60),color = "gray30", size = 3) +
  geom_label(aes(x = 2015, label = "1 National strikes", y = 140),color = "black", size = 4) +
  geom_label(aes(x = 2016, label = "3 National strikes", y = 170),color = "black", size = 4) +
  geom_label(aes(x = 2016, label = "NO+AFP", y = 55),color = "gray30", size = 3) +
  geom_label(aes(x = 2018, label = "Feminist\nMay", y = 71),color = "gray30", size = 3) +
  geom_label(aes(x = 2018, label = "7 National strikes", y = 230),color = "black", size = 4) +
  geom_label(aes(x = 2019, label = "6 National strikes", y = 200),color = "black", size = 4) +
  labs(title = "Strikes evolution in Chile (1989-2019)",
       x="",
       y = "Number of strikes",
       caption = "Source: Strike repository Andrade&Ratto (2021) based on OHL") +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Lawless")) +
  scale_x_continuous(breaks = c(1979,1983,1986,1989,1991,1994,1997,2000,2003,2006,2008,
                                2011,2016,2019))


ggsave(plot = last_plot(),
       filename = "output/graph/strikes-evolution-social-national.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)

# Op 2
ohl %>% 
  filter(!is.na(sector), general_strike %in% c("Firm or sector strike")) %>%
  group_by(general_strike, yr) %>% 
  summarise(n = n()) %>% 
  ggplot(aes( x = yr, y = n)) +
  geom_line(size = 1.3, color = "brown1")   +
  geom_vline(xintercept = c(1989, 1997,2002, 2003, 2008, 2015, 2016, 2018, 2019),
             color = "black", size = 1,  linetype = "longdash") +
  geom_vline(xintercept = c(2001, 2006,2011, 2016, 2018),
             color = "gray50", size = 1,  linetype = "dashed") +
  geom_label(aes(x = 1989, label = "1 National strike", y = 400),color = "black", size = 4) +
  geom_label(aes(x = 1997, label = "1 National strike", y = 400),color = "black", size = 4) +
  geom_label(aes(x = 2001, label = "Mochilazo", y = 55),color = "gray30", size = 3) +
  geom_label(aes(x = 2002, label = "2 National strikes", y = 440),color = "black", size = 4) +
  geom_label(aes(x = 2003, label = "7 National strikes", y = 510),color = "black", size = 4) +
  geom_label(aes(x = 2006, label = "Penguins'\nRevolution", y = 60),color = "gray30", size = 3) +
  geom_label(aes(x = 2008, label = "1 National strike", y = 400),color = "black", size = 4) +
  geom_label(aes(x = 2011, label = "Chilean Student\nprotest (2011)", y = 60),color = "gray30", size = 3) +
  geom_label(aes(x = 2015, label = "1 National strikes", y = 400),color = "black", size = 4) +
  geom_label(aes(x = 2016, label = "3 National strikes", y = 470),color = "black", size = 4) +
  geom_label(aes(x = 2016, label = "NO+AFP", y = 55),color = "gray30", size = 3) +
  geom_label(aes(x = 2018, label = "Feminist\nMay", y = 71),color = "gray30", size = 3) +
  geom_label(aes(x = 2018, label = "7 National strikes", y = 510),color = "black", size = 4) +
  geom_label(aes(x = 2019, label = "6 National strikes", y = 490),color = "black", size = 4) +
  geom_label(aes(x = 2019, label = "Estallido\nSocial", y = 100),color = "black", size = 3) +
    labs(title = "Figure 1. Evolution of number of strikes in Chile (1989-2019)",
       x="",
       y = "Number of strikes",
       caption = "Source: Strike repository Andrade&Ratto (2021) based on OHL") +
  scale_color_manual(name="",values = c("1"="darkblue","2"="brown1"),
                     labels = c("Legal","Lawless")) +
  scale_x_continuous(breaks = c(1979,1983,1986,1989,1991,1994,1997,2000,2003,2006,2008,
                                2011,2016,2019))


ggsave(plot = last_plot(),
       filename = "output/graph/strikes-evolution-social-national.png",
       device = "png",
       dpi = "retina",
       units = "cm",
       width = 33,
       height = 20)


# Demands -----------------------------------------------------------------

# General demands ---------------------------------------------------------
a<-ohl %>% subset(yr>=2010) %>% group_by(dem1) %>% tally() %>% rename(demanda=dem1,d1=n)
b<-ohl %>% subset(yr>=2010) %>% group_by(dem2) %>% tally() %>% rename(demanda=dem2,d2=n)
c<-ohl %>% subset(yr>=2010) %>% group_by(dem3) %>% tally() %>% rename(demanda=dem3,d3=n)
d<-ohl %>% subset(yr>=2010) %>% group_by(dem4) %>% tally() %>% rename(demanda=dem4,d4=n)
e<-ohl %>% subset(yr>=2010) %>% group_by(dem5) %>% tally() %>% rename(demanda=dem5,d5=n)
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


a %>% filter(demanda != "no demand mentioned") %>% 
  ggplot(aes(x=reorder(stringr::str_wrap(demanda,60),d), y=d)) +
  geom_bar(stat="identity", fill="darkblue", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(d, big.mark = ",", scientific = FALSE)), hjust=-0.5, colour = "black", size=3.0) +
  labs(title="Strikes demands (2010-2019)",
       x="Demands", 
       y = "Mentions")

ggsave(
  plot = last_plot(),
  filename = "output/graph/strikes-demands.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 15
)


# National demands --------------------------------------------------------
a<-ohl %>% subset(sector%in% c("National Strikes")) %>% group_by(dem1) %>% tally() %>% rename(demanda=dem1,d1=n)
b<-ohl %>% subset(sector%in% c("National Strikes")) %>% group_by(dem2) %>% tally() %>% rename(demanda=dem2,d2=n)
c<-ohl %>% subset(sector%in% c("National Strikes")) %>% group_by(dem3) %>% tally() %>% rename(demanda=dem3,d3=n)
d<-ohl %>% subset(sector%in% c("National Strikes")) %>% group_by(dem4) %>% tally() %>% rename(demanda=dem4,d4=n)
e<-ohl %>% subset(sector%in% c("National Strikes")) %>% group_by(dem5) %>% tally() %>% rename(demanda=dem5,d5=n)

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


a %>% filter(demanda != "no demand mentioned") %>% 
  ggplot(aes(x=reorder(stringr::str_wrap(demanda,40),d), y=d)) +
  geom_bar(stat="identity", fill="darkblue", colour="black", width=0.8) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  theme_bw() +  coord_flip() + 
  geom_text(aes(label = format(d, big.mark = ",", scientific = FALSE)), hjust=-0.5, colour = "black", size=3.0) +
  labs(title="National strikes demands (1979-2019)",
       x="Demands", 
       y = "Mentions")

ggsave(
  plot = last_plot(),
  filename = "output/graph/national-strikes-demands.png",
  device = "png",
  dpi = "retina",
  units = "cm",
  width = 33,
  height = 15
)

