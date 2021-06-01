
install.packages("a")
remove.packages("a")

#OPERAZIONI PRELIMINARI
#------------------------------------------------

library("dplyr")
library("tidyverse")
library("extrafont")
library("ggplot2")
library("gapminder")
library("plyr")
library("plotly")
library("tidyverse")
library("htmlwidgets")
library("grid")
library("png")
library("ggimage")
library("patchwork")
library("cowplot")
library("ggridges")
library("scales")

#library("conflicted") # Possibile risoluzione per errore
                      #Unable to find an inherited method for function select for signature "data.frame" (conflitti tra pacchetti)


loldataset=read.csv("./10.16_LeagueOfLegends_Games.csv", sep=";")
View(loldataset)
loldataset2=read.csv("./riot_champion.csv" , sep=",")
View(loldataset2)



n<-dim(loldataset)[1]              #per eliminare ultima righa
loldataset<-loldataset[1:(n-1),]
  


#Aggiornamento dataset (di modo che i dati corrispondano temporalmente)  

loldataset2 <- loldataset2 %>% add_row(X = 0, 
                                   version = "10.16.1", 
                                   id = "Yone", 
                                   key = 777, 
                                   name = "Yone", 
                                   title = "The Unforgotten" , 
                                   blurb = "In life, he was Yone, half-brother of Yasuo, and renowned student of his village sword school. But upon his death at the hands of his brother, he found himself hunted by a malevolent entity of the spirit realm, and was forced to slay it with its own sword. Now, cursed to wear its demonic mask upon his face, Yone tirelessly hunts all such creatures in order to understand what he has become.",
                                   tags = "['Assassin']", 
                                   partype="None", 
                                   info.attack = 9, 
                                   info.defense = 2, 
                                   info.magic = 4, 
                                   info.difficulty = 8, 
                                   image.full = "Yone.png", 
                                   image.sprite = "Champion2.png", 
                                   image.group="champion" , 
                                   image.x = 48, 
                                   image.y = 96, 
                                   image.w = 48, 
                                   image.h = 48, 
                                   stats.hp = 550.00, 
                                   stats.hpperlevel = 64, 
                                   stats.mp = 0.00, 
                                   stats.mpperlevel = 0.0, 
                                   stats.movespeed = 345 , 
                                   stats.armor = 28.000, 
                                   stats.armorperlevel = 2.45, 
                                   stats.spellblock =  32.0, 
                                   stats.spellblockperlevel =  0.90 ,
                                   stats.attackrange = 175, 
                                   stats.hpregen =   7.50, 
                                   stats.hpregenperlevel =  0.54, 
                                   stats.mpregen =  0.000, 
                                   stats.mpregenperlevel = 0.000, 
                                   stats.crit = 0 , 
                                   stats.critperlevel = 0, 
                                   stats.attackdamage = 60.0000, 
                                   stats.attackdamageperlevel =  1.4000, 
                                   stats.attackspeedperlevel =  1.000, 
                                   stats.attackspeed = 0.625)


loldataset2 <- loldataset2 %>% add_row(X = 0, 
                                      version = "10.16.1", 
                                      id = "Lillia", 
                                      key = 876, 
                                      name = "Lillia", 
                                      title = "The Bashful Bloom" , 
                                      blurb = "Intensely shy, the fae fawn Lillia skittishly wanders Ionia's forests. Hiding just out of sight of mortals whose mysterious natures have long captivated, but intimidated, her Lillia hopes to discover why their dreams no longer reach the ancient Dreaming Tree. She now travels Ionia with a magical branch in hand, in an effort to find people's unrealized dreams. Only then can Lillia herself bloom and help others untangle their fears to find the sparkle within. Eep!",
                                      tags = "['Mage', 'Fighter']", 
                                      partype="None", 
                                      info.attack = 3, 
                                      info.defense = 1, 
                                      info.magic = 8, 
                                      info.difficulty = 10, 
                                      image.full = "Lillia.png", 
                                      image.sprite = "Champion4.png", 
                                      image.group="champion" , 
                                      image.x = 48, 
                                      image.y = 96, 
                                      image.w = 48, 
                                      image.h = 48, 
                                      stats.hp = 580.00, 
                                      stats.hpperlevel = 64, 
                                      stats.mp = 410.00, 
                                      stats.mpperlevel = 36.0, 
                                      stats.movespeed = 330 , 
                                      stats.armor = 20.000, 
                                      stats.armorperlevel = 2.90, 
                                      stats.spellblock =  32.0, 
                                      stats.spellblockperlevel =  0.55 ,
                                      stats.attackrange = 325, 
                                      stats.hpregen =   9, 
                                      stats.hpregenperlevel =  0.54, 
                                      stats.mpregen =  11.500, 
                                      stats.mpregenperlevel = 0.680, 
                                      stats.crit = 0 , 
                                      stats.critperlevel = 0, 
                                      stats.attackdamage = 61.0000, 
                                      stats.attackdamageperlevel =  2.2300, 
                                      stats.attackspeedperlevel =  1.000, 
                                      stats.attackspeed = 0.625) 

imgs=file.path(".","champ")                             #Funzione per generare un percorso file a cui attingere
loldataset2 <- loldataset2 %>% mutate (Immagine = paste(imgs,image.full, sep = "/"))  #Creazione colonna per trovare la directory dove Ã¨ stata salvata l'immagine
  
#------------------------------------------------
#Funzioni di prova

#Restituisce tutti i dati di un campione inserendo il nome
loldataset2 %>% filter(str_detect(name, "Ahri")) 

#restituisce l'id di un campione inserendo il nome
Nomechamp = readline();
Nomechamp = as.string(Nomechamp);
loldataset2 %>% filter(str_detect(name, Nomechamp))%>% 
                select(key)

#Restituisce lidentificativo partita inserendo l'id di un campione
idchamp = readline();
idchamp = as.integer(idchamp);
loldataset %>% filter(t1p1_champId == idchamp | t1p2_champId == idchamp | t1p3_champId == idchamp | t1p4_champId == idchamp | t1p5_champId == idchamp | t2p1_champId == idchamp | t2p2_champId == idchamp | t2p3_champId == idchamp | t2p4_champId == idchamp | t2p5_champId == idchamp)%>%
               select(1)

#Restituisce lidentificativo partita in cui è presente il campione con id 103
loldataset %>% filter(t1p1_champId == 103 | t1p2_champId == 103 | t1p3_champId == 103 | t2p4_champId == 103 | t1p5_champId == 103)%>%
               select(1)

#Restituisce il campione con più range di attacco
loldataset2 %>% 
  select(3,30) %>%
  top_n(1)




#PARTE 1: ANALISI GENERALE
#------------------------------------------------
#SUPPORT

SUPBLU=count(loldataset, vars = "t1p1_champId") #Somma del numero di utilizzi dei campioni in ruolo "Supporto" nel team Blu
SUPRED=count(loldataset, vars = "t2p1_champId") #""""""" team Red

colnames(SUPBLU) <- c("key", "SUP_BLU") #cambio nomi delle colonne per effettuare la full join
colnames(SUPRED) <- c("key", "SUP_RED")

SUPPORT <- data.frame(SUPBLU %>%
              full_join(SUPRED, by = "key")) #Full join tra i due dataset (mette in relazione i dati anche se non hanno corrispettivi nell'altra tabella")

SUPPORT <- SUPPORT %>% mutate_all(~replace(., is.na(.), 0)) #Considera NA come 0 per la somma

SUPPORT <- SUPPORT %>% mutate (support = SUP_BLU + SUP_RED) # Somma il numero di utilizzi dei campioni nel ruolo "Supporto" in totale


#------------------------------------------------
#ADCARRY

ADCBLU=count(loldataset, vars = "t1p2_champId")
ADCRED=count(loldataset, vars = "t2p2_champId")

colnames(ADCBLU) <- c("key", "ADC_BLU") 
colnames(ADCRED) <- c("key", "ADC_RED")

ADCARRY <- data.frame(ADCBLU %>%
              full_join(ADCRED, by = "key"))

ADCARRY <- ADCARRY %>% mutate_all(~replace(., is.na(.), 0))

ADCARRY <- ADCARRY %>% mutate (adcarry = ADC_BLU + ADC_RED)


#------------------------------------------------
#MIDLANER

MIDBLU=count(loldataset, vars = "t1p3_champId")
MIDRED=count(loldataset, vars = "t2p3_champId")

colnames(MIDBLU) <- c("key", "MID_BLU") 
colnames(MIDRED) <- c("key", "MID_RED")

MIDLANER <- data.frame(MIDBLU %>%
              full_join(MIDRED, by = "key"))

MIDLANER <- MIDLANER %>% mutate_all(~replace(., is.na(.), 0))
              
MIDLANER <- MIDLANER %>% mutate (midlaner = MID_BLU + MID_RED)              

#------------------------------------------------
#JUNGLER

JUNGBLU=count(loldataset, vars = "t1p4_champId")
JUNGRED=count(loldataset, vars = "t2p4_champId")

colnames(JUNGBLU) <- c("key", "JUNG_BLU") 
colnames(JUNGRED) <- c("key", "JUNG_RED")

JUNGLER <- data.frame(JUNGBLU %>%
              full_join(JUNGRED, by = "key"))

JUNGLER <- JUNGLER %>% mutate_all(~replace(., is.na(.), 0))
              
JUNGLER <- JUNGLER %>% mutate (jungler = JUNG_BLU + JUNG_RED)              

#------------------------------------------------
#TOPLANER

TOPBLU=count(loldataset, vars = "t1p5_champId")
TOPRED=count(loldataset, vars = "t2p5_champId")

colnames(TOPBLU) <- c("key", "TOP_BLU") 
colnames(TOPRED) <- c("key", "TOP_RED")

TOPLANER <- data.frame(TOPBLU %>%
              full_join(TOPRED, by = "key"))

TOPLANER <- TOPLANER %>% mutate_all(~replace(., is.na(.), 0))
              
TOPLANER <- TOPLANER %>% mutate (toplaner = TOP_BLU + TOP_RED)              
            
#-----------------------------------------------------------
#Join con loldataset2, identifica specifiche dei campioni (nome, descrizione, statistiche varie) e grafici (utilizzo dei campioni)

#PRIMA GEOM_COUNT ma text non funzionava, quindi geom point. Geom_point cambiato in geom_col per maggiore visibilità dati

FULLSUPPORT <- data.frame(SUPPORT %>%
                     full_join(loldataset2, by = "key"))


FULLSUPPORT <- FULLSUPPORT %>% 
  select (key, support, name, tags, title, info.difficulty, Immagine)


FULLSUPPORT <- FULLSUPPORT %>% 
  filter(support >= 690) #Filtra n utilizzi uguali o superiori a 690 (da migliorare, trovare funzione generica top 15)


colnames(FULLSUPPORT) <- c("ID", "Utilizzi","Nome","Ruoli","titolo", "Difficoltà", "Immagine") 


SPRT <- FULLSUPPORT %>%
  ggplot(aes(label = Nome, y = Utilizzi, x = Difficoltà, fill = Ruoli, alpha = 1, text = paste("Nome:", Nome, "<br>", "Ruoli:", Ruoli ,"<br>", "Numero di Utilizzi:",  Utilizzi,"<br>", "Difficoltà:", Difficoltà ))) +
  geom_col(colour = "grey40")+
  
  labs(
    title="Top 15 Campioni utilizzati: Ruolo Supporto",
    subtitle = "Ruolo Supporto",
    x = "Difficoltà",
    y = "Numero di utilizzi"
  )+
  
  scale_size_continuous(range = c(4, 12))+
  
  scale_fill_identity(aesthetics="colour")+
  
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10))+ # impostazioni dei segni del grafico nell'asse X
  
  scale_y_continuous(breaks=c(500, 1000, 1500, 2000, 3000, 4000, 5000, 6000, 7000))+# impostazioni dei segni del grafico nell'asse Y
  
  guides(alpha = FALSE)+
  
  theme(                                  
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    text=element_text(size=10,  family="Helvetica")
  )


ggplotly(SPRT, tooltip=("text"))

#------------------------------------------------

FULLADCARRY <- data.frame(ADCARRY  %>%
                     full_join(loldataset2, by = "key"))


FULLADCARRY <- FULLADCARRY %>% 
  select(key, adcarry, name, tags, title, info.difficulty, Immagine)


FULLADCARRY <- FULLADCARRY %>%
  filter(adcarry >= 456)


colnames(FULLADCARRY) <- c("ID", "Utilizzi","Nome","Ruoli","titolo", "Difficoltà") 


ADCR <- FULLADCARRY %>%
  ggplot(aes(label = Nome, y = Utilizzi, x = Difficoltà, fill = Ruoli, alpha = 1, text = paste("Nome:", Nome, "<br>", "Ruoli:", Ruoli ,"<br>", "Numero di Utilizzi:",  Utilizzi,"<br>", "Difficoltà:", Difficoltà ))) +
  geom_col(colour = "grey40")+
  
  labs(
    title="Top 15 Campioni utilizzati",
    subtitle = "Ruolo ADCarry",
    x = "Difficoltà",
    y = "Numero di utilizzi"
  )+
  
  scale_size_continuous(range = c(4, 12))+
  
  scale_fill_identity(aesthetics="colour")+
  
  scale_x_continuous(breaks=c(0,2, 4, 6, 8, 10))+
  
  scale_y_continuous(breaks=c(500, 1000, 2000, 3000, 4000, 5000, 6000, 8000, 10000))+ 
  
  guides(alpha = FALSE)+
  
  theme(                                  
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    text=element_text(size=10,  family="Helvetica")
  )


ggplotly(ADCR, tooltip=("text"))

#------------------------------------------------

FULLMIDLANER <- data.frame(MIDLANER  %>%
                     full_join(loldataset2, by = "key"))

FULLMIDLANER <- FULLMIDLANER %>%  
  select(key, midlaner, name, tags, title, info.difficulty, Immagine)

FULLMIDLANER <- FULLMIDLANER %>%
  filter(midlaner >= 712)

colnames(FULLMIDLANER) <- c("ID", "Utilizzi","Nome","Ruoli","titolo", "Difficoltà") 


MIDL <- FULLMIDLANER %>%
  ggplot(aes(label = Nome, y = Utilizzi, x = Difficoltà, fill = Ruoli, alpha = 1, text = paste("Nome:", Nome, "<br>", "Ruoli:", Ruoli ,"<br>", "Numero di Utilizzi:",  Utilizzi,"<br>", "Difficoltà:", Difficoltà ))) +
  geom_col(colour = "grey40")+
  
  labs(
    title="Top 15 Campioni utilizzati: Ruolo Mid Laner",
    subtitle = "Ruolo Mid Laner",
    x = "Difficoltà",
    y = "Numero di utilizzi"
  )+
  
  scale_size_continuous(range = c(4, 12))+
  
  scale_fill_identity(aesthetics="colour")+
  
  scale_x_continuous(breaks=c(0,2, 4, 6, 8, 10))+
  
  scale_y_continuous(breaks=c(500, 1000, 2000, 3000, 4000, 5000, 6000))+ 
  
  guides(alpha = FALSE)+
  
  theme(                                  
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    text=element_text(size=10,  family="Helvetica")
  )


ggplotly(MIDL, tooltip=("text"))

#------------------------------------------------
  
FULLJUNGLER <- data.frame(JUNGLER  %>%
                     full_join(loldataset2, by = "key"))

FULLJUNGLER <- FULLJUNGLER %>% 
  select(key, jungler, name, tags, title, info.difficulty, Immagine)


FULLJUNGLER <- FULLJUNGLER %>%
  filter(jungler >= 712)

colnames(FULLJUNGLER) <- c("ID", "Utilizzi","Nome","Ruoli","titolo", "Difficoltà") 


JNGL <- FULLJUNGLER %>%
  ggplot(aes(label = Nome, y = Utilizzi, x = Difficoltà, fill = Ruoli, alpha = 1, text = paste("Nome:", Nome, "<br>", "Ruoli:", Ruoli ,"<br>", "Numero di Utilizzi:",  Utilizzi,"<br>", "Difficoltà:", Difficoltà ))) +
  geom_col(colour = "grey40")+
  labs(
    title="Top 15 Campioni utilizzati: Ruolo Jungler",
    subtitle = "Ruolo Jungler",
    x = "Difficoltà",
    y = "Numero di utilizzi"
  )+
  scale_size_continuous(range = c(4, 12))+
  
  scale_fill_identity(aesthetics="colour")+
  
  scale_x_continuous(breaks=c(0,2, 4, 6, 8, 10))+
  
  scale_y_continuous(breaks=c(500, 1000, 2000, 3000, 4000, 5000, 6000))+ 
  
  guides(alpha = FALSE)+
  
  theme(                                  
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    text=element_text(size=10,  family="Helvetica")
  )


ggplotly(JNGL, tooltip=("text"))

#------------------------------------------------
  
FULLTOPLANER <- data.frame(TOPLANER  %>%
                     full_join(loldataset2, by = "key"))

FULLTOPLANER <- FULLTOPLANER %>% 
  select(key, toplaner, name, tags, title, info.difficulty, Immagine)

FULLTOPLANER <- FULLTOPLANER %>%
  filter(toplaner >= 550)

colnames(FULLTOPLANER) <- c("ID", "Utilizzi","Nome","Ruoli","titolo", "Difficoltà") 


TOPL <- FULLTOPLANER %>%
  ggplot(aes(label = Nome, y = Utilizzi, x = Difficoltà, fill = Ruoli, alpha = 1, text = paste("Nome:", Nome, "<br>", "Ruoli:", Ruoli ,"<br>", "Numero di Utilizzi:",  Utilizzi,"<br>", "Difficoltà:", Difficoltà ))) +
  geom_col(colour = "grey40")+
  labs(
    title="Top 15 Campioni utilizzati: Ruolo Top Laner",
    subtitle = "Ruolo Top Laner",
    x = "Difficoltà",
    y = "Numero di utilizzi"
  )+
  scale_size_continuous(range = c(4, 12))+
  
  scale_fill_identity(aesthetics="colour")+
  
  scale_x_continuous(breaks=c(0,2, 4, 6, 8, 10))+
  
  scale_y_continuous(breaks=c(500, 1000, 2000, 3000, 4000, 5000, 6000))+ 
  
  guides(alpha = FALSE)+

  theme(                                  
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    text=element_text(size=10,  family="Helvetica"),
    
  )


ggplotly(TOPL, tooltip=("text"))

#------------------------------------------------

FULLSUPPORT <- data.frame(SUPPORT %>%
                            full_join(loldataset2, by = "key"))

FULLADCARRY <- data.frame(ADCARRY  %>%
                            full_join(loldataset2, by = "key"))

FULLMIDLANER <- data.frame(MIDLANER  %>%
                             full_join(loldataset2, by = "key"))

FULLJUNGLER <- data.frame(JUNGLER  %>%
                            full_join(loldataset2, by = "key"))

FULLTOPLANER <- data.frame(TOPLANER  %>%
                             full_join(loldataset2, by = "key"))

FULLSUPPORT <- FULLSUPPORT %>% 
  select (key, support, name, image.full, info.difficulty)

FULLADCARRY <- FULLADCARRY %>% 
  select (key, adcarry, name, image.full, info.difficulty)

FULLMIDLANER <- FULLMIDLANER %>% 
  select (key, midlaner, name, image.full, info.difficulty)

FULLJUNGLER <- FULLJUNGLER %>% 
  select (key, jungler, name, image.full, info.difficulty)

FULLTOPLANER <- FULLTOPLANER %>% 
  select (key, toplaner, name, image.full, info.difficulty)

#------------------------------------------------


COMPLETE <- data.frame(FULLSUPPORT %>%
                         full_join(FULLADCARRY, by=c("key", "image.full", "name", "info.difficulty")))

COMPLETE <- data.frame(COMPLETE %>%
                         full_join(FULLTOPLANER, by=c("key", "image.full","name", "info.difficulty")))

COMPLETE <- data.frame(COMPLETE %>%
                         full_join(FULLMIDLANER, by=c("key", "image.full","name", "info.difficulty")))

COMPLETE <- data.frame(COMPLETE %>%
                         full_join(FULLJUNGLER, by=c("key", "image.full","name", "info.difficulty")))

COMPLETE$NUM <- as.numeric(row.names(COMPLETE)) #funzione che assegna un numero alla riga (come utilizzare indice dataset come dato a se stante)

COMPLETE <- COMPLETE[,c(10,1,3,4,5,2,6,7,8,9)]


COMPLETE <- COMPLETE %>% mutate_all(~replace(., is.na(.), 0))


COMPLETE <- data.frame(COMPLETE %>%
                         mutate(Totali = support + adcarry + midlaner + jungler + toplaner))

COMPLETE <- COMPLETE %>%
  filter(Totali >= 2345)

COMPLETE$NUM <- as.numeric(row.names(COMPLETE)) #Richiama funzione per errore in ggplot successivo


g = ggplot(data = COMPLETE)+
  geom_col(mapping = aes( y = Totali, x = NUM, fill = "#ffa600", color = "grey33"))+
  
  theme(axis.title.x=element_blank(), #tre funzioni per eliminare titolo, segni e testo dalla linea delle ascisse (per inserimento immagini successivo)
        
        axis.text.x=element_blank(),
        
        axis.ticks.x=element_blank(),
        
        )+
  labs(
    
    title="Campioni più Utilizzati",
    
    y="Numero di Utilizzi Totali"
    
  )+
  
  scale_fill_identity(aesthetics="colour")+
  
  scale_fill_identity(aesthetics="fill")+
  
  
  
annotate("text", x=1, y=1000, angle=90, label="Ashe")+
  annotate("text", x=1, y=1550, angle=90, label="2984")+

annotate("text", x=2, y=1000, angle=90, label="Akali")+
  annotate("text", x=2, y=1550, angle=90, label="2607")+
  
annotate("text", x=3, y=1000, angle=90, label="Lux")+
  annotate("text", x=3, y=1550, angle=90, label="2634")+
  
annotate("text", x=4, y=1000, angle=90, label="Graves")+
  annotate("text", x=4, y=1550, angle=90, label="2447")+
  
annotate("text", x=5, y=1000, angle=90, label="Camille")+
  annotate("text", x=5, y=1550, angle=90, label="3080")+
  
annotate("text", x=6, y=1000, angle=90, label="Jhin")+
  annotate("text", x=6, y=1550, angle=90, label="3292")+
  
annotate("text", x=7, y=1000, angle=90, label="Senna")+
  annotate("text", x=7, y=1550, angle=90, label="2831")+
  
annotate("text", x=8, y=1000, angle=90, label="Thresh")+
  annotate("text", x=8, y=1550, angle=90, label="3117")+
  
annotate("text", x=9, y=1000, angle=90, label="Bard")+
  annotate("text", x=9, y=1550, angle=90, label="2570")+
  
annotate("text", x=10, y=1000, angle=90, label="Yone")+
  annotate("text", x=10, y=1550, angle=90, label="2543")+
  
annotate("text", x=11, y=1000, angle=90, label="Ezreal")+
  annotate("text", x=11, y=1550, angle=90, label="4109")+
  
annotate("text", x=12, y=1000, angle=90, label="Caitlyn")+
  annotate("text", x=12, y=1550, angle=90, label="4036")+
  
annotate("text", x=13, y=1000, angle=90, label="Kha'Zix")+
  annotate("text", x=13, y=1550, angle=90, label="2345")+
  
annotate("text", x=14, y=1000, angle=90, label="Lucian")+
  annotate("text", x=14, y=1550, angle=90, label="3383")+
  
annotate("text", x=15, y=1000, angle=90, label="Nidalee")+
  annotate("text", x=15, y=1550, angle=90, label="2520")


ximg <- axis_canvas(g, axis = 'x') + 
  draw_image("./champ/Ashe.png", x = 0.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Akali.png", x = 1.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Lux.png", x = 2.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Graves.png", x = 3.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Camille.png", x = 4.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Jhin.png", x = 5.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Senna.png", x = 6.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Thresh.png", x = 7.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Bard.png", x = 8.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Yone.png", x = 9.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Ezreal.png", x = 10.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Caitlyn.png", x = 11.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Khazix.png", x = 12.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Lucian.png", x = 13.5, y = 0.2, scale = 0.8) +
  draw_image("./champ/Nidalee.png", x = 14.5, y = 0.2, scale = 0.8) 



ggdraw(insert_xaxis_grob(g, ximg, position = "bottom"))






#------------------------------------------------
#PARTE DUE: ANALISI RED VS BLU

#Durata partita in minuti
loldataset <- loldataset %>% 
  mutate (Minutes = gameDuration / 60) 

#Gold Totale team 1 (BLU)
loldataset <- loldataset %>% 
  mutate (goldteam1 = t1p1_goldEarned + t1p2_goldEarned + t1p3_goldEarned + t1p4_goldEarned + t1p5_goldEarned)

#Gold Totale team 2 (RED)
loldataset <- loldataset %>% 
  mutate (goldteam2 = t2p1_goldEarned + t2p2_goldEarned + t2p3_goldEarned + t2p4_goldEarned + t2p5_goldEarned)

#Average Gold team 1 (BLU): Quanto gold è stato guadagnato al minuto da tutti e 5 i componenti del team
loldataset <- loldataset %>% 
  mutate (avg_goldteam1= goldteam1 / Minutes)

#Average Gold team 2 (RED) 
loldataset <- loldataset %>% 
  mutate (avg_goldteam2= goldteam2 / Minutes)

#------------------------------------------------
#Grafico avg_gold/avg_lp squadra blu: la parte blu attorno è l'intervallo di confidenza, posizione dove potrebbe essere possibile trovare valori


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = avg_goldteam1, colour="black", fill="grey33"))+
  
labs(
    title="Media dell'oro guadagnato in rapporto alla media dei League Points",
    subtitle = "Squadra Blu",
    x = "Media dei League Points della partita",
    y = "Media dell' oro guadagnato"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ #funzione che rende color/fill un parametro prettamente stilistico e non legato ai dati
  
  scale_fill_identity(aesthetics="fill" )+
  
  theme(                                    #funzione che permette di personalizzare il grafico (linee, fill, font)
    
    panel.background = element_rect(fill = "skyblue2",
                                    colour = "skyblue2",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
)


ggplot(data = loldataset)+
    geom_smooth(mapping = aes(x = average_lp, y = avg_goldteam2, colour="black", fill="grey33" ))+
  
labs(
  
    title="Media dell'oro guadagnato in rapporto alla media dei League Points",
    subtitle = "Squadra Rossa",
    x = "Media dei League Points della partita",
    y = "Media dell' oro guadagnato"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  
  scale_fill_identity(aesthetics="fill")+
  
theme(
  panel.background = element_rect(fill = "#F17D7D",
                                  colour = "#F17D7D",
                                  size = 0.5, linetype = "solid"),
  
  panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                  colour = "grey"), 
  
  panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                  colour = "grey"),
  
  text=element_text(size=10,  family="Helvetica")
)

#grafico rosso inferiore, meno cs, meno strutture?
#------------------------------------------------  
#Grafico avg_kills/avg_lp squadra rossa

loldataset <- loldataset %>% 
  mutate (killteam1 = t1p1_kills + t1p2_kills + t1p3_kills + t1p4_kills + t1p5_kills)

loldataset <- loldataset %>% 
  mutate (killteam2 = t2p1_kills + t2p2_kills + t2p3_kills + t2p4_kills + t2p5_kills)

loldataset <- loldataset %>% 
  mutate (deathteam1 = t1p1_deaths + t1p2_deaths + t1p3_deaths + t1p4_deaths + t1p5_deaths)

loldataset <- loldataset %>% 
  mutate (deathteam2 = t2p1_deaths + t2p2_deaths + t2p3_deaths + t2p4_deaths + t2p5_deaths)

loldataset <- loldataset %>% 
  mutate (Kd_Ratio_1 = killteam1 / deathteam1)
          
loldataset <- loldataset %>% 
  mutate (Kd_Ratio_2 = killteam2 / deathteam2)
          

ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = killteam1, colour="black", fill="blue" ))+
  
  labs(
    
    title="Uccisioni in rapporto ai League Points",
    subtitle = "Squadra Blu",
    x = "Media dei League Points della partita",
    y = "Uccisioni della Squadra"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "skyblue2",
                                    colour = "skyblue2",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
  )


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = killteam2, colour="black", fill="red" ))+
  
  labs(
    
    title="Uccisioni in rapporto ai League Points",
    subtitle = "Squadra Rossa",
    x = "Media dei League Points della partita",
    y = "Uccisioni della Squadra"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "#F17D7D",
                                    colour = "#F17D7D",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
  )

#Grafico rosso superiore, più aggressività.
#------------------------------------------------  
#Grafico avg_assists/avg_lp squadra rossa

loldataset <- loldataset %>% 
  mutate (assistteam1 = t1p1_assists + t1p2_assists + t1p3_assists + t1p4_assists + t1p5_assists)

loldataset <- loldataset %>% 
  mutate (assistteam2 = t2p1_assists + t2p2_assists + t2p3_assists + t2p4_assists + t2p5_assists)


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = assistteam1, colour="black", fill="blue" ))+
  
  labs(
    
    title="Assist in rapporto ai League Points",
    subtitle = "Squadra Blu",
    x = "Media dei League Points della partita",
    y = "Assist della squadra"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "skyblue2",
                                    colour = "skyblue2",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
  )


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = assistteam2, colour="black", fill="red" ))+
  
  labs(
    
    title="Assist in rapporto ai League Points",
    subtitle = "Squadra Rossa",
    x = "Media dei League Points della partita",
    y = "Assist della squadra"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "#F17D7D",
                                    colour = "#F17D7D",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")


)


#Grafico blu superiore, più teamwork e più
#------------------------------------------------  
#Grafico avg_visionscore/avg_lp squadra blu


loldataset <- loldataset %>% 
  mutate (Visionscore1 = t1p1_visionScore + t1p2_visionScore + t1p3_visionScore + t1p4_visionScore + t1p5_visionScore)

loldataset <- loldataset %>% 
  mutate (Visionscore2 = t2p1_visionScore + t2p2_visionScore + t2p3_visionScore + t2p4_visionScore + t2p5_visionScore)


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = Visionscore1, colour="black", fill="blue" ))+
  
  labs(
    
    title="Punti visione in rapporto ai League Points",
    subtitle = "Squadra Blu",
    x = "Media dei League Points della partita",
    y = "Punteggio visione della squadra"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "skyblue2",
                                    colour = "skyblue2",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")

)


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = Visionscore2, colour="black", fill="red" ))+
  
  labs(
    
    title="Punti visione in rapporto ai League Points",
    subtitle = "Squadra Rossa",
    x = "Media dei League Points della partita",
    y = "Punteggio visione della squadra"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "#F17D7D",
                                    colour = "#F17D7D",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
    
    
)

#grafico blu superiore, più ward piazzate e in posti migliori
#------------------------------------------------  
#danni alle torri

loldataset <- loldataset %>% 
  mutate (Turretd1 = t1p1_damageDealtToTurrets + t1p2_damageDealtToTurrets + t1p3_damageDealtToTurrets + t1p4_damageDealtToTurrets + t1p5_damageDealtToTurrets)

loldataset <- loldataset %>% 
  mutate (Turretd2 = t2p1_damageDealtToTurrets + t2p2_damageDealtToTurrets + t2p3_damageDealtToTurrets + t2p4_damageDealtToTurrets + t2p5_damageDealtToTurrets)


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = Turretd1, colour="black", fill="blue" ))+
  
  labs(
    
    title="Danni alle torri",
    subtitle = "Squadra Blu",
    x = "Media dei League Points della partita",
    y = "Danni alle torri"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "skyblue2",
                                    colour = "skyblue2",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
    
  )


ggplot(data = loldataset)+
  geom_smooth(mapping = aes(x = average_lp, y = Turretd2, colour="black", fill="red" ))+
  
  labs(
    
    title="Danni alle torri",
    subtitle = "Squadra Rossa",
    x = "Media dei League Points della partita",
    y = "Danni alle torri"
    
  ) +
  
  scale_fill_identity(aesthetics="colour")+ 
  scale_fill_identity(aesthetics="fill")+
  
  theme(
    panel.background = element_rect(fill = "#F17D7D",
                                    colour = "#F17D7D",
                                    size = 0.5, linetype = "solid"),
    
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"), 
    
    panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                    colour = "grey"),
    
    text=element_text(size=10,  family="Helvetica")
    
    
  )

#grafico  superiore, più 
#------------------------------------------------  
#Grafico percentuali vittorie


VITTORIE <- data.frame(loldataset %>%
                         select(t1_win))

VITTORIE <- VITTORIE %>% 
mutate(tot=as.numeric(row.names(VITTORIE)))

VITTORIE <- VITTORIE %>% 
mutate(Vittorie=sum(t1_win))

VITTORIE <- VITTORIE %>% 
filter(tot==14109)

VITTORIE <- VITTORIE %>% 
add_row(t1_win = 1, tot= 14109, Vittorie=tot-7141) 

VITTORIE <- VITTORIE %>% 
  mutate(percentuale= Vittorie/tot*100)

VITTORIE <- VITTORIE %>% 
  mutate(tot=as.numeric(row.names(VITTORIE)))

vtr <- VITTORIE %>%
ggplot(aes(x = tot, y = Vittorie, fill=c(high="skyblue2", low="#F17D7D"), width=0.95, height =2, text = paste("Vittorie:", Vittorie, "<br>","Percentuale:", percentuale, "%")))+  #factor(data) per permettere di inserire colori manualmente) 
  geom_col()+
  theme(axis.title.x=element_blank(), #tre funzioni per eliminare titolo, segni e testo dalla linea delle ascisse (per inserimento immagini successivo)
        
        axis.text.x =element_blank(),
        
        axis.ticks.x=element_blank(),
        )+
labs(
    
    title="Numero di vittorie"
    )+
  
  scale_x_continuous(expand=c(0,0.75)) +
  
theme(

      panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                colour = "grey"), 

      panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                colour = "grey"),

      text=element_text(size=10,  family="Helvetica"),

      legend.position = "none",

)
  
ggplotly(vtr, tooltip=("text"))


#Grafici molto simili: ipotesi non confermata, serve ulteriore ricerca a livelli più bassi (più influenza sulla psicologia) o più dati in generale? 
