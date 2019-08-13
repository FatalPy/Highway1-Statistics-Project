pairs(Highway1)

unfallpromiokm <-Highway1$rate*1.60934

abschnittslaenge <- Highway1$len*1.60934

tempolimitkmh <- Highway1$slim*1.60934 

auffahrtenprokm <- Highway1$acpt/1.60934

kreuzungprokm <- Highway1$itg/1.60934

spurbreiteinm <- Highway1$lwid*0.3048
spurbreiteinm 
standstreifen <- Highway1$shld*0.3048

AmpelAnzahlprokm <- Highway1$sigs1/1.60934

Spurenanzahl <- Highway1$lane

ProzentanteilLKW <- Highway1$trks

FahrzeuganzahlInTaus <- Highway1$adt  #MC  Major Collector, 
#FAI Federal Interstate Highways  Die Interstate Highways sind ein Fernstra�ennetz in den USA
#PA Prinicpal Arterial Highway
#MA Major Arterial Highways)  Verteilerstra�en zu Standrand und Stadt

Stra�enart <-Highway1$htype #


HighwayDEUTSCH <- data.frame(unfallpromiokm,abschnittslaenge,FahrzeuganzahlInTaus,ProzentanteilLKW,AmpelAnzahlprokm,
                             tempolimitkmh,standstreifen, Spurenanzahl, auffahrtenprokm, kreuzungprokm, spurbreiteinm)

summary((HighwayDEUTSCH))


str(HighwayDEUTSCH)
plot(unfallpromiokm, ylab = "Unfallrate pro millionen Kilometer", col = "red", pch = 16)
a <- spurbreiteinm
dfe<-sort(a)
plot(dfe,ylab = "Spurbreite in Meter",  col = "red", pch = 16)


plot(spurbreiteinm, main = "Datenverteilung Stra�enbreite", ylab = "Stra�enbreite in Metern", xlab
     = "Datens�tze", col = "red", pch = 16)

sd(strassenbreiteinm)
plot(spurbreiteinm)
plot(kreuzungprokm)

sd(AmpelAnzahlprokm)
plot()
sd(FahrzeuganzahlInTaus)
sd(Spurenanzahl)
sd(kreuzungprokm)
sd(auffahrtenprokm)
sd(abschnittslaenge)

pairs(HighwayDEUTSCH)

cor(HighwayDEUTSCH$unfallpromiokm,HighwayDEUTSCH$tempolimitkmh)
cor(abschnittslaenge,unfallpromiokm)
cor(auffahrtenprokm,unfallpromiokm)
cor(kreuzungprokm,unfallpromiokm)
cor(standstreifen,unfallpromiokm)
cor(AmpelAnzahlprokm,unfallpromiokm)
cor(Spurenanzahl,unfallpromiokm)
cor(ProzentanteilLKW,unfallpromiokm)
cor(FahrzeuganzahlInTaus,unfallpromiokm)
cor(AmpelAnzahlprokm,unfallpromiokm)
cor(HighwayDEUTSCH$Stra�enart,HighwayDEUTSCH$unfallpromiokm)

cor(Spurenanzahl,FahrzeuganzahlInTaus)

cor(tempolimitkmh,auffahrtenprokm)


#################### FahrzeuganzahlinTaus und Kreuzungprokm

jl <- ggplot(HighwayDEUTSCH, aes(x=kreuzungprokm,y=FahrzeuganzahlInTaus))

jl2 <- jl + geom_point(aes(color=ProzentanteilLKW),size=3)+ scale_color_gradient(high='red', low = "blue") + xlab("Kreuzungen pro Kilometer") + ylab("Fahrzeuganzahl in Tausend") + ggtitle("Korrelation von der Fahrzeuganzahl und Kreuzungen") #printed die Unf�llepromillionenAutos mit Auffahrtenprokm 

jl3 <- jl2 + geom_smooth(aes(group=1),method ='lm',se=FALSE,color='red') + theme_stata()

jplotly0 <- ggplotly(jl3)

print(jplotly0)

lm(FahrzeuganzahlInTaus ~ kreuzungprokm)


###############################################

#####unfallpromiokm, auffahrtenprokm mit ProzentanteilLKW
pl <- ggplot(HighwayDEUTSCH, aes(x=auffahrtenprokm, y=unfallpromiokm))
pl2 <- pl + geom_point(aes(color=ProzentanteilLKW),size=3)+ scale_color_gradient(high='red', low = "blue") + xlab("Auffahrten pro Km") + ylab("Unfallrate pro Millionen Fahrzeugkilometer") + ggtitle("Korrelation von Unf�llen und Auffahrten") #printed die Unf�llepromillionenAutos mit Auffahrtenprokm 
pl3 <- pl2 + geom_smooth(aes(group=1),method ='lm',formula = y ~ log(x),se=FALSE,color='red') + theme_stata()
plotly0 <- ggplotly(pl3)
print(plotly0)

##############################################

#####unfallpromiokm, tempolimitkmh mit auffahrtenprokm
g <- ggplot(HighwayDEUTSCH, aes(x=tempolimitkmh,y=unfallpromiokm))

g1 <- g + geom_point(aes(color=auffahrtenprokm),size=3)+ scale_color_gradient(high='red', low = "blue") + xlab("Tempolimit in km/h") + ylab("Unfallrate pro mio. Km") + ggtitle("Korrelation von Unfallrate und Tempolimit") 

g2 <- g1 +geom_smooth(aes(group=1),method ='lm',formula = y ~ log(x),se=FALSE,color='red') + theme_stata()

plotly <- ggplotly(g2)

print(plotly) 



p <- plot_ly(HighwayDEUTSCH, x = ~tempolimitkmh, y = ~unfallpromiokm, z = ~auffahrtenprokm, 
             marker = list(color = ~ProzentanteilLKW, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Tempolimit in Km/h'),
                      yaxis = list(title = 'Unfallrate pro mio. Km'),
                      zaxis = list(title = 'Auffahrten pro Km')),
         annotations = list(
           text = 'Prozentuale Anteil LKWs',
           x = 1.1,
           y = 1.05,
           showarrow = FALSE
           
         ))




print(p)

############################ ALLE KORRELATIONEN



num.cols <- sapply(HighwayDEUTSCH, is.numeric)

cor.data <- cor(HighwayDEUTSCH[, num.cols])

cor.data

corrplot(cor.data, method = 'color')
corrgram(HighwayDEUTSCH)


#########Ampelanzahl + Unfallpromiokm

h <- ggplot(HighwayDEUTSCH, aes(x=AmpelAnzahlprokm, y=unfallpromiokm))

h1 <- h + geom_point(color="blue",size=2) + xlab("Anzahl der Ampeln") + ylab("Unfallrate pro mio. Km" ) + ggtitle("Korrelation von Unfallrate und Anzahl der Ampeln") 

h2 <- h1 +geom_smooth(aes(group=1),method ='lm',formula = y ~ log(x),se=FALSE,color='red') + theme_stata()

hplotly <- ggplotly(h2)

print(hplotly) 

