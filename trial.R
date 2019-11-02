library(readxl)
library(dplyr)
library(here)
library(ggplot2)
library(ggthemes)
library(scales)
theme_set(theme_fivethirtyeight() +  
            theme(axis.title.x=element_blank(),
                  # axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.title = element_blank(),
                  legend.position="bottom", 
                  legend.direction="horizontal",
                  legend.margin=margin(t = 0, unit='cm'),
                  legend.key.width = unit(1.5,"cm")))

dr_here()
image_type <- ".jpeg"
path <- paste0(getwd(), "/")
dat <- read_excel(paste0(path, "RO_polls_presidential_2019.xlsx"))


dat <- dat %>%
  filter(is.na(Notes)) %>% # dropping the "irregular" polls
  mutate(`Polling company`=factor(`Polling company`),
         `Fieldwork end data`=as.Date(`Fieldwork end data`))

Dancila <- paste0("D", sprintf('\u0103'), "ncil", sprintf('\u0103'))

ggplot(dat, aes(x=`Fieldwork end data`)) +
  geom_point(aes(y=`Klaus Iohannis`, color="Iohannis")) +
  geom_smooth(aes(y=`Klaus Iohannis`, color="Iohannis"), se=FALSE) +
  geom_point(aes(y=`Viorica Dăncilă`, color=Dancila)) +
  geom_smooth(aes(y=`Viorica Dăncilă`, color=Dancila), se=FALSE) +
  geom_point(aes(y=`Mircea Diaconu`, color="Diaconu")) +
  geom_smooth(aes(y=`Mircea Diaconu`, color="Diaconu"), se=FALSE) +
  geom_point(aes(y=`Dan Barna`, color="Barna")) +
  geom_smooth(aes(y=`Dan Barna`, color="Barna"), se=FALSE) +
  geom_point(aes(y=`Theodor Paleologu`, color="Paleologu")) +
  geom_smooth(aes(y=`Theodor Paleologu`, color="Paleologu"), se=FALSE) +
  geom_point(aes(y=`Kelemen Hunor`, color="Kelemen")) +
  geom_smooth(aes(y=`Kelemen Hunor`, color="Kelemen"), se=FALSE) +
  geom_vline(xintercept = as.numeric(as.Date("2019-10-10")), color="black") +
  scale_colour_manual("",   limits=c("Iohannis", Dancila, "Barna", "Diaconu", "Paleologu", "Kelemen"),
                      values = c("#cc9900", "#C1333C", "#FF4F01", "black", "#0863B2", "#038139")) +
  scale_x_date(labels = date_format("%b-%y"))