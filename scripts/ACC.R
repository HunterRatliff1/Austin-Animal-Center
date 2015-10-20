# ---------------------------------------------------------------------------------------------------
# Required packages
packs= c("ggplot2", "dplyr", "lubridate", "reshape2", "magrittr", "stringi", 
         "tidyr", "ggthemes", "scales", "RSocrata")
# Read them
sapply(packs, require, character.only=TRUE)  

# Functions
simple_breed = function (data_frame, cutoff, replace_name = "Other")
{
  newDF = group_by(data_frame, Breed) %>% 
    mutate(Animal.Breed = ifelse(n() > cutoff, Breed, replace_name)) %>% 
    ungroup() 
  
  newDF$Breed = as.factor(newDF$Animal.Breed)
  select(newDF, -Animal.Breed)
  
}
SN_Sex_replacer = function (data_frame)
{
  require("dplyr")
  # Construct data frame to match "Sex.upon.Outcome"
  Sex.upon.Outcome = c("Intact Male", "Intact Female", "Neutered Male", "Spayed Female", "Unknown")
  SN    = c("Intact",      "Intact",        "Neutered",      "Spayed",        NA)
  Sex   = c("M",           "F",             "M",             "F",             NA)
  Sex.upon.Outcome = data_frame(Sex.upon.Outcome, SN, Sex)
  
  newDF = merge(data_frame, Sex.upon.Outcome, by=c("Sex.upon.Outcome", "Sex.upon.Outcome"))
  newDF$SN  = as.factor(newDF$SN)
  newDF$Sex = as.factor(newDF$Sex)
  
  select(newDF, -Sex.upon.Outcome)
}
age_fxn = function (string)
{
  require("lubridate")
  results = unlist(strsplit(string, " "))
  # str(results)
  # str(results)
  # results[1]
  # results[2]
  
  dur = duration(num = as.integer(results[1]), units = as.character(results[2]))
  as.duration(dur)
  
}

citation = function (text="Hunter Ratliff | 2015", email="HunterRatliff1@gmail.com", 
                     text.color="black", email.color="grey")
{
  require(grid)
  require(gridExtra)
  grid.text(text,
            x = unit(0.97, "npc"), y = unit(0.03, "npc"), just = c("right", "bottom"), 
            gp = gpar(fontface = "bold", fontsize = 14, col = text.color))
  grid.text(email,
            x = unit(0.03, "npc"), y = unit(0.03, "npc"), just = c("left", "bottom"), 
            gp = gpar(fontsize = 9, col = email.color))
}
 

# ---------------------------------------------------------------------------------------------------

## Austin Animal Center FY15 Outcomes *Updated Hourly*
# https://data.austintexas.gov/Government/Austin-Animal-Center-FY15-Outcomes-Updated-Hourly-/fb53-k8de
HourOutcomes <- RSocrata::read.socrata("https://data.austintexas.gov/resource/fb53-k8de.csv")
Outcomes = mutate(
  HourOutcomes, Outcome.Date = mdy(Outcome.Date)) %>% 
  filter(Animal.Type %in% c("Cat", "Dog")) %>%
  mutate(
    Outcome.Type     = ifelse(Outcome.Type %in% c("Adoption", "Euthanasia", "Return to Owner", "Transfer"), Outcome.Type, "Other"),
    Breed            = gsub(" Mix", "", Breed)
  ) 
# group_by(HourOutcomes, Age.upon.Outcome) %>% mutate(Age = age_fxn(Age.upon.Outcome)) %>% ungroup() %>% summary()
# strsplit(HourOutcomes$Age.upon.Outcome, " ")
# unlist(strsplit(string, " "))
Outcomes = SN_Sex_replacer(Outcomes)
Outcomes = simple_breed(Outcomes, cutoff = 30)
# Outcomes$Age.upon.Outcome = seconds_to_period(Outcomes$Age.upon.Outcome)
Outcomes$Outcome.Type     = as.factor(Outcomes$Outcome.Type)
Outcomes$Outcome.Subtype  = as.factor(Outcomes$Outcome.Subtype )
Outcomes$Animal.Type      = as.factor(Outcomes$Animal.Type )
Outcomes$Animal.ID        = as.factor(Outcomes$Animal.ID)





## Austin Animal Center FY15 Intakes *Updated Hourly*
# https://data.austintexas.gov/Government/Austin-Animal-Center-FY15-Intakes-Updated-Hourly-/hjeh-idye
HourIntakes <- RSocrata::read.socrata("https://data.austintexas.gov/resource/hjeh-idye.csv")

## Austin Animal Center Stray Map
# https://data.austintexas.gov/Government/Austin-Animal-Center-Stray-Map/kz4x-q9k5
# This map shows all stray cats and dogs that are currently listed in AAC's 
# database for no longer than a week.
HourMissing <- RSocrata::read.socrata("https://data.austintexas.gov/resource/kz4x-q9k5.csv")


# ---------------------------------------------------------------------------------------------------
# AAC Euthanasia: Number of animals over year by subtype
setwd("~/Google Drive/100 - Publicly hosted/rootR/APA/outputs/AAC")
png(filename = "AAC Euthanasia: Number of animals over year by subtype.png",
    width = 1200, height = 300, units = "px")
Outcomes %>% select(everything()) %>%
  filter(Outcome.Type=="Euthanasia") %>%
  mutate(Outcome.Date = ceiling_date(Outcome.Date, unit="week")) %>%
  group_by(Outcome.Date, Outcome.Subtype, Animal.Type) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x=Outcome.Date, y=Outcome.Subtype)) + 
  ggtitle("AAC Euthanasia: \nNumber of animals over year by subtype") + labs(x="Week", y="Sub-type") +
  theme_fivethirtyeight() + guides(fill=FALSE) +# scale_alpha_continuous()
  geom_tile(aes(z=count, alpha=count, fill=Animal.Type)) + facet_wrap("Animal.Type")
citation()
dev.off()

## * * *
# AAC Outcomes: Number of animals over year by subtype
png(filename = "AAC Outcomes: Number of animals over year by subtype.png",
    width = 1200, height = 300, units = "px")
Outcomes %>% select(everything()) %>%
  mutate(Outcome.Date = ceiling_date(Outcome.Date, unit="week")) %>%
  group_by(Outcome.Date, Outcome.Type, Animal.Type) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x=Outcome.Date, y=Outcome.Type)) + 
  ggtitle("AAC Outcomes: \nNumber of animals over year by subtype") + labs(x="Week", y="Outcome") +
  theme_fivethirtyeight() + guides(fill=FALSE) +
  geom_tile(aes(z=count, alpha=count, fill=Animal.Type)) + facet_wrap("Animal.Type")
citation()
dev.off()

## -----
# Build data frame
euth.breed = Outcomes %>% select(everything()) %>% 
  group_by(Animal.Type, Breed) %>% mutate(Num.Breed = n()) %>% ungroup() %>%
  group_by(Animal.Type, Outcome.Type, Breed) %>% mutate(Num.Breed.Out = n()) %>% ungroup() %>%
  filter(Outcome.Type=="Euthanasia") %>%
  mutate(Outcome.Date = ceiling_date(Outcome.Date, unit="week")) %>%
  group_by(Animal.Type, Breed, Outcome.Subtype, Num.Breed, Num.Breed.Out) %>% 
  summarise(count = n()) %>% ungroup() %>% group_by(Animal.Type, Breed)

## * * *
# Euthanasia rates for Dogs by Breed
png(filename = "Euthanasia rates for Dogs by Breed.png",
    width = 800, height = 800, units = "px")
group_by(euth.breed, Animal.Type, Breed, Num.Breed) %>% 
  summarise(Num.Breed.Out = sum(count)) %>% ungroup() %>% 
  filter(Animal.Type=="Dog", Num.Breed.Out/Num.Breed > 0.05) %>%
  
  ggplot(aes(y=Num.Breed.Out, x=Num.Breed.Out/Num.Breed, color=Breed)) + 
  scale_x_continuous(labels=percent) + scale_y_log10() +
  scale_size_continuous(range = c(2, 5)) + scale_alpha_continuous(range=c(0.2,0.9)) + 
  ggtitle("Euthanasia rates for Dogs by Breed") + 
  labs(x="Percent of Breed euthanized", y="Number of Breed that is euthanized") + 
  theme_economist_white() + guides(color=FALSE, size=FALSE, alpha=FALSE) +
  geom_point(aes(size=Num.Breed), alpha=0.3, color="black") + 
  geom_point(aes(size=Num.Breed.Out)) + 
  geom_text(vjust=0, hjust=1, angle=80, size=4,
            aes(alpha=Num.Breed, position = "jitter", 
                label = paste0(Breed, " (", Num.Breed.Out, " of ", Num.Breed, ")")))
citation()
dev.off()        
  
## * * *
# AAC Euthanasia: Number of outcome subtypes by breed
png(filename = "AAC Euthanasia: Number of outcome subtypes by breed.png",
    width = 1500, height = 1000, units = "px")
ggplot(euth.breed, aes(x=Outcome.Subtype, y=Breed)) + 
  geom_tile(aes(z=count, fill=Num.Breed.Out/Num.Breed, alpha=count)) + 
  facet_wrap("Animal.Type", scales = "free") + 
  ggtitle("AAC Euthanasia: \nNumber of outcome subtypes by breed") + labs(x="Sub-type", y="Breed") +
  theme_fivethirtyeight() + scale_fill_continuous(name = "Percent of breed Euthanized", low="green", high="red")
citation()
dev.off()

## * * *
# AAC Euthanasia: Reason by Breed
png(filename = "AAC Euthanasia: Reason by Breed.png",
    width = 1000, height = 1000, units = "px")
ggplot(filter(euth.breed, Animal.Type=="Dog"), aes(x=Breed, y=Num.Breed.Out)) + coord_flip() + 
  ggtitle("AAC Euthanasia: Reason by Breed") + 
  geom_bar(stat="identity", aes(alpha=Num.Breed.Out/Num.Breed, fill=Outcome.Subtype)) +
  labs(x="Breed", y="Number of animals euthanized") + theme_economist_white() + 
  scale_alpha_continuous(name="Percent of Breed that is euthanized") + 
  scale_color_discrete(name="Reason for euthanasia")
citation(text = "", email = "Hunter Ratliff\nhunterratliff1@gmail.com")
dev.off()
 
# --------------------------------------------------------------------------------------------------- 
# euth.breed + geom_tile(aes(z=count, fill=count/Num.Breed.Out, alpha=Num.Breed.Out)) + 
#    facet_wrap("Animal.Type", scales = "free")
# 
# euth.breed + geom_tile(aes(z=count)) + facet_wrap("Animal.Type", scales = "free")
# euth.breed + geom_tile(aes(z=Num.Breed.Out/Num.Breed.Out)) + facet_wrap("Animal.Type", scales = "free")

  # scale_alpha_continuous(name="Number of animals euthanized") + 
  # scale_color_discrete(name="Reason for euthanasia")
  # geom_point(aes(size=Num.Breed), alpha=0.3) + geom_point(aes(size=count, color=Outcome.Subtype))
  #, fill=Num.Breed.Out/Num.Breed.Out, alpha=Num.Breed)) + 
 
# ggplot(HourOutcomes, aes(x=factor(Breed), y=factor(Outcome.Type))) + 
  # geom_violin(scale = "width") # + 
  # geom_point(aes(group=Outcome.Date, position=factor(Breed)), alpha=0.05) + 
  # geom_point(position=position_jitter(height = 0.05)) + 
  # coord_flip()
qplot(data=Outcomes, x=Outcome.Date, y=eyears(Age.upon.Outcome), color=Age.upon.Outcome, position = "jitter", alpha=0.3) + 
  # facet_grid(Animal.Type ~ Outcome.Type) +
  theme_fivethirtyeight()

qplot(data=Outcomes, x=Breed, fill=Outcome.Type, position = "fill") + coord_flip() + theme_fivethirtyeight()


  
  geom_density2d(aes())#geom_point(aes(color=Outcome.Subtype), position = "dodge")  + coord_flip() +
  theme_economist() # + facet_wrap("Outcome.Type", scales = "free", ncol = 1)



# ---------------------------------------------------------------------------------------------------


# length(levels(HourOutcomes2$Breed))
# HourOutcomes %>% separate(col=Sex.upon.Outcome, into = "SN", "sex", sep = " ") %>% View()
# arrange(desc(breed)  )
# mutate(scrambled, running = order_by(year, cumsum(value)))
# arrange(right, year)