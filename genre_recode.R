
library(tidyverse)

#Load in MXM data
mxm <- read_csv("~/Dropbox/music-change/mxm_results.csv")

#Standardize for analysis later
for (varnum in seq(6, 98)){
  mxm[ , varnum] <- scale(mxm[ , varnum])[, 1]
}

#Clean up genre information. Reducing the number of genres
#from 172 down to 42 different genres, about 20 of which 
#are meaningful. The rest I'm not sure how to recode,
#like opera and spoken word, which are small.
#Feel free to check my work on aligning some of these genres.

# Also code all of an artist's songs into the same genre -- their 
# most common genre (unless that's missing). 
# This might or might not be defensible, but I think 
# it mostly cuts out some poorly coded genres.
artist_genre <- mxm %>% 
  mutate(genre = recode(genre, "Underground Rap"="Hip Hop/Rap",
                        "Traditional Folk"="Folk",
                        "Teen Pop"="Pop", 
                        "Garage"="Rock",
                        "Contemporary Folk"="Folk",
                        "Contemporary Country"="Country",
                        "Celtic Folk"="Folk",
                        "Vocal Jazz"="Jazz",
                        "Latin Jazz"="Jazz",
                        "Arena Rock"="Rock",
                        "Roots Rock"="Rock",
                        "Latin Rap"="Latin",
                        "Honky Tonk"="Country",
                        "Rockabilly"="Rock",
                        "East Coast Rap"="Hip Hop/Rap",
                        "Contemporary Latin"="Latin",
                        "Contemporary Gospel"="Christian & Gospel",
                        "West Coast Rap"="Hip Hop/Rap",
                        "Dirty South"="Hip Hop/Rap",
                        "Smooth Jazz"="Jazz",
                        "Novelty"="Pop",
                        "College Rock"="Rock",
                        "Acoustic Blues"="Blues",
                        "Soft Rock"="Rock",
                        "House"="Electronic",
                        "Christian Rock"="Christian & Gospel",
                        "Punk"="Rock",
                        "Indie Rock"="Rock",
                        "Gospel"="Christian & Gospel",
                        "Christian Metal"="Christian & Gospel",
                        "Hair Metal"="Rock",
                        "Rap"="Hip Hop/Rap",
                        "Industrial"="Electronic",
                        "Electronica"="Electronic",
                        "CCM"="Christian & Gospel",
                        "Arabic Pop"="World",
                        "Britpop"="Pop",
                        "Cantopop"="World",
                        "German Folk"="World",
                        "Oldies"="R&B/Soul",
                        "Sufi"="World",
                        "Zydeco"="World",
                        "Africa"="World",
                        "Caribbean"="World",
                        "Disco"="Pop",
                        "Europe"="World",
                        "K-Pop"="World",
                        "Regional Mexicano"="Latin",
                        "Samba"="Latin",
                        "Ska"="Rock",
                        "Doo Wop"="R&B/Soul",
                        "Mandopop"="World",
                        "Soul"="R&N/Soul",
                        "Adult Contemporary"="Pop",
                        "British Invasion"="Pop",
                        "German Pop"="World",
                        "Turkish"="World",
                        "Trance"="Electronic",
                        "Celtic"="World",
                        "J-Pop"="World",
                        "American Trad Rock"="Rock",
                        "Blues-Rock"="Rock",
                        "Easy Listening"="Pop",
                        "Goth Rock"="Rock",
                        "Alternative Rap"="Hip Hop/Rap",
                        "Latin Urban"="Latin",
                        "Death Metal/Black Metal"="Heavy Metal",
                        "Hard Rock"="Rock",
                        "Adult Alternative"="Pop",
                        "Hardcore Rap"="Hip Hop/Rap",
                        "Prog-Rock/Art Rock"="Rock",
                        "Alternative & Rock in Span"="Latin",
                        "Hip-Hop"="Hip Hop/Rap",
                        "Pop/Rock"="Rock",
                        "French Pop"="World",
                        "Pop in Spanish"="Latin",
                        "Folk-Rock"="Folk",
                        "Contemporary Celtic"="World",
                        "Brazilian"="World",
                        "Contemporary Singer/Songw"="Singer/Songwriter",
                        "Rap"="Hip Hop/Rap",
                        "Americana"="Country",
                        "Gangsta Rap"="Hip Hop/Rap",
                        "Salsa y Tropical"="Latin",
                        "Christmas"="Holiday",
                        "Rock & Roll"="Rock",
                        "Psychedelic"="Electronic",
                        "New Wave"="Pop",
                        "Alternative Folk"="Folk",
                        "New Age"="Pop",
                        "Dance"="Electronic",
                        "R&N/Soul"="R&B/Soul")) %>%
  group_by(artist, genre) %>%
  select(artist, genre) %>%
  summarise(n = n()) %>%
  arrange(artist, desc(n)) %>% 
  mutate(new_genre = genre[1]) %>% 
  group_by(artist, new_genre) %>%
  summarise(count = sum(n)) %>% 
  mutate(new_genre = ifelse(is.na(new_genre), "missing", new_genre)) %>%
  group_by(artist, new_genre) %>% summarise(n = sum(count)) 


#Add "new" genre data to the main mxm data
mxm_clean <- left_join(mxm, artist_genre, by = c("artist"="artist")) %>%
  mutate(genre = new_genre) %>%
  select(-new_genre)

#Order:
#Rock: 49683
#Pop: 29406
#Alternative: 27520
#Heavy Metal: 9441
#Missing: 9185
#Country: 8304
#Christian and Gospel: 5741
#R&B/Soul: 5284
#Hip Hop/Rap: 4589
#Electornic: 4087

save(mxm_clean, file = "~/Dropbox/music-change/mxm_clean.Rdata")

     