##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              FUN WITH SPOTIFYR                           ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Packages Needed                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(spotifyr)
library(ggplot2)
library(ggridges)
library(lubridate)
library(tidyverse)
library(scales)
library(RColorBrewer)
library(highcharter)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          General Spotifyr Things                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
id = "~~~~"       # From Spotify Dev API #
secret = "~~~"   # From Spotify Dev API #
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token = get_spotify_access_token()

scopes = function() {
     xml2::read_html("https://developer.spotify.com/documentation/general/guides/authorization/scopes/") %>% 
          rvest::html_elements("code") %>% rvest::html_text() %>% 
          unique()
}
assignInNamespace("scopes", scopes, ns = "spotifyr")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                    Fun Things to Extract with Spotifyr                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#...............................................................................
#                                                                              .
#  get_playlist_audio_features() returns information for extraction,           .
#  including more developer like things, and the fun metrics to use with       .
#  analysis! The arguments are the username of the Spotify user, the playlist  .
#  uris, and the authorization token:                                          .
#                                                                              .
#...............................................................................
username = 'hguthrie051'                      # you can change this to be you! #
play_uri = '0NvVaU9x3Sdj8tgTvBWbeJ'           # this is the playlist URI
#...............................................................................
#                                                                                                                             
#  The Spotfiy playlist URI for any playlist can be found by looking at the                                                   
#  URL: 
#  Top 5+                                                                                                                
#  https://open.spotify.com/playlist/0NvVaU9x3Sdj8tgTvBWbeJ?si=87ff975adbfd41a0
#  It's the characters between the / and the ? :
#  0NvVaU9x3Sdj8tgTvBWbeJ                                                                                                                           .
#................................................................................
top_5 = get_playlist("0NvVaU9x3Sdj8tgTvBWbeJ",access_token)
top_5$description                                                        # lol #
#...............................................................................
#                                                                              .
#  get_playlist_audio_features() returns all the actual cool stuff- API        .
#  related info you can extract, and all the fun metrics!                      .                                                                            .
#...............................................................................
alpen_glow = get_playlist("0E7KWV9UyAyjArnxCTQKOH", access_token)
alp_glow_feats = get_playlist_audio_features(username = username, playlist_uris = "0E7KWV9UyAyjArnxCTQKOH")
head(alp_glow_feats)

top5_feats = get_playlist_audio_features(username = username, playlist_uris = play_uri)
dusty_jg = get_playlist_audio_features(username = username, playlist_uris = "6hmU0yRH40KdJuUbpdc3gs")
dust_jg_b = get_playlist_audio_features(username = username, playlist_uris = "3Hht7bwSylxtH0qqLFyMip")
lsb404 = get_playlist_audio_features(username = username, playlist_uris = "5k9e5SyufB1KRezclYQXRE")
lsb404_b = get_playlist_audio_features(username = username, playlist_uris = "7EKUDla2KmIdKVAcgEdosP")
s_jamz = get_playlist_audio_features(username = username, playlist_uris = "1is8xVZd1WS13BLyFGVDDv")
s_jamz_2 = get_playlist_audio_features(username = username, playlist_uris = "4UfUSqJANH3JXLvkW5BbGB")

plays = rbind(top5_feats, dusty_jg, lsb404,dust_jg_b, lsb404_b, alp_glow_feats, s_jamz, s_jamz_2)

#...............................................................................
#                                                                              .
#  Valence is a measure of how somber a track sounds, bounded between 0 and    .
#  1. Tracks that sound more 'positive' will have a valence score closer to    .
#  1, while tracks that sound more 'negative' will have a valence score        .
#  closer to 0. The important note here is that valence doesn't account for    .
#  any lyricism choices in the track, it's a metric based on sound alone. So,  .
#  how do our playlists rank up, according to spotify?                         .
#                                                                              .
#...............................................................................
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Plots that SLAP                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#...............................................................................
#                                                                              .
#  From the ggridges documentation: 'geom_density_ridges arranges multiple     .
#  density plots in a staggered fashion, as in the cover of the famous Joy     .
#  Division album Unknown Pleasures', so let's see how the playlists stack     .
#  up...                                                                       .                                                                             .
#...............................................................................

## The below makes labels for the plot ##
play_names_label = plays %>%
     arrange(playlist_name) %>%
     mutate(label = str_glue('{playlist_name}')) %>%
     pull(label) %>%
     unique

## Here's the sick plot ##
play_val = ggplot(plays, aes(x = valence, y = as.character(playlist_name), fill = playlist_name))+
     geom_density_ridges() +
     scale_fill_cyclical(values = c("#5F4B8B","#7CB7A3", "#7CB7A3", "#CC79A7","#AE2573", "#2B4278", "#27234F","#7B768D"))+
     theme_ridges(center_axis_labels = TRUE, grid = FALSE, font_size = 6) +
     theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           plot.subtitle = element_text(size = 10, hjust = 0.6)) +
     ggtitle('Are H and G Okay?', 'Density Plots of Valence as a Measure of Melancholy, by Playlist') +
     labs(x = 'Song Valence', y = '') +
     scale_x_continuous(breaks = c(0,.25,.5,.75,1)) +
     scale_y_discrete(labels = play_names_label)

play_val

#...............................................................................
#                                                                              .
#  get_artist_audio_features() works similarly to the                          .
#  get_playlist_audio_features() command, but returns the same information     .
#  for the entire discography available on spotify:                            .
#                                                                              .
#...............................................................................
mac = get_artist_audio_features(artist = "Mac Miller")

## Here I'm extracting just the studio albums, along with metrics I'll use 
## in a bit
studio_albums = c('Blue Slide Park', 'Watching Movies with the Sound Off (Deluxe Edition)', 'GO:OD AM',
                   'The Divine Feminine', 'Swimming', 'Circles')

albs_mac <- filter(mac, album_name %in% studio_albums) %>%
     select(explicit,track_name, album_name, valence, energy, speechiness, loudness, tempo, duration_ms, album_release_year,album_images) %>%
     unique()

## Like above, here is the label I'll use for the following plots: ##
# make label for plot
alb_names_label <- albs_mac %>%
     arrange(album_release_year) %>%
     mutate(label = str_glue('{album_name} ({album_release_year})')) %>%
     pull(label) %>%
     unique

## How sad do the studio albums sound? ##
ggplot(albs_mac, aes(x = valence, y = as.character(album_name), fill = album_name))+
     geom_density_ridges() +
     scale_fill_cyclical(values = c("#00478B","#DA291C","#D1D2D1","#FCC2CC","#A04E78","#53565A"))+
     theme_ridges(center_axis_labels = TRUE, grid = FALSE, font_size = 6) +
     theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           plot.subtitle = element_text(size = 10, hjust = 0.6)) +
     ggtitle('Mac Miller Plot 1!', 'Density Plots of Track Valence, by Album') +
     labs(x = 'Song Valence', y = '') +
     scale_x_continuous(breaks = c(0,.25,.5,.75,1)) +
     scale_y_discrete(labels = alb_names_label)

## How energetic do the studio albums sound? ##
ggplot(albs_mac, aes(x = energy, y = as.character(album_name), fill = album_name))+
     geom_density_ridges() +
     scale_fill_cyclical(values = c("#00478B","#DA291C","#D1D2D1","#FCC2CC","#A04E78","#53565A"))+
     theme_ridges(center_axis_labels = TRUE, grid = FALSE, font_size = 6) +
     theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           plot.subtitle = element_text(size = 10, hjust = 0.6)) +
     ggtitle('Mac Miller Plot 2!', 'Density Plots of Track Energy, by Album') +
     labs(x = 'Song Energy', y = '') +
     scale_x_continuous(breaks = c(0,.25,.5,.75,1)) +
     scale_y_discrete(labels = alb_names_label)
#...............................................................................                                                                           .
#  'speechiness' is a metric that detects presence of spoken words in a        .
#  track. Allegedly, Rap should have higher 'speechiness' frequencies.         .                                                                            .
#...............................................................................
ggplot(albs_mac, aes(x = speechiness, y = as.character(album_name), fill = album_name))+
     geom_density_ridges() +
     scale_fill_cyclical(values = c("#00478B","#DA291C","#D1D2D1","#FCC2CC","#A04E78","#53565A"))+
     theme_ridges(center_axis_labels = TRUE, grid = FALSE, font_size = 6) +
     theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0),
           plot.subtitle = element_text(size = 10, hjust = 0.6)) +
     ggtitle('Mac Miller Plot 3!', 'Density Plots of Track Speechiness, by Album') +
     labs(x = 'Song Speechiness', y = '') +
     scale_x_continuous(breaks = c(0,.25,.5,.75,1)) +
     scale_y_discrete(labels = alb_names_label)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##                              LYRICAL ANALYSIS                            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#...............................................................................                                                                            .
#  For ease, I've collected the above metrics and lyrics from web scraping     .
#  into something more clean- call it below:                                   .                                                                             .
#...............................................................................
mac_miller = read.csv(file.choose(), header = TRUE)
# This is removing some duplicates I missed #
mac_miller = mac_miller[-c(88:95),] 
mac_miller_2 = mac_miller                                        # for ease... #

#...............................................................................                                                                            .
#  The below brings in the lexicon used to analyze the lyrics:                 .                                                                           .
#...............................................................................
library(tidytext)

nrc = get_sentiments("nrc") %>% 
     mutate(lexicon = "nrc", 
            words_in_lexicon = n_distinct(word))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                   How Sad is the Mac Miller Discography?                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sad_words = nrc %>%
     filter(sentiment == 'sadness') %>%
     select(word) %>%
     mutate(sad = T)

## The below filters out words that aren't useful to qualifying sentiment ##
sent_df =  mac_miller %>%
     unnest_tokens(word, lyrics) %>%
     anti_join(stop_words, by = 'word') %>%
     left_join(sad_words, by = 'word') %>%
     group_by(track_name) %>%
     summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
               word_count = n()) %>%
     ungroup

## Calculates the percentage of sad words in each track ##
sent_df %>%
     select(pct_sad, track_name) %>%
     arrange(-pct_sad) %>%
     head(10)

### Gloom Index ###

mac_miller = mac_miller %>%
     left_join(sent_df, by = 'track_name') %>%
     mutate_at(c('pct_sad', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>%
     mutate(lyrical_density = word_count / duration_ms * 1000,
            gloom_index = round(rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2, to = c(1, 100)), 2))
mac_miller %>%
     select(gloom_index, track_name) %>%
     arrange(gloom_index) %>%
     head(10)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                          Plots that SLAP: Part Two                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Plotting Gloom Index ###
#...............................................................................                                                                           .
#  Below, I'm extracting the album images from the                             .
#  get_artist_audio_features() information:                                    .                                                                           .
#...............................................................................
mac_imgs = mac[,c(5,30,36)]
mac_imgs = filter(mac_imgs, album_name %in% studio_albums) %>%
     unique()
mac_imgs = mac_imgs[-c(36:62),]

a_img = matrix(data = NA, nrow = 87, ncol = 1)
for(i in 1:nrow(mac_imgs)){
     img = mac_imgs[[1]][[i]]
     album_img = img[2,2]
     a_img[i] = album_img
}

mac_miller$album_img = a_img

#...............................................................................                                                                          .
#  Now I'm using the above to make the interactive plot:                       .                                                                           .
#...............................................................................
plot_df = mac_miller %>%
     rowwise %>%
     mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Track:</b> ', track_name)) %>%
     ungroup

avg_line = plot_df %>%
     group_by(album_release_year, album_name, album_img) %>%
     summarise(avg = mean(gloom_index)) %>%
     ungroup %>%
     transmute(x = as.numeric(as.factor(album_release_year)),
               y = avg,
               tooltip = paste0('<a style = "margin-right:55px">',
                                '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                                '<b>Album:</b> ', album_name,
                                '<br><b>Average Gloom Index:</b> ', round(avg, 2),
                                '</a>'))
plot_track_df = plot_df %>%
     mutate(tooltip = paste0(tooltip, '<br><b>Gloom Index:</b> ', gloom_index, '</a>'),
            album_number = as.numeric(as.factor(album_release_year))) %>%
     ungroup

album_chart = hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = gloom_index, group = album_name)) %>%
     hc_add_series(data = avg_line, type = 'line') %>%
     hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
     hc_colors(c(sample(brewer.pal(n_distinct(mac_miller$album_name), 'Paired')), 'black')) %>%
     hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>%
     hc_yAxis(max = 100, title = list(text = 'Gloom Index')) %>%
     hc_title(text = 'Mac Miller Melancholia, By Album') %>%
     hc_subtitle(text = 'Depression, Statistically') %>%
     hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[7]]$name <- 'Album Averages'
album_chart

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  How Angry is the Mac Miller Discography?                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
angry_words = nrc %>%
     filter(sentiment == 'anger') %>%
     select(word) %>%
     mutate(anger = T)
head(angry_words)

anger_df = mac_miller_2 %>%
     unnest_tokens(word, lyrics) %>%
     anti_join(stop_words, by = 'word') %>%
     left_join(angry_words, by = 'word') %>%
     group_by(track_name) %>%
     summarise(pct_anger = round(sum(anger, na.rm = T) / n(), 4),
               word_count = n()) %>%
     ungroup

anger_df %>%
     select(pct_anger, track_name) %>%
     arrange(-pct_anger) %>%
     head(10)

mac_miller_2 = mac_miller_2 %>%
     left_join(anger_df, by = 'track_name')%>%
     mutate_at(c('pct_anger', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>%
     mutate(lyrical_density = word_count / duration_ms * 1000,
            anger_index = round(rescale((lyrical_density^0.5)*(pct_anger^0.5), to = c(1, 100)), 2))
mac_miller_2 %>%
     select(anger_index, track_name) %>%
     arrange(anger_index)
  
mac_miller_2$album_img = a_img

plot_df = mac_miller_2 %>%
     rowwise %>%
     mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Track:</b> ', track_name)) %>%
     ungroup

avg_line = plot_df %>%
     group_by(album_release_year, album_name, album_img) %>%
     summarise(avg = mean(anger_index)) %>%
     ungroup %>%
     transmute(x = as.numeric(as.factor(album_release_year)),
               y = avg,
               tooltip = paste0('<a style = "margin-right:55px">',
                                '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                                '<b>Album:</b> ', album_name,
                                '<br><b>Average Anger Index:</b> ', round(avg, 2),
                                '</a>'))
plot_track_df = plot_df %>%
     mutate(tooltip = paste0(tooltip, '<br><b>Anger Index:</b> ', anger_index, '</a>'),
            album_number = as.numeric(as.factor(album_release_year))) %>%
     ungroup

album_chart = hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = anger_index, group = album_name)) %>%
     hc_add_series(data = avg_line, type = 'line') %>%
     hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>%
     hc_colors(c(sample(brewer.pal(n_distinct(mac_miller$album_name), 'Paired')), 'black')) %>%
     hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>%
     hc_yAxis(max = 100, title = list(text = 'Anger Index')) %>%
     hc_title(text = 'Mac Miller Malcontent, By Album') %>%
     hc_subtitle(text = 'Rage, Statistically') %>%
     hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[7]]$name <- 'Album Averages'
album_chart
