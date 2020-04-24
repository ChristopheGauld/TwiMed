#!/usr/bin/env Rscript
# coding=utf-8
# ==============================================================================
# description     : Geolocalisation from Twitter
# date            : 2020-04-24
# version         : 1
# ==============================================================================


### retrieve datas from Twitter

#### Geolocalisation
autist <- lat_lng(autis)
par(mar = c(0, 0, 0, 0))


#chargement carte
maps::map('world',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )
with(autist, points(lng, lat, pch = 20, cex = .25, col = rgb(0, .3, .7, .75)))
(ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +     coord_fixed(1.3)
  par(mar = c(0, 0, 0, 0)))



## search for 10,000 tweets sent from the US
autist <- search_tweets("lang:en", geocode = lookup_coords("usa"), n = 10000)
autist <- lat_lng(autist)
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25,col="grey",fill=TRUE)



##maps::map('usa',col="grey", fill=TRUE, bg="white", lwd=0.05, mar=rep(0,4),border=0, ylim=c(-80,80) )
with(autist, points(lng, lat, pch = 10, cex = .25, col = rgb(0, .3, .7, .75)))
(ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +     coord_fixed(1.3)
  par(mar = c(0, 0, 0, 0)))

