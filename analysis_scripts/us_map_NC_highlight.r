library(maps)
library(mapproj)

foo = map('state', projection="albers", par = c(lat0=30, lat1 = 40), plot = F)

cols = rep('white', length(foo$names))
cols[grepl("north carolina", foo$names)] = 'gray90'

pdf('output/plots/paper_plots/US_map.pdf', height = 8, width = 12)
map('state', projection="albers", par = c(lat0=30, lat1 = 40), fill = T, col = cols)
dev.off()
