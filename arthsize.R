# Arthropod size change over the season


lepl <- amsurvey.pr[amsurvey.pr$arthCode == 'LEPL',]
leplsize <- aggregate(lepl$length, by = list(jd = lepl$julianday), FUN = mean)
plot(leplsize$jd, leplsize$x, type = 'l')

leplbs <- beatsheet.pr[beatsheet.pr$arthCode == 'LEPL',]
leplsizebs <- aggregate(leplbs$length, by = list(jd = leplbs$julianday), FUN = mean)
plot(leplsizebs$jd, leplsizebs$x, type = 'l')

orth <- amsurvey.pr[amsurvey.pr$arthCode == 'ORTH',]
orthsize <- aggregate(orth$length, by = list(jd = orth$julianday), FUN = mean)
plot(orthsize$jd, orthsize$x, type = 'l')

orthbs <- beatsheet.pr[beatsheet.pr$arthCode == 'ORTH',]
orthsizebs <- aggregate(orthbs$length, by = list(jd = orthbs$julianday), FUN = mean)
plot(orthsizebs$jd, orthsizebs$x, type = 'l')