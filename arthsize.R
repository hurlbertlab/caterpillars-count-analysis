
library(quantreg)


#----Arthropod size change over the season----

par(mar = c(2,4,1.5,1), mfrow = c(2,2), oma = c(3,1,2,2))

# LEPL

LEPL <- amsurvey.pr[amsurvey.pr$arthCode == 'LEPL',]
LEPLsize <- aggregate(LEPL$length, by = list(jd = LEPL$julianday), FUN = mean)
plot(LEPLsize$jd, LEPLsize$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("LEPL Visual Survey", line = 1.25, side = 3)
LEPLlm <- lm(LEPLsize$x ~ LEPLsize$jd)
abline(LEPLlm)

LEPLbs <- beatsheet.pr[beatsheet.pr$arthCode == 'LEPL',]
LEPLsizebs <- aggregate(LEPLbs$length, by = list(jd = LEPLbs$julianday), FUN = mean)
plot(LEPLsizebs$jd, LEPLsizebs$x, type = 'l', xlab = "", ylab = "")
mtext("LEPL Beat Sheet", line = 1.25, side = 3)
LEPLbslm <- lm(LEPLsizebs$x ~ LEPLsizebs$jd)
abline(LEPLbslm)

plot(LEPL$julianday, LEPL$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(LEPL$length ~ LEPL$julianday)
abline(quant90)

plot(LEPLbs$julianday, LEPLbs$length, xlab = "", ylab = "")
quant90 = rq(LEPLbs$length ~ LEPLbs$julianday)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# ORTH

ORTH <- amsurvey.pr[amsurvey.pr$arthCode == 'ORTH',]
ORTHsize <- aggregate(ORTH$length, by = list(jd = ORTH$julianday), FUN = mean)
plot(ORTHsize$jd, ORTHsize$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("ORTH Visual Survey", line = 1.25, side = 3)
ORTHlm <- lm(ORTHsize$x ~ ORTHsize$jd)
abline(ORTHlm)

ORTHbs <- beatsheet.pr[beatsheet.pr$arthCode == 'ORTH',]
ORTHsizebs <- aggregate(ORTHbs$length, by = list(jd = ORTHbs$julianday), FUN = mean)
plot(ORTHsizebs$jd, ORTHsizebs$x, type = 'l', xlab = "", ylab = "")
mtext("ORTH Beat Sheet", line = 1.25, side = 3)
ORTHbslm <- lm(ORTHsizebs$x ~ ORTHsizebs$jd)
abline(ORTHbslm)

plot(ORTH$julianday, ORTH$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(ORTH$length ~ ORTH$julianday)
abline(quant90)

plot(ORTHbs$julianday, ORTHbs$length, xlab = "", ylab = "")
quant90 = rq(ORTHbs$length ~ ORTHbs$julianday)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# AUCH

AUCH <- amsurvey.pr[amsurvey.pr$arthCode == 'AUCH',]
AUCHsize <- aggregate(AUCH$length, by = list(jd = AUCH$julianday), FUN = mean)
plot(AUCHsize$jd, AUCHsize$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("AUCH Visual Survey", line = 1.25, side = 3)
AUCHlm <- lm(AUCHsize$x ~ AUCHsize$jd)
abline(AUCHlm)

AUCHbs <- beatsheet.pr[beatsheet.pr$arthCode == 'AUCH',]
AUCHsizebs <- aggregate(AUCHbs$length, by = list(jd = AUCHbs$julianday), FUN = mean)
plot(AUCHsizebs$jd, AUCHsizebs$x, type = 'l', xlab = "", ylab = "")
mtext("AUCH Beat Sheet", line = 1.25, side = 3)
AUCHbslm <- lm(AUCHsizebs$x ~ AUCHsizebs$jd)
abline(AUCHbslm)

plot(AUCH$julianday, AUCH$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(AUCH$length ~ AUCH$julianday)
abline(quant90)

plot(AUCHbs$julianday, AUCHbs$length, xlab = "", ylab = "")
quant90 = rq(AUCHbs$length ~ AUCHbs$julianday)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)


# ARAN

ARAN <- amsurvey.pr[amsurvey.pr$arthCode == 'ARAN',]
ARANsize <- aggregate(ARAN$length, by = list(jd = ARAN$julianday), FUN = mean)
plot(ARANsize$jd, ARANsize$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("ARAN Visual Survey", line = 1.25, side = 3)
ARANlm <- lm(ARANsize$x ~ ARANsize$jd)
abline(ARANlm)

ARANbs <- beatsheet.pr[beatsheet.pr$arthCode == 'ARAN',]
ARANsizebs <- aggregate(ARANbs$length, by = list(jd = ARANbs$julianday), FUN = mean)
plot(ARANsizebs$jd, ARANsizebs$x, type = 'l', xlab = "", ylab = "")
mtext("ARAN Beat Sheet", line = 1.25, side = 3)
ARANbslm <- lm(ARANsizebs$x ~ ARANsizebs$jd)
abline(ARANbslm)

plot(ARAN$julianday, ARAN$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(ARAN$length ~ ARAN$julianday)
abline(quant90)

plot(ARANbs$julianday, ARANbs$length, xlab = "", ylab = "")
quant90 = rq(ARANbs$length ~ ARANbs$julianday)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)


# HEMI

HEMI <- amsurvey.pr[amsurvey.pr$arthCode == 'HEMI',]
HEMIsize <- aggregate(HEMI$length, by = list(jd = HEMI$julianday), FUN = mean)
plot(HEMIsize$jd, HEMIsize$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("HEMI Visual Survey", line = 1.25, side = 3)
HEMIlm <- lm(HEMIsize$x ~ HEMIsize$jd)
abline(HEMIlm)

HEMIbs <- beatsheet.pr[beatsheet.pr$arthCode == 'HEMI',]
HEMIsizebs <- aggregate(HEMIbs$length, by = list(jd = HEMIbs$julianday), FUN = mean)
plot(HEMIsizebs$jd, HEMIsizebs$x, type = 'l', xlab = "", ylab = "")
mtext("HEMI Beat Sheet", line = 1.25, side = 3)
HEMIbslm <- lm(HEMIsizebs$x ~ HEMIsizebs$jd)
abline(HEMIbslm)

plot(HEMI$julianday, HEMI$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(HEMI$length ~ HEMI$julianday)
abline(quant90)

plot(HEMIbs$julianday, HEMIbs$length, xlab = "", ylab = "")
quant90 = rq(HEMIbs$length ~ HEMIbs$julianday)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)


#----Stacked Bar----