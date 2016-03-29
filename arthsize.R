
library(quantreg)

# Change data to include smaller sizes (not just less than 5 mm)
amsurvey.prall <- surveySubset(cleandata.pr, subset = "visual am")
beatsheet.prall <- surveySubset(cleandata.pr, subset = "beat sheet")

amsurvey.bgall <- surveySubset(cleandata.bg, subset = "visual am")
beatsheet.bgall <- surveySubset(cleandata.bg, subset = "beat sheet")


#----Arthropod size change over the season----

# Prairie Ridge

par(mar = c(2,4,1.5,1), mfrow = c(2,2), oma = c(3,1,2,2))

# ARAN

ARAN.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'ARAN',]
ARANsize.pr <- aggregate(ARAN.pr$length, by = list(jd = ARAN.pr$julianday), FUN = mean)
plot(ARANsize.pr$jd, ARANsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("ARAN Visual Survey", line = 1.25, side = 3)
ARANlm.pr <- lm(ARANsize.pr$x ~ ARANsize.pr$jd)
abline(ARANlm.pr)

ARANbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'ARAN',]
ARANsizebs.pr <- aggregate(ARANbs.pr$length, by = list(jd = ARANbs.pr$julianday), FUN = mean)
plot(ARANsizebs.pr$jd, ARANsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("ARAN Beat Sheet", line = 1.25, side = 3)
ARANbslm.pr <- lm(ARANsizebs.pr$x ~ ARANsizebs.pr$jd)
abline(ARANbslm.pr)

plot(ARAN.pr$julianday, ARAN.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(ARAN.pr$length ~ ARAN.pr$julianday, 0.9)
abline(quant90)

plot(ARANbs.pr$julianday, ARANbs.pr$length, xlab = "", ylab = "")
quant90 = rq(ARANbs.pr$length ~ ARANbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# AUCH

AUCH.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'AUCH',]
AUCHsize.pr <- aggregate(AUCH.pr$length, by = list(jd = AUCH.pr$julianday), FUN = mean)
plot(AUCHsize.pr$jd, AUCHsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("AUCH Visual Survey", line = 1.25, side = 3)
AUCHlm.pr <- lm(AUCHsize.pr$x ~ AUCHsize.pr$jd)
abline(AUCHlm.pr)

AUCHbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'AUCH',]
AUCHsizebs.pr <- aggregate(AUCHbs.pr$length, by = list(jd = AUCHbs.pr$julianday), FUN = mean)
plot(AUCHsizebs.pr$jd, AUCHsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("AUCH Beat Sheet", line = 1.25, side = 3)
AUCHbslm.pr <- lm(AUCHsizebs.pr$x ~ AUCHsizebs.pr$jd)
abline(AUCHbslm.pr)

plot(AUCH.pr$julianday, AUCH.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(AUCH.pr$length ~ AUCH.pr$julianday, 0.9)
abline(quant90)

plot(AUCHbs.pr$julianday, AUCHbs.pr$length, xlab = "", ylab = "")
quant90 = rq(AUCHbs.pr$length ~ AUCHbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# COLE

COLE.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'COLE',]
COLEsize.pr <- aggregate(COLE.pr$length, by = list(jd = COLE.pr$julianday), FUN = mean)
plot(COLEsize.pr$jd, COLEsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("COLE Visual Survey", line = 1.25, side = 3)
COLElm.pr <- lm(COLEsize.pr$x ~ COLEsize.pr$jd)
abline(COLElm.pr)

COLEbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'COLE',]
COLEsizebs.pr <- aggregate(COLEbs.pr$length, by = list(jd = COLEbs.pr$julianday), FUN = mean)
plot(COLEsizebs.pr$jd, COLEsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("COLE Beat Sheet", line = 1.25, side = 3)
COLEbslm.pr <- lm(COLEsizebs.pr$x ~ COLEsizebs.pr$jd)
abline(COLEbslm.pr)

plot(COLE.pr$julianday, COLE.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(COLE.pr$length ~ COLE.pr$julianday, 0.9)
abline(quant90)

plot(COLEbs.pr$julianday, COLEbs.pr$length, xlab = "", ylab = "")
quant90 = rq(COLEbs.pr$length ~ COLEbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# DIPT

DIPT.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'DIPT',]
DIPTsize.pr <- aggregate(DIPT.pr$length, by = list(jd = DIPT.pr$julianday), FUN = mean)
plot(DIPTsize.pr$jd, DIPTsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("DIPT Visual Survey", line = 1.25, side = 3)
DIPTlm.pr <- lm(DIPTsize.pr$x ~ DIPTsize.pr$jd)
abline(DIPTlm.pr)

DIPTbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'DIPT',]
DIPTsizebs.pr <- aggregate(DIPTbs.pr$length, by = list(jd = DIPTbs.pr$julianday), FUN = mean)
plot(DIPTsizebs.pr$jd, DIPTsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("DIPT Beat Sheet", line = 1.25, side = 3)
DIPTbslm.pr <- lm(DIPTsizebs.pr$x ~ DIPTsizebs.pr$jd)
abline(DIPTbslm.pr)

plot(DIPT.pr$julianday, DIPT.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(DIPT.pr$length ~ DIPT.pr$julianday, 0.9)
abline(quant90)

plot(DIPTbs.pr$julianday, DIPTbs.pr$length, xlab = "", ylab = "")
quant90 = rq(DIPTbs.pr$length ~ DIPTbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# LEPL

LEPL.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'LEPL',]
LEPLsize.pr <- aggregate(LEPL.pr$length, by = list(jd = LEPL.pr$julianday), FUN = mean)
plot(LEPLsize.pr$jd, LEPLsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("LEPL Visual Survey", line = 1.25, side = 3)
LEPLlm.pr <- lm(LEPLsize.pr$x ~ LEPLsize.pr$jd)
abline(LEPLlm.pr)

LEPLbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'LEPL',]
LEPLsizebs.pr <- aggregate(LEPLbs.pr$length, by = list(jd = LEPLbs.pr$julianday), FUN = mean)
plot(LEPLsizebs.pr$jd, LEPLsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("LEPL Beat Sheet", line = 1.25, side = 3)
LEPLbslm.pr <- lm(LEPLsizebs.pr$x ~ LEPLsizebs.pr$jd)
abline(LEPLbslm.pr)

plot(LEPL.pr$julianday, LEPL.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(LEPL.pr$length ~ LEPL.pr$julianday, 0.9)
abline(quant90)

plot(LEPLbs.pr$julianday, LEPLbs.pr$length, xlab = "", ylab = "")
quant90 = rq(LEPLbs.pr$length ~ LEPLbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# OPIL

OPIL.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'OPIL',]
OPILsize.pr <- aggregate(OPIL.pr$length, by = list(jd = OPIL.pr$julianday), FUN = mean)
plot(OPILsize.pr$jd, OPILsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("OPIL Visual Survey", line = 1.25, side = 3)
OPILlm.pr <- lm(OPILsize.pr$x ~ OPILsize.pr$jd)
abline(OPILlm.pr)

OPILbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'OPIL',]
OPILsizebs.pr <- aggregate(OPILbs.pr$length, by = list(jd = OPILbs.pr$julianday), FUN = mean)
plot(OPILsizebs.pr$jd, OPILsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("OPIL Beat Sheet", line = 1.25, side = 3)
OPILbslm.pr <- lm(OPILsizebs.pr$x ~ OPILsizebs.pr$jd)
abline(OPILbslm.pr)

plot(OPIL.pr$julianday, OPIL.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(OPIL.pr$length ~ OPIL.pr$julianday, 0.9)
abline(quant90)

plot(OPILbs.pr$julianday, OPILbs.pr$length, xlab = "", ylab = "")
quant90 = rq(OPILbs.pr$length ~ OPILbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# ORTH

ORTH.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'ORTH',]
ORTHsize.pr <- aggregate(ORTH.pr$length, by = list(jd = ORTH.pr$julianday), FUN = mean)
plot(ORTHsize.pr$jd, ORTHsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("ORTH Visual Survey", line = 1.25, side = 3)
ORTHlm.pr <- lm(ORTHsize.pr$x ~ ORTHsize.pr$jd)
abline(ORTHlm.pr)

ORTHbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'ORTH',]
ORTHsizebs.pr <- aggregate(ORTHbs.pr$length, by = list(jd = ORTHbs.pr$julianday), FUN = mean)
plot(ORTHsizebs.pr$jd, ORTHsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("ORTH Beat Sheet", line = 1.25, side = 3)
ORTHbslm.pr <- lm(ORTHsizebs.pr$x ~ ORTHsizebs.pr$jd)
abline(ORTHbslm.pr)

plot(ORTH.pr$julianday, ORTH.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(ORTH.pr$length ~ ORTH.pr$julianday, 0.9)
abline(quant90)

plot(ORTHbs.pr$julianday, ORTHbs.pr$length, xlab = "", ylab = "")
quant90 = rq(ORTHbs.pr$length ~ ORTHbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# HEMI

HEMI.pr <- amsurvey.prall[amsurvey.prall$arthCode == 'HEMI',]
HEMIsize.pr <- aggregate(HEMI.pr$length, by = list(jd = HEMI.pr$julianday), FUN = mean)
plot(HEMIsize.pr$jd, HEMIsize.pr$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("HEMI Visual Survey", line = 1.25, side = 3)
HEMIlm.pr <- lm(HEMIsize.pr$x ~ HEMIsize.pr$jd)
abline(HEMIlm.pr)

HEMIbs.pr <- beatsheet.prall[beatsheet.prall$arthCode == 'HEMI',]
HEMIsizebs.pr <- aggregate(HEMIbs.pr$length, by = list(jd = HEMIbs.pr$julianday), FUN = mean)
plot(HEMIsizebs.pr$jd, HEMIsizebs.pr$x, type = 'l', xlab = "", ylab = "")
mtext("HEMI Beat Sheet", line = 1.25, side = 3)
HEMIbslm.pr <- lm(HEMIsizebs.pr$x ~ HEMIsizebs.pr$jd)
abline(HEMIbslm.pr)

plot(HEMI.pr$julianday, HEMI.pr$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(HEMI.pr$length ~ HEMI.pr$julianday, 0.9)
abline(quant90)

plot(HEMIbs.pr$julianday, HEMIbs.pr$length, xlab = "", ylab = "")
quant90 = rq(HEMIbs.pr$length ~ HEMIbs.pr$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)



# Grouped plots

# Visual Survey

par(mar = c(4,4,4,2), mfrow = c(1,1), oma = c(0,0,0,0))
plot(5,5, type = 'n', ylim = c(0,20), xlim = c(135,204), ylab = "Average Length (mm)", xlab = "Julian day")
title('Prairie Ridge Visual Survey Average Length')

abline(ARANlm.pr, col = 'cadetblue', lwd = 2)
abline(AUCHlm.pr, col = 'chartreuse', lwd = 2)
abline(COLElm.pr, col = 'red', lwd = 2)
abline(DIPTlm.pr, col = 'orange', lwd = 2)
abline(LEPLlm.pr, col = 'plum', lwd = 2)
abline(OPILlm.pr, col = 'royalblue', lwd = 2)
abline(ORTHlm.pr, col = 'magenta4', lwd = 2)
abline(HEMIlm.pr, col = 'yellow', lwd = 2)

#legend()

# Beat sheets

plot(5,5, type = 'n', ylim = c(0,20), xlim = c(135,204), ylab = "Average Length (mm)", xlab = "Julian day")
title('Prairie Ridge Beat Sheet Average Length')

abline(ARANbslm.pr, col = 'cadetblue', lwd = 2)
abline(AUCHbslm.pr, col = 'chartreuse', lwd = 2)
abline(COLEbslm.pr, col = 'red', lwd = 2)
abline(DIPTbslm.pr, col = 'orange', lwd = 2)
abline(LEPLbslm.pr, col = 'plum', lwd = 2)
abline(OPILbslm.pr, col = 'royalblue', lwd = 2)
abline(ORTHbslm.pr, col = 'magenta4', lwd = 2)
abline(HEMIbslm.pr, col = 'yellow', lwd = 2)





# Botanical Garden

par(mar = c(2,4,1.5,1), mfrow = c(2,2), oma = c(3,1,2,2))

# ARAN

ARAN.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'ARAN',]
ARANsize.bg <- aggregate(ARAN.bg$length, by = list(jd = ARAN.bg$julianday), FUN = mean)
plot(ARANsize.bg$jd, ARANsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("ARAN Visual Survey", line = 1.25, side = 3)
ARANlm.bg <- lm(ARANsize.bg$x ~ ARANsize.bg$jd)
abline(ARANlm.bg)

ARANbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'ARAN',]
ARANsizebs.bg <- aggregate(ARANbs.bg$length, by = list(jd = ARANbs.bg$julianday), FUN = mean)
plot(ARANsizebs.bg$jd, ARANsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("ARAN Beat Sheet", line = 1.25, side = 3)
ARANbslm.bg <- lm(ARANsizebs.bg$x ~ ARANsizebs.bg$jd)
abline(ARANbslm.bg)

plot(ARAN.bg$julianday, ARAN.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(ARAN.bg$length ~ ARAN.bg$julianday, 0.9)
abline(quant90)

plot(ARANbs.bg$julianday, ARANbs.bg$length, xlab = "", ylab = "")
quant90 = rq(ARANbs.bg$length ~ ARANbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# AUCH

AUCH.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'AUCH',]
AUCHsize.bg <- aggregate(AUCH.bg$length, by = list(jd = AUCH.bg$julianday), FUN = mean)
plot(AUCHsize.bg$jd, AUCHsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("AUCH Visual Survey", line = 1.25, side = 3)
AUCHlm.bg <- lm(AUCHsize.bg$x ~ AUCHsize.bg$jd)
abline(AUCHlm.bg)

AUCHbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'AUCH',]
AUCHsizebs.bg <- aggregate(AUCHbs.bg$length, by = list(jd = AUCHbs.bg$julianday), FUN = mean)
plot(AUCHsizebs.bg$jd, AUCHsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("AUCH Beat Sheet", line = 1.25, side = 3)
AUCHbslm.bg <- lm(AUCHsizebs.bg$x ~ AUCHsizebs.bg$jd)
abline(AUCHbslm.bg)

plot(AUCH.bg$julianday, AUCH.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(AUCH.bg$length ~ AUCH.bg$julianday, 0.9)
abline(quant90)

plot(AUCHbs.bg$julianday, AUCHbs.bg$length, xlab = "", ylab = "")
quant90 = rq(AUCHbs.bg$length ~ AUCHbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# COLE

COLE.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'COLE',]
COLEsize.bg <- aggregate(COLE.bg$length, by = list(jd = COLE.bg$julianday), FUN = mean)
plot(COLEsize.bg$jd, COLEsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("COLE Visual Survey", line = 1.25, side = 3)
COLElm.bg <- lm(COLEsize.bg$x ~ COLEsize.bg$jd)
abline(COLElm.bg)

COLEbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'COLE',]
COLEsizebs.bg <- aggregate(COLEbs.bg$length, by = list(jd = COLEbs.bg$julianday), FUN = mean)
plot(COLEsizebs.bg$jd, COLEsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("COLE Beat Sheet", line = 1.25, side = 3)
COLEbslm.bg <- lm(COLEsizebs.bg$x ~ COLEsizebs.bg$jd)
abline(COLEbslm.bg)

plot(COLE.bg$julianday, COLE.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(COLE.bg$length ~ COLE.bg$julianday, 0.9)
abline(quant90)

plot(COLEbs.bg$julianday, COLEbs.bg$length, xlab = "", ylab = "")
quant90 = rq(COLEbs.bg$length ~ COLEbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# DIPT

DIPT.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'DIPT',]
DIPTsize.bg <- aggregate(DIPT.bg$length, by = list(jd = DIPT.bg$julianday), FUN = mean)
plot(DIPTsize.bg$jd, DIPTsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("DIPT Visual Survey", line = 1.25, side = 3)
DIPTlm.bg <- lm(DIPTsize.bg$x ~ DIPTsize.bg$jd)
abline(DIPTlm.bg)

DIPTbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'DIPT',]
DIPTsizebs.bg <- aggregate(DIPTbs.bg$length, by = list(jd = DIPTbs.bg$julianday), FUN = mean)
plot(DIPTsizebs.bg$jd, DIPTsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("DIPT Beat Sheet", line = 1.25, side = 3)
DIPTbslm.bg <- lm(DIPTsizebs.bg$x ~ DIPTsizebs.bg$jd)
abline(DIPTbslm.bg)

plot(DIPT.bg$julianday, DIPT.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(DIPT.bg$length ~ DIPT.bg$julianday, 0.9)
abline(quant90)

plot(DIPTbs.bg$julianday, DIPTbs.bg$length, xlab = "", ylab = "")
quant90 = rq(DIPTbs.bg$length ~ DIPTbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# LEPL

LEPL.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'LEPL',]
LEPLsize.bg <- aggregate(LEPL.bg$length, by = list(jd = LEPL.bg$julianday), FUN = mean)
plot(LEPLsize.bg$jd, LEPLsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("LEPL Visual Survey", line = 1.25, side = 3)
LEPLlm.bg <- lm(LEPLsize.bg$x ~ LEPLsize.bg$jd)
abline(LEPLlm.bg)

LEPLbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'LEPL',]
LEPLsizebs.bg <- aggregate(LEPLbs.bg$length, by = list(jd = LEPLbs.bg$julianday), FUN = mean)
plot(LEPLsizebs.bg$jd, LEPLsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("LEPL Beat Sheet", line = 1.25, side = 3)
LEPLbslm.bg <- lm(LEPLsizebs.bg$x ~ LEPLsizebs.bg$jd)
abline(LEPLbslm.bg)

plot(LEPL.bg$julianday, LEPL.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(LEPL.bg$length ~ LEPL.bg$julianday, 0.9)
abline(quant90)

plot(LEPLbs.bg$julianday, LEPLbs.bg$length, xlab = "", ylab = "")
quant90 = rq(LEPLbs.bg$length ~ LEPLbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# OPIL

OPIL.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'OPIL',]
OPILsize.bg <- aggregate(OPIL.bg$length, by = list(jd = OPIL.bg$julianday), FUN = mean)
plot(OPILsize.bg$jd, OPILsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("OPIL Visual Survey", line = 1.25, side = 3)
OPILlm.bg <- lm(OPILsize.bg$x ~ OPILsize.bg$jd)
abline(OPILlm.bg)

OPILbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'OPIL',]
OPILsizebs.bg <- aggregate(OPILbs.bg$length, by = list(jd = OPILbs.bg$julianday), FUN = mean)
plot(OPILsizebs.bg$jd, OPILsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("OPIL Beat Sheet", line = 1.25, side = 3)
OPILbslm.bg <- lm(OPILsizebs.bg$x ~ OPILsizebs.bg$jd)
abline(OPILbslm.bg)

plot(OPIL.bg$julianday, OPIL.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(OPIL.bg$length ~ OPIL.bg$julianday, 0.9)
abline(quant90)

plot(OPILbs.bg$julianday, OPILbs.bg$length, xlab = "", ylab = "")
quant90 = rq(OPILbs.bg$length ~ OPILbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# ORTH

ORTH.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'ORTH',]
ORTHsize.bg <- aggregate(ORTH.bg$length, by = list(jd = ORTH.bg$julianday), FUN = mean)
plot(ORTHsize.bg$jd, ORTHsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("ORTH Visual Survey", line = 1.25, side = 3)
ORTHlm.bg <- lm(ORTHsize.bg$x ~ ORTHsize.bg$jd)
abline(ORTHlm.bg)

ORTHbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'ORTH',]
ORTHsizebs.bg <- aggregate(ORTHbs.bg$length, by = list(jd = ORTHbs.bg$julianday), FUN = mean)
plot(ORTHsizebs.bg$jd, ORTHsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("ORTH Beat Sheet", line = 1.25, side = 3)
ORTHbslm.bg <- lm(ORTHsizebs.bg$x ~ ORTHsizebs.bg$jd)
abline(ORTHbslm.bg)

plot(ORTH.bg$julianday, ORTH.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(ORTH.bg$length ~ ORTH.bg$julianday, 0.9)
abline(quant90)

plot(ORTHbs.bg$julianday, ORTHbs.bg$length, xlab = "", ylab = "")
quant90 = rq(ORTHbs.bg$length ~ ORTHbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)

# HEMI

HEMI.bg <- amsurvey.bgall[amsurvey.bgall$arthCode == 'HEMI',]
HEMIsize.bg <- aggregate(HEMI.bg$length, by = list(jd = HEMI.bg$julianday), FUN = mean)
plot(HEMIsize.bg$jd, HEMIsize.bg$x, type = 'l', xlab = "", ylab = "Average Length (mm)")
mtext("HEMI Visual Survey", line = 1.25, side = 3)
HEMIlm.bg <- lm(HEMIsize.bg$x ~ HEMIsize.bg$jd)
abline(HEMIlm.bg)

HEMIbs.bg <- beatsheet.bgall[beatsheet.bgall$arthCode == 'HEMI',]
HEMIsizebs.bg <- aggregate(HEMIbs.bg$length, by = list(jd = HEMIbs.bg$julianday), FUN = mean)
plot(HEMIsizebs.bg$jd, HEMIsizebs.bg$x, type = 'l', xlab = "", ylab = "")
mtext("HEMI Beat Sheet", line = 1.25, side = 3)
HEMIbslm.bg <- lm(HEMIsizebs.bg$x ~ HEMIsizebs.bg$jd)
abline(HEMIbslm.bg)

plot(HEMI.bg$julianday, HEMI.bg$length, xlab = "", ylab = "Length (mm)")
quant90 = rq(HEMI.bg$length ~ HEMI.bg$julianday, 0.9)
abline(quant90)

plot(HEMIbs.bg$julianday, HEMIbs.bg$length, xlab = "", ylab = "")
quant90 = rq(HEMIbs.bg$length ~ HEMIbs.bg$julianday, 0.9)
abline(quant90)

mtext('Julian day', side = 1, outer = TRUE, line = 1.5)



# Grouped plots

# Visual Survey

par(mar = c(4,4,4,2), mfrow = c(1,1), oma = c(0,0,0,0))
plot(5,5, type = 'n', ylim = c(0,20), xlim = c(135,204), ylab = "Average Length (mm)", xlab = "Julian day")
title('Botanical Garden Visual Survey Average Length')

abline(ARANlm.bg, col = 'cadetblue', lwd = 2)
abline(AUCHlm.bg, col = 'chartreuse', lwd = 2)
abline(COLElm.bg, col = 'red', lwd = 2)
abline(DIPTlm.bg, col = 'orange', lwd = 2)
abline(LEPLlm.bg, col = 'plum', lwd = 2)
abline(OPILlm.bg, col = 'royalblue', lwd = 2)
abline(ORTHlm.bg, col = 'magenta4', lwd = 2)
abline(HEMIlm.bg, col = 'yellow', lwd = 2)

#legend()

# Beat sheets

plot(5,5, type = 'n', ylim = c(0,20), xlim = c(135,204), ylab = "Average Length (mm)", xlab = "Julian day")
title('Botanical Garden Beat Sheet Average Length')

abline(ARANbslm.bg, col = 'cadetblue', lwd = 2)
abline(AUCHbslm.bg, col = 'chartreuse', lwd = 2)
abline(COLEbslm.bg, col = 'red', lwd = 2)
abline(DIPTbslm.bg, col = 'orange', lwd = 2)
abline(LEPLbslm.bg, col = 'plum', lwd = 2)
abline(OPILbslm.bg, col = 'royalblue', lwd = 2)
abline(ORTHbslm.bg, col = 'magenta4', lwd = 2)
abline(HEMIbslm.bg, col = 'yellow', lwd = 2)







#----Stacked Bar----

prebar <- aggregate(beatsheet.pr$count, by = list(jd = beatsheet.pr$julianday, arth = beatsheet.pr$arthCode), 
                  FUN = sum)
bar <- prebar[prebar$arth == "ARAN" | prebar$arth == "AUCH" | prebar$arth == "COLE" | prebar$arth == "DIPT" |
                prebar$arth == "LEPL" | prebar$arth == "OPIL" | prebar$arth == "ORTH" | prebar$arth == "HEMI",]
counts <- table(bar$arth, bar$jd)
barplot(counts, main = "Arthropod Assemblages Over the Season", xlab = 'Julian Day', 
        col = c('cadetblue', 'chartreuse', 'red', 'orange', 'plum', 'royalblue', 'magenta4', 'yellow'))