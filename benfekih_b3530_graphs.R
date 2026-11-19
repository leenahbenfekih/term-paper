## research paper graphs 
## made by Leenah Benfekih 202311289
## 01/11/2026

# making dataframe of genotype and counts
om_counts <- data.frame(
  w1118 = c(708, 677, 690, 677),
  gal4 = c(691, 659, 829, 636),
  lacz = c(752, 762, 762, 747), 
  mnb = c(671, 694, 663, 687),
  foxo = c(318, 327, 301, 309), 
  mnfo = c(469, 485, 441, 458)
)

om_countsM <- colMeans(om_counts, na.rm = TRUE) # mean of each genotype ommatidia count 
om_countsSD <- apply(om_counts, 2, sd, na.rm = TRUE) # sd of each genotype ommatidia count

# dataframe of area for 7 ommatidia   
om7 <- data.frame( # this is all the 7 ommatidia data so i can find mean and then div by 7 
  w1118 = c(1452.614, 1477.103, 1531.645, 1740.354, 1718.649, 1745.363, 1543.333, 
            1552.238, 1556.134, 1467.641, 1451.501, 1467.641),
  gal4 = c(1448.706, 1331.47, 1436.424, 1241.589, 1099.23, 1227.074, 1344.086,
           1336.294, 953.382, 1342.96, 1421.434, 1273.391),
  lacz = c(1322.924, 1469.297, 1456.496, 1189.352, 1420.321, 1493.229, 1773.991,
           1870.056, 1900.654, 1990.723, 2052.822, 2089.224),
  mnb = c(2259.434, 2037.158, 2214.685, 2150.13, 1927.121, 2139.86, 2043.48,
          1960.382, 1945.208, 2053.148, 2120.866, 2004.881),
  foxo = c(1105.856, 1723.065, 1031.279, 1395.816, 1123.665, 1645.148, 1040.715, 
           1289.333, 1022.561, 1077.061, 1234.421, 1199.355), 
  mnfo = c(968.575, 829.001, 1059.571, 914.982, 1025.517, 1128.795, 1121.479,
           1128.781, 1082.31, 1070.215, 1091.575, 1122.473)
)

# grouping above into the 3 observations they were originally taken in + mean of those 3
om7mean <- aggregate(
  om7, 
  by = list(rep(1:4, each = 3)), # group each columns results in three 
  FUN = mean 
)[-1] # -1 to auto remove the extra column aggregate adds

omM <- om7mean / 7 # divide the mean of each group of 3 we created by 7 

omM2 <- colMeans(omM) # mean of the 4 groups above for each genotype 
omSD <- apply(omM, 2, sd) # sd of the 4 groups above 


# bar graph + arrows for ommatidia count 
par(mar = c(6.5, 6.5, 2, 2), mgp = c(5, 1, 0))
bp1 <- barplot(om_countsM, ylab = "Average count of ommatidia",
        names.arg = c("A", "B", "C", "D", "E", "F"),
        col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", "indianred", 
                "palegreen3", "mediumorchid3"), beside = TRUE, 
        main = "Average count of total ommatidia across six different genotypes",
        las = 1, ylim = c(0, 800))

arrows(bp1, om_countsM - om_countsSD,
       bp1, om_countsM + om_countsSD, 
       lwd = 1, angle = 90, code = 3, length = 0.08
       )
mtext("a)", side = 3, adj = -0.1, line = 0.5, font = 2, cex = 1.5)
legend("topright", c("w1118", "GMR-gal4", "GMR-gal4/UAS-LacZ", "GMR-gal4/UAS-Mnb", "GMR-gal4/UAS-Foxo", "GMR-gal4/UAS-Mnb, UAS-Foxo"),
       pch = c("A", "B", "C", "D", "E", "F"),
       col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", "indianred", "palegreen3", "mediumorchid3"),
       bty = "o", cex = 0.6)

# bar graph and arrows for average area of one ommatidium
bp2 <- barplot(omM2, ylab = "Average area of ommatidium (um^2)", 
               names.arg = c("A", "B", "C", "D", "E", "F"),
               col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", "indianred",
                       "palegreen3", "mediumorchid3"), beside = TRUE, 
               main = "Average area of ommatidium across six different geneotypes",
               las = 1, ylim = c(0, 350))
arrows(bp2, omM2 - omSD, 
       bp2, omM2 + omSD, 
       lwd = 1, angle = 90, code = 3, length = 0.08)
mtext("b)", side = 3, adj = -0.1, line = 0.5, font = 2, cex = 1.5)
legend("topright", c("w1118", "GMR-gal4", "GMR-gal4/UAS-LacZ", "GMR-gal4/UAS-Mnb", "GMR-gal4/UAS-Foxo", "GMR-gal4/UAS-Mnb, UAS-Foxo"),
       pch = c("A", "B", "C", "D", "E", "F"),
       col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", "indianred", "palegreen3", "mediumorchid3"),
       bty = "o", cex = 0.6)

# making dataframe of entire eye area 
eyes <- data.frame(
  w1118 = c(118524.419, 127118.219, 111383.234, 103895.314),
  gal4 = c(101213.821, 97533.308, 92424.670, 97846.094),
  lacz = c(120209.118, 118981.909, 120803.521, 122932.352),
  mnb = c(104705.805, 110528.919, 101646.266, 102324.709),
  foxo = c(50156.934, 52567.940, 53450.640, 50493.095),
  mnfo = c(76494.890, 78554.152, 76191.010, 69692.091)
)
eyesM <- colMeans(eyes)
eyesSD <- apply(eyes, 2, sd)

# barplot of eye area 
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mar = c(6.5, 6.5, 2, 2), mgp = c(5, 1, 0))
bp3 <- barplot(eyesM, ylab = "Average area (um^2)",
               names.arg = c("A", "B", "C", "D", "E", "F"), 
               col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", 
                       "indianred", "palegreen3", "mediumorchid3"), beside = TRUE, 
               main = "Average area of entire eye across six different genotypes", 
               las = 1, ylim = c(0, 150000))
arrows(bp3, eyesM - eyesSD,
       bp3, eyesM + eyesSD, 
       lwd = 1, angle = 90, code = 3, length = 0.08)
mtext("c)", side = 3, adj = -0.1, line = 0.5, font = 2, cex = 1.5)
legend("topright", c("w1118", "GMR-gal4", "GMR-gal4/UAS-LacZ", "GMR-gal4/UAS-Mnb", "GMR-gal4/UAS-Foxo", "GMR-gal4/UAS-Mnb, UAS-Foxo"),
       pch = c("A", "B", "C", "D", "E", "F"), 
       col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", "indianred", "palegreen3", "mediumorchid3"),
       bty = "o", cex = 0.6)

# setting up for graph of bristle count 
bristle <- data.frame(
  w1118 = c(559, 526, 544, 518),
  gal4 = c(525, 480, 495, 487),
  lacz = c(561, 595, 582, 567),
  mnb = c(522, 508, 493, 498),
  foxo = c(12, 27, 16, 12),
  mnfo = c(47, 42, 18, 34)
)

bristle_m <- colMeans(bristle)
bristle_sd <- apply(bristle, 2, sd)

par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mar = c(6.5, 6.5, 2, 2), mgp = c(5, 1, 0))
bp4 <- barplot(bristle_m, ylab = "Average number of bristles", 
               names.arg = c("A", "B", "C", "D", "E", "F"),
               col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", 
                       "indianred", "palegreen3", "mediumorchid3"), beside = TRUE,
               main = "Average number of bristles found on the eyes of six different genotypes",
               las = 1, ylim = c(0, 600))
arrows(bp4, bristle_m - bristle_sd,
       bp4, bristle_m + bristle_sd,
       lwd = 1, angle = 90, code = 3, length = 0.08)
mtext("d)", side = 3, adj = -0.1, line = 0.5, font = 2, cex = 1.5)
legend("topright", c("w1118", "GMR-gal4", "GMR-gal4/UAS-LacZ", "GMR-gal4/UAS-Mnb", "GMR-gal4/UAS-Foxo", "GMR-gal4/UAS-Mnb, UAS-Foxo"),
       pch = c("A", "B", "C", "D", "E", "F"), 
       col = c("paleturquoise3", "lightgoldenrod3", "navajowhite3", "indianred", "palegreen3", "mediumorchid3"),
       bty = "o", cex = 0.6)
