# color_survey_DRAFT.R
# 2019-04-19 adc
# Analyzes csv files submitted by students for color survey

# This here script lives in .../color_survey/code/
# csv files live in .../color_survey/submissions/

library(ggplot2)
library(colorspace)
library(RColorBrewer)
library(tm) # for word clouds
library(wordcloud)

###################################
# Set user flags to control script below
# For plots, limit colors to rainbow colors
#   plus a few other (1)
# or to the complement (0)
rainbowFlag <- 1;
###################################

# Directories

plotDir <- "../plots/"

# Create large list with one data frame per list item
dataFiles <- lapply(Sys.glob("../submissions/*.csv"), read.csv)


# Number of individuals who completed survey
nIDs <- length(dataFiles)

# Loop through dataFiles
# Goals are
#   1) Add columns to data frames that might be missing some
#   2) Add column with subject ID number
#   3) Concatenate all df's into one big one

# Define list of df columns that should be there after reading in csv files
csvCols <- c(
  "Name",
  "H",
  "S",
  "B",
  "Like.1.no.7.yes",
  "Like.to.wear",
  "Misery.1..Pleasure.7.",
  "Sleepiness.1..Arousal.7.",
  "Adjective.of.your.choice",
  "Example.object",
  "Letter",
  "Numeral",
  "Are.you.a.synesthete.",
  "Gender",
  "Are.you..colorblind..in.any.sense.of.the.word"
)


# # HAND FIX one person's data frame after initial inspection using for loop below
# # df in list position 40 has "Name" column named "X" instead
# colnames(dataFiles[[40]]) <- csvCols


dfCols <- c(
  "Name",
  "H",
  "S",
  "B",
  "Like",
  "Wear",
  "Pleasure",
  "Arousal",
  "Adjective",
  "Example",
  "Letter",
  "Numeral",
  "Synesthete",
  "Gender",
  "Colorblind",
  "ID"
)

df <- data.frame(matrix(ncol = length(dfCols), nrow = 0))
colnames(df) <- dfCols

for (i in 1:nIDs) {

  # Find any missing columsn
  colDiff <- setdiff(csvCols,colnames(dataFiles[[i]]))

  # Append missing columns; ASSUMES they are all at end
  if (length(colDiff) > 0) {
    for (j in length(colDiff)) {
      dataFiles[[i]] <- cbind(dataFiles[[i]],NA)
      colnames(dataFiles[[i]])[length(colnames(dataFiles[[i]]))] <- colDiff[j]
    }
  }

  # Add id numbers; just use loop iteration number
  dataFiles[[i]] <- cbind(dataFiles[[i]],i)

  # Fix column names, include name for the new ID column
  colnames(dataFiles[[i]]) <- dfCols

  df <- rbind(df,dataFiles[[i]])

}

# Clean up data frame

# get rid of uppercase chars
df$Name <- tolower(df$Name)
df$Adjective <- tolower(df$Adjective)
df$Example <- tolower(df$Example)
df$Synesthete <- tolower(df$Synesthete)
df$Gender <- tolower(df$Gender)
df$Colorblind <- tolower(df$Colorblind)

# Change to factors
df$Name <- factor(df$Name)
df$Adjective <- factor(df$Adjective)
df$Example <- factor(df$Example)
df$Letter <- factor(df$Letter)
df$Numeral <- factor(df$Numeral)
df$Synesthete <- factor(df$Synesthete)
df$Gender <- factor(df$Gender)
df$Colorblind <- factor(df$Colorblind)

# Fill in other rows per ID for these vars

for (i in 1:nIDs) {

  thisResp <- df[df$ID == i,]$Synesthete[df[df$ID == i,]$Synesthete != ""]
  df[df$ID == i,]$Synesthete <- thisResp[1] # Take only first if more than one

  thisResp <- df[df$ID == i,]$Gender[df[df$ID == i,]$Gender != ""]
  df[df$ID == i,]$Gender <- thisResp[1] # Take only first if more than one

  thisResp <- df[df$ID == i,]$Colorblind[df[df$ID == i,]$Colorblind != ""]
  df[df$ID == i,]$Colorblind <- thisResp[1] # Take only first if more than one

}


# Don't apply to entire df or numeric vars will get converted to factors for some reason

df$Name <- as.factor(sapply(df$Name, function(x) {gsub(" ","_",x)}))
df$Adjective <- as.factor(sapply(df$Adjective, function(x) {gsub(" ","_",x)}))
df$Example <- as.factor(sapply(df$Example, function(x) {gsub(" ","_",x)}))

# Remove blank spaces (now underscores) at end of words
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("_$","",x)}))
df$Adjective <- as.factor(sapply(df$Adjective, function(x) {gsub("_$","",x)}))
df$Example <- as.factor(sapply(df$Example, function(x) {gsub("_$","",x)}))


df[grepl("\\[", df$Name),] <- NA
df[grepl("\\*", df$Name),] <- NA
df[grepl("^$", df$Name),] <- NA

df <- droplevels(df)


# Hand fix some color Names

# Order should matter, but still get problems (see Kludge below)

# For easier comparisons:
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("chicago_maroon","maroon_chicago",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("hokie_orange_","orange_hokie",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("hot_pink","pink_hot",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("baby_pink","pink_baby",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("baby_yellow","yellow_baby",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("lime","green_lime",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("lime_green","green_lime",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("midnight_purple","purple_midnight",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("mint","green_mint",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("mint_green","green_mint",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("mustard","yellow_mustard",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("mustard_yellow","yellow_mustard",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("navy","blue_navy",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("navy_blue","blue_navy",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("baby_blue","blue_baby",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("pale_blue","blue_pale",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("navy_blue","blue_navy",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("royal_blue","blue_royal",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("sea_foam_green","green_seafoam",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("seafoam_green","green_seafoam",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("tardis_blue","blue_tardis",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("wierd_purple_","purple_weird",x)}))

# Kludge to fix
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("yellow_yellow_mustard","yellow_mustard",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("green_green_lime","green_lime",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("green_green_mint","green_mint",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("blue_blue_navy","blue_navy",x)}))

# Correcting typos, etc.
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("fuschia","fuchsia",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("fushia","fuchsia",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("magneta","magenta",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("baby_pink","pink_baby",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("peach_","peach",x)}))
df$Name <- as.factor(sapply(df$Name, function(x) {gsub("turqoise","turquoise",x)}))


#df <- df[df$Colorblind == "no",]

df <- df[!is.na(df$H),]
df <- df[!is.na(df$S),]
df <- df[!is.na(df$B),]

# # Later, HSV and LUV produce NaNs for HSB coords (0,0,0)
# df <- df[df$H + df$S + df$B != 0,]

# White (0,0,1) and black (0,0,0) produce errors with HSV and LUV below
df <- df[df$Name != "white",]
df <- df[df$Name != "black",]

# Data validation; somehow bad values for B got through
df <- df[df$H <= 360,]
df <- df[df$S <= 100,]
df <- df[df$B <= 100,]

rainbowColors <- c("red","orange","yellow","green","blue","indigo","violet","brown","gray")

###############################################
# LIMIT to ROYGBIV plus a few others
if (rainbowFlag){
  df <- df[df$Name %in% rainbowColors,]
} else {
# ... or the complement
df <- df[!(df$Name %in% rainbowColors),]
}
###############################################


df <- droplevels(df)

attach(df)

# Make copies of the HSV variables that are normalized to range 0,1
# Some color coordinates require values in that range, or at least as they are implemented in R
# Keep the original variables, too.
df$h <- H/360;
df$s <- S/100;
df$v <- B/100;


attach(df)

# Super kludgy code to convert HSV values to L*u*v coordinates:

x <- HSV(cbind(H,s,v))

# There seems to be a weird problem with the colorspace package's HSV object class
# Other classes from this package (e.g. RGB ) can be converted to the LUV class,
# but not HSV for some reason.
#
# Consider the following error:
# > as(x,"LUV")
# Error in cbind(L, if (missing(U)) NULL else U, if (missing(V)) NULL else V) :
#   Ambiguous conversion
#
# So Anthony worked around this by converting HSV to RGB (which works) first.

y <- as(as(x,"RGB"),"LUV")

# Another problem: can't directly coerce LUV class to data frame:
# > n <- as.data.frame(y)
# Error in as.data.frame.default(y) :
#   cannot coerce class "structure("LUV", package = "colorspace")" to a data.frame
#
# So, first convert to matrix, then to data frame:

m <- coords(y);
mdf <- as.data.frame(m);

# Calculate means for each category (color name)
meanColors <- rbind(tapply(m[,2],Name,mean,na.rm = TRUE),tapply(m[,3],Name,mean,na.rm = TRUE))

#-----------------
# Draw the plots
#-----------------

# String to add to plot file names to distinguish between rainbow color and complement versions
if (rainbowFlag){
  rainbowStr <- 'rainbow'
} else {
  rainbowStr <- 'non-rainbow'
}

# Theme to remove background
# From: https://felixfan.github.io/ggplot2-remove-grid-background-margin/
blankTheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"))


# The second and third dimensions of the LUV color space are the ones related to hue and saturation.  The first is the lightness dimension, so leave that out here
cp <- ggplot(mdf,aes(mdf[,2],mdf[,3],color=hsv(h,s,v),group=Name,label=Name))

# For comparison, make plots where the colors are displayed not as the students chose them, but with a constant saturation or value level
cpKSat <- ggplot(mdf,aes(mdf[,2],mdf[,3],color=hsv(h,1,v),group=Name,label=Name))
cpKVal <- ggplot(mdf,aes(mdf[,2],mdf[,3],color=hsv(h,s,1),group=Name,label=Name))


# Plot the colors as they appeared when students chose them:
svg(
  paste0(plotDir,'LUV_as_chosen_',rainbowStr,'.svg')
)
pCP <- cp + geom_point(size=8) + scale_color_identity(guide="none") + coord_fixed() + labs(list(title = "Colors as chosen", x = "L", y = "U")) + blankTheme
print(pCP)
dev.off()

# Plot the colors as they would appear if they all had the same SATURATION
svg(
  paste0(plotDir,'LUV_kSat_',rainbowStr,'.svg')
)
pCPkSat <- cpKSat + geom_point(size=8) + scale_color_identity(guide="none") + coord_fixed() + labs(list(title = "Colors constrained to have constant SATURATION", x = "L", y = "U")) + blankTheme
print(pCPkSat)
dev.off()

# Plot the colors as they would appear if they all had the same VALUE
svg(
  paste0(plotDir,'LUV_kVal_',rainbowStr,'.svg')
)
pCPkVal <- cpKVal + geom_point(size=8) + scale_color_identity(guide="none") + coord_fixed() + labs(list(title = "Colors constrained to have constant VALUE", x = "L", y = "U"))  + blankTheme
print(pCPkVal)
dev.off()

# # Version with 95% confidence ellipses
# svg(
#   paste0(plotDir,'LUV_as_chosen_ellipses_',rainbowStr,'.svg')
# )
# pCPellipses <- cp + geom_point(size=8) + scale_color_identity(guide="none") + coord_fixed() + stat_ellipse()
# print(pCPellipses)
# dev.off()

# Funny all-text versions:
# cp + scale_color_identity(guide="none") + coord_fixed() + stat_ellipse() + geom_text(fontface="bold")
# cp + scale_color_identity(guide="none") + coord_fixed() + stat_ellipse() + geom_label(fill=hsv(h,s,v),color="gray",size=3)

# With linetype to distinguish ellipses, which also makes a legend
# (AC couldn't figure out how to make line colors different easily)

if (rainbowFlag){   # usually too few points for non-rainbow colors
svg(
  paste0(plotDir,'LUV_as_chosen_ellipses_',rainbowStr,'.svg')
)
pCPEllipses <- cp + geom_point(size=6) + scale_color_identity() + coord_fixed() + stat_ellipse(aes(linetype=Name)) + scale_linetype_manual(values=c(1,2,3,4,5,6,1,2,3)) + labs(list(title = "Colors as chosen", x = "L", y = "U")) + blankTheme
print(pCPEllipses)
dev.off()
}

svg(
  paste0(plotDir,'LUV_Like_',rainbowStr,'.svg')
)
pCPLike <- cp + geom_point(size=Like) + scale_color_identity() + coord_fixed() + labs(list(title = "Rating: Like this color", x = "L", y = "U")) + blankTheme
print(pCPLike)
dev.off()

svg(
  paste0(plotDir,'LUV_Wear_',rainbowStr,'.svg')
)
pCPWear <- cp + geom_point(size=Wear) + scale_color_identity() + coord_fixed() + labs(list(title = "Rating: Would wear this color", x = "L", y = "U")) + blankTheme
print(pCPWear)
dev.off()


# Difference between Wear and Like ratings
# Add integer to difference to avoid negative numbers, because difference score will determine symbol size in plots
df$WLDiff <- df$Wear - df$Like + 5

attach(df)

svg(
  paste0(plotDir,'LUV_WearLikeDiff_',rainbowStr,'.svg')
)
pCPWLDiff <- cp + geom_point(size=WLDiff) + scale_color_identity() + coord_fixed() + labs(list(title = "Difference between Wear and Like ratings - larger means more likely to wear", x = "L", y = "U")) + blankTheme
print(pCPWLDiff)
dev.off()

# # Funny all-text versions:
# # (UNCOMMENT these lines to activate the code)
# #----
# cp + scale_color_identity(guide="none") + coord_fixed() + stat_ellipse() +
# geom_text(fontface="bold")


svg(
  paste0(plotDir,'color_text_',rainbowStr,'.svg')
)
pCPtext <- cp + scale_color_identity(guide="none") + coord_fixed() + geom_text(fontface="bold")  + labs(list(title = "Additional colors as chosen and named by students", x = "L", y = "U")) + blankTheme
print(pCPtext)
dev.off()


svg(
  paste0(plotDir,'LUV_text_',rainbowStr,'.svg')
)
pCPtextBox <- cp + scale_color_identity(guide="none") + coord_fixed() +
  geom_label(fill=hsv(h,s,v),color="black",size=3) + labs(list(title = "Additional colors as chosen and named by students", x = "L", y = "U")) + blankTheme
print(pCPtextBox)
dev.off()

# cp + scale_color_identity(guide="none") +
#   coord_fixed() +
#   geom_label(fill=hsv(h,s,v),color="black",size=3)








##################
# Halt script unless plotting rainbow colors

if (rainbowFlag == FALSE){
  stop()
}

##################





#----------------------------
# Histograms of HSV variables
#----------------------------

hueShift <- H;

# Adjust as needed to avoid splitting points from same color to opposite ends of x-axis

hueShift[hueShift > 320] <- hueShift[hueShift > 320] - 360;

df$hueShift <- hueShift;

attach(df)


# Plot histogram of HUE numbers
svg(
  paste0(plotDir,'hist_hueShift_',rainbowStr,'.svg')
)
histHueShift <- ggplot(df[Name!="indigo",],aes(hueShift,group=Name,color=Name)) + geom_freqpoly(bins=36,size=1) + scale_color_identity() + labs(list(title = "Histogram of hue angles", x = "Hue angle", y = "count")) + blankTheme
print(histHueShift)
dev.off()

# Plot histogram of SATURATION numbers
svg(
  paste0(plotDir,'hist_Sat_',rainbowStr,'.svg')
)
histSat <- ggplot(df[Name!="indigo",],aes(S,group=Name,color=Name)) + geom_freqpoly(bins=10,size=1) + scale_color_identity() + labs(list(title = "Histogram of Saturation", x = "Saturation", y = "count")) + blankTheme
print(histSat)
dev.off()


# Plot histogram of BRIGHTNESS numbers
svg(
  paste0(plotDir,'hist_Bright_',rainbowStr,'.svg')
)
hist_Bright <- ggplot(df[Name!="indigo",],aes(B,group=Name,color=Name)) + geom_freqpoly(bins=10,size=1) + scale_color_identity() + labs(list(title = "Histogram of Brightness", x = "Brightness", y = "count")) + blankTheme
print(hist_Bright)
dev.off()


# Scatterplots to visualize correlations between HSV variables

svg(
  paste0(plotDir,'scatter_BrightSat_',rainbowStr,'.svg')
)
scatter_BrightSat <- ggplot(df[Name!="indigo",],aes(B,S,group=Name,color=Name)) + geom_point() + scale_color_identity() + theme(aspect.ratio = 1) + stat_smooth(formula = y ~ x, method = "lm", se=FALSE,size=2) + labs(list(title = "Correlations between Brightness and Saturation", x = "Brightness", y = "Saturation")) + blankTheme
print(scatter_BrightSat)
dev.off()


svg(
  paste0(plotDir,'scatter_HueSat_',rainbowStr,'.svg')
)
scatter_HueSat <- ggplot(df[Name!="indigo",],aes(hueShift,S,group=Name,color=Name)) + geom_point() + scale_color_identity() + theme(aspect.ratio = 1) + stat_smooth(formula = y ~ x, method = "lm", se=FALSE, size=2) + labs(list(title = "Correlations between Hue and Saturation", x = "Hue angle", y = "Saturation")) + blankTheme
print(scatter_HueSat)
dev.off()


ggplot(df[Name!="indigo",],aes(hueShift,B,group=Name,color=Name)) + geom_point() + scale_color_identity() + theme(aspect.ratio = 1) + stat_smooth(formula = y ~ x, method = "lm", se=FALSE)


# # To examine consistency or correlation between Hue ratings for various colors within subjects
#
# dfBV <- as.data.frame(cbind(S[Name=="blue"],S[Name=="violet"]))
# colnames(dfBV) <- c("BlueSat","VioletSat")
# ggplot(dfBV,aes(BlueHue,VioletHue)) + geom_point() + coord_fixed() # + theme(aspect.ratio = 1) # + stat_smooth(formula = y ~ x, method = "lm")





#----------------------------------------------------
# ANOVAs
#----------------------------------------------------

# summary(aov(Like ~ Gender*Name + Error(ID), data=df))
# summary(aov(Wear ~ Gender*Name + Error(ID), data=df))
# summary(aov(Arousal ~ Gender*Name + Error(ID), data=df))
# summary(aov(Pleasure ~ Gender*Name + Error(ID), data=df))

#----------------------------------------------------
# WORD CLOUDS
#----------------------------------------------------

# # Method 1
#
# wc <- count(df[df$Name == "red",],Adjective)
#
# wc <- count(df[df$Name == "red",],Adjective)
# wordcloud(wc$Adjective,wc$n,colors=brewer.pal(8,"Reds"),min.freq = 1)
#
# wc <- count(df[df$Name == "red",],Example)
# wordcloud(wc$Example,wc$n,colors=brewer.pal(8,"Reds"),min.freq = 1)

# Method 2

makeWC <- function(dfFactorIn,stemFlag=0,minFlag=1,colorStr=NULL){
  x <- Corpus(VectorSource(dfFactorIn))
  x <- tm_map(x, removePunctuation)
  if (stemFlag) {x <- tm_map(x, stemDocument)}
  tdm <- TermDocumentMatrix(x)
  m <- as.matrix(tdm)
  tdmV <- sort(rowSums(m),decreasing = TRUE)
  d <- data.frame(word = names(tdmV),freq=tdmV)
  if (minFlag){
    if (is.null(colorStr)) {wordcloud(d$word,d$freq,min.freq=1)} else {
      wordcloud(d$word,d$freq,min.freq=1,colors=colorStr)
    }
  } else {
    if (is.null(colorStr)) {wordcloud(d$word,d$freq)} else {
      wordcloud(d$word,d$freq,colors=colorStr)
    }
  }
}
#
# makeWC(df$Adjective)
# makeWC(df$Example)

if (rainbowFlag){
for (i in rainbowColors) {

  if (i == "indigo") {

    svg(paste0(plotDir,"wordcloud_",i,"_adjective.svg"))
    makeWC(df[df$Name == i,]$Adjective,colorStr="blueviolet")
    dev.off()

    svg(paste0(plotDir,"wordcloud_",i,"_example.svg"))
    makeWC(df[df$Name == i,]$Example,colorStr="blueviolet")
    dev.off()

  } else {

    svg(paste0(plotDir,"wordcloud_",i,"_adjective.svg"))
    makeWC(df[df$Name == i,]$Adjective,colorStr=i)
    dev.off()

    svg(paste0(plotDir,"wordcloud_",i,"_example.svg"))
    makeWC(df[df$Name == i,]$Example,colorStr=i)
    dev.off()

  }
}
}

# makeWC(df[!(df$Name %in% rainbowColors),]$Name,minFlag = 1)


# for (i in rainbowColors) {
#
# wc <- count(df[df$Name == i,],Letter)
# wc <- wc[wc$Letter != "FALSE",]
# wc <- wc[!(is.na(wc$Letter)),]
# wordcloud(wc$Letter,wc$n,color=i,min.freq = 1,rot.per = 0)
#
# }
#
# for (i in rainbowColors) {
#
#   wc <- count(df[df$Name == i,],Numeral)
#   wc <- wc[wc$Numeral != "FALSE",]
#   wc <- wc[!(is.na(wc$Numeral)),]
#   wordcloud(wc$Numeral,wc$n,color=i,min.freq = 1,rot.per = 0)
#
# }
#
# for (i in rainbowColors) {
#   if (i == "indigo") {
#     makeWC(df[df$Name == i,]$Adjective,colorStr="blueviolet",stemFlag = 1)
#     makeWC(df[df$Name == i,]$Example,colorStr="blueviolet", stemFlag = 1)
#   } else {
#     makeWC(df[df$Name == i,]$Adjective,colorStr=i,stemFlag = 1)
#     makeWC(df[df$Name == i,]$Example,colorStr=i,stemFlag = 1)
#   }
# }
