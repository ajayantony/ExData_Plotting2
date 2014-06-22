# Adverse Weather - Impact Analysis on Population Health and Economy

## Synopsis
NOAA Storm Database Data was analysed to help determine the answers to these two important questions
1. Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

The impact on health due adverse weather is determined by quantifying the fatalities and injuries caused by various events. 
The events causing the >=90 perecentile was seperated out and visualized to try and understand the top killer events.

Similarly economic impact of such events were also analyzed (>= 99 percentile), in order to do this monetary value associated with the damage caused by
such events were determined.

Finally the correlation was visualized between events and both health and Economic impact.

## Data Processing
### Loading data
* Reading the csv file,read.csv can be directly used to read from .bz2 file.It is assumed that the working directory has the required dataset, the file is using read.csv()*

```r
raw_data <- read.csv("repdata-data-StormData.csv.bz2",header=TRUE,sep=",",stringsAsFactors=F)
```

### Data Clensing/Transformation
* Creating Year from the Begining Date column, to allow for aggregating data by year to support analysis and Converting to character date to date field, to ensure consistency

```r
datetime <- strptime(raw_data$BGN_DATE,"%m/%d/%Y")
year <- strftime(datetime, format="%Y")
prcd_data1 <- cbind(year,datetime,raw_data,stringAsFactors=F)
```

* Converting property damage/crop damage to exact $ value by using the exponent variable, how each exponent is interpreted is self explanatory from the code below

```r
#$ value of property damage
t_propdamage=ifelse(prcd_data1$PROPDMGEXP == '1',prcd_data1$PROPDMG * 10,
			 ifelse(prcd_data1$PROPDMGEXP == '2' | prcd_data1$PROPDMGEXP == 'h' | prcd_data1$PROPDMGEXP == 'H',prcd_data1$PROPDMG * 100,
			 ifelse(prcd_data1$PROPDMGEXP == '3' | prcd_data1$PROPDMGEXP == 'k' | prcd_data1$PROPDMGEXP == 'K',prcd_data1$PROPDMG * 1000,
			 ifelse(prcd_data1$PROPDMGEXP == '4',prcd_data1$PROPDMG * 10000,
			 ifelse(prcd_data1$PROPDMGEXP == '5',prcd_data1$PROPDMG * 100000,
			 ifelse(prcd_data1$PROPDMGEXP == '6' | prcd_data1$PROPDMGEXP == 'm' | prcd_data1$PROPDMGEXP == 'M',prcd_data1$PROPDMG * 1000000,
			 ifelse(prcd_data1$PROPDMGEXP == '7',prcd_data1$PROPDMG * 1000000,
			 ifelse(prcd_data1$PROPDMGEXP == '8',prcd_data1$PROPDMG * 10000000,
			 ifelse(prcd_data1$PROPDMGEXP == 'B',prcd_data1$PROPDMG * 100000000,
			 prcd_data1$PROPDMG
			)))))))))

#$ value of crop damage			
t_cropdamage=ifelse(prcd_data1$CROPDMGEXP == '1',prcd_data1$CROPDMG * 10,
			 ifelse(prcd_data1$CROPDMGEXP == '2' | prcd_data1$CROPDMGEXP == 'h' | prcd_data1$CROPDMGEXP == 'H',prcd_data1$CROPDMG * 100,
			 ifelse(prcd_data1$CROPDMGEXP == '3' | prcd_data1$CROPDMGEXP == 'k' | prcd_data1$CROPDMGEXP == 'K',prcd_data1$CROPDMG * 1000,
			 ifelse(prcd_data1$CROPDMGEXP == '4',prcd_data1$CROPDMG * 10000,
			 ifelse(prcd_data1$CROPDMGEXP == '5',prcd_data1$CROPDMG * 100000,
			 ifelse(prcd_data1$CROPDMGEXP == '6' | prcd_data1$CROPDMGEXP == 'm' | prcd_data1$CROPDMGEXP == 'M',prcd_data1$CROPDMG * 1000000,
			 ifelse(prcd_data1$CROPDMGEXP == '7',prcd_data1$CROPDMG * 1000000,
			 ifelse(prcd_data1$CROPDMGEXP == '8',prcd_data1$CROPDMG * 10000000,
			 ifelse(prcd_data1$CROPDMGEXP == 'B',prcd_data1$CROPDMG * 100000000,
			 prcd_data1$CROPDMG
			)))))))))
			
#Binding data
prcd_data2 <- cbind(prcd_data1,t_propdamage,t_cropdamage,stringAsFactors=F)
```

### Data Aggregation
* Using plyr package, aggregating by Event type and year, also determining total damage = property + crop


```r
library("plyr")
sum_data <- ddply(prcd_data2,.(EVTYPE,year),summarise,t_fatalities=sum(FATALITIES),t_injuries=sum(INJURIES),t_propdamage=sum(t_propdamage),t_cropdamage=sum(t_cropdamage),t_damage = sum(t_propdamage) + sum(t_cropdamage))
```

* Aggregating by event type

```r
ev_sum_data <- ddply(sum_data,.(EVTYPE),summarise,t_fatalities=sum(t_fatalities),t_injuries=sum(t_injuries),t_propdamage=sum(t_propdamage),t_cropdamage=sum(t_cropdamage),t_damage = sum(t_propdamage) + sum(t_cropdamage))
```

## Results
### Analysis to answer the question - Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

* Sorting events that caused most fatalities and injuries
** Top 10 killer weather events **

```r
sorted_data <- ev_sum_data[order(ev_sum_data$t_fatalities,ev_sum_data$t_injuries,decreasing=T),]
# Top 10 killer events
tk <- head(sorted_data,n=10)
names(tk) <- c("Event Type","Total Fatalities","Total Injuries")
row.names(tk) <- NULL
tk[,1:3]
```

```
##        Event Type Total Fatalities Total Injuries
## 1         TORNADO             5633          91346
## 2  EXCESSIVE HEAT             1903           6525
## 3     FLASH FLOOD              978           1777
## 4            HEAT              937           2100
## 5       LIGHTNING              816           5230
## 6       TSTM WIND              504           6957
## 7           FLOOD              470           6789
## 8     RIP CURRENT              368            232
## 9       HIGH WIND              248           1137
## 10      AVALANCHE              224            170
```
**It evident that Tornado is the number one KILLER weather event.**

* Removing outliers(which distorts charts) and considering only >= 90 percentile

```r
#removing Event - Tornado which is an outlier
tmp_data <- ev_sum_data[ev_sum_data$EVTYPE != 'TORNADO',]

#removing low lying observations(to avoid distortion of the plot)
tmp_data <- tmp_data[tmp_data$t_fatalities > 10 | tmp_data$t_injuries > 10,]

#Considering only the top killers (>= 90 percentile)
fat_90 <- quantile(tmp_data$t_fatalities, 0.90)
inj_90 <- quantile(tmp_data$t_injuries, 0.90)
tmp_data2 <- tmp_data[tmp_data$t_fatalities >= fat_90 | tmp_data$t_injuries >= inj_90,]
```

* Plotting a Scatter plot, outliers will indicate the top killers [ Note - Its already determined Tornado is the number one killer, its removed from this chart to allow for more clean plotting]

```r
library("ggplot2")
ggplot(tmp_data2, aes(x=t_injuries, y=t_fatalities, color=EVTYPE)) + geom_point(size=3) + geom_text(aes(label=EVTYPE), size=3,vjust=-1) + xlab("Total # of Injuries") + ylab("Total # of Fatalities") + ggtitle("Fatalities Vs Injuries(>90 percentile)")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

### Analysis to answer the second question - Across the United States, which types of events have the greatest economic consequences?
* Sorting events that caused most economic impact
** Top 10 events impacting economy **

```r
#Events sorted by highest economic impact
#combined total damage by adding in property and crop damage
e_sorted_data <- ev_sum_data[order(ev_sum_data$t_damage,decreasing=T),]

# Top 10 events by economic damage
te <- head(e_sorted_data,n=10)
names(te) <- c("Event Type","Total Fatalities","Total Injuries","Property Damage($)","Crop Damage($)","Total Damage($)")
row.names(te) <- NULL
te[,c(1,4,5,6)]
```

```
##           Event Type Property Damage($) Crop Damage($) Total Damage($)
## 1            TORNADO          5.218e+10      4.150e+08       5.259e+10
## 2              FLOOD          3.441e+10      5.662e+09       4.007e+10
## 3               HAIL          1.412e+10      3.026e+09       1.714e+10
## 4        FLASH FLOOD          1.531e+10      1.421e+09       1.673e+10
## 5            DROUGHT          1.046e+09      1.262e+10       1.367e+10
## 6  HURRICANE/TYPHOON          1.036e+10      1.249e+09       1.160e+10
## 7          HURRICANE          6.738e+09      2.742e+09       9.480e+09
## 8          TSTM WIND          4.485e+09      5.540e+08       5.039e+09
## 9        STORM SURGE          5.020e+09      5.000e+03       5.020e+09
## 10         HIGH WIND          4.100e+09      6.386e+08       4.739e+09
```
** It evident that Tornado is again the number one event that is impacting economy **

* Considering only >= 99 percentile

```r
#Considering only the top events by damange (>= 99 percentile)
eco_99 <- quantile(ev_sum_data$t_damage, 0.99)
tmp_data3 <- ev_sum_data[ev_sum_data$t_damage >= eco_99,]
tmp_data4 <- tmp_data3[order(tmp_data3$t_damage,decreasing=T),]


#Getting the year lables
ev_lables <- as.character(tmp_data4$EVTYPE)
```

* Plotting a Bar plot sorted in decending order showing the top events impacting economy

```r
barplot(tmp_data4$t_damage,col="red",main="Total Economic Damage($)",xlab="",ylab="Total Damage($)",names.arg=ev_lables,las=3)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

### Visualizing Corelation between top weather events , health and economic damage.

```r
#Determing corelation between health(95 percentile) and economy(99 percentile) by these events.
comb_data = merge(tmp_data2,tmp_data3,by.x="EVTYPE",by.y="EVTYPE")
c_ev_list = comb_data[,1]

#Adding in tornado which was removed from the health list
c_ev_list <- c(c_ev_list,"TORNADO")

#Dataset with events higly impacting both health and economy.
both_data <- sum_data[sum_data$EVTYPE %in% c_ev_list,]
qplot(t_injuries,t_fatalities,data=both_data,geom="point",facets=EVTYPE~.,stat="identity",color=EVTYPE,main="Event Type - Health Vs Economic Damage",ylab="Fatalities",xlab="Injuries") + geom_point(aes(size = both_data$t_damage))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

**The panelized scatter plot shows the top events impacting both health and economy. Each point shows the aggregated value for each year.
It clear from the plot that Tornado occurs very frequently and is the top killer and causes lot of economic damage**

