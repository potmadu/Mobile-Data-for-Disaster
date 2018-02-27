library(dplyr);

setwd("/home/pulselab/analysis/dataset-rd-2102/weekly/");

volcano_week = c("week-36-result.csv","week-37-result.csv",
"week-38-result.csv","week-39-result.csv",
"week-40-result.csv","week-41-result.csv",
"week-42-result.csv","week-43-result.csv","week-44-result.csv");

normal_week = c("week-11-result.csv","week-12-result.csv");

for(i in 1:length(volcano_week)){
        if(i==1){
                dat_volcano = read.csv(volcano_week[i],stringsAsFactors=FALSE);
                dat_volcano$disaster_type="volcano";
                dat_volcano$week_type=volcano_week[i];
        } else{
                dat = read.csv(volcano_week[i],stringsAsFactors=FALSE);
                dat$disaster_type="volcano";
                dat$week_type=volcano_week[i];
                dat_volcano=rbind(dat,dat_volcano);
        }
}

for(i in 1:length(normal_week)){
                if(i==1){
                dat_normal = read.csv(normal_week[i],stringsAsFactors=FALSE);
                dat_normal$disaster_type="normal";
                dat_normal$week_type=normal_week[i];
        } else{
                dat = read.csv(normal_week[i],stringsAsFactors=FALSE);
                dat$disaster_type="normal";
                dat$week_type=normal_week[i];
                dat_normal=rbind(dat,dat_normal);
        }
}

dat_volcano$latlong=gsub("\\(","",dat_volcano$user_home_antenna__location);
dat_volcano$latlong=gsub("\\)","",dat_volcano$latlong);
dat_volcano$latlong=gsub(" ","",dat_volcano$latlong);

dat_normal$latlong=gsub("\\(","",dat_normal$user_home_antenna__location);
dat_normal$latlong=gsub("\\)","",dat_normal$latlong);
dat_normal$latlong=gsub(" ","",dat_normal$latlong);

all_dat = rbind(dat_volcano,dat_normal);
rm(dat_volcano,dat_normal);

all_dat$waktu = strptime(all_dat$reporting__start_time,"%Y-%m-%d %H:%M:%S");
all_dat$timestamp = as.numeric(all_dat$waktu);

latlong = all_dat$latlong;
latlong_split = strsplit(latlong,",");

all_dat_byName = all_dat %>%
dplyr::select(-waktu) %>%
arrange(name,timestamp) %>%
as.data.frame();

all_dat_byName %>%
dplyr::select(name,reporting__start_time,latlong,disaster_type) %>%
 head(100);

library(geosphere);

all_dat_byName_fill = all_dat_byName %>%
filter(latlong!="") %>%
as.data.frame();

##########################
### CALC before and after volcano by 1 sample 
##########################

name_lists = unique(all_dat_byName_fill$name);

user_loc = as.data.frame(name_lists);
user_loc$normal_lat=NA;
user_loc$normal_long=NA;
user_loc$volcano_lat=NA;
user_loc$volcano_long=NA;
user_loc$distance=NA;

user_loc$normal_lat=as.numeric(user_loc$normal_lat);
user_loc$normal_long=as.numeric(user_loc$normal_long);
user_loc$volcano_lat=as.numeric(user_loc$volcano_lat);
user_loc$volcano_long=as.numeric(user_loc$volcano_long);
user_loc$distance=as.numeric(user_loc$distance);

for(i in 1:nrow(user_loc)){

        library(geosphere);

        username=user_loc$name_lists[i];

        loc_normal = all_dat_byName_fill %>% filter(name==username & disaster_type=="normal") %>% dplyr::select(latlong) %>% head(1) %>% as.data.frame();
        loc_volcano = all_dat_byName_fill %>% filter(name==username & disaster_type=="volcano") %>% dplyr::select(latlong) %>% head(1) %>% as.data.frame();

        latlong_normal = loc_normal$latlong[1];
        latlong_volcano = loc_volcano$latlong[1];

        if(is.na(latlong_normal)){
                user_loc$normal_lat[i]=NA;
                user_loc$normal_long[i]=NA;
        } else{
                dat = strsplit(latlong_normal,",");
                lat=dat[[1]][1];
                long=dat[[1]][2];

                user_loc$normal_lat[i]=as.numeric(lat);
                user_loc$normal_long[i]=as.numeric(long);
        }

        if(is.na(latlong_volcano)){
                user_loc$volcano_lat[i]=NA;
                user_loc$volcano_long[i]=NA;
        } else{
                dat = strsplit(latlong_volcano,",");
                lat=dat[[1]][1];
                long=dat[[1]][2];

                user_loc$volcano_lat[i]=as.numeric(lat);
                user_loc$volcano_long[i]=as.numeric(long);
        }

        if(is.na(user_loc$volcano_lat[i]) || is.na(user_loc$normal_lat[i])){
                dist=NA;
        } else{
                dist = distGeo(c(user_loc$volcano_long[i],user_loc$volcano_lat[i]),c(user_loc$normal_long[i],user_loc$normal_lat[i]));
        }

        user_loc$distance[i]=dist;

}

write.csv(user_loc,"/home/pulselab/analysis/Rizal/user_loc.csv")

user_loc_fill = user_loc[!is.na(user_loc),];

###################

##########################
### CALC before and after volcano by 1 using centroid 
##########################

name_lists = unique(all_dat_byName_fill$name);

user_loc = as.data.frame(name_lists);
user_loc$normal_lat=NA;
user_loc$normal_long=NA;
user_loc$volcano_lat=NA;
user_loc$volcano_long=NA;
user_loc$distance=NA;

user_loc$normal_lat=as.numeric(user_loc$normal_lat);
user_loc$normal_long=as.numeric(user_loc$normal_long);
user_loc$volcano_lat=as.numeric(user_loc$volcano_lat);
user_loc$volcano_long=as.numeric(user_loc$volcano_long);
user_loc$distance=as.numeric(user_loc$distance);

for(i in 1:nrow(user_loc)){

        library(geosphere);

        username=user_loc$name_lists[i];

        loc_normal = all_dat_byName_fill %>% filter(name==username & disaster_type=="normal") %>% dplyr::select(latlong) %>% as.data.frame();
        loc_volcano = all_dat_byName_fill %>% filter(name==username & disaster_type=="volcano") %>% dplyr::select(latlong) %>% as.data.frame();

        count_loc_normal = loc_normal %>% group_by(latlong) %>% summarise(jml=n()) %>% arrange(jml) %>% as.data.frame();
        count_loc_volcano = loc_volcano %>% group_by(latlong) %>% summarise(jml=n()) %>% arrange(jml) %>% as.data.frame();

        if(nrow(count_loc_normal)!=0){
                latlong_normal = count_loc_normal$latlong[nrow(count_loc_normal)];
        } else{
                latlong_normal = NA;
        }

        if(nrow(count_loc_volcano)!=0){
                latlong_volcano = count_loc_volcano$latlong[nrow(count_loc_volcano)];
        } else{
                latlong_volcano = NA;
        }

        if(is.na(latlong_normal)){
                user_loc$normal_lat[i]=NA;
                user_loc$normal_long[i]=NA;
        } else{
                dat = strsplit(latlong_normal,",");
                lat=dat[[1]][1];
                long=dat[[1]][2];

                user_loc$normal_lat[i]=as.numeric(lat);
                user_loc$normal_long[i]=as.numeric(long);
        }

        if(is.na(latlong_volcano)){
                user_loc$volcano_lat[i]=NA;
                user_loc$volcano_long[i]=NA;
        } else{
                dat = strsplit(latlong_volcano,",");
                lat=dat[[1]][1];
                long=dat[[1]][2];

                user_loc$volcano_lat[i]=as.numeric(lat);
                user_loc$volcano_long[i]=as.numeric(long);
        }

        if(is.na(user_loc$volcano_lat[i]) || is.na(user_loc$normal_lat[i])){
                dist=NA;
        } else{
                dist = distGeo(c(user_loc$volcano_long[i],user_loc$volcano_lat[i]),c(user_loc$normal_long[i],user_loc$normal_lat[i]));
        }

        user_loc$distance[i]=dist;

}

write.csv(user_loc,"/home/pulselab/analysis/Rizal/user_loc.csv")

user_loc_fill = user_loc[!is.na(user_loc),];

###################

csv_cell_vanuatu = read.csv("/home/pulselab/analysis/dataset-rd-2102/mobility-user (do not use this, raw-data-weekly instead)/csv_cell_vanuatu.csv",stringsAsFactors=FALSE);

long = csv_cell_vanuatu$longitude;
lat = csv_cell_vanuatu$latitude;

library(dplyr);
library(raster);
library(lubridate);
library(rgeos);
library(sp);
library(maptools);
library(readr);
library(rgdal);

vanuatu_shp = shapefile("/home/pulselab/analysis/Rizal/Vanuatu-SHP/Vanuatu-SHP/VUT_adm_shp/VUT_adm2.shp");

coordinates(csv_cell_vanuatu) = ~ longitude + latitude;
proj4string(csv_cell_vanuatu) = CRS("+proj=longlat");

csv_cell_vanuatu = spTransform(csv_cell_vanuatu,proj4string(vanuatu_shp));

proj4string(csv_cell_vanuatu) = proj4string(vanuatu_shp);

results = over(csv_cell_vanuatu,vanuatu_shp);

csv_cell_vanuatu = merge(csv_cell_vanuatu,results,by="row.names");
csv_cell_vanuatu$Row.names=NULL;

csv_cell_vanuatu$long=long;
csv_cell_vanuatu$lat=lat;

write.csv(csv_cell_vanuatu,"/home/pulselab/analysis/Rizal/csv_cell_vanuatu_province.csv");

########################

user_loc = read.csv("/home/pulselab/analysis/Rizal/user_loc.csv",stringsAsFactors=FALSE);

csv_cell_vanuatu$latlong=paste(csv_cell_vanuatu$lat,csv_cell_vanuatu$long,sep=",");

user_loc$normal_latlong=paste(user_loc$normal_lat,user_loc$normal_long,sep=",");
user_loc$volcano_latlong=paste(user_loc$volcano_lat,user_loc$volcano_long,sep=",");

cell_vanuatu = as.data.frame(csv_cell_vanuatu) %>% dplyr::select(latlong,NAME_0,NAME_1,NAME_2) %>% unique() %>% as.data.frame()

user_loc_prov = user_loc %>%
left_join(cell_vanuatu,by=c("normal_latlong"="latlong")) %>%
as.data.frame();

colnames(user_loc_prov)[10]="SOURCE_0";
colnames(user_loc_prov)[11]="SOURCE_1";
colnames(user_loc_prov)[12]="SOURCE_2";

user_loc_prov = user_loc_prov %>%
dplyr::left_join(cell_vanuatu,by=c("volcano_latlong"="latlong")) %>%
as.data.frame();

colnames(user_loc_prov)[13]="DEST_0";
colnames(user_loc_prov)[14]="DEST_1";
colnames(user_loc_prov)[15]="DEST_2";

write.csv(user_loc_prov,"/home/pulselab/analysis/Rizal/user_loc_prov.csv");

########################

library(geosphere);
library(dplyr);

user_loc_prov = read.csv("/home/pulselab/analysis/Rizal/user_loc_prov_complete.csv",stringsAsFactors=FALSE);

mount_kenai = c(167.830453,-15.390058);

user_loc_prov$dist_mountain_o = NA;
user_loc_prov$dist_mountain_d = NA;

user_loc_prov$dist_mountain_o = as.numeric(user_loc_prov$dist_mountain_o);
user_loc_prov$dist_mountain_d = as.numeric(user_loc_prov$dist_mountain_d);

for(i in 1:nrow(user_loc_prov)){

        normal_lat = user_loc_prov$normal_lat[i];
        normal_long = user_loc_prov$normal_long[i];
        volcano_lat = user_loc_prov$volcano_lat[i];
        volcano_long = user_loc_prov$volcano_long[i];

        if(!is.na(user_loc_prov$normal_lat[i])){

                user_loc_prov$dist_mountain_o[i] = distGeo(mount_kenai,c(normal_long,normal_lat));

        }

        if(!is.na(user_loc_prov$volcano_lat[i])){

                user_loc_prov$dist_mountain_d[i] = distGeo(mount_kenai,c(volcano_long,volcano_lat));

        }


}

#############
## Calculate Contiguity? 
## Possibilities: Using connection created by port and airport
#############

#############
## Port Location
#############

lolowai_harbor_ambae = c(167.9731783,-15.3048188);

longana_airport_ambae = c(167.967222,-15.306666);
walaha_airport_ambae = c(167.5525168,-15.4127448);
redcliff_airport_ambae = c(167.833333,-15.466667);

