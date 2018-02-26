datasets_dir = "E:/Datasets/Mobile-Data-for-Disaster/";

lists = list.files(datasets_dir);

for (i in 1:length(lists)) {

    loc_file = paste(datasets_dir,lists[i],sep = "");

    if (i==1) {
        output = read.csv(loc_file,stringsAsFactors=FALSE);
    } else {
        dat = read.csv(loc_file, stringsAsFactors = FALSE);
        output = rbind(output,dat);
    }

}

<<<<<<< HEAD
<<<<<<< HEAD
dat = output;

#############################
# PREPROCESSING
#############################

dat_isna = dat[is.na(dat$reporting__number_of_records),];
dat_filt = dat[!is.na(dat$reporting__number_of_records),];

home_antenna_loc = unlist(dat_filt$user_home_antenna__location);
home_antenna_loc = gsub('\\(','',home_antenna_loc);
home_antenna_loc = gsub("\\)", "", home_antenna_loc);
home_antenna_loc = gsub(" ", "", home_antenna_loc);
home_antenna_loc = strsplit(home_antenna_loc,",");

write.csv(dat_filt,"week-43-52-result.csv");

#############################
# METHOD
#############################


=======
>>>>>>> parent of 2b9f819... update script code
=======
>>>>>>> parent of 2b9f819... update script code
