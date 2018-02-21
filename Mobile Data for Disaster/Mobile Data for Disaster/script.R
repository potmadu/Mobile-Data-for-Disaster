datasets_dir = "E:/Datasets/Mobile-Data-for-Disaster/";

datasets_dir = "C:/Datasets/Mobile-Data-for-Disaster/";

lists = list.files(datasets_dir);

for (i in 1:length(lists)) {

    loc_file = paste(datasets_dir,lists[i],sep = "");

    if (i==1) {
        output = read.csv(loc_file, stringsAsFactors = FALSE);
        output$week_type = lists[i];
    } else {
        dat = read.csv(loc_file, stringsAsFactors = FALSE);
        dat$week_type = lists[i];
        output = rbind(output,dat);
    }

}

dat = output;

#############################
# PREPROCESSING
#############################

dat_isna = dat[is.na(dat$reporting__number_of_records),];
dat_filt = dat[!is.na(dat$reporting__number_of_records),];

write.csv(dat_filt,"week-43-52-result.csv");

#############################
# METHOD
#############################
