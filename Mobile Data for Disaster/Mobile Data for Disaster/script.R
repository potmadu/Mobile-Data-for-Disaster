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

