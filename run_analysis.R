run_analysis <- function(){
        #read table
        xtest <- read.table("./test/X_test.txt")
        ytest <- read.table("./test/Y_test.txt")
        stest <- read.table("./test/subject_test.txt")
        xtrain <- read.table("./train/X_train.txt")
        ytrain <- read.table("./train/Y_train.txt")
        strain <- read.table("./train/subject_train.txt")
        features <- read.table("features.txt")
        a_lavel <- read.table("activity_labels.txt")
        
        #put descriptive activity names
        for(i in 1:nrow(ytest)){
                ytest[i,] <- as.character(a_lavel[ytest[i,],2])
        }
        
        for(i in 1:nrow(ytrain)){
                ytrain[i,] <- as.character(a_lavel[ytrain[i,],2])
        }
        
        #merge data
        #test <- cbind(xtest, ytest)
        #train <- cbind(xtrain, ytrain)
        mergeddata <- rbind(xtest, xtrain)
        activity <- rbind(ytest, ytrain)
        subject <- rbind(stest, strain)
        
        #put col names
        cname <- t(features)[2,]
        #colnames(mergeddata) <- cname
        
        #extract
        ext_s <- grep(pattern="-std()", fixed=TRUE, cname)
        ext_m <- grep(pattern="-mean()", fixed=TRUE, cname)
        ext <- sort(c(ext_m, ext_s))

        ext_data <- mergeddata[,ext[1]]        
        for(i in 2:length(ext)){
                ext_data <- cbind(ext_data, mergeddata[,ext[i]])
        }
        
        ext_name <- as.character(cname[ext[1]])
        for(i in 2:length(ext)){
                ext_name <- c(ext_name, as.character(cname[ext[i]]))
        }        
        
        ext <- data.frame(list(subject, activity, ext_data))
        colnames(ext) <- c("subject", "activity", ext_name)
        
        g_sub <- group_by(ext, subject)
        g_act <- group_by(ext, activity)
        
        dcast(narrow, subject ~ ., max)
        
        
        #average
}