#
#  NEWS_DIFF EXTRACT BASED ON INPUT URL FROM RSS FEED(S)
#  

#  START TIME AND LOAD LIBRARIES

Sys.setenv(TZ='America/New_York')
rssstarttime <- proc.time()

library(feedeR)
library(data.table)
library(rvest)
library(dplyr)
library(rtweet)
library(RSelenium)
library(methods)
library(magick)

#  USER CONFIGURABLE INPUT PARAMETERS

# Edit these three lines for the respective directory locations
# It is suggested to make images and backups as subdirectories of the main_dir
# i.e., img_dir = /main_dir/images/, bkup_dir = /main_dir/backups/
# Note - you must include the trailing slash and you must leave the quotation marks
main_dir <- "INSERT MAIN DIRECTORY NAME HERE"
img_dir <- "INSERT IMAGE STORAGE DIRECTOR NAME HERE"
bkup_dir <- "INSERT BACKUP STORAGE DIRECTORY NAME HERE"

# Replace "maggie_diff_twitter_token.rds" with your twitter token file name
# As noted below, it is suggested to place this file in the main_dir
# See http://rtweet.info/articles/auth.html on how to create this twitter token
# You must leave the quotation marks and list your filename within those
twitter_token <- readRDS(paste0(main_dir,"INSERT YOUR TWITTER TOKEN FILENAME"))

# newsdiffs.org instance you plan to collect diffs from. This can be a "homegrown"
# instance based on the newsdiffs git (https://github.com/ecprice/newsdiffs/)
# or their main website at newsdiffs.org ("http://newsdiffs.org")
# Note - you should NOT include the trailing slash and you must leave the quotation marks
newsdiff_main <- "INSERT NEWSDIFF WEBSITE INSTANCE HERE"

# Input URL parameter (Note: Edit this with caution! You need to ensure 
# that the NYT author you insert here is also scraped via the newsdiffs instance!)
rss_getlink1 <- "https://www.nytimes.com/svc/collections/v1/publish/http://www.nytimes.com/by/maggie-haberman/rss.xml"



###                                                                                   ###
### YOU SHOULD NOT EDIT ANYTHING BELOW THIS UNLESS YOU REALLY KNOW WHAT YOU ARE DOING ###
###                                                                                   ###

newsdiff_file <- paste0(main_dir,"maggie_diff.csv")
newsdiff_file_bkup <- paste0(paste0(bkup_dir,"maggie_diff_"),format(Sys.time(), "%Y%m%d_%H%M%S"),".csv")
newsdiff_csv <- read.csv(newsdiff_file, header = TRUE, sep = ",", check.names=FALSE)

sink_msgs <- file(paste0(main_dir,"std_msgs.txt"), open="at")
sink(sink_msgs,type=c("message"),append = TRUE)
sink(sink_msgs,type=c("output"),append = TRUE)

#  RSS LINK SCRAPE

rss_linktbl <- feed.extract(rss_getlink1)
rss_linklist <- data.frame(rss_linktbl$items["title"],rss_linktbl$items["link"],rss_linktbl$items["date"])
rss_linklist$title <- sub("â€™", "'", rss_linklist$title)
rss_linklist$title <- sub("â€˜", "'", rss_linklist$title)
rss_linklist$title <- sub("’", "'", rss_linklist$title)
rss_linklist$title <- sub("’", "'", rss_linklist$title)
rss_linklist$title <- sub("‘", "'", rss_linklist$title)

#  FILTER FOR ARTICLES ONLY WITHIN RECENT 7 DAYS
rss_linklist <- rss_linklist[rss_linklist$date >= (Sys.Date()-7),]
rss_linklist <- rss_linklist[,c("title","link")]

#  NEWS_DIFF LINK BUILD

rss_linklist$linkstrip <- sub("https://", "",rss_linklist$link)
rss_linklist$newsdiff_link <- paste0(newsdiff_main,"/article-history/https%3A/",rss_linklist$linkstrip)
newsdiff_linklist <- rss_linklist["newsdiff_link"]

#  NEWS_DIFF SCRAPE

newsdiff_tbllist <- list()
ndindex <- 1

for (ndx in 1:nrow(newsdiff_linklist)) {
  newsdiff_tbllist[[ndindex]] <- data.frame("diff_link_list" = matrix(unlist(html_attr(html_nodes(read_html(newsdiff_linklist[ndx,1]), "a"), "href")),byrow=T),stringsAsFactors = FALSE)
  newsdiff_tbllist[[ndindex]][,"newsdiff_link"] <- newsdiff_linklist[ndx,1]
  ndindex = ndindex+1
}

newsdiff_tbl <- rbindlist(newsdiff_tbllist[1:length(newsdiff_tbllist)],fill=TRUE)
setDF(newsdiff_tbl)

#  CLEANUP AND ORDERING

newsdiff_tbl <- newsdiff_tbl[grep("/diff/", newsdiff_tbl$diff_link_list), ]
newsdiff_tbl <- merge(newsdiff_tbl,rss_linklist,by=c("newsdiff_link"),all.x=TRUE)
newsdiff_tbl$newsdiff_fulllink <- paste0(newsdiff_main,newsdiff_tbl$diff_link_list)
newsdiff_tbl$order <- as.integer(sub('.*\\/','',sub('/https.*','',newsdiff_tbl$diff_link_list)))
newsdiff_tbl <- newsdiff_tbl[order(-newsdiff_tbl$order),]
newsdiff_final <- newsdiff_tbl[, c("order","title","newsdiff_fulllink","link")]

#  COMPARE AND CHECK IF ANY NEW NEWSDIFFS

newsdiff_csv$title <- as.character(newsdiff_csv$title)
newsdiff_csv$newsdiff_fulllink <- as.character(newsdiff_csv$newsdiff_fulllink)
newsdiff_csv$link <- as.character(newsdiff_csv$link)
newsdiff_new <- anti_join(newsdiff_final,newsdiff_csv,by=c("order","newsdiff_fulllink","link"))

#  GET ORIGINAL MAGGIET1M3 STATUS_ID FOR NEW NEWSDIFFS

if(nrow(newsdiff_new)>0){
  for (mdx in 1:nrow(newsdiff_new)) {
    search_query <- paste0("from:maggiet1m3 ",newsdiff_new[mdx,"link"])
    maggietime <- search_tweets(search_query,token=twitter_token)
    newsdiff_new[mdx,"maggiestatus"] <- maggietime$status_id
    Sys.sleep(1)
  }
}

# IMAGE CAPTURE FOR NEW NEWSDIFFS

if(nrow(newsdiff_new)>0){
  
  newsdiff_new <- newsdiff_new[order(newsdiff_new$order),]
  
  # RSelenium Setup and Page Navigation
  eCaps <- list(chromeOptions = list(args = c('--headless','--hide-scrollbars','--window-size=414,736','--force-device-scale-factor=2')))
  rD <- rsDriver(extraCapabilities = eCaps)
  remDr <- rD$client
  
  for (imdx in 1:nrow(newsdiff_new)) {
    
    remDr$navigate(newsdiff_new$newsdiff_fulllink[imdx])
    
    # Get page elements of interest and calculate locations
    pg_elements_del <- remDr$findElements(value = "//*[@id=\"compare\"]/del")
    pg_elements_ins <- remDr$findElements(value = "//*[@id=\"compare\"]/ins")
    
    pg_elements_del_tbllist <- data.frame(matrix(nrow = length(pg_elements_del), ncol = 0))
    pg_elements_ins_tbllist <- data.frame(matrix(nrow = length(pg_elements_ins), ncol = 0))
    
    if(nrow(pg_elements_del_tbllist)>0){
      for (pgddx in 1:length(pg_elements_del)) {
        pg_elements_del_tbllist[pgddx,"x"] <- pg_elements_del[[pgddx]]$getElementLocation()$x
        pg_elements_del_tbllist[pgddx,"y"] <- pg_elements_del[[pgddx]]$getElementLocation()$y
        pg_elements_del_tbllist[pgddx,"width"] <- pg_elements_del[[pgddx]]$getElementSize()$width
        pg_elements_del_tbllist[pgddx,"height"] <- pg_elements_del[[pgddx]]$getElementSize()$height
      }
    }
    
    if(nrow(pg_elements_ins_tbllist)>0){
      for (piddx in 1:length(pg_elements_ins)) {
        pg_elements_ins_tbllist[piddx,"x"] <- pg_elements_ins[[piddx]]$getElementLocation()$x
        pg_elements_ins_tbllist[piddx,"y"] <- pg_elements_ins[[piddx]]$getElementLocation()$y
        pg_elements_ins_tbllist[piddx,"width"] <- pg_elements_ins[[piddx]]$getElementSize()$width
        pg_elements_ins_tbllist[piddx,"height"] <- pg_elements_ins[[piddx]]$getElementSize()$height
      }
    }
    
    del_ins_df <- rbind(pg_elements_del_tbllist,pg_elements_ins_tbllist)
    del_ins_df <- del_ins_df[del_ins_df$width!=0,]
    del_ins_df <- del_ins_df[order(del_ins_df$y),]
    del_ins_df$max_x <- del_ins_df$x + del_ins_df$width
    del_ins_df$max_y <- del_ins_df$y + del_ins_df$height
    del_ins_df$min_height <- min(del_ins_df$height)
    del_ins_df$max_height <- max(del_ins_df$height)
    
    left <- 0
    top <- min(del_ins_df$y)
    total_height <- max(del_ins_df$max_y)-top
    
    # Very large conditional section for screen capture and tweet
    # Note: iPhone8+ is 414x736, max that looks reasonable is 414x980. With padding this is 414x900
    
    max_screen_w <- 414
    max_screen_h <- 981.5
    
    if(total_height <= 900) {
      
      # This is Case 1. Entire screencap is less than 1.33x mobile height. Just screencap/tweet.
      
      top_s <- min(del_ins_df$y)-40
      windowy <- max(del_ins_df$max_y)+42
      
      remDr$setWindowSize(max_screen_w,windowy)
      screencap <- remDr$screenshot(file = paste0(img_dir,"1_",newsdiff_new$order[imdx],".png"))
      cropread <- image_read(paste0(img_dir,"1_",newsdiff_new$order[imdx],".png"))
      cropsize <- paste0(2*(max_screen_w),"x",2*(total_height+40+41.5),"+",2*(left),"+",2*(top_s))
      screencrop <- image_crop(cropread, cropsize)
      image_write(screencrop, path = paste0(img_dir,"1_",newsdiff_new$order[imdx],".png"), format = "png")
      # Post Tweet
      imagelink <- paste0(img_dir,"1_",newsdiff_new$order[imdx],".png")
      news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
      post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
      
    } else if(nrow(del_ins_df)==1){
      
      # This is 2nd case. This is to strictly handle a single edit. We will check number of screens needed
      # for the single edit. If > 4, we will watermark and tweet first screen only. If < 4 we will loop
      # as needed to create images and tweet.
      
      height <- total_height
      num_screen <- ceiling(height/900)
      
      if(num_screen>4){
        top_s <- min(del_ins_df$y)-40
        windowy <- max(del_ins_df$max_y)+42
        # Screen capture the full page+edit
        remDr$setWindowSize(max_screen_w,windowy)
        screencap <- remDr$screenshot(file = paste0(img_dir,"2_",newsdiff_new$order[imdx],".png"))
        cropread <- image_read(paste0(img_dir,"2_",newsdiff_new$order[imdx],".png"))
        # Now we crop that single edit to max display height and add the "watermark"
        cropsize <- paste0(2*(max_screen_w),"x",2*(max_screen_h),"+",2*(left),"+",2*(top_s))
        screencrop <- image_crop(cropread, cropsize)
        # Watermark section
        wmark_h <- 98.5
        bcropsize <- paste0(2*(max_screen_w),"x",2*wmark_h,"+",2*(left),"+",2*(top_s)+.5*(2*max_screen_h - 2*wmark_h))
        bcrop <- image_crop(cropread, bcropsize)
        bcropblur <- image_blur(bcrop,5,4)
        img <- image_draw(bcropblur)
        rect(0,wmark_h-40,2*max_screen_w,2*(wmark_h-30),col= rgb(1.0,1.0,1.0,alpha=0.80),border=NA)
        text(max_screen_w-25,wmark_h,"There are large changes - click link in tweet to view all",col="red",cex=2.5,font=2)
        dev.off()
        # Paste the watermark on top of the screencrop
        bcropoffset <- paste0("+0+",(.5*2*max_screen_h - .5*2*wmark_h))
        screencrop <- image_composite(screencrop,img, offset = bcropoffset)
        image_write(screencrop, path = paste0(img_dir,"2_",newsdiff_new$order[imdx],".png"), format = "png")
        # Post Tweet
        imagelink <- paste0(img_dir,"2_",newsdiff_new$order[imdx],".png")
        news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
        post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
      } else {
        top_s <- min(del_ins_df$y)-40
        windowy <- max(del_ins_df$max_y)+42
        # Screen capture the full page+edit
        remDr$setWindowSize(max_screen_w,windowy)
        screencap <- remDr$screenshot(file = paste0(img_dir,"2_",newsdiff_new$order[imdx],".png"))
        cropread <- image_read(paste0(img_dir,"2_",newsdiff_new$order[imdx],".png"))
        # Now we loop to crop that single edit
        screen_overlap <- 20
        imagelink <- list()
        for (sc2dx in 1:num_screen){
          cropsize <- paste0(2*(max_screen_w),"x",2*(max_screen_h),"+",2*(left),"+",2*(top_s+(max_screen_h*(sc2dx-1)))-ifelse(sc2dx==1,0,2*screen_overlap*(sc2dx-1)))
          screencrop <- image_crop(cropread, cropsize)
          image_write(screencrop, path = paste0(img_dir,"2_",newsdiff_new$order[imdx],"_",sc2dx,".png"), format = "png")
          imagelink[sc2dx] <- paste0(img_dir,"2_",newsdiff_new$order[imdx],"_",sc2dx,".png")
        }
        # Post Tweet
        news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
        post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
        file.remove(paste0(img_dir,"2_",newsdiff_new$order[imdx],".png"))
      }
    } else if(nrow(del_ins_df)==2){
      
      # This is 3rd case. This is to strictly handle two edits. We will check number of screens needed
      # for the edits. If > 4, we will watermark and tweet first screen only. If < 4 we will loop
      # as needed to create images and tweet.
      
      tot_num_screen <- sum(ceiling(del_ins_df$height/900))
      del_ins_df$num_screen <- ceiling(del_ins_df$height/900)
      
      if(tot_num_screen>4){
        windowy <- max(del_ins_df$max_y)+42
        # Screen capture the full page+edit
        remDr$setWindowSize(max_screen_w,windowy)
        screencap <- remDr$screenshot(file = paste0(img_dir,"3_",newsdiff_new$order[imdx],".png"))
        cropread <- image_read(paste0(img_dir,"3_",newsdiff_new$order[imdx],".png"))
        # This section finds the first instance of the >4x mobile height edit
        maxh_del_ins_df <- del_ins_df[del_ins_df$height == del_ins_df$max_height,]
        maxh_del_ins_df$min_y <- min(maxh_del_ins_df$y)
        maxh_del_ins_df <- maxh_del_ins_df[maxh_del_ins_df$y == maxh_del_ins_df$min_y,]
        maxh_del_ins_df$min_x <- min(maxh_del_ins_df$x)
        maxh_del_ins_df <- maxh_del_ins_df[maxh_del_ins_df$x == maxh_del_ins_df$min_x,]
        top_s <- maxh_del_ins_df$y-40
        # Now we crop that single edit to max display height and add the "watermark"
        cropsize <- paste0(2*(max_screen_w),"x",2*(max_screen_h),"+",2*(left),"+",2*(top_s))
        screencrop <- image_crop(cropread, cropsize)
        # Watermark section
        wmark_h <- 98.5
        bcropsize <- paste0(2*(max_screen_w),"x",2*wmark_h,"+",2*(left),"+",2*(top_s)+.5*(2*max_screen_h - 2*wmark_h))
        bcrop <- image_crop(cropread, bcropsize)
        bcropblur <- image_blur(bcrop,5,4)
        img <- image_draw(bcropblur)
        rect(0,wmark_h-40,2*max_screen_w,2*(wmark_h-30),col= rgb(1.0,1.0,1.0,alpha=0.80),border=NA)
        text(max_screen_w-25,wmark_h,"There are large changes - click link in tweet to view all",col="red",cex=2.5,font=2)
        dev.off()
        # Paste the watermark on top of the screencrop
        bcropoffset <- paste0("+0+",(.5*2*max_screen_h - .5*2*wmark_h))
        screencrop <- image_composite(screencrop,img, offset = bcropoffset)
        image_write(screencrop, path = paste0(img_dir,"3_",newsdiff_new$order[imdx],".png"), format = "png")
        # Post Tweet
        imagelink <- paste0(img_dir,"3_",newsdiff_new$order[imdx],".png")
        news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
        post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
      } else {
        top_s <- min(del_ins_df$y)-40
        windowy <- max(del_ins_df$max_y)+42
        # Screen capture the full page+edit
        remDr$setWindowSize(max_screen_w,windowy)
        screencap <- remDr$screenshot(file = paste0(img_dir,"3_",newsdiff_new$order[imdx],".png"))
        cropread <- image_read(paste0(img_dir,"3_",newsdiff_new$order[imdx],".png"))
        # Now we loop to crop the two edits
        screen_overlap <- 20
        imagelink <- list()
        for (sc3dx1 in 1:nrow(del_ins_df)){
          sc3_screencap <- del_ins_df[sc3dx1,]
          sc3_top <- sc3_screencap$y-40
          sc3_height <- sc3_screencap$max_y-sc3_top+40+41.5
          sc3_windowy <- sc3_screencap$max_y+42
          for (sc3dx2 in 1:sc3_screencap$num_screen){
            remDr$setWindowSize(max_screen_w,sc3_windowy)
            screencap <- remDr$screenshot(file = paste0(img_dir,"3_",newsdiff_new$order[imdx],"_",sc3dx1,"_",sc3dx2,".png"))
            cropread <- image_read(paste0(img_dir,"3_",newsdiff_new$order[imdx],"_",sc3dx1,"_",sc3dx2,".png"))
            cropsize <- paste0(2*(max_screen_w),"x",2*(max_screen_h),"+",2*(left),"+",2*(sc3_top+(max_screen_h*(sc3dx2-1)))-ifelse(sc3dx2==1,0,2*screen_overlap*(sc3dx2-1)))
            screencrop <- image_crop(cropread, cropsize)
            image_write(screencrop, path = paste0(img_dir,"3_",newsdiff_new$order[imdx],"_",sc3dx1,"_",sc3dx2,".png"))
            imagelink[sc3dx2] <- paste0(img_dir,"3_",newsdiff_new$order[imdx],"_",sc3dx1,"_",sc3dx2,".png")
          }
          # Post Tweet
          news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
          post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
        }
        file.remove(paste0(img_dir,"3_",newsdiff_new$order[imdx],".png"))
      }
    } else {
      
      # This is 4th case. This is to handle 3+ edits. We will check number of screens needed
      # for the edits. If > 4, we will watermark and tweet first screen only. If < 4 we will loop
      # as needed to create images and tweet. Note, we spatially cluster the edits by 
      # kmeans before calculating screen height needed.
      
      # Manipulations to get the kmeans clustering correct. Order by y-locn and ensure kmeans is calculated
      # on both max-y and next starting-y of each block
      del_ins_df$lead_my <- lead(del_ins_df$max_y, n = 1L, order_by = del_ins_df$y,default=max(del_ins_df$max_y))
      del_ins_df$cluster2_raw <- kmeans(cbind(del_ins_df$y,del_ins_df$max_y,del_ins_df$lead_my),centers = 2,nstart = 20)$cluster
      del_ins_df <- del_ins_df %>% group_by(cluster2_raw) %>% mutate(c2_height = max(max_y)-min(y))
      if(length(unique(del_ins_df$y)) == 4){
        del_ins_df$cluster3_raw <- kmeans(cbind(del_ins_df$max_y,del_ins_df$lead_my),centers = 3,nstart = 20)$cluster
        del_ins_df <- del_ins_df %>% group_by(cluster3_raw) %>% mutate(c3_height = max(max_y)-min(y))
      }
      if(length(unique(del_ins_df$y)) > 4){
        del_ins_df$cluster3_raw <- kmeans(cbind(del_ins_df$max_y,del_ins_df$lead_my),centers = 3,nstart = 20)$cluster
        del_ins_df <- del_ins_df %>% group_by(cluster3_raw) %>% mutate(c3_height = max(max_y)-min(y))
        del_ins_df$cluster4_raw <- kmeans(cbind(del_ins_df$max_y,del_ins_df$lead_my),centers = 4,nstart = 20)$cluster
        del_ins_df <- del_ins_df %>% group_by(cluster4_raw) %>% mutate(c4_height = max(max_y)-min(y))
      }
      ### NEED TO HANDLE MULTI SCREEN CAPTURES WHERE MAX SCREENS IS < 4
      if(max(del_ins_df$c2_height)<=max_screen_h){
        colnames(del_ins_df)[names(del_ins_df) == "cluster2_raw"] <- "cluster_raw"
        colnames(del_ins_df)[names(del_ins_df) == "c2_height"] <- "c_height"
        del_ins_df <- del_ins_df[,c("x","y","width","height","max_x","max_y","min_height","max_height","lead_my","cluster_raw","c_height"),]
        km_indx <- 2
      } else if(max(del_ins_df$c3_height)<=max_screen_h){
        colnames(del_ins_df)[names(del_ins_df) == "cluster3_raw"] <- "cluster_raw"
        colnames(del_ins_df)[names(del_ins_df) == "c3_height"] <- "c_height"
        del_ins_df <- del_ins_df[,c("x","y","width","height","max_x","max_y","min_height","max_height","lead_my","cluster_raw","c_height"),]
        km_indx <- 3
      } else if(max(del_ins_df$c4_height)<=max_screen_h & length(unique(del_ins_df$y)) > 4){
        colnames(del_ins_df)[names(del_ins_df) == "cluster4_raw"] <- "cluster_raw"
        colnames(del_ins_df)[names(del_ins_df) == "c4_height"] <- "c_height"
        del_ins_df <- del_ins_df[,c("x","y","width","height","max_x","max_y","min_height","max_height","lead_my","cluster_raw","c_height"),]
        km_indx <- 4
      } else{
        colnames(del_ins_df)[names(del_ins_df) == "cluster2_raw"] <- "cluster_raw"
        colnames(del_ins_df)[names(del_ins_df) == "c2_height"] <- "c_height"
        del_ins_df <- del_ins_df[,c("x","y","width","height","max_x","max_y","min_height","max_height","lead_my","cluster_raw","c_height"),]
        km_indx <- 1
      }
      
      del_ins_df$cluster <- ifelse(del_ins_df$cluster_raw==lag(del_ins_df$cluster_raw, n = 1L, order_by = del_ins_df$y,default=1),0,1)
      del_ins_df[1,"cluster"] <- 1
      del_ins_df$cluster <- cumsum(del_ins_df$cluster)
      
      # Screencap/Crop/Pad for each cluster
      if (km_indx==1){
        windowy <- max(del_ins_df$max_y)+42
        # Screen capture the full page+edit
        remDr$setWindowSize(max_screen_w,windowy)
        screencap <- remDr$screenshot(file = paste0(img_dir,"4_",newsdiff_new$order[imdx],".png"))
        cropread <- image_read(paste0(img_dir,"4_",newsdiff_new$order[imdx],".png"))
        # This section finds the first instance of the >4x mobile height edit
        maxh_del_ins_df <- del_ins_df[del_ins_df$height == del_ins_df$max_height,]
        maxh_del_ins_df$min_y <- min(maxh_del_ins_df$y)
        maxh_del_ins_df <- maxh_del_ins_df[maxh_del_ins_df$y == maxh_del_ins_df$min_y,]
        maxh_del_ins_df$min_x <- min(maxh_del_ins_df$x)
        maxh_del_ins_df <- maxh_del_ins_df[maxh_del_ins_df$x == maxh_del_ins_df$min_x,]
        top_s <- maxh_del_ins_df$y-40
        # Now we crop that single edit to max display height and add the "watermark"
        cropsize <- paste0(2*(max_screen_w),"x",2*(max_screen_h),"+",2*(left),"+",2*(top_s))
        screencrop <- image_crop(cropread, cropsize)
        # Watermark section
        wmark_h <- 98.5
        bcropsize <- paste0(2*(max_screen_w),"x",2*wmark_h,"+",2*(left),"+",2*(top_s)+.5*(2*max_screen_h - 2*wmark_h))
        bcrop <- image_crop(cropread, bcropsize)
        bcropblur <- image_blur(bcrop,5,4)
        img <- image_draw(bcropblur)
        rect(0,wmark_h-40,2*max_screen_w,2*(wmark_h-30),col= rgb(1.0,1.0,1.0,alpha=0.80),border=NA)
        text(max_screen_w-25,wmark_h,"There are large changes - click link in tweet to view all",col="red",cex=2.5,font=2)
        dev.off()
        # Paste the watermark on top of the screencrop
        bcropoffset <- paste0("+0+",(.5*2*max_screen_h - .5*2*wmark_h))
        screencrop <- image_composite(screencrop,img, offset = bcropoffset)
        image_write(screencrop, path = paste0(img_dir,"4_",newsdiff_new$order[imdx],".png"), format = "png")
        # Post Tweet
        imagelink <- paste0(img_dir,"4_",newsdiff_new$order[imdx],".png")
        news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
        post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
      } else{
        screen_overlap <- 20
        imagelink <- list()
        for (sc4dx in 1:km_indx){
          ksc_screencap_m <- del_ins_df[del_ins_df$cluster==sc4dx,]
          ksc_top <- min(ksc_screencap_m$y)-40
          ksc_height <- max(ksc_screencap_m$max_y)-top+40+41.5
          ksc_windowy <- max(ksc_screencap_m$max_y)+42
          remDr$setWindowSize(max_screen_w,ksc_windowy)
          screencap <- remDr$screenshot(file = paste0(img_dir,"4_",newsdiff_new$order[imdx],"_",sc4dx,".png"))
          cropread <- image_read(paste0(img_dir,"4_",newsdiff_new$order[imdx],"_",sc4dx,".png"))
          cropsize <- paste0(2*(max_screen_w),"x",2*(ksc_height),"+",2*(left),"+",2*(ksc_top))
          screencrop <- image_crop(cropread, cropsize)
          image_write(screencrop, path = paste0(img_dir,"4_",newsdiff_new$order[imdx],"_",sc4dx,".png"), format = "png")
          imagelink[sc4dx] <- paste0(img_dir,"4_",newsdiff_new$order[imdx],"_",sc4dx,".png")
        }
        # Post Tweet
        news_diff_tweet <- paste0(".@maggiet1m3 Update to: '",newsdiff_new$title[imdx],"' ",newsdiff_new$newsdiff_fulllink[imdx])
        post_tweet(status = news_diff_tweet,in_reply_to_status_id = newsdiff_new[imdx,"maggiestatus"],media = imagelink,token=twitter_token)
      }
    } 
  }
  remDr$close()
}

# SAVE NEW COMPARISON FILE

if(nrow(newsdiff_new)>0){
  write.csv(newsdiff_final,file = newsdiff_file,row.names=FALSE)
  write.csv(newsdiff_final,file = newsdiff_file_bkup,row.names=FALSE)
  newsdiff_new["maggiestatus"] <- NA
}

# FINAL TIME TO RUN CALCULATION

rssendtime <- proc.time() - rssstarttime
rssrunsecs <- rssendtime[3]
print(rssrunsecs)
print(Sys.time())
cat("\n\n")

sink(type="message")
sink(type="output")
close(sink_msgs)
