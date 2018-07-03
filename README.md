# maggie_diffs
This is the R code that tweets via the twitter.com/maggiediffs account

This code polls Maggie Haberman's RSS feed, checks a newsdiffs instance for any article updates, and then replies to the corresponding 
https://twitter.com/maggiet1m3 tweet accordingly with a newsdiffs link and screen capture of changes.

Note - this project does not exist with the very valuable and important newsdiffs github (https://github.com/ecprice/newsdiffs/)

Install and Setup Guidance:

The info below is designed for an Linux/Ubuntu setup. It can likely be adpated with little change for MacOS as well.

1. Save the R code to a directory. i.e., /Users/maggie_diff_bot/maggie_diff.R
2. Edit the lines at the top of the code under the "USER CONFIGURABLE INPUT PARAMETERS" section This is done as follows:
2a. Directories: Pick whatever works for you. A suggested structure and notes are listed in the code comments.
2b. Twitter token: If you do not have an rtweet twitter token, the method for obtaining and saving one is here: http://rtweet.info/articles/auth.html
2c. Newsdiffs instance: This is the URL of the newsdiffs instance you plan to scrape from.
2d. RSS link: This is the RSS link of interest to scrape. Two notes: 1) This code was originally designed to work with the https://twitter.com/maggiet1m3 account, hence Maggie Haberman's RSS is listed. 2) Ensure that the author/RSS inserted is also scraped via the newsdiffs instance. Otherwise, you will never have any diffs.
3. Create an empty file where data can be stored. As of July 2, 2018, the file must be in the main_dir and must be specifically named maggie_diff.csv (Linux/Ubuntu command to do this: touch maggie_diff.csv)
4. Create an empty file where errors can be logged. As of July 2, 2018, the file must be in the main_dir and must be specifically named std_msgs.txt (Linux/Ubuntu command to do this: touch std_msgs.txt)
5. At this time, if you run the code, it will populate the file and generate tweets for the latest diffs that it can already find. Note that it only polls those articles that https://twitter.com/maggiet1m3 in past 7 days (limitation of twitter API)
6. If you want to continue polling and tweeting, schedule as needed with crontab (or other scheduling tool). Suggested run frequency is every 5-10 minutes.

Upcoming v1.1 improvements planned (as of July 2, 2018):
1. Improved twitter threading and reply status_id tracking (will allow for tracking > 7 days since reply_id will always be known)
2. Improvement for specific case where number of total screenshots is < 4, but one individual screenshot is > 1 screen
3. Generalize code and take out the Maggie Haberman specific items. Make it as user configurable as possible (so that it works on any Author Feed + NewsDiffs combination)
4. Vectorization of R code and removal of for loops (yes, I know, I'm *still* figuring out my way around R)

Upcoming v2.0 improvements planned (as of July 2, 2018):
Overall to improve integration, performance, and effeciency
1. Integrate https://twitter.com/maggiet1m3 and have a combined account that tweets new articles and diffs (as threaded replies).
2. Do not rely on RSS as it seems to occasionally lag; directly scrape from online author page to get latest articles
3. Enhance diff monitoring: If link < 1 week old, monitor every 1 min; if link < 1 month old, monitor every 10 min; if link < 3 month old, monitor every 30 min; all else, monitor every hour.
4. Ensure other than initial author link input, no dependencies on external sources
5. Optimize code so that total runtime is < 1 sec (excluding the external lookups/scrapes)
