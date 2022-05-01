# Advanced-Technology-Trend
A real-time tool, Tech Trend Tracker, which automates the generation of bar charts and graphs by using R language to crawl the Indeed.com and perform keywords frequency analysis of different categories of technologies.

ShinyApp link: https://nyuprof.shinyapps.io/TechTrendTracker_DuoduoWang/

**Project inside Process:**
1. Scrap the job URL from the Indeed.com
2. Scanning and analysing the text from each URL
3. Generate CSV files and store it in the Dropbox cloud account
4. Generate bar charts and graphs

**User Guide of the tool:**

Step 1: Select category to scan

1. Select None: Check the data without scanning, you don't need extra time to wait. Skip the Scanning page and click the Trend page to view the charts.
2. Multiple Choice: Select one or many categories for scanning, and only the scanned data can generate the latest data. Select the categories needed to scan, click the scanning button, the font color in the box will turn gray. After the 'Scanning is finished' appears, click the Trend page to view the result.

		Tips: Due to the anti-crawl settings of the Indeed.com , it takes a few minutes to scan each category (which also depends on the internet speed). Please make sure you have enough time to scan. The scanning may also be terminated due to the poor network conditions, so please make sure the stable internet network.

Step 2: Check the charts on the Trend page.
    
   	Tips: If you need the latest data, make sure the Scanning page is scanning successfully.
