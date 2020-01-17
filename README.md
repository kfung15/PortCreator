# PortCreator

## Who am I?
First of all, thanks for stopping by! My name is Kennard Fung, and I'm currently a student at Hamilton College, Clinton, NY. I've taken courses in Financial Economics, Accounting, and Machine Learning, and I'm hoping to put these skills to the test in the big wonky world of finance! In the future, I hope to work in the field of risk management, so that I can help people grow/preserve their wealth.

## Ok, what am I looking at?

Well, I'm glad you asked! What you are seeing are several projects, coded in R, that create optimal investment portfolios. Harry Markowitz' Modern Portfolio Theory (MPT) forms the basis of these projects, but the code allows for **experimentation and expansion** whenever someone happens to have a bright idea or two.

In the spirit of MPT, these investments portfolios strive to maximize expected returns per unit of risk, after taking into account constraints. An example of these constraints include the risk-free rates, start-end backdating dates, and liquidity per day.

## How do I use this thing?
Just a quick note before we start: I tested all the code on a Mac, but not (yet) on Windows/Linux, so if there happens to be a bunch of red lines after you run them in those latter OSes, let me know and I'll squish those bugs ASAP.

Also, for this demo, I shall be presenting data on the LQ45. Think of it like the Indonesian S&P500, except 45 stocks.

1. For a quick and easy demo, download all three file and put them in the same folder.
2. Download the latest version of R (3.6.2 at the time of writing this)
3. Download RStudio
4. Open RStudio
5. Open both the Tangency Portfolio Maker and the Brute Force Portfolio Maker in RStudio
6. **After** you have made the changes in the sections below, highlight all the code (in the respective projects) and hit Run.

### Tangency Portfolio Maker (Machine Generated, No Stationarity)

Lines 33, 43: (Mandatory) Adjust the path to match the download folder (CMD+I for Mac, right-click properties for Windows)

Lines 52, 53: (Optional) Adjust the start and end dates

Line 151: (Optional) Adjust the liquidity per day (200MM Indonesian Rupiah (~14.6k USD) by default)

Line 229: (Optional) Adjust the risk free rate (explanation is on lines 225 - 228)

Line 319: (Optional) Replace “TopStocks.csv” with whatever file name that you want

Lines 337, 341: (Optional) If you adjusted the start and end dates in lines 33 and 43, you have to adjust the “end=“ dates in lines 337, 341 to obtain one price. If not you will run into an error.

### Brute Force (Machine Generated, No Stationarity, Asset Weight Limited)

Lines 45, 55: (Mandatory) Adjust the path to match the download folder (CMD+I for Mac, right-click properties for Windows)

Lines 64: (Optional) Type how many stock you want in the final portfolio (default is 10)

Lines 67, 68: (Optional) Adjust the start and end dates

Line 164: (Optional) Adjust the liquidity per day (200MM Indonesian Rupiah by default)

Line 242: (Optional) Adjust the risk free rate (explanation is on lines 238 - 241)

Lines 305, 308: (Optional) Adjust how many portfolios you want to get considered

Line 319: (Mandatory) Adjust the minimum and maximum asset weights. (Default is 5% minimum and 10% maximum). Note that this project will throw out hypothetical portfolios that have a total asset weight greater than 100%, so if you're catching nothing at the end, try bumping down the min and max values.

Lines 482, 486: (Optional) If you adjusted the start and end dates in lines 67 and 68, you have to adjust the “end=“ dates in lines 482, 486 to obtain a start and an end price.

Line 543: Replace “Best Portfolio So Far (2nd Try seed 1238).csv” with whatever file name that you want

## Final notes

If for some reason something doesn't work, check your internet connection first. Internet is required to download data off Yahoo Finance.
If something is still broken, please send me a message on Issues. I'll get back to you ASAP.

## To-do list
[ ] Post projects that include the stationarity option.

[ ] Post nysedata Prices and Updater files.

[ ] Post projects that assume that historical data is already available in the filesystem.

[ ] Stretch my creativity and think of other ways to evaluate data and create nice, reliable portfolios.

[ ] Talk to more people about this project.
