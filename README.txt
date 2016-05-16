DESCRIPTION:
===========
The automated investment tools in RAutoInvest run simple (read: crude, untested and most likely wrong) fundamental residual income
stock valuations on the SP1500 universe in order to build risk/return optimized portfolios. Automated trading is not facilitated.

Input data can be loaded through a file based interface, likewise portfolio position changes can be picked up on a file basis
reading the relevant files in the output directory. At the begining the tools asks for basic investment data and start of with
a default investment policy statement (IPS).
The IPS creation process could be customized looking at cookie data in order to select better tax rate, lifeexpectany and other data.

The code comes with a historic data collection from free sources for demonstration purpose. Read the warning!

WARNING:
========
EDUCATIONAL MATERIAL, NO INVESTMENT OFFER OR RECOMMENDATION OR ADVICE, NOT ASSOCIATED WITH ANY EMPLOYER OR ORGANIZATION NOR ENDORSED BY ANY, MAY CONTAIN MATERIAL ERRORS, NOT FIT FOR ANY BUSINESS USE, IF YOU USE RAutoInvest THE AUTHOR IS AUTOMATICALLY RELEASED FROM LIABILITY WHETHER CAUSED BY NEGLIGENCE OR OTHERWISE

USAGE:
======
1. You need a Quandl account and authentication key to use this, place the key in a file named ".pwd" with the sole content " yourQuandlCode <- 'THECODE' "
2. The INPUT directory contains pre-collected data. If you want this to run say every day and need data updates rename "DataCollectorUtility.bak" to "DataCollectorUtility.R" and schedule your cron jobs accordingly.
3. Not all of the data files are updates through data collection, check that input values are in-line with your expectations
4. To connect with a trading engine refer to the files generated in the OUTPUT directory, resp. write your own output as necessary (i.e., you may prefer weights rather than returns)
5. Remove the investment horizon limit of two years. It's there for work with the limited data set.

INSTALLATION:
=============
1. Ensure that you are either installing into /home/rstudio, define a symbolic link (ln -s) with such a name or change the executionPath argument in eah file
2. If running on a shiny server, create a symbolic link to the installation directory in the shiny server directory (e.g. "/srv/shiny-server")
3. Install dependent packages gloablly so that executing users have the packages available, for an ubuntu setup e.g.:
a) sudo su - -c "R -e \"install.packages(c('PerformanceAnalytics', 'fPortfolio', 'urca', 'vars', 'zoo', 'BLCOP', 'Quandl', 'dygraphs', 'shiny', 'rmarkdown', 'uuid'), repos='http://cloud.r-project.org/')\""
4. Pre-requisit installation of R, shiny server are required, for an ubuntu setup e.g.:
a) sudo apt-get install gdebi-core
b) wget http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.3.0.403-amd64.deb
c) sudo gdebi shiny-server-1.3.0.403-amd64.deb
5. Setup your crontatb entries using the supplied example scripts (NOTE: outside web-server dir to avoid accidentially serving these to users), e.g.:
a) crontab -e # add: 0 4 * * * sudo /home/rstudio/dailyDataBatch.sh
b) crontab -e # add: 0 5 * * * sudo /home/rstudio/dailyOptimizeBatch.sh
6. Ensure that the shiny server does not run as root but as the user shiny:
a) sudo vi /etc/shiny-server/shiny-server.conf # change: run_as shiny;
b) sudo mkdir /var/run/shiny
c) sudo chown shiny /var/run/shiny
d) sudo chgrp shiny /var/run/shiny
e) sudo chmod 755 /var/run/shiny
f) sudo stop shiny-server
g) sudo usermod -G rstudio shiny
h) sudo chmod 775 OUTPUT/
i) sudo chmod 775 INPUT/
j) sudo start shiny-server
