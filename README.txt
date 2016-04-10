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
