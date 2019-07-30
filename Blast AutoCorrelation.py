# -*- coding: utf-8 -*-
"""
Created on Tue Jul 16 19:18:12 2019

@author: zachz
"""
import pandas as pd
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import acf

'''READ IN DATA'''
msft_prices = pd.read_csv("D:MSFT.csv", index_col = "Date")
msft_prices.index = pd.to_datetime(msft_prices.index)
msft_prices = pd.DataFrame(msft_prices)
msft_prices = msft_prices.resample(how='last', rule='m')
msft_returns = msft_prices.pct_change()
msft_returns = msft_returns.dropna()
title = "Microsoft Monthly % Returns"              
outPath = "D:/{title}.csv".format(title=title)           

print(msft_returns['Adj Close'])

print(acf(msft_returns))
plot_acf(msft_returns, lags = 10, alpha = .05)

'''READ IN DATA'''
otsuka_blast = pd.read_csv("D:Otsuka Blast.csv", index_col = "Date")

otsuka_blast = pd.DataFrame(otsuka_blast)


otsuka_blast = otsuka_blast.dropna()
title = "Microsoft Monthly % Returns"              
outPath = "D:/{title}.csv".format(title=title)           

print(otsuka_blast['Plane Score MA'])
print(acf(otsuka_blast['Plane Score MA']))
plot_acf(otsuka_blast['Plane Score MA'], lags = 10, alpha = .05, title="Plane Score Autocorrelation")

'''READ IN DATA'''
otsuka_blast = pd.read_csv("D:Otsuka Blast.csv", index_col = "Date")

otsuka_blast = pd.DataFrame(otsuka_blast)


otsuka_blast = otsuka_blast.dropna()
title = "Microsoft Monthly % Returns"              
outPath = "D:/{title}.csv".format(title=title)           

print(otsuka_blast['Connection Score MA'])

print(acf(otsuka_blast['Connection Score MA']))
plot_acf(otsuka_blast['Connection Score MA'], lags = 10, alpha = .05, title="Connection Score MA")

'''READ IN DATA'''
otsuka_blast = pd.read_csv("D:Otsuka Blast.csv", index_col = "Date")

otsuka_blast = pd.DataFrame(otsuka_blast)


otsuka_blast = otsuka_blast.dropna()
title = "Microsoft Monthly % Returns"              
outPath = "D:/{title}.csv".format(title=title)           

print(otsuka_blast['Rotation Score MA'])

print(acf(otsuka_blast['Rotation Score MA']))
plot_acf(otsuka_blast['Rotation Score MA'], lags = 10, alpha = .05, title="Rotation Score MA")