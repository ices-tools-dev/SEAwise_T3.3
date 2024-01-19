SEAwise Analysis and data of Gulf on Riga herring. Weight at age and mean annual growth of 2-5 year old spring spawning herring.

data.R: 
1) Data sources. Environmental data, i.e. air temperature is obtained from Kihnu weather station (58.09861°N, 23.97028°E), provided by the Estonian Weather Service. Zooplankton data from BIOR. Gulf of Riga herring weight and SSB values are obtained from ICES 2023 report: https://doi.org/10.17895/ices.pub.23123768.v1 and Ojaveer et al. 2004. 
2) Time-period: 1961-2020.
3) Data files (do not contain raw data, except SSB and fish weight values): GoR_growth_2024; GoR_WAA_2024.01;

model.R: 
1) GAM models of WAA 2-5 (1961-2020)
2) GAM models of mean annual growth 2-5 (1961-2020); and GAM models of mean annual growth 2-5 of separate time-series: TS1 (1961-1988) and TS2 (1989-2020)

output.R: 
1) Time-series figures (1960-2020)
2) GAM figures for GoR herring WAA 2-5 (1961-2020)
3) GAM figures for GoR herring mean annual growth of ages 2-5 (1961-2020); and GAM figures for GoR herring mean annual growth of ages 2-5 TS1 (1961-1988) and TS2 (1989-2020)
4) Sliding window analysis figures of mean annual growth of ages 2-5 (1961-2020)
   
