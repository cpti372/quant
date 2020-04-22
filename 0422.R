pkg = c('magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
        'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
        'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
        'highcharter', 'plotly', 'PerformanceAnalytics',
        'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
        'timetk', 'broom', 'stargazer', 'timeSeries')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)}
#API 이용해서 주식 분석하기 
url.aapl = "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl = read.csv(url.aapl)
head(data.aapl)
#getsymbols 함수 이용하기 
library(quantmod)
getSymbols('AAPL')
head(AAPL)
chart_Series(AAPL)
chart_Series(Ad(AAPL))
#open: 시가 High:고가 Low: 저가 Close:종가 Volume:거래량 Adjusted: 배당이 반영된 수정주가 
data = getSymbols('AAPL',
                  from = '2000-01-01', to = '2018-12-31',
                  auto.assign = FALSE)
head(data)
#페이스북과 엔비디아의 티커
ticker=c('FB','NVDA')
getSymbols(ticker)
head(FB)
head(NVDA)

#국내 종목 주가 다운로드 
#삼성전자 코스피 티커 
getSymbols('005930.KS',
           from = '2000-01-01', to = '2018-12-31')
tail(Ad(`005930.KS`))
tail(Cl(`005930.KS`))
#셀트리온 코스닥 티커 
getSymbols("068760.KQ",
           from = '2000-01-01', to = '2018-12-31')
tail(Cl(`068760.KQ`))
#미국 연준 티커
getSymbols('DGS10', src='FRED')
chart_Series(DGS10)
#DEX티커
getSymbols('DEXKOUS', src='FRED')
tail(DEXKOUS)
