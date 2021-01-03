#주가 다운로드 
url.aapl = "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl=read.csv(url.aapl)
head(data.aapl)

#주가 다운로드 2
library(quantmod)
getSymbols('AAPL') #애플티커 입력,xts 형태로 
head(AAPL) #open:시가/ high:고가, low:저가, close:종가, Volume:거래량, Adjusted: 배당이 반영된 수정주가 
chart_Series(Ad(AAPL))

#getsymbol옵션 파악
data=getSymbols('AAPL',from='2020-03-01', to ='2021-01-03',auto.assign = FALSE) #원하는 변수에 저장할 수 있게함 

#한 번에 여러개 
ticker=c("FB","NVDA")
getSymbols(ticker)
head(FB)

#국내 종목 주가 다운로드: 티커.KS/ 티커. KQ
#삼성전가 주가 파악하기 
getSymbols('005930.KS',
           from = '2000-01-01', to = '2018-12-31')
tail(Ad(`005930.KS`))
tail(Cl(`005930.KS`)) #종가파악하기 

#셀트리온 제약 주가 파악하기 
getSymbols("068760.KQ",
           from = '2020-03-01', to = '2020-12-31')
tail(Cl(`068760.KQ`))

#미국 연준 데이터
#미국 국채 10년물 금리 파악
getSymbols('DGS10', src='FRED')
chart_Series(DGS10)

#한국 환율 파악하기 
getSymbols('DEXKOUS', src='FRED')
tail(DEXKOUS)

#GET 방식: 데이터나 파일을 요청, 선택 항목에 따라 웹페이지 주소가 변경된다
#POST 방식: body를 통해 전송하는 것
library(rvest)
library(httr)
url='https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258'
data=GET(url)
print(data)
data_title=data %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes('dl') %>% #해당 태그를 추출하는 함수이다
  html_nodes('.articleSubject') %>% #클래스 속성이니까 앞에 . 붙인다. 
  html_nodes('a') %>%
  html_attr('title')
print(data_title)

#기업 공시 정보 불러오기 
library(httr)
library(rvest)

Sys.setlocale("LC_ALL", "English")

url = 'https://dev-kind.krx.co.kr/disclosure/todaydisclosure.do'
#POST 함수를 통해 원하는 쿼리를 요청하고, 쿼리를 body내에 리스트 형태로 입력해준다. 개발자 도구 화면의 Form Data와 동일하게 입력해주고, 값이 없는 항목은 입력하지 않아도 된다. 
data = POST(url, body = 
              list(
                method = 'searchTodayDisclosureSub',
                currentPageSize = '15',
                pageIndex = '1',
                orderMode = '0',
                orderStat = 'D',
                forward = 'todaydisclosure_sub',
                chose = 'S',
                todayFlag = 'Y',
                selDate = '2018-12-28'
              ))

data = read_html(data) %>% #html 내용을 읽어온다
  html_table(fill = TRUE) %>% #테이블 형태의 데이터를 읽어온다. 셀 병합이 된 열이 있으면 fill=TRUE추가해준다
  .[[1]]

Sys.setlocale("LC_ALL", "Korean")
print(head(data))

#네이버 금융에서 주식티커 크롤링 
library(httr)
library(rvest)
data=list()
for (i in 0:1) {
  ticker = list()
  url = paste0('https://finance.naver.com/sise/',
               'sise_market_sum.nhn?sosok=',i,'&page=1') #코스피 시가총액 페이지의 url만들기 
  down_table = GET(url) #페이지 내용을 받아 변수에 저장 
  #마지막 페이지가 몇 번째인지 찾아보기 
  
  ##최종 페이지 번호 찾아주기 
  navi.final=read_html(down_table,encoding = 'EUC-KR')%>%
    html_nodes(.,'.pgRR') %>% #클래스 속성이면 앞에 .
    html_nodes(., 'a') %>%  #태그 정보
    html_attr(., 'href') #속성 정보 
    strsplit(., "=") %>%#전체 문장을 특정 글자 기준으로 
    unlist() %>% #결과를 벡터 형태로 저장
    tail(.,1) %>%
    as.numeric()
  
  #코스피 코스닥 데리고 오기 
  for(j in 1:navi.final){
    #각 페이지에 해당하는 url 형성 
    url = paste0('https://finance.naver.com/sise/',
                 'sise_market_sum.nhn?sosok=',i,'&page=',j) 
    down_table=GET(url)
    Sys.setlocale("LC_ALL", "English")
    
    table = read_html(down_table, encoding = "EUC-KR") %>%
      html_table(fill = TRUE)
    table = table[[2]] #원하는 테이블 추출
    
    Sys.setlocale("LC_ALL", "Korean")
    print(head(table))
    
    table[, ncol(table)] = NULL
    table = na.omit(table)
 
    
    # 티커를 들고오기
    symbol=read_html(down_table,encoding = 'EUC-KR') %>%
      html_nodes(.,'tbody')%>%
      html_nodes(., 'td') %>%
      html_nodes(., 'a') %>%
      html_attr(., 'href')
    
    
    #끝 여섯자리만 
    library(stringr)
    symbol=sapply(symbol,function(x){
      str_sub(x,-6,-1)
    })

    symbol=unique(symbol) #동일내용 두번 반복하지 않도
 
    #테이블에 티커를 넣어준 후, 테이블 정리 
    table$N=symbol#위에서 구한 티커를 N열에 입력한다
    colnames(table)[1]='종목코드' #해당 열이름 바꾸기
    rownames(table)=NULL #행이름 초기화
    ticker[[j]]=table #ticker의 j번째 리스트에 정리된 데이터 입력하기
    
    Sys.sleep(0.5)
  }
    #do.call을 통해 리스트를 데이터 프레임으로 묶기
  ticker=do.call(rbind,ticker)
  data[[i+1]]=ticker
}
data = do.call(rbind, data)
data


####네이버 티커
data = list()

# i = 0 은 코스피, i = 1 은 코스닥 종목
for (i in 0:1) {
  
  ticker = list()
  url =
    paste0('https://finance.naver.com/sise/',
           'sise_market_sum.nhn?sosok=',i,'&page=1')
  
  down_table = GET(url)
  
  # 최종 페이지 번호 찾아주기
  navi.final = read_html(down_table, encoding = "EUC-KR") %>%
    html_nodes(., ".pgRR") %>%
    html_nodes(., "a") %>%
    html_attr(.,"href") %>%
    strsplit(., "=") %>%
    unlist() %>%
    tail(., 1) %>%
    as.numeric()
  
  # 첫번째 부터 마지막 페이지까지 for loop를 이용하여 테이블 추출하기
  for (j in 1:navi.final) {
    
    # 각 페이지에 해당하는 url 생성
    url = paste0(
      'https://finance.naver.com/sise/',
      'sise_market_sum.nhn?sosok=',i,"&page=",j)
    down_table = GET(url)
    
    Sys.setlocale("LC_ALL", "English")
    # 한글 오류 방지를 위해 영어로 로케일 언어 변경
    
    table = read_html(down_table, encoding = "EUC-KR") %>%
      html_table(fill = TRUE)
    table = table[[2]] # 원하는 테이블 추출
    
    Sys.setlocale("LC_ALL", "Korean")
    # 한글을 읽기위해 로케일 언어 재변경
    
    table[, ncol(table)] = NULL # 토론식 부분 삭제
    table = na.omit(table) # 빈 행 삭제
    
    # 6자리 티커만 추출
    symbol = read_html(down_table, encoding = "EUC-KR") %>%
      html_nodes(., "tbody") %>%
      html_nodes(., "td") %>%
      html_nodes(., "a") %>%
      html_attr(., "href")
    
    symbol = sapply(symbol, function(x) {
      str_sub(x, -6, -1) 
    })
    
    symbol = unique(symbol)
    
    # 테이블에 티커 넣어준 후, 테이블 정리
    table$N = symbol
    colnames(table)[1] = "종목코드"
    
    rownames(table) = NULL
    ticker[[j]] = table
    
    Sys.sleep(0.5) # 페이지 당 0.5초의 슬립 적용
  }
  
  # do.call을 통해 리스트를 데이터 프레임으로 묶기
  ticker = do.call(rbind, ticker)
  data[[i + 1]] = ticker
}

data = do.call(rbind, data)
data
