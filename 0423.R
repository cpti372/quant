#금융속보 크롤링 
library(rvest)
library(httr)
library(xml2)
url="https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258"
data=GET(url ="https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258" )
print(data) #status 가 200(잘 돌아가고있고), 인코딩은EUC-KR
data_title=data %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes('dl') %>% #태그 추출
  html_nodes('.articleSubject') %>% #클래스 추출
  html_nodes('a') %>%  #속성추출
  html_attr('title')
print(data_title)
  

#오늘의 공시 불러오기 
library(httr)
library(rvest)
#로케일 언어 설정해주기 
url = 'http://kind.krx.co.kr/disclosure/todaydisclosure.do'
#개발자 도구 화면의 FormDATA와 동일하게 입력해주기
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
                selDate = '2020-04-23'
              ))
data=read_html(data)%>%
  html_table(fill = TRUE) %>%
  .[[1]] #첫번째 리스트 선택
print(head(data))

#주식티커 크롤링- 페이지 받아오기 
library(rvest)
library(httr)
library(xml2)
i=0
ticker=list()
url=paste0('https://finance.naver.com/sise/','sise_market_sum.nhn?sosok=',i,'&page=1') #코스피 시가 총액의 url이다. 
down_table=GET(url)
navi.final = read_html(down_table, encoding = 'EUC-KR') %>%
  html_nodes(., '.pgRR') %>%
  html_nodes(., 'a') %>%
  html_attr(., 'href')
print(navi.final)
navi.final=navi.final %>%
  strsplit(.,'=') %>% #뒷부분만 필요하니까
  unlist() %>% #결과 벡터 형태 저장
  tail(.,1) %>%#첫번째 데이터만 뽑아오기 
  as.numeric()
print(navi.final)
#주식티커 크롤링- 원하는 데이터를 추출한다 
i=0
j=1
url=paste0('https://finance.naver.com/sise/','sise_market_sum.nhn?sosok=',i,'&page=',j) #코스피 시가 총액의 url이다. 
down_table=GET(url)
table = read_html(down_table, encoding = "EUC-KR") %>%
  html_table(fill = TRUE)
table = table[[2]]
print(head(table))
table[,ncol(table)]=NULL
table=na.omit(table)
print(head(table))

#6자리 티커출력
symbol = read_html(down_table, encoding = 'EUC-KR') %>%
  html_nodes(., 'tbody') %>%
  html_nodes(., 'td') %>%
  html_nodes(., 'a') %>%
  html_attr(., 'href')
print(head(symbol, 10)) #symbol에는 herf속성에 해당하는 링크 주소들이 저장됨 
#마지막 6자리만 추출하고 싶어요
library(stringr)
symbol=sapply(symbol, function(x){
  str_sub(x,-6,-1)
})

print(head(symbol,10))
symbol = unique(symbol)#중복제거
print(head(symbol, 10))
table$N=symbol#구한 티커를 N열에 입력한다. 
colnames(table)[1]='종목코드'#열을 종목코드
rownames(table)=NULL#행이름 초기화
ticker[[j]]=table#j번째 리스트에 정리된 데이터를 입력한다. 
print(table)
