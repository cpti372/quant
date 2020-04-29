#산업별 현황 크롤링
library(httr)
library(rvest)
library(readr)
library(xml2)
#원하는 항목을 제출할 URL을 입력한다. 
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = 'MKD/03/0303/03030103/mkd03030103',
  tp_cd = 'ALL',
  date = '20200429',
  lang = 'ko',
  pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')
#post는 url에 쿼리를 전송하면 이에 해당하는 데이터를 받게 된다. otp를 통해 원하는 데이터를 다운로드 할 수 있다. 
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()
print(down_sector)
write.csv(down_sector,'kospi.csv')
#5.1.2 개별종목 지표 크롤링
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = "MKD/13/1302/13020401/mkd13020401",
  market_gubun = 'ALL',
  gubun = '1',
  schdate = '20200429',
  pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()
print(down_ind)
write.csv(down_ind,'krx_ind.csv')
#5.1.3 최근 영업일 기준 데이터 받기
url='https://finance.naver.com/sise/sise_index.nhn?code=KOSPI'

library(httr)
library(rvest)
library(stringr)

url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_0"]/div/ul[2]/li') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')

print(biz_day)
#최신 산업별 현황 OTP발급
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = 'MKD/03/0303/03030103/mkd03030103',
  tp_cd = 'ALL',
  date = biz_day,
  lang = 'ko',
  pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
#데이터가 있으면 csv를 써라 
ifelse(dir.exists('data'),FALSE,dir.create('data'))
write.csv(down_sector,'uptodate_sector.csv')

#거래소 데이터 정리하기: 위에 데이터는 중복된 열이 있고, 불필요한 데이터가 있기 때문에 하나의 테이블로 합친 후 정리할 필요가 있다. 
down_sector=read.csv('uptodate_sector.csv',row.names = 1,stringsAsFactors = FALSE)
#문자열이 Factor안되게 함,row.names=1을 통해 첫번째 열을 행이름으로 설정 
down_ind=read.csv('krx_ind.csv',row.names = 1,stringsAsFactors = FALSE)
intersect(names(down_sector),names(down_ind))
#down_sector에는 있는데 ind 는 없는거 추출
setdiff(down_sector[,'종목명'],down_ind[,'종목명'])
 kor_tiker=merge(down_sector,down_ind,by=intersect(names(down_sector),names(down_ind)),all = FALSE)#by기준으로 합치고, all을 반환하면 합집합, false는 교집합이다. 
kor_tiker=kor_tiker[order(-kor_tiker['시가총액.원.']),]#내림차순으로 바꾼다. 
print(head(kor_tiker))
#grepl:함수 종목명에 스팩이 들어가는걸 찾기,특정 텍스트 검색할때 
kor_tiker[grepl('스팩',kor_tiker[,'종목명']),'종목명']
#종목코드 끝이 0이 아닌 우선주 항목을 먼저 찾기
kor_tiker[str_sub(kor_tiker[,'종목코드'],-1,-1)!=0,]
rownames(kor_tiker)=NULL
write.csv(kor_tiker,'kor_ticker.csv')

#5.2 WICS 기준 섹터정보 크롤링 
url = 'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20190607&sec_cd=G10'
data = fromJSON(url)
lapply(data, head)#R lapply: list+apply:list형태로 실행결과가 출력이 된다. 리스트의인자는 데이터의 길이만큼 생성된다. 
sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  Sys.sleep(1)
}
data_sector=do.call(rbind,data_sector)
print(data_sector)
write.csv(data_sector,'kor_sector.csv')
