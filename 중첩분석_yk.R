# 개별연구 중첩분석
# 동작구: 노량진동, 사당동, 상도동 / 강남구: 대치동
# 변수: cctv, 여성안심이집, 치안센터
library(xlsx)
library(dplyr)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(stringr)
library(geosphere)

#dong_boundary = data.table::fread('data/dong_boundary.csv')
register_google(key='AIzaSyD3C-tRD-ezdOjaDAx7ftZpoUxvYumMcKE')

#cctv
daechi_cctv <- read.csv("C:/HGU/21-2/개별연구/강남구_대치동CCTV주소.csv")
dongjak_cctv <- read.csv("C:/HGU/21-2/개별연구/서울특별시_동작구_CCTV.csv")

#여성안심이집
#safehouse <- read.csv("C:/HGU/21-2/개별연구/서울특별시 여성안심지킴이집 정보.csv")
safehouse <- read.xlsx("C:/HGU/21-2/개별연구/서울 여성안심지킴이집 주소 좌표.xlsx", 'Sheet1')

#치안센터
#security <- read.csv("C:/HGU/21-2/개별연구/경찰청_전국 치안센터 주소_20210203.csv")
security <- read.xlsx("C:/HGU/21-2/개별연구/서울 치안센터 주소 좌표.xlsx", 'Sheet1')

#지하철역
subway <- read.xlsx('C:/HGU/21-2/개별연구/서울 지하철역 좌표.xlsx','Sheet1')
subway %>%
  filter(동 %in% c('대치', '대치동') | 역명 %in% c('삼성', '선릉', '도곡')) -> subway_dc

subway %>%
  filter(동 %in% c('노량', '노량진동')) -> subway_nr
subway %>%
  filter(동 %in% c('사당', '사당동')) -> subway_sd
subway_sd <- subway_sd[-c(1),]

subway %>%
  filter(동 %in% c('상도', '상도동', '상도1동')) -> subway_sdo

#버스정류장
bus <- read.xlsx('C:/HGU/21-2/개별연구/서울 버스정류장 주소 좌표.xlsx','Sheet1')

bus %>% 
  filter(동 == '대치동') -> bus_dc
bus %>% 
  filter(동 == '노량진동') -> bus_nr
bus %>% 
  filter(동 == '사당동') -> bus_sd
bus %>% 
  filter(동 == '상도동') -> bus_sdo

#주소->좌표 변환코드
#register_google(key='AIzaSyD3C-tRD-ezdOjaDAx7ftZpoUxvYumMcKE')
#daechi_cctv <- enc2utf8(daechi_cctv)
#safehouse <- enc2utf8(safehouse)
#security <- enc2utf8(security)

#safehouse <- mutate_geocode(safehouse, location=주소, source='google')
#security <- mutate_geocode(security, location=주소, source='google')

#write.xlsx(safehouse,'C:/HGU/21-2/개별연구/서울 여성안심지킴이집 주소 좌표.xlsx')
#write.xlsx(security,'C:/HGU/21-2/개별연구/서울 치안센터 주소 좌표.xlsx')

# 안심택배함 위치
ansim <- read.xlsx('C:/HGU/21-2/개별연구/서울 안심택배함.xlsx','Sheet1')
ansim$lat <- as.numeric(ansim$lat) #lat
ansim$lon <- as.numeric(ansim$lon) #lon
dc_ansim <- ansim %>% filter(ansim$시설명 %in% ansim$시설명[(str_detect(ansim$시설명,"대치"))])
nr_ansim <- ansim %>% filter(ansim$시설명 %in% ansim$시설명[(str_detect(ansim$시설명,"노량진"))])
sd_ansim <- ansim %>% filter(ansim$시설명 %in% ansim$시설명[(str_detect(ansim$시설명,"사당"))])
sdo_ansim <- ansim %>% filter(ansim$시설명 %in% ansim$시설명[(str_detect(ansim$시설명,c("상도"))|
                                                          str_detect(ansim$시설명,c("예그리나"))|
                                                             str_detect(ansim$시설명,c("동작청소년문화의집")))])                  
# 보안등
gangnam_security_light <- read.csv("C:/HGU/21-2/개별연구/서울특별시_강남구_보안등정보.csv")
light_dc <- gangnam_security_light[grep("대치", gangnam_security_light$보안등위치명),]
light_dc <- light_dc %>% filter(!is.na(lat))

dongjak_security_light <- read.csv("C:/HGU/21-2/개별연구/서울특별시_동작구_보안등정보.csv")
light_nr <- dongjak_security_light[grep("노량진", dongjak_security_light$보안등위치명),]
light_nr %>% filter(!is.na(lat))

light_sd <- dongjak_security_light[grep("사당", dongjak_security_light$보안등위치명),]
light_sd %>% filter(!is.na(lat))

light_sdo <- dongjak_security_light[grep("상도", dongjak_security_light$소재지지번주소),]
light_sdo %>% filter(!is.na(lat))

#서울 읍면동 SHAPEFILE 불러오기 & 데이터프레임 변환
seoul <- shapefile("C:/Users/byk11/Downloads/EMD_202101/EMD_202101/TL_SCCO_EMD.shp")
#seoul <- shapefile("C:/Users/User/Desktop/EMD_202101/TL_SCCO_EMD.shp")            
seoul <- spTransform(seoul, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
seoul <- fortify(seoul, region = 'EMD_KOR_NM')

## 강남구-대치동 ##
seoul %>%
  filter(id=='대치동')-> daechi
summary(daechi)

daechi_cctv <- daechi_cctv[(min(daechi$long) < daechi_cctv$lon) & (daechi_cctv$lon < max(daechi$long))
                                & (min(daechi$lat) < daechi_cctv$lat) & (daechi_cctv$lat < max(daechi$lat)),]
daechi_cctv <- unique(daechi_cctv)
dc_safehouse_geo <- safehouse[grep("대치", safehouse$주소),]
dc_safehouse_geo <- dc_safehouse_geo[(min(daechi$long) < dc_safehouse_geo$lon) & (dc_safehouse_geo$lon < max(daechi$long))
                 & (min(daechi$lat) < dc_safehouse_geo$lat) & (dc_safehouse_geo$lat < max(daechi$lat)),]
dc_security_geo <- security[grep("대치", security$주소),]
dc_security_geo <- dc_security_geo[(min(daechi$long) < dc_security_geo$lon) & (dc_security_geo$lon < max(daechi$long))
                                     & (min(daechi$lat) < dc_security_geo$lat) & (dc_security_geo$lat < max(daechi$lat)),]

# 범위내 변수들 위경도 추출
daechi_cctv %>% 
  filter(lon >= 127.050 & lon <= 127.062) %>%
  filter(lat >= 37.4975 & lat <= 37.5062) -> data_in_area
dc_cctv_cent <- centroid(data_in_area[, 3:2])

dc_safehouse_geo %>%
  filter(lon >= 127.050 & lon <= 127.062) %>%
  filter(lat >= 37.4975 & lat <= 37.5062) -> data_in_area
dc_safehouse_cent <- centroid(data_in_area[, 7:8])

dc_ansim %>% 
  filter(lon >= 127.050 & lon <= 127.062) %>%
  filter(lat >= 37.4975 & lat <= 37.5062) -> data_in_area
dc_ansim_cent <- centroid(data_in_area[, 7:8])

subway_dc %>% 
  filter(lon >= 127.050 & lon <= 127.062) %>%
  filter(lat >= 37.4975 & lat <= 37.5062) -> data_in_area
dc_subway_cent <- centroid(data_in_area[, 3:2])

bus_dc %>% 
  filter(long >= 127.050 & long <= 127.062) %>%
  filter(lat >= 37.4975 & lat <= 37.5062) -> data_in_area
dc_bus_cent <- centroid(data_in_area[, 2:3])

light_dc %>% 
  filter(lon >= 127.050 & lon <= 127.062) %>%
  filter(lat >= 37.4975 & lat <= 37.5062) -> data_in_area
dc_light_cent <- centroid(data_in_area[, 6:5])

df_cent <- rbind(dc_cctv_cent, dc_safehouse_cent,dc_ansim_cent,dc_bus_cent,dc_light_cent)
dc_cent <- data.frame(centroid(df_cent)) #대치동 최종 centroid

#대치동 중첩분석
#구글맵
#dc_map <- get_map(location=c(lon= 127.054, lat=37.503), zoom=16, maptype='roadmap', color='bw')
#lon=mean(daechi$long), lat=mean(daechi$lat))
dc_map <- get_map(location=c(lon=mean(daechi$long), lat=mean(daechi$lat)), zoom=14, maptype='roadmap', color='bw')
ggmap(dc_map) +
  geom_point(alpha=0.5, light_dc, mapping=aes(x=lon, y=lat),color='tan', size=1)+
  geom_point(alpha=0.5, bus_dc, mapping=aes(x=long, y=lat),col='#26A69A', size=2)+
  geom_point(alpha=0.7, shape=15, subway_dc, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, daechi_cctv, mapping=aes(x=lat, y=lon, col='cctv'),col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, dc_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  #geom_point(shape=9, stroke=2, dc_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), col='#FB8C00', size=4)+
  geom_point(alpha=0.8, shape=4, dc_ansim, mapping=aes(x=경도, y=위도, col='안심택배함'),col='navy',stroke=3,size=4)+
  geom_point(alpha=0.8, shape=4, dc_cent, mapping=aes(x=lon, y=lat, col='최종입지추천지'),col='red',stroke=3,size=4)+
  theme_void()

#행정동 선따기
ggplot(daechi, aes(x=long,y=lat)) +
  geom_polygon(fill='#FFFFFF', color='black') + 
  geom_point(alpha=0.5, light_dc, mapping=aes(x=경도, y=위도),color='tan', size=1)+
  geom_point(alpha=0.5, bus_dc, mapping=aes(x=long, y=lat),col='#26A69A', size=2)+
  geom_point(alpha=0.7, shape=15, subway_dc, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, daechi_cctv, mapping=aes(x=y, y=x, col='cctv'),col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, dc_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  #geom_point(shape=9, stroke=2, dc_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), col='#FB8C00', size=4)+
  geom_point(alpha=0.8, shape=4, dc_ansim, mapping=aes(x=경도, y=위도, col='안심택배함'),col='navy',stroke=3,size=4)+
  
  theme_void()

## 동작구-노량진동 ##
seoul %>%
  filter(id=='노량진동')-> norangjin

norang_cctv <- dongjak_cctv[grep("노량진", dongjak_cctv$주소),]
#norang_cctv <- norang_cctv[(min(norangjin$long) < norang_cctv$lon) & (norang_cctv$lon < max(norangjin$long))
#                           & (min(norangjin$lat) < norang_cctv$lat) & (norang_cctv$lat < max(norangjin$lat)),]
norang_cctv <- unique(norang_cctv)

nr_safehouse_geo <- safehouse[grep("노량진", safehouse$주소),]
nr_safehouse_geo <- nr_safehouse_geo[(min(norangjin$long) < nr_safehouse_geo$lon) & (nr_safehouse_geo$lon < max(norangjin$long))
                                     & (min(norangjin$lat) < nr_safehouse_geo$lat) & (nr_safehouse_geo$lat < max(norangjin$lat)),]
nr_security_geo <- security[grep("노량진", security$주소),]
nr_security_geo <- nr_security_geo[(min(norangjin$long) < nr_security_geo$lon) & (nr_security_geo$lon < max(norangjin$long))
                                   & (min(norangjin$lat) < nr_security_geo$lat) & (nr_security_geo$lat < max(norangjin$lat)),]

#노량진동 중첩분석
#구글맵
nr_map <- get_map(location=c(lon=mean(norangjin$long), lat=mean(norangjin$lat)),zoom=15, maptype='roadmap', color='bw')
ggmap(nr_map) +
  geom_point(alpha=0.5, light_nr, mapping=aes(x=경도, y=위도),color='tan', size=1)+
  geom_point(alpha=0.5, bus_nr, mapping=aes(x=long, y=lat, col='버스정류장'),col='#26A69A', size=2)+
  geom_point(alpha=0.7, shape=15, subway_nr, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, norang_cctv, mapping=aes(x=경도, y=위도, col='cctv'), col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, nr_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  #geom_point(shape=9, stroke=2, nr_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), ,col='#FB8C00', size=4)+
  geom_point(alpha=0.5, light_nr, mapping=aes(x=경도, y=위도, ,col='보안등'), size=1)+
  theme_void()

#행정동 선따기
ggplot(noryangjin, aes(x=long,y=lat)) +
  geom_polygon(fill='#FFFFFF', color='black') + 
  geom_point(alpha=0.5, light_nr, mapping=aes(x=경도, y=위도),color='tan', size=1)+
  geom_point(alpha=0.5, bus_nr, mapping=aes(x=long, y=lat, col='버스정류장'),col='#26A69A', size=2)+
  geom_point(alpha=0.7, shape=15, subway_nr, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, norang_cctv, mapping=aes(x=경도, y=위도, col='cctv'), col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, nr_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  #geom_point(shape=9, stroke=2, nr_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), ,col='#FB8C00', size=4)+
  geom_point(alpha=0.5, light_nr, mapping=aes(x=경도, y=위도, ,col='보안등'), size=1)+
  
  theme_void()


## 동작구-사당동 ##
seoul %>%
  filter(id=='사당동')-> sadang

sadang_cctv <- dongjak_cctv[grep("사당", dongjak_cctv$주소),]
#sadang_cctv <- sadang_cctv[(min(sadang$long) < sadang_cctv$경도) & (sadang_cctv$경도 < max(sadang$long))
#                           & (min(sadang$lat) < sadang_cctv$위도) & (sadang_cctv$위도 < max(sadang$lat)),]

sd_safehouse_geo <- safehouse[grep("사당", safehouse$주소),]
sd_safehouse_geo <- sd_safehouse_geo[(min(sadang$long) < sd_safehouse_geo$lon) & (sd_safehouse_geo$lon < max(sadang$long))
                                     & (min(sadang$lat) < sd_safehouse_geo$lat) & (sd_safehouse_geo$lat < max(sadang$lat)),]

sd_security_geo <- security[grep("사당", security$주소),]
sd_security_geo <- sd_security_geo[(min(sadang$long) < sd_security_geo$lon) & (sd_security_geo$lon < max(sadang$long))
                                    & (min(sadang$lat) < sd_security_geo$lat) & (sd_security_geo$lat < max(sadang$lat)),]

#사당동 중첩분석
#구글맵
sd_map <- get_map(location=c(lon=mean(sadang$long), lat=mean(sadang$lat)),zoom=14, maptype='roadmap', color='bw')
ggmap(sd_map) +
  #ggplot(sadang, aes(x=long,y=lat)) +
  #geom_polygon(fill='#FFFFFF', color='black') + 
  geom_point(alpha=0.5, light_sd, mapping=aes(x=경도, y=위도),color='tan', size=1)+
  geom_point(alpha=0.5, bus_sd, mapping=aes(x=long, y=lat, col='버스정류장'), col='#26A69A',size=2)+
  geom_point(alpha=0.7, shape=15, subway_sd, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, sadang_cctv, mapping=aes(x=경도, y=위도, col='cctv'), col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, sd_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  geom_point(shape=9, stroke=2, sd_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), col='#FB8C00',size=4)+
  geom_point(alpha=0.8, shape=4, sd_ansim, mapping=aes(x=경도, y=위도, col='안심택배함'),col='navy',stroke=3, size=4)+
  geom_point(alpha=0.5, light_sd, mapping=aes(x=경도, y=위도, ,col='보안등'), size=1)+
  
  theme_void()

#행정동 선따기
ggplot(sadang, aes(x=long,y=lat)) +
  geom_polygon(fill='#FFFFFF', color='black') + 
  geom_point(alpha=0.5, light_sd, mapping=aes(x=경도, y=위도),color='tan', size=1)+
  geom_point(alpha=0.5, bus_sd, mapping=aes(x=long, y=lat, col='버스정류장'), col='#26A69A',size=2)+
  geom_point(alpha=0.7, shape=15, subway_sd, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, sadang_cctv, mapping=aes(x=경도, y=위도, col='cctv'), col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, sd_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  geom_point(shape=9, stroke=2, sd_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), col='#FB8C00',size=4)+
  geom_point(alpha=0.8, shape=4, sd_ansim, mapping=aes(x=경도, y=위도, col='안심택배함'),col='navy',stroke=3, size=4)+
  geom_point(alpha=0.5, light_sd, mapping=aes(x=경도, y=위도, ,col='보안등'), size=1)+
  
  theme_void()

## 동작구-상도동 ##
seoul %>%
  filter(id=='상도동')-> sangdo
sangdo %>%
  filter(group=='상도동.1')-> sangdo

sangdo_cctv <- dongjak_cctv[grep("상도", dongjak_cctv$주소),]
#sangdo_cctv <- sangdo_cctv[(min(sadang$long) < sangdo_cctv$경도) & (sangdo_cctv$경도 < max(sadang$long))
#                           & (min(sadang$lat) < sangdo_cctv$위도) & (sangdo_cctv$위도 < max(sadang$lat)),]

sdo_safehouse_geo <- safehouse[grep("상도", safehouse$주소),]
sdo_safehouse_geo <- sdo_safehouse_geo[(min(sangdo$long) < sdo_safehouse_geo$lon) & (sdo_safehouse_geo$lon < max(sangdo$long))
                                     & (min(sangdo$lat) < sdo_safehouse_geo$lat) & (sdo_safehouse_geo$lat < max(sangdo$lat)),]

sdo_security_geo <- security[grep("상도", security$주소),]
sdo_security_geo <- sdo_security_geo[(min(sangdo$long) < sdo_security_geo$lon) & (sdo_security_geo$lon < max(sangdo$long))
                                   & (min(sangdo$lat) < sdo_security_geo$lat) & (sdo_security_geo$lat < max(sangdo$lat)),]

# 범위내 변수들 위경도 추출
#위쪽
#sangdo_cctv %>% 
#  filter(lon >= 126.932 & lon <= 126.945) %>%
#  filter(lat >= 37.496 & lat <= 37.505) -> data_in_area
#sdo_cctv_cent <- centroid(data_in_area[, 4:3])

sangdo_cctv %>% 
  filter(lon >= 126.930 & lon <= 126.937) %>%
  filter(lat >= 37.490 & lat <= 37.502) -> data_in_area
sdo_cctv_cent <- centroid(data_in_area[, 4:3])

sdo_safehouse_geo %>%
  filter(lon >= 126.930 & lon <= 126.937) %>%
  filter(lat >= 37.490 & lat <= 37.502) -> data_in_area
sdo_safehouse_cent <- data_in_area[,7:8]

sdo_ansim %>% 
  filter(lon >= 126.930 & lon <= 126.937) %>%
  filter(lat >= 37.490 & lat <= 37.502) -> data_in_area
sdo_ansim_cent <- data_in_area[,8:7]

subway_sdo %>% 
  filter(lon >= 126.930 & lon <= 126.937) %>%
  filter(lat >= 37.490 & lat <= 37.502) -> data_in_area
#sdo_subway_cent <- centroid(data_in_area[, 10:9])
sdo_subway_cent <- data_in_area[,9:10]

bus_sdo %>% 
  filter(lon >= 126.930 & lon <= 126.937) %>%
  filter(lat >= 37.490 & lat <= 37.502) -> data_in_area
sdo_bus_cent <- data.frame('lon'=mean(data_in_area$lon),'lat'=mean(data_in_area$lat))

light_sdo %>% 
  filter(lon >= 126.930 & lon <= 126.937) %>%
  filter(lat >= 37.490 & lat <= 37.502) -> data_in_area
sdo_light_cent <- centroid(data_in_area[, 6:5])

df_cent <- rbind(sdo_cctv_cent,sdo_safehouse_cent,sdo_ansim_cent,sdo_subway_cent,sdo_bus_cent,sdo_light_cent)
sdo_cent <- data.frame(centroid(df_cent)) #상도동 최종 centroid


#상도동 중첩분석
#구글맵
sdo_map <- get_map(location=c(lon=mean(sangdo$long), lat=mean(sangdo$lat)),zoom=14, maptype='roadmap', color='bw')
#위쪽
#sdo_map <- get_map(location=c(lon=126.938, lat=37.501),zoom=16, maptype='roadmap', color='bw')
#아래쪽
sdo_map <- get_map(location=c(lon=126.935, lat=37.497),zoom=16, maptype='roadmap', color='bw')

ggmap(sdo_map) +
  geom_point(alpha=0.5, light_sdo, mapping=aes(x=lon, y=lat),color='tan', size=1)+
  geom_point(alpha=0.5, bus_sdo, mapping=aes(x=lon, y=lat, col='버스정류장'), col='#26A69A', size=2)+
  geom_point(alpha=0.7, shape=15, subway_sdo, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, sangdo_cctv, mapping=aes(x=lon, y=lat, col='cctv'), col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, sdo_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  geom_point(shape=9, stroke=2, sdo_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), col='#FB8C00', size=4)+
  geom_point(alpha=0.8, shape=4, sdo_ansim, mapping=aes(x=lon, y=lat, col='안심택배함'),col='navy',stroke=3,size=4)+
  geom_point(alpha=0.8, shape=4, sdo_cent, mapping=aes(x=lon, y=lat, col='최종입지추천지'),col='red',stroke=3,size=4)+
  
  theme_void()

#행정동 선따기
ggplot(sangdo, aes(x=long,y=lat)) +
  geom_polygon(fill='#FFFFFF', color='black') + 
  geom_point(alpha=0.5, light_sdo, mapping=aes(x=경도, y=위도),color='tan', size=1)+
  geom_point(alpha=0.5, bus_sdo, mapping=aes(x=long, y=lat, col='버스정류장'), col='#26A69A', size=2)+
  geom_point(alpha=0.7, shape=15, subway_sdo, mapping=aes(x=lon, y=lat, col='지하철역'), col='#795548', size=5)+
  geom_point(alpha=0.5, shape=18, sangdo_cctv, mapping=aes(x=경도, y=위도, col='cctv'), col='#EF5350', size=2)+
  geom_point(alpha=0.5, shape=17, sdo_safehouse_geo, mapping=aes(x=lon, y=lat, col='여성안심이집'), col='#1565C0', size=4)+
  geom_point(shape=9, stroke=2, sdo_security_geo, mapping=aes(x=lon, y=lat, col='치안센터'), col='#FB8C00', size=4)+
  geom_point(alpha=0.8, shape=4, sdo_ansim, mapping=aes(x=경도, y=위도, col='안심택배함'),col='navy',stroke=3,size=4)+
  geom_point(alpha=0.8, shape=4, sdo_cent, mapping=aes(x=lon, y=lat, col='최종입지추천지'),col='red',stroke=3,size=4)+
  
  theme_void()
