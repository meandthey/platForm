import xarray as xr
import pandas as pd
import os
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely.geometry import Point
import netCDF4
import fiona

## [일사량 데이터]
# NetCDF 파일들이 저장된 폴더 경로 # 100m 격자의 일사량 데이터터
rawData_irr_file_path = "C:/Users/DESKTOP/Desktop/allData/KMA/solarResource_GHI_byHours_byTotalPeriod/KMAPP_solar_FWS_total_mean.nc"
rawData_irr = xr.open_dataset(rawData_irr_file_path)["SWDN_flat_with_shading"].to_dataframe().reset_index()


## [경도 위도 데이터] : 일사량 데이터에 붙어 있는
rawData_latlon_file_path = "C:/Users/DESKTOP/Desktop/allData/KMA/appendix/KMAP_latlon.nc"
rawData_latlon = xr.open_dataset(rawData_latlon_file_path).to_dataframe().reset_index()



########## Merge irrData and latlonData ##########
# 데이터프레임을 병합 (좌우 순서 유지)
merged_df = pd.concat([rawData_irr, rawData_latlon], axis=1)

# 결측값(NaN) 제거
merged_df_clean = merged_df.dropna()

# 기간의 평균 (W/m2 -> Wh/m2/year)로 바꿔줘야 함.
merged_df_clean = merged_df_clean.copy()  # 명확한 복사본 생성
merged_df_clean['Total'] = merged_df_clean['SWDN_flat_with_shading'] * 8760

# 필요한 칼럼만 추출.
merged_df_clean = merged_df_clean[['latitude', 'longitude', 'Total']]



########## Shp file SGG  ##########
# SHP 파일 로드
shp_file = "C:/Users/DESKTOP/Desktop/solarAnalysis\data/ctprvn_20230729/ctprvn.shp"  # 실제 파일 경로로 변경하세요
#gdf_shp = gpd.read_file(shp_file)
gdf_shp = gpd.read_file(shp_file, encoding='euc-kr', engine='fiona')

# 좌표계를 EPSG:5179로 수동 설정
gdf_shp.set_crs("EPSG:5179", inplace=True)  # 또는 "EPSG:5181" 가능성 있음

# EPSG:4326 (WGS 84, 위경도 좌표계)로 변환
gdf_shp = gdf_shp.to_crs("EPSG:4326")


# 데이터 확인
print(gdf_shp.head())

# 지도 시각화
gdf_shp.plot(edgecolor="black", figsize=(8, 8))
plt.show()

df = merged_df_clean

# GeoDataFrame 변환
geometry = [Point(xy) for xy in zip(df["longitude"], df["latitude"])]
gdf_points = gpd.GeoDataFrame(df, geometry=geometry, crs="EPSG:4326")  # WGS84 좌표계 지정


# 3️⃣ 공간 조인: 시도 경계 안에 포함된 포인트만 추출
gdf_filtered = gpd.sjoin(gdf_points, gdf_shp, how="inner", predicate="within")

# 4️⃣ 결과 저장 & 출력
gdf_filtered.to_csv("filtered_data.csv", index=False)  # 파일로 저장
print(gdf_filtered.head())  # 데이터 일부 확인


print("시도 경계 좌표계:", gdf_shp.crs)
print("포인트 데이터 좌표계:", gdf_points.crs)
print(gdf_points[['latitude', 'longitude']].describe())


print(gdf_points.head())  # geometry 컬럼이 있는지 확인
print(gdf_points.geometry.head())  # Point 객체가 올바르게 생성되었는지 확인



## 지도위에 점을 전부 뿌릴려면 너무 많다.
#fig, ax = plt.subplots(figsize=(8, 8))
#gdf_shp.plot(ax=ax, color="lightgrey", edgecolor="black")  # 시도 경계
#gdf_points.plot(ax=ax, color="red", markersize=1, alpha=0.5)  # 원본 포인트 데이터
#plt.show()




gdf_seoul = gdf_points[(gdf_points["longitude"] > 126) & (gdf_points["longitude"] < 127) &
                        (gdf_points["latitude"] > 37) & (gdf_points["latitude"] < 38)]

# Gyeonggi-do(경기도)만 필터링
gdf_shp_gyeonggi_region = gdf_shp[gdf_shp["CTP_ENG_NM"] == "Gyeonggi-do"]

# 공간 조인 (포인트가 Gyeonggi-do에 포함되는 것만 선택)
points_in_gyeonggi = gpd.sjoin(gdf_points, gdf_shp_gyeonggi_region, predicate="within")



# 샘플링
sample_points_in_gyeonggi = points_in_gyeonggi.sample(n=3000, random_state=42)

# 지도 시각화
fig, ax = plt.subplots(figsize=(8, 8))
gdf_shp.plot(ax=ax, color="lightgrey", edgecolor="black")  # 시도 경계
sample_points_in_gyeonggi.plot(ax=ax, color="blue", markersize=1, alpha=0.5)  # 서울 지역 포인트
plt.show()


### 잠재량 수치 확인

#gdf_filtered['avgbyMonth'] = gdf_filtered['yearTotal'] / 12 * 8760

irr_total = gdf_filtered['Total'].sum()

irr_total / 10**12 * 10**4