import psycopg2
import ee
import pandas as pd
import math

execfile('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/production_connection.py')

con = psycopg2.connect(host=hostname, user=username, password=password, dbname=database, port=5444)

cur = con.cursor()

cur.execute("SELECT hh_refno, gpsse_lat, gpsse_long, end_date FROM householdcontact")

out = cur.fetchall()

ee.Initialize()

#Get full raster stack
forest13 = ee.Image("UMD/hansen/global_forest_change_2014")
forest14 = ee.Image("UMD/hansen/global_forest_change_2015")
forest15 = ee.Image("UMD/hansen/global_forest_change_2015_v1_3")

#Get tree cover for 2000 as a binary raster
forest00 = forest13.select('treecover2000').where(forest13.select('treecover2000').gte(10), 1).where(forest13.select('treecover2000').lt(10), 0)

#Get 
fcover13 = forest00.add(forest13.select('gain')).subtract(forest13.select('loss'))
fcover14 = forest00.add(forest14.select('gain')).subtract(forest14.select('loss'))
fcover15 = forest00.add(forest15.select('gain')).subtract(forest15.select('loss'))

#Get permanent water bodies and areas of no data
#   0 No Forest
#   1 Forest
#   2 Water Bodies
#   9 No Data
fcover13 = fcover13.where(forest13.select('datamask').eq(2), 2).where(forest13.select('datamask').eq(0), 9)
fcover14 = fcover14.where(forest13.select('datamask').eq(2), 2).where(forest14.select('datamask').eq(0), 9)
fcover15 = fcover15.where(forest13.select('datamask').eq(2), 2).where(forest15.select('datamask').eq(0), 9)


#Get all cci land cover
cci13 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2013-v207")
cci14 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v207")
cci15 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v207")

pop15 = ee.Image("CIESIN/GPWv4/population-count/2015")
                          
accum = pd.DataFrame()
for row in out:
    print row[0]

    point = ee.Geometry.Point(row[2], row[1]).buffer(10000)

    if row[3].year == 2013:
        forest = fcover13.reduceRegion(reducer=ee.Reducer.histogram(), geometry=point).getInfo()
        cci = cci13.reduceRegion(reducer=ee.Reducer.histogram(), geometry=point).getInfo()

    elif row[3].year == 2014:
        forest = fcover14.reduceRegion(reducer=ee.Reducer.histogram(), geometry=point).getInfo()
        cci = cci14.reduceRegion(reducer=ee.Reducer.histogram(), geometry=point).getInfo()
        
    else:
        forest = fcover15.reduceRegion(reducer=ee.Reducer.histogram(), geometry=point).getInfo()
        cci = cci15.reduceRegion(reducer=ee.Reducer.histogram(), geometry=point).getInfo()
        
    pop = pop15.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()

    cci_cols = ['cci_' + str(math.floor(i)) for i in cci['b1']['bucketMeans']]
    cci_vals = cci['b1']['histogram']
    
    for_cols = ['for_' + str(math.floor(i)) for i in forest['treecover2000']['bucketMeans']]
    for_vals = forest['treecover2000']['histogram']
    
    oth_c = ['hh_refno', 'lat', 'long', 'end_date']
    oth_v = list(row)
    
    pop_c = ['pop15']
    pop_v = pop['population-count']
    
    temp = pd.DataFrame(data=[oth_v + cci_vals + for_vals + pop_c], 
                        columns = oth_c + cci_cols + for_cols + pop_v)
    
    accum = accum.append(temp)