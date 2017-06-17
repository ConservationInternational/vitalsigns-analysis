import psycopg2
import ee
import pandas as pd
import time

execfile('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/production_connection.py')

con = psycopg2.connect(host=hostname, user=username, password=password, dbname=database, port=5444)

cur = con.cursor()

cur.execute("SELECT hh_refno, gpsse_lat, gpsse_long, end_date FROM householdcontact WHERE round = '1'")

out = cur.fetchall()

ee.Initialize()

def rename_dict(pref, d):
    for i in d.keys():
        d[pref + i] = d.pop(i)
    return(d)

def merge_dicts(*dicts):
    superdict = {}
    for d in dicts:
        for k, v in d.iteritems():
            superdict[k] = v
    return(superdict)

def maskvalues(img, vals):
    for v in vals:
        img = img.where(img.eq(v), 0)
    return(img)

#Get PAs
PA = ee.Image('users/mcooper/wdpa')

#Get full raster stack
forest13 = ee.Image("UMD/hansen/global_forest_change_2014")
forest14 = ee.Image("UMD/hansen/global_forest_change_2015")
forest15 = ee.Image("UMD/hansen/global_forest_change_2015_v1_3")

#Get tree cover for 2000 as a binary raster
forest00 = forest13.select('treecover2000').where(forest13.select('treecover2000').gte(10), 1).where(forest13.select('treecover2000').lt(10), 0)

#Get 
fcover13 = forest00.add(forest13.select('gain')).subtract(forest13.select('loss')).multiply(PA)
fcover14 = forest00.add(forest14.select('gain')).subtract(forest14.select('loss')).multiply(PA)
fcover15 = forest00.add(forest15.select('gain')).subtract(forest15.select('loss')).multiply(PA)

#Get permanent water bodies and areas of no data
#   0 No Forest
#   1 Forest
#   2 Water Bodies
#   9 No Data
fcover13 = fcover13.where(forest13.select('datamask').eq(2), 2).where(forest13.select('datamask').eq(0), 9)
fcover14 = fcover14.where(forest13.select('datamask').eq(2), 2).where(forest14.select('datamask').eq(0), 9)
fcover15 = fcover15.where(forest13.select('datamask').eq(2), 2).where(forest15.select('datamask').eq(0), 9)


#Get all cci land cover
cci13 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2013-v207").multiply(PA)
cci14 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v207").multiply(PA)
cci15 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v207").multiply(PA)

pop15 = ee.Image("CIESIN/GPWv4/population-count/2015")
mkt10 = ee.Image("users/mcooper/TT_50K--SSA1")


#Overall Productivity
prd13 = ee.Image("users/geflanddegradation/ndvi/kut_integral_ndvi_modis_2001_2015").select('b13')
prd14 = ee.Image("users/geflanddegradation/ndvi/kut_integral_ndvi_modis_2001_2015").select('b14')
prd15 = ee.Image("users/geflanddegradation/ndvi/kut_integral_ndvi_modis_2001_2015").select('b15')

#Forest Productivity
fr_prd13 = prd13.multiply(maskvalues(fcover13, [-1, 2, 9]))
fr_prd14 = prd14.multiply(maskvalues(fcover14, [-1, 2, 9]))
fr_prd15 = prd15.multiply(maskvalues(fcover15, [-1, 2, 9]))

#Non-Agricultural Areas
cci_mask = [0, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220]

#Agricultural Productivity
ag_prd13 = prd13.multiply(maskvalues(cci13, cci_mask))
ag_prd14 = prd14.multiply(maskvalues(cci14, cci_mask))
ag_prd15 = prd15.multiply(maskvalues(cci15, cci_mask))
                        
accum = pd.DataFrame()

#Restart from the next line if memory limits are exceeded
#out = out[out.index(row): ]
for row in out:
    print row[0]

    point = ee.Geometry.Point(row[2], row[1]).buffer(10000)

    if row[3].year == 2013:
        forest = fcover13.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=point).getInfo()
        cci = cci13.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=point).getInfo()
        fr_prod = fr_prd13.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
        ag_prod = ag_prd13.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
        fr_prod = {'fr_prod': fr_prod['b13']}
        ag_prod = {'ag_prod': ag_prod['b13']} 

    elif row[3].year == 2014:
        forest = fcover14.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=point).getInfo()
        cci = cci14.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=point).getInfo()
        fr_prod = fr_prd14.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
        ag_prod = ag_prd14.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
        fr_prod = {'fr_prod': fr_prod['b14']}
        ag_prod = {'ag_prod': ag_prod['b14']}
        
    else:
        forest = fcover15.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=point).getInfo()
        cci = cci15.reduceRegion(reducer=ee.Reducer.frequencyHistogram(), geometry=point).getInfo()
        fr_prod = fr_prd15.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
        ag_prod = ag_prd15.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
        fr_prod = {'fr_prod': fr_prod['b15']}
        ag_prod = {'ag_prod': ag_prod['b15']} 
    
    time.sleep(1)
    
    pop = pop15.reduceRegion(reducer=ee.Reducer.sum(), geometry=point).getInfo()
    mkt = mkt10.reduceRegion(reducer=ee.Reducer.mean(), geometry=point).getInfo()

    
    time.sleep(1)

    cci = rename_dict('cci_', cci['b1'])
    fr = rename_dict('for_', forest['treecover2000'])
    head = dict(zip(['hh_refno', 'lat', 'long', 'end_date'], row))
    pop = {'pop15': pop['population-count']}
    mkt = {'market_dist_hrs': mkt['b1']}
    
    temp = pd.DataFrame(merge_dicts(cci, fr, head, pop, mkt, fr_prod, ag_prod), index = [0])
    
    accum = accum.append(temp)

accum.to_csv('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/ee_export.csv', index=False)