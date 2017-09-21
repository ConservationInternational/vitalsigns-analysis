import psycopg2
import ee
import pandas as pd

execfile('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/production_connection.py')

con = psycopg2.connect(host=host, user=user, password=password, dbname=dbname, port=port)

cur = con.cursor()

cur.execute("""SELECT hh_refno, gpsse_lat, gpsse_long, end_date, landscape_no, country FROM 
(SELECT *,
count(*) OVER (PARTITION BY hh_refno, round) AS count
FROM 
householdcontact) a
WHERE count = 1 and round = '1'""")

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
    img = img.where(img.eq(0), 1)
    for v in vals:
        img = img.where(img.eq(v), 0)
    img = img.where(img.neq(0), 1)
    return(img)


y2013 = []
y2014 = []
y2015 = []

for row in out:
    geom = ee.Geometry.Point(row[2], row[1]).buffer(7500)
    feat = ee.Feature(geom, {'hh_refno':row[0], 'lat':row[1], 'long':row[2], 'date':str(row[3]),
                             'landscape_no':row[4], 'country':row[5]})
    if row[3].year == 2013:
        y2013.append(feat)
    elif row[3].year == 2014:
        y2014.append(feat)
    else:
        y2015.append(feat)

y2013 = ee.FeatureCollection(y2013)
y2014 = ee.FeatureCollection(y2014)
y2015 = ee.FeatureCollection(y2015)

#################################
#Forests
#################################

#Get full raster stack
forest13 = ee.Image("UMD/hansen/global_forest_change_2014")
forest14 = ee.Image("UMD/hansen/global_forest_change_2015")
forest15 = ee.Image("UMD/hansen/global_forest_change_2015_v1_3")

#Get tree cover for 2000 as a binary raster
forest00 = forest13.select('treecover2000').where(forest13.select('treecover2000').gte(10), 1).where(forest13.select('treecover2000').lt(10), 0)

#Add gain and remove loss from 2000 tree cover 
fcover13 = forest00.add(forest13.select('gain')).subtract(forest13.select('loss')).where(forest13.select('datamask').eq(2), 2).where(forest13.select('datamask').eq(0), 9)
fcover14 = forest00.add(forest14.select('gain')).subtract(forest14.select('loss')).where(forest13.select('datamask').eq(2), 2).where(forest14.select('datamask').eq(0), 9)
fcover15 = forest00.add(forest15.select('gain')).subtract(forest15.select('loss')).where(forest13.select('datamask').eq(2), 2).where(forest15.select('datamask').eq(0), 9)

#Get permanent water bodies and areas of no data
#   0 No Forest
#   1 Forest
#   2 Water Bodies
#   9 No Data

#Reduce
f13 = fcover13.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2013).getInfo()
f14 = fcover14.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2014).getInfo()
f15 = fcover15.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2015).getInfo()

faccum = pd.DataFrame()
for f in [f13, f14, f15]:
    for i in f['features']:
        temp = pd.DataFrame(merge_dicts(rename_dict('for_', i['properties']['histogram']),
                                        {'hh_refno': i['properties']['hh_refno']}), index = [0])
        faccum = faccum.append(temp)


#########################
#Forests In PAs
##########################
#Get PAs
PA = ee.Image('users/mcooper/wdpa')

#==============================================================================
# 
# fpa13 = fcover13.where(PA.eq(1), 0).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2013).getInfo()
# fpa14 = fcover14.where(PA.eq(1), 0).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2014).getInfo()
# fpa15 = fcover15.where(PA.eq(1), 0).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2015).getInfo()
# 
# fpaaccum = pd.DataFrame()
# for f in [fpa13, fpa14, fpa15]:
#     for i in f['features']:
#         temp = pd.DataFrame(merge_dicts(rename_dict('forPA_', i['properties']['histogram']),
#                                         {'hh_refno': i['properties']['hh_refno']}), index = [0])
#         fpaaccum = fpaaccum.append(temp)
# 
#==============================================================================
############################
#Get all cci land cover
############################
cci13 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2013-v207")
cci14 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2014-v207")
cci15 = ee.Image("users/geflanddegradation/lcov/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v207")

cci13r = cci13.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2013).getInfo()
cci14r = cci14.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2014).getInfo()
cci15r = cci15.reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2015).getInfo()

cciaccum = pd.DataFrame()
for f in [cci13r, cci14r, cci15r]:
    for i in f['features']:
        temp = pd.DataFrame(merge_dicts(rename_dict('cci_', i['properties']['histogram']),
                                        {'hh_refno': i['properties']['hh_refno']}), index = [0])
        cciaccum = cciaccum.append(temp)
        
     
############################
#Get all cci masking PAs
############################   
#==============================================================================
# cci13PA = cci13.where(PA.eq(1), 0).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2013).getInfo()
# cci14PA = cci14.where(PA.eq(1), 0).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2014).getInfo()
# cci15PA = cci15.where(PA.eq(1), 0).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2015).getInfo()
# 
# ccipaaccum = pd.DataFrame()
# for f in [cci13PA, cci14PA, cci15PA]:
#     for i in f['features']:
#         temp = pd.DataFrame(merge_dicts(rename_dict('cciPA_', i['properties']['histogram']),
#                                         {'hh_refno': i['properties']['hh_refno']}), index = [0])
#         ccipaaccum = ccipaaccum.append(temp)
#==============================================================================

############################
#Get total PA area
############################
PA13r = PA.where(PA.eq(1),0).where(PA.eq(0), 1).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2013).getInfo()
PA14r = PA.where(PA.eq(1),0).where(PA.eq(0), 1).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2014).getInfo()
PA15r = PA.where(PA.eq(1),0).where(PA.eq(0), 1).reduceRegions(reducer=ee.Reducer.frequencyHistogram(), collection=y2015).getInfo()

paaccum = pd.DataFrame()
for p in [PA13r, PA14r, PA15r]:
    for i in p['features']:
        temp = pd.DataFrame(merge_dicts(rename_dict('PA_', i['properties']['histogram']),
                                        {'hh_refno': i['properties']['hh_refno']}), index = [0])
        paaccum = paaccum.append(temp)

############################
#Mean Ag Productivity
############################
#Overall Productivity
#==============================================================================
# prd13 = ee.Image("users/mcooper/Integrals_2001_2015").select('b13')
# prd14 = ee.Image("users/mcooper/Integrals_2001_2015").select('b14')
# prd15 = ee.Image("users/mcooper/Integrals_2001_2015").select('b15')
# ag_mask = [10, 11, 12, 20, 30, 190, 200]
# 
# a_prd13 = prd13.multiply(maskvalues(cci13, ag_mask)).reduceRegions(reducer=ee.Reducer.mean(), collection=y2013).getInfo()
# a_prd14 = prd14.multiply(maskvalues(cci14, ag_mask)).reduceRegions(reducer=ee.Reducer.mean(), collection=y2014).getInfo()
# a_prd15 = prd15.multiply(maskvalues(cci15, ag_mask)).reduceRegions(reducer=ee.Reducer.mean(), collection=y2015).getInfo()
# 
# agprdaccum = pd.DataFrame()
# for p in [a_prd13, a_prd14, a_prd15]:
#     for i in p['features']:
#         temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 'mean_ag_prd': i['properties']['mean']}, index = [0])
#         agprdaccum = agprdaccum.append(temp)
#==============================================================================

############################
#Mean Forest Productivity
############################
#==============================================================================
# fr_prd13 = prd13.multiply(maskvalues(fcover13, [-1, 0, 2, 9])).reduceRegions(reducer=ee.Reducer.mean(), collection=y2013).getInfo()
# fr_prd14 = prd14.multiply(maskvalues(fcover14, [-1, 0, 2, 9])).reduceRegions(reducer=ee.Reducer.mean(), collection=y2014).getInfo()
# fr_prd15 = prd15.multiply(maskvalues(fcover15, [-1, 0, 2, 9])).reduceRegions(reducer=ee.Reducer.mean(), collection=y2015).getInfo()
# 
# frprdaccum = pd.DataFrame()
# for p in [fr_prd13, fr_prd14, fr_prd15]:
#     for i in p['features']:
#         temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 'mean_fr_prd': i['properties']['mean']}, index = [0])
#         frprdaccum = frprdaccum.append(temp)
#==============================================================================

############################
#Mean Non-Ag Productivity
############################
#==============================================================================
# #Non-Agricultural Areas
# nonag_mask = [40, 50, 60, 61, 62, 80, 90, 100, 110, 120, 122, 130, 160, 170, 180, 190]
# 
# na_prd13 = prd13.multiply(maskvalues(cci13, nonag_mask)).reduceRegions(reducer=ee.Reducer.mean(), collection=y2013).getInfo()
# na_prd14 = prd14.multiply(maskvalues(cci14, nonag_mask)).reduceRegions(reducer=ee.Reducer.mean(), collection=y2014).getInfo()
# na_prd15 = prd15.multiply(maskvalues(cci15, nonag_mask)).reduceRegions(reducer=ee.Reducer.mean(), collection=y2015).getInfo()
# 
# naprdaccum = pd.DataFrame()
# for p in [fr_prd13, fr_prd14, fr_prd15]:
#     for i in p['features']:
#         temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 'mean_nonag_prd': i['properties']['mean']}, index = [0])
#         naprdaccum = naprdaccum.append(temp)
#==============================================================================


############################
#Sum Ag Productivity
############################
#==============================================================================
# a_prd13 = prd13.multiply(maskvalues(cci13, ag_mask)).reduceRegions(reducer=ee.Reducer.sum(), collection=y2013).getInfo()
# a_prd14 = prd14.multiply(maskvalues(cci14, ag_mask)).reduceRegions(reducer=ee.Reducer.sum(), collection=y2014).getInfo()
# a_prd15 = prd15.multiply(maskvalues(cci15, ag_mask)).reduceRegions(reducer=ee.Reducer.sum(), collection=y2015).getInfo()
# 
# sagprdaccum = pd.DataFrame()
# for p in [a_prd13, a_prd14, a_prd15]:
#     for i in p['features']:
#         temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 'sum_ag_prd': i['properties']['sum']}, index = [0])
#         sagprdaccum = sagprdaccum.append(temp)
#==============================================================================

############################
#Sum Forest Productivity
############################
#==============================================================================
# fr_prd13 = prd13.multiply(maskvalues(fcover13, [-1, 0, 2, 9])).reduceRegions(reducer=ee.Reducer.sum(), collection=y2013).getInfo()
# fr_prd14 = prd14.multiply(maskvalues(fcover14, [-1, 0, 2, 9])).reduceRegions(reducer=ee.Reducer.sum(), collection=y2014).getInfo()
# fr_prd15 = prd15.multiply(maskvalues(fcover15, [-1, 0, 2, 9])).reduceRegions(reducer=ee.Reducer.sum(), collection=y2015).getInfo()
# 
# sfrprdaccum = pd.DataFrame()
# for p in [fr_prd13, fr_prd14, fr_prd15]:
#     for i in p['features']:
#         temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 'sum_fr_prd': i['properties']['sum']}, index = [0])
#         sfrprdaccum = sfrprdaccum.append(temp)
#==============================================================================

############################
#Sum Non-Ag Productivity
############################
#==============================================================================
# na_prd13 = prd13.multiply(maskvalues(cci13, nonag_mask)).reduceRegions(reducer=ee.Reducer.sum(), collection=y2013).getInfo()
# na_prd14 = prd14.multiply(maskvalues(cci14, nonag_mask)).reduceRegions(reducer=ee.Reducer.sum(), collection=y2014).getInfo()
# na_prd15 = prd15.multiply(maskvalues(cci15, nonag_mask)).reduceRegions(reducer=ee.Reducer.sum(), collection=y2015).getInfo()
# 
# snaprdaccum = pd.DataFrame()
# for p in [fr_prd13, fr_prd14, fr_prd15]:
#     for i in p['features']:
#         temp = pd.DataFrame({'hh_refno': i['properties']['hh_refno'], 'sum_nonag_prd': i['properties']['sum']}, index = [0])
#         snaprdaccum = snaprdaccum.append(temp)
#==============================================================================
        
###############################
#Combine
#############################

hh = reduce((lambda x, y: pd.merge(x, y, on='hh_refno')), [faccum, cciaccum, paaccum#, fpaaccum, ccipaaccum, agprdaccum, frprdaccum,
                                                            #naprdaccum, sagprdaccum, sfrprdaccum, snaprdaccum
                                                            ])


ls_cur = con.cursor()
ls_cur.execute("""SELECT country, landscape_no, lower_right_latitude, lower_right_longitude, lower_left_latitude, lower_left_longitude,
                           upper_left_latitude, upper_left_longitude, lower_left_latitude, lower_left_longitude
                   FROM landscape WHERE country <> 'KEN' AND landscape_no <> '000'""")
ls_out = ls_cur.fetchall()

ls = []
for row in ls_out:
    poly = ee.Geometry.Polygon([[row[3], row[2]], [row[5], row[4]], [row[7], row[6]], [row[9], row[8]]])
    feat = ee.Feature(poly, {'country': row[0], 'landscape_no': row[1]})
    ls.append(feat)

ls = ee.FeatureCollection(ls)

popaccum = pd.DataFrame()
pop15 = ee.Image("users/mcooper/afripop").reduceRegions(reducer=ee.Reducer.sum(), collection=ls).getInfo()
for i in pop15['features']:
    temp = pd.DataFrame({'country': i['properties']['country'], 'landscape_no': i['properties']['landscape_no'], 'pop': i['properties']['sum']}, index=[0])
    popaccum = popaccum.append(temp) 

mktaccum = pd.DataFrame()
mkt10 = ee.Image("users/mcooper/TT_50K--SSA1").reduceRegions(reducer=ee.Reducer.mean(), collection=ls).getInfo()
for i in mkt10['features']:
    temp = pd.DataFrame({'country': i['properties']['country'], 'landscape_no': i['properties']['landscape_no'], 'market': i['properties']['mean']}, index=[0])
    mktaccum = mktaccum.append(temp) 


ls_dat = pd.merge(popaccum, mktaccum, on=['landscape_no', 'country'])

hh['country'] = hh.hh_refno.str[:3]
hh['landscape_no'] = hh.hh_refno.str[4:7]

final = pd.merge(hh, ls_dat, on=['country', 'landscape_no'])

final.to_csv('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/ee_export.csv', index=False)