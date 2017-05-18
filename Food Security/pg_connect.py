import psycopg2
import ee

execfile('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/production_connection.py')

con = psycopg2.connect(host=hostname, user=username, password=password, dbname=database, port=5444)

cur = con.cursor()

cur.execute("SELECT uuid, gpsse_lat, gpsse_long, end_date FROM household_pii")

out = cur.fetchall()

ee.Initialize()

image = ee.Image('UMD/hansen/global_forest_change_2015')
image = image.select('last_b30')

for row in out:
    point = ee.Geometry.Point(row[1], row[2]).buffer(1000)

    res = image.reduceRegion(reducer=ee.Reducer.histogram(), 
                       geometry=point).getInfo()

