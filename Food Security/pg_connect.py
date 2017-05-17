import psycopg2
import os

execfile('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/production_connection.py')

con = psycopg2.connect(host=hostname, user=username, password=password, dbname=database, port=5444)

cur = con.cursor()

cur.execute("SELECT uuid, gpsse_lat, gpsse_long, end_date FROM household_pii")

out = cur.fetchall()

for row in out:
    point = ee.Geometry.Point(row[1], row[2]).buffer(10000)
    res = point.area()
    
import ee

image = ee.Image('srtm90_v4')