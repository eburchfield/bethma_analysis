from osgeo import gdal, gdalnumeric, ogr, osr
gdal.UseExceptions()
import numpy
import glob
import os

d = "E://ls//Bulk Order 565137//L8 OLI_TIRS"
os.chdir(d)

#making binary quality mask
#http://courses.neteler.org/processing-landsat8-data-in-grass-gis-7/
qa = sorted(glob.glob("*QA.tif"))

for i in range(len(qa)):
	t = gdal.Open(qa[i]).ReadAsArray()
	t[t==49152] = 9999
	t[t!=49152] = 0
	arr = t
	data = gdal.Open(d + '//LC81410542014165LGN00_B1.TIF')
	nodatav = 9999

	[cols, rows] = arr.shape
	trans = data.GetGeoTransform()
	proj = data.GetProjection()
	outfile = 'E://ls//' + qa[i][6:16] + 'bin.tif'

	outdriver = gdal.GetDriverByName('GTiff')
	outdata = outdriver.Create(str(outfile), rows, cols, 1, gdal.GDT_Float32)

	srs = osr.SpatialReference()
	srs.ImportFromEPSG(4326)
	outdata.SetProjection(srs.ExportToWkt())
	outdata.SetGeoTransform(trans)
	outdata.SetProjection(proj)
	 
	#write array to file
	outdata.GetRasterBand(1).WriteArray(arr)
	outdata.GetRasterBand(1).SetNoDataValue(nodatav)
	outdata = None