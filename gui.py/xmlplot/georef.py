import os.path
import zipfile
import StringIO

# NumPy
import numpy

# Try importing basemap
hasbasemap = True
try:
    import mpl_toolkits.basemap
except ImportError:
    hasbasemap = False

# GDAL (try two installation conventions)
try:
    from osgeo import gdal,osr
except ImportError:
    try:
        import gdal,osr
    except ImportError:
        gdal,osr = None,None

# Custom modules
import plot
import xmlstore

debug = False
                
class GeoRefExporter(plot.BaseExporter):
    propertyxml = """<?xml version="1.0"?>
<element name="Settings">
  <element name="Width" label="width" type="int" unit="px"/>
</element>
    """

    def __init__(self,source):
        # Create XML stores for default and customized properties.
        self.schema = xmlstore.xmlstore.Schema(self.propertyxml,sourceisxml=True)
        store = xmlstore.xmlstore.TypedStore(self.schema)
        self.defaultstore = xmlstore.xmlstore.TypedStore(self.schema)
        self.defaultstore['Width'].setValue(1024)
        store.setDefaultStore(self.defaultstore)

        sourcecopy = plot.Figure()
        sourcecopy.copyFrom(source)
        plot.BaseExporter.__init__(self,sourcecopy,store)

        self.figure = self.source.figure
        self.dpi = 96

        # No need to auto-update when we change properties - we will explicitly call draw when required.
        self.source.setUpdating(False)
        
    @classmethod
    def getFileTypes(cls,source):
        filetypes = {}
        if source.basemap is not None:
            if gdal is not None: filetypes['GeoTIFF'] = ('tif','tiff')
            filetypes['Keyhole Markup Language'] = ('kmz',)
        return filetypes

    def draw(self):
        self.source['Map/FillContinents'].setValue(False)
        self.source['Map/DrawCoastlines'].setValue(False)
        self.source['Map/DrawMapBoundary'].setValue(False)
        self.source['Map/DrawRivers'].setValue(False)
        self.source['Map/DrawCountries'].setValue(False)
        self.source['Map/DrawStates'].setValue(False)
        self.source['Map/DrawParallels'].setValue(False)
        self.source['Map/DrawMeridians'].setValue(False)
    
        widthpx = self.properties['Width'].getValue(usedefault=True)
    
        self.source.draw()
        
        self.figure.set_facecolor('None')
        self.figure.set_edgecolor('None')
        self.figure.set_dpi(self.dpi)
        
        self.figure.subplots_adjust(left=0., bottom=0., right=1., top=1.)
        #self.figure.gca().patch.set_visible(False)
        self.figure.gca().set_frame_on(False)
        
        if self.source.colorbar is not None:
            self.source.colorbar.ax.set_visible(False)

        # Calculate effective aspect ratio
        ax = self.figure.gca()
        xl,yl = ax.get_xlim(),ax.get_ylim()
        aspect = (yl[1]-yl[0])/(xl[1]-xl[0])

        # Adjust figure size according to desired aspect ratio
        # Add epsilon to desired size in inches, because MPL floors size in pixels,
        # which means that numerical inaccuracies could result in 1 pixel less width or height.
        self.figure.set_size_inches((widthpx+0.001)/self.dpi,(widthpx*aspect+0.001)/self.dpi)
        
    def export(self,path,format):
        assert format in ('GeoTIFF','Keyhole Markup Language'), 'Unknown export format "%s" requested.' % format
        if format=='GeoTiff':
            self.exportGeoTiff(path)
        else:
            self.exportKml(path)
        
    def exportGeoTiff(self,path):
        self.draw()

        # Draw the map in an in-memory buffer.
        if debug: print 'Drawing image in memory...'
        self.source.canvas.draw()
        widthpx,heightpx = map(int,self.source.canvas.renderer.get_canvas_width_height())
        buffer = self.source.canvas.renderer._renderer.buffer_rgba(0,0)
        arr = numpy.frombuffer(buffer,numpy.uint8)  # This should create a view on the buffer, rather than a copy of the data
        arr.shape = heightpx,widthpx,4
        if debug: print 'Resulting image is %i x %i pixels.' % (widthpx,heightpx)

        # Get minimum and maximum of axes to determine pixel dimensions and map offset.
        ax = self.figure.gca()
        xmin,xmax = ax.get_xlim()
        ymin,ymax = ax.get_ylim()

        # Create a GeoTIFF object. Note we use 4 bands, to accommodate RBGA.
        driver = gdal.GetDriverByName('GTiff')
        dst_ds = driver.Create(str(path), widthpx, heightpx, 4, gdal.GDT_Byte)

        # Set the image transform (pixel to map units)
        # Note that the y offset is the maximum y value, and [correspondingly] the y dimension is always negative.
        tf = [ xmin, (xmax-xmin)/widthpx, 0, ymax, 0, (ymin-ymax)/heightpx]
        dst_ds.SetGeoTransform(tf)
            
        # Determine spatial reference system [map projection]
        srs = osr.SpatialReference()
        if self.source.basemap.projection=='cyl':
            # For some reason, the Proj.4 string returned by basemap does not work here.
            # Explicitly use WGS84 instead.
            srs.SetWellKnownGeogCS('WGS84')
        else:
            # Convert Proj.4 string returned by basemap to a SpatialReference object
            proj4 = self.source.basemap.srs
            srs.ImportFromProj4(proj4)
            
        # Set map projection
        wkt = srs.ExportToWkt()
        dst_ds.SetProjection(wkt)

        # Store data in GeoTIFF
        if debug: print 'Writing image to GeoTIFF...'
        for i in range(4):  # RGBA
            dst_ds.GetRasterBand(i+1).WriteArray(arr[:,:,i])

        # Once we're done, close the dataset
        dst_ds = None

    def exportKml(self,path):
        # KML only supports one projection.
        self.source['Map/Projection'].setValue('cyl')
        
        # Build up the figure.
        self.draw()
        
        # Create KMZ file (ZIP container)
        out = zipfile.ZipFile(path,'w',zipfile.ZIP_DEFLATED)
    
        # Get map extent
        ax = self.figure.gca()
        xmin,xmax = ax.get_xlim()
        ymin,ymax = ax.get_ylim()
        
        title = self.source['Title'].getValue(usedefault=True)

        # Image file name
        imgpath = 'img.png'
        
        # Create the KML string.
        strkml = """<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
    <GroundOverlay>
        <name>%s</name>
        <Icon>
            <href>%s</href>
        </Icon>
        <LatLonBox>
            <north>%s</north>
            <south>%s</south>
            <east>%s</east>
            <west>%s</west>
            <rotation>0.</rotation>
        </LatLonBox>
    </GroundOverlay>
</kml>""" % (title,imgpath,ymax,ymin,xmax,xmin)
        
        # Save the KML string to the KMZ (ZIP) container.
        out.writestr('doc.kml',strkml)

        # Save the in-memory figure to the KMZ (ZIP) container.
        fpng = StringIO.StringIO()
        self.source.canvas.print_figure(fpng,dpi=self.dpi,facecolor='None',edgecolor='None',orientation='portrait')
        out.writestr(imgpath,fpng.getvalue())
        fpng.close()

        # Close the KMZ (ZIP) container.
        out.close()
        
if hasbasemap:
    plot.Figure.registerExporter('geo-referenced image file',GeoRefExporter)
