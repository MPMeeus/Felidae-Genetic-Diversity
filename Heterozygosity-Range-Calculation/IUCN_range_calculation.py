#!/usr/bin/python3

###USAGE
#Run as script or parsed code in the QGIS Python Console
#SET UP THE GENERAL PROPERTIES OF QGIS SO THAT WGS84 IS THE DEFAULT ELLIPSOID!

###AUTHOR
#Alexandros Bantounas
#written on 25 April 2023

#Importing QGIS functions
from qgis.core import QgsVectorLayer, QgsVectorDataProvider
from qgis.PyQt.QtCore import QVariant

# from a layer stored somewhere in your computer
#layer = QgsVectorLayer('PATH_TO_SHP', 'INTERNAL_NAME_FOR_FILE', 'ogr') #Use double backslash in the path

# Here we get the capabilities of your layer (Add attribute layer, edit feature ect ..
caps = layer.dataProvider().capabilities()

# We make a list of fields from their name
fields_name = [f.name() for f in layer.fields()]

# We check if we can add an attribute to the layer.
if caps & QgsVectorDataProvider.AddAttributes:
    # We check if the attribute field is not exist
    if "Area" not in fields_name:
        # We add the field name Area and with the double type (it can be integer or text
        layer.dataProvider().addAttributes([QgsField("Area", QVariant.Double)])
        # We update layer's field otherwise we'll not have the field
        layer.updateFields()
        # Recreate the list field by the name to have index of the field
        fields_name = [f.name() for f in layer.fields()]
        # we get the index of the Area field
        fareaidx = fields_name.index('Area')
    else:
        # We are here because there is a field name Area
        print("The Area field is already added")
        # Recreate the list field by the name to have index of the field
        fields_name = [f.name() for f in layer.fields()]
        # we get the index of the Area field
        fareaidx = fields_name.index('Area')
        
#We set  up the calculator that will calculate 2D areas on the ellipsoid 
d=QgsDistanceArea()

#We set the ellipsoid to the default projection system (WGS84/EPSG:4326)
d.setEllipsoid('WGS84')
  
# Here we check if we can change the attributes of the layer
if caps & QgsVectorDataProvider.ChangeAttributeValues:
    # we loop on every feature
    for feature in layer.getFeatures():
        # For each feature :
        # We calculate the area in km2 and put the index of the field Area
        attrs = {fareaidx : d.convertAreaMeasurement(d.measureArea(feature.geometry()), QgsUnitTypes.AreaSquareKilometers)}
        # We change the the value of Area Field for this feature.
        layer.dataProvider().changeAttributeValues({feature.id() : attrs})

# Load layer in QGIS
iface
#lyr = iface.addVectorLayer('PATH_TO_SHP', 'INTERNAL_NAME_FOR_FILE', 'ogr')
