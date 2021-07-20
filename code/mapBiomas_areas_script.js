
// var mB = ee.Image('projects/mapbiomas-workspace/public/collection3_1/mapbiomas_collection31_integration_v1');
var mB = ee.Image('projects/mapbiomas-workspace/public/collection5/mapbiomas_collection50_integration_v1');
print(mB, 'mapbiomas'); // image with 35 bands. bandnames, classification_1985 etc to 2019

var bandNames = mB.bandNames();
print(bandNames);
// // need to convert bandmae to ee.String() to use variable as band selector
// print(bandNames.get(0));
// print(mB.select(ee.String(bandNames.get(0))));

// import feature collections
var aoi = ee.FeatureCollection('users/cdevenish/hya_macaw/aoi_wgs_new');
var occ = ee.FeatureCollection('users/cdevenish/hya_macaw/pts_21'); // FeatureCollection with 1452 elements

// export aoi
var aoi_exp = ee.Geometry.Rectangle([4123000, 7462000, 6332000, 10424000], 'EPSG:5880', false, true); //(xMin, yMin, xMax, yMax)
/*
xmin       : 4123000 
xmax       : 6332000 
ymin       : 7462000
ymax       : 10424000
*/

// iterate over bandames, select each one and add to list

var addImage = function(bn, imList){
  imList = ee.List(imList);
  bn = ee.String(bn);
  var im = mB.select(bn);
  return imList.add(im);
  };
  
var imageList = bandNames.iterate(addImage, ee.List([]));
// print(imageList);

// Convert list to imageCollection
var mBcollection = ee.ImageCollection.fromImages(imageList);
// print(mBcollection, 'mB image Collection');

// set id of each element to the year
// print(mBcollection.propertyNames(), 'properties');

var setID = function(im){
  var yr = im.bandNames().get(0);
  return ee.Image(im).set('system:id', yr)};

mBcollection = mBcollection.map(setID);
// print(mBcollection, 'mB image Collection');

/// Reclassify to reduced classes //////

// Reclassifier function
var rc = function(image) {
  
  // add reclass name to year
  var bNames = image.bandNames();
  var namesF = bNames.map(function(nm) {return ee.String(nm).cat('_forest');});
  var namesNF = bNames.map(function(nm) {return ee.String(nm).cat('_nonForest');});
  var namesAg = bNames.map(function(nm) {return ee.String(nm).cat('_agri');});
  var namesWet = bNames.map(function(nm) {return ee.String(nm).cat('_wetL');});
  var namesPs = bNames.map(function(nm) {return ee.String(nm).cat('_past');});
  
  // don't convert to float here (not like in mabBiomas_analysis) as we need integers fro grouping
  return image.addBands(image
      //.clipToCollection(aoi)
      .remap([3], [1], 0) // Forest reclass -- forest(3), - minimal area (mangrove not included)
      //.toFloat() // so that the sum produces a float, not integer
      .rename(namesF)).addBands(image
        //.clipToCollection(aoi)
        .remap([4,12,13], [1,1,1], 0) // Non Forest reclass -- grassland(12), other(13), Savanna(4)
        //.toFloat()
        .rename(namesNF)).addBands(image
          //.clipToCollection(aoi)
          .remap([39,20,41,36], [1,1,1,1], 0) // Agri reclass -- soy(39), sugar(20), temp crops(41), perenn crop(36)
          //.toFloat()
          .rename(namesAg).addBands(image
        //.clipToCollection(aoi)
        .remap([11], [1], 0) // Wetland reclass -- wetland(11)
        //.toFloat()
        .rename(namesWet)).addBands(image
        //.clipToCollection(aoi)
        .remap([15,21], [1,1], 0) // Pasture reclass -- pasture(15), , mosaic(21)
        //.toFloat()
        .rename(namesPs))
          );
};

// DO reclassification and windows 
var mB_prep = mBcollection
            .map(rc)
            ;
            
print(mB_prep, 'reclassified bands');


// Do areas per year of one image
// https://spatialthoughts.com/2020/06/19/calculating-area-gee/

/*
// create area image and add a single band with the classes
var areaImage = ee.Image.pixelArea().addBands(mB_prep.first().select('classification_1985'));
print(areaImage);

// sum area image for each class
var areas = areaImage.reduceRegion({
      reducer: ee.Reducer.sum().group({
      groupField: 1,
      groupName: 'class',
    }),
    geometry: aoi_exp,
    scale: 1000,
    maxPixels: 1e10
    }); 
 
print(areas);

// format dictionary for export - flatten and convert class names to strings (for dict keys)
var classAreas = ee.List(areas.get('groups')); // get 'groups' item from dict of dictionaries
 
var classAreaLists = classAreas.map(function(item) {
  
  var areaDict = ee.Dictionary(item);
  var classNumber = ee.Number(areaDict.get('class')).format();
  
  var area = ee.Number(
    areaDict.get('sum')).divide(1e6).round(); // convert to km^2 here 
  
  return ee.List([classNumber, area]);
  
});

var result = ee.Dictionary(classAreaLists.flatten());
print(result);
*/


// map through all images for all areas

/*
// get all bandames (toBands from the collection renames them, so here preseve all bandnames)
var bandNames = mB_prep.toList(mB_prep.size()).map(function(im) {return(ee.Image(im).bandNames());}).flatten();
print(bandNames, 'bandNames');
print(mB_prep.first().select([bandNames.get(0)]), 'first band');
*/

// flatten collection for selecting bands - OJO changes bandnames
var mB_all = mB_prep.toBands();
var bandNames = mB_all.bandNames(); // ee? list of strings
// rename bands 
bandNames = bandNames.map(function(s) {return(ee.String(s).replace('[[:digit:]]{1,2}_', ''));});
print(bandNames, 'bandNames');
// update image
mB_all = mB_all.rename(bandNames);

// print(mB_all.select([bandNames.get(0)]), 'first band');

// do just unclassified images
var addID = function(z, strList){
  strList = ee.List(strList);
  z = ee.String(ee.Number(z).int());
  z = ee.String('classification_').cat(z);
  return strList.add(z);
  };
var selNames = ee.List.sequence(1995, 2019, 1).iterate(addID, ee.List([]));
print(selNames, 'selected band names');

var allCols = ee.Dictionary(['0', 0.0, '1', 0.0, '11', 0.0, '12', 0.0, '13', 0.0, '15', 0.0, '20', 0.0, '21', 0.0, '23', 0.0, '24', 0.0, '25', 0.0, '29', 0.0, '3', 0.0, '30', 0.0, '31', 0.0, '32', 0.0, '33', 0.0, '36', 0.0, '4', 0.0, '41', 0.0, '5', 0.0, '9', 0.0]);
print(allCols.size());

// get imageCollection with these bands

// function
// map through bandames and get area dictionaries
var getAreas = function(b){
  
  // get names
  var name = ee.String(b);
  var yr = name.replace('classification_', '');
  
  var areaImage = ee.Image.pixelArea().addBands(mB_all.select([b]));
  
  // sum area image for each class
  var areas = areaImage.reduceRegion({
      reducer: ee.Reducer.sum().group({
      groupField: 1,
      groupName: 'class',
    }),
    geometry: aoi,
    scale: 100,
    maxPixels: 1e10
    }); 
    
  // format dictionary for export - flatten and convert class names to strings (for dict keys)
  var classAreas = ee.List(areas.get('groups')); // get 'groups' item from dict of dictionaries
  
  var classAreaLists = classAreas.map(function(item) {
    
    var areaDict = ee.Dictionary(item);
    var classNumber = ee.Number(areaDict.get('class')).format();
    var area = ee.Number(
      areaDict.get('sum')).divide(1e6).round(); // convert to km^2 here 
    
    return ee.List([classNumber, area]);
  });
  
  var result = ee.Dictionary(classAreaLists.flatten());
  // check no. columns
  var nc = result.size();
  
  // make sure all columns are present. combine with empty dictionary with all cols. second dict trumps
  result = allCols.combine(result);
  
  // convert to feature for export
  var feature = ee.Feature(null, result);
  
  // add properties here
  return feature.set('name', name, 'year', yr, 'ncol', nc);
  
};


// trial
// bandNames = bandNames.slice(0,5);
var areaRes = bandNames.map(getAreas);
// print(areaRes);

// just selected
// selNames = ee.List(['classification_2019']);
var selAreas = ee.List(selNames).map(getAreas);  // why isnt' selNames an eeList already?

// Ojo to join as feature colection must have same dictionary keys (aka column headings)

// export as table
// Wrap the Feature in a FeatureCollection for export.
var featureCollection = ee.FeatureCollection(selAreas);
// print(featureCollection);


// Export the FeatureCollection.
Export.table.toDrive({
  collection: featureCollection,
  description: 'biomapAreas_unclassified100',
  fileFormat: 'CSV',
  folder: 'HyacinthMacaw'
  });


var feature2 = ee.FeatureCollection(areaRes);
Export.table.toDrive({
  collection: feature2,
  description: 'biomapAreas100',
  fileFormat: 'CSV',
  folder: 'HyacinthMacaw'
  });
  

// Check the area I'm calculating for = clipped aoi... 

// Center the Map.
Map.setCenter(-52, -11, 5);

// set display parameters
var kelly_cols = ['F2F3F4', '222222', 'F3C300', '875692', 'F38400', 'A1CAF1', 'BE0032', 'C2B280', '848482', '008856', 'E68FAC', '0067A5', 'F99379', '604E97', 'F6A600', 'B3446C', 'DCD300', '882D17', '8DB600', '654522', 'E25822', '2B3D26'];
var visParams = {min: 0, max: 22, palette: kelly_cols};
var visRC = {min:0, max:1, palette: ['b5b5b5', '25890c']}; 

// Display the image.
Map.addLayer(mB_prep.first().select('classification_1985').clip(aoi_exp), visParams, 'mapbiomas 1985 aoi exp');
Map.addLayer(mB_prep.first().select('classification_1985_forest').clip(aoi), visRC, 'forest RC 1985 aoi');

// Map.addLayer(occ, {}, 'occurrence points');

