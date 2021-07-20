
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
print(imageList);

// Convert list to imageCollection
var mBcollection = ee.ImageCollection.fromImages(imageList);
print(mBcollection, 'mB image Collection');

// set id of each element to the year
print(mBcollection.propertyNames(), 'properties');

// var props = mBcollection.toList(mBcollection.size()).map(function(im){return ee.Image(im).propertyNames();});
// print(props);

// var ids = mBcollection.toList(mBcollection.size()).map(function(im){return ee.Image(im).get('system:id');});
// print(ids);

var setID = function(im){
  var yr = im.bandNames().get(0);
  return ee.Image(im).set('system:id', yr)};

mBcollection = mBcollection.map(setID);
print(mBcollection, 'mB image Collection');

/// Reclassify to reduced classes //////

// var bName = ee.String('forest');
// print(bName);


var bName2 = mBcollection.first().bandNames().get(0);
print(bName2);
bName2 = ee.List([ee.String(bName2).cat('_forest')]);
print(bName2);

var bName3 = mBcollection.first().bandNames();
print(bName3, 'bName3');

var bNames30 = bName3.map(function (nm) {
  return ee.String(nm).cat('_30k');
});
print(bNames30);

// var im1 = mBcollection.first().remap([2,3], [1,1], 0).rename('forest');
// var im1 = mBcollection.first().remap([2,3], [1,1], 0).rename(bName2);
// print(im1);


// Reclassifier function
var rc = function(image) {
  
  // add reclass name to year
  var bNames = image.bandNames();
  var namesF = bNames.map(function(nm) {return ee.String(nm).cat('_forest');});
  var namesNF = bNames.map(function(nm) {return ee.String(nm).cat('_nonForest');});
  var namesAg = bNames.map(function(nm) {return ee.String(nm).cat('_agri');});
  
  return image.addBands(image
      //.clipToCollection(aoi)
      .remap([3,5], [1,1], 0) // Forest reclass -- forest(3), mangrove(5)- minimal area 
      .toFloat() // so that the sum produces a float, not integer
      .rename(namesF)).addBands(image
        //.clipToCollection(aoi)
        .remap([4,11,12,13], [1,1,1,1], 0) // Non Forest reclass -- wetland(11), grassland(12), other(13), Savanna(4)
        .toFloat()
        .rename(namesNF)).addBands(image
          //.clipToCollection(aoi)
          .remap([15,39,20,21,41,36], [1,1,1,1,1,1], 0) // Agri reclass -- pasture(15), soy(39), sugar(20), temp crops(41), perenn crop(36), mosaic(21)
          .toFloat()
          .rename(namesAg)
        );
};

// Window proportion functions - 
var wK = function(image){
   
  var bNames = image.bandNames();
  var bNames30 = bNames.map(function(nm) {return ee.String(nm).cat('_30k');});
  var bNames10 = bNames.map(function(nm) {return ee.String(nm).cat('_10k');});

// kernel>  measured in pixels (at native resolution of 30 m).
// 166*2+1 = 333^2 pixels in the kernel. (333*30) = 9990 m sided square, 10k resolution
// 500*2+1 = 1001^2 (1001 * 30 ) = 30,030 resolution
  
  return image.addBands(image
      .reduceNeighborhood({
        reducer: ee.Reducer.sum(),
        kernel: ee.Kernel.square(15000,'meters', true) // third argument is normalise: true. Default. So, sum is weigthed sum, so gives mean over kernel. 
        })
      .rename(bNames30)).addBands(image
        .reduceNeighborhood({
          reducer: ee.Reducer.sum(),
          kernel: ee.Kernel.square(5000,'meters', true)
          })
        .rename(bNames10)
        ); // for 5k -- kernel: ee.Kernel.square(2500,'meters')
};


// DO reclassification and windows 
var mB_prep = mBcollection
            .map(rc)
            .map(wK)
            ;
            
print(mB_prep, 'reclassified bands');


/*
// Center the Map.
Map.setCenter(-52, -11, 5);

// set display parameters
var kelly_cols = ['F2F3F4', '222222', 'F3C300', '875692', 'F38400', 'A1CAF1', 'BE0032', 'C2B280', '848482', '008856', 'E68FAC', '0067A5', 'F99379', '604E97', 'F6A600', 'B3446C', 'DCD300', '882D17', '8DB600', '654522', 'E25822', '2B3D26'];
var visParams = {min: 0, max: 22, palette: kelly_cols};
var visRC = {min:0, max:1, palette: ['b5b5b5', '25890c']}; 

// Display the image.
Map.addLayer(mB.select('classification_1985'), visParams, 'mapbiomas_c5_1985');
// Map.addLayer(mB.select('classification_2019'), visParams, 'mapbiomas_c5_2019');

// Map.addLayer(occ, {}, 'occurrence points');

Map.addLayer(mB_prep.first().select('classification_1985_forest'), visRC, 'forest RC 1985');
Map.addLayer(mB_prep.first().select('classification_1985_nonForest'), visRC, 'non forest RC 1985');
Map.addLayer(mB_prep.first().select('classification_1985_agri'), visRC, 'agri RC 1985');

// Map.addLayer(mB_prep.first().select('classification_1985_forest_30k'), visRC, 'forest 30k 1985');
// Map.addLayer(mB_prep.first().select('classification_1985_forest_10k'), visRC, 'forest 30k 1985');

Map.addLayer(aoi, {}, 'study area',true,0.3);
*/


/*
// working for time steps - this is the basic idea: 
var all1995 = ee.Image(mB_prep.toList(mB_prep.size()).get(10));
print('1995 images', all1995);

var all1985 = ee.Image(mB_prep.toList(mB_prep.size()).get(0));
print('1985 layers', all1985);

// subtract time intervals from each other
var tNames = all1995.bandNames();
tNames = tNames.map(function(nm) {return ee.String(nm).cat('_10y');});
var allSubtr = all1995.subtract(all1985).rename(tNames);
print(allSubtr, 'subtract');
*/


// Make a list of numbers from 10 to 35,  loop through each and do subtraction above, and add to list of images, then to collection
// Make new mB_prep with unweigthed sum, and only for windows. 
// ee.List(repeat(10,])

// sequence will subtract image 10 years previous from 1995 to 2019
var yrs = ee.List.sequence(10, 34, 1);
print(yrs, 'years list');

// Time step function

// create holding list

var timeStep = function(yr){
  
  // define year
  var yr1 = ee.Number(yr);
  var yr2 = yr1.subtract(10);
  
  
  // define first image
  var all1 = ee.Image(mB_prep.toList(mB_prep.size()).get(yr1));
  // define second image
  var all2 = ee.Image(mB_prep.toList(mB_prep.size()).get(yr2));

  // subtract time intervals from each other
  var tNames = all1.bandNames();
  tNames = tNames.map(function(nm) {return ee.String(nm).cat('_10y');});
  var subtr = all1.subtract(all2).rename(tNames);
  
  // return subtracted images - will add to list in result
  return(subtr);
  
  // testing
  // return(ee.List([yr1, yr2]));
  
};

// loop throught the years - get a list of images
var mB_tmStepL = yrs.map(timeStep);
// print(mB_tmStepL);

// Convert list to imageCollection
var mB_tmStep = ee.ImageCollection.fromImages(mB_tmStepL);
// print(mB_tmStep, 'time step image Collection');

// set id of each element to the year
// print(mB_tmStep.propertyNames(), 'properties'); // no properties on this collection

/*
// GET image ids
var addID = function(z, strList){
  strList = ee.List(strList);
  z = ee.String(ee.Number(z).int());
  z = ee.String('classification_').cat(z);
  return strList.add(z);
  };
  
var tmIDs = ee.List.sequence(1995, 2019, 1).iterate(addID, ee.List([]));
print(tmIDs);
*/

// set IDs (allow to join collections later)
var setID2 = function(im){
  var yr = im.bandNames().get(0);
  yr = ee.String(yr).slice(0, 19);
  return ee.Image(im).set('system:id', yr)};

mB_tmStep = mB_tmStep.map(setID2);
// print(mB_tmStep, 'timeStep image Collection');


// join

var filter = ee.Filter.equals({
  leftField: 'system:id',
  rightField: 'system:id'
});

// Create the join.
var simpleJoin = ee.Join.inner();

// Inner join -- produces feature collection
var innerJoin = ee.ImageCollection(simpleJoin.apply(mB_tmStep, mB_prep, filter));
// print(innerJoin, 'inner join');

// convert to imageCollection again... 
var  mB_final = innerJoin.map(function(feature) {
  return ee.Image.cat(feature.get('primary'), feature.get('secondary'));
});

// print(mB_final, 'final image Collection');
// print(mB_final.first().bandNames(), 'bandnames - first year of combined');

/*
0: classification_1995_10y
1: classification_1995_forest_10y
2: classification_1995_nonForest_10y
3: classification_1995_agri_10y
4: classification_1995_30k_10y
5: classification_1995_forest_30k_10y
6: classification_1995_nonForest_30k_10y
7: classification_1995_agri_30k_10y
8: classification_1995_10k_10y
9: classification_1995_forest_10k_10y
10: classification_1995_nonForest_10k_10y
11: classification_1995_agri_10k_10y
12: classification_1995
13: classification_1995_forest
14: classification_1995_nonForest
15: classification_1995_agri
16: classification_1995_30k
17: classification_1995_forest_30k
18: classification_1995_nonForest_30k
19: classification_1995_agri_30k
20: classification_1995_10k
21: classification_1995_forest_10k
22: classification_1995_nonForest_10k
23: classification_1995_agri_10k
*/

// Set year property
var setID3 = function(im){
  var yr = im.bandNames().get(0);
  yr = ee.String(yr).slice(15, 19);
  return ee.Image(im).set('system:year', ee.Number(yr))};

mB_tmStep = mB_tmStep.map(setID3);
print(mB_tmStep, 'final image Collection');


// ONLY Necessary if doing sum above in reducer and using pixels, but I'm using proportions directly.
// Final make proportion, so divide by 333^2 and 501^2 each 10k and 30k raster

/*
// Check on map
// Center the Map.
Map.setCenter(-52, -11, 5);

// set display parameters
var kelly_cols = ['F2F3F4', '222222', 'F3C300', '875692', 'F38400', 'A1CAF1', 'BE0032', 'C2B280', '848482', '008856', 'E68FAC', '0067A5', 'F99379', '604E97', 'F6A600', 'B3446C', 'DCD300', '882D17', '8DB600', '654522', 'E25822', '2B3D26'];
var visParams = {min: 0, max: 22, palette: kelly_cols};
var visRC = {min:0, max:1, palette: ['b5b5b5', '25890c']}; 
var visProp = {min:0, max:1, palette: ['#fee8c8','#fdbb84','#e34a33']};

// Display the image.
// Map.addLayer(mB.select('classification_1995'), visParams, 'mapbiomas_c5_1985');
// Map.addLayer(mB.select('classification_2019'), visParams, 'mapbiomas_c5_2019');

// Map.addLayer(occ, {}, 'occurrence points');

Map.addLayer(mB_final.first().select('classification_1995_forest'), visRC, 'forest RC 1995');
// Map.addLayer(mB_final.first().select('classification_1995_nonForest'), visRC, 'non forest RC 1995');
// Map.addLayer(mB_final.first().select('classification_1995_agri'), visRC, 'agri RC 1995');

Map.addLayer(mB_final.first().select('classification_1995_forest_30k'), visProp, 'forest 30k 1995');
Map.addLayer(mB_final.first().select('classification_1995_forest_10k'), visProp, 'forest 10k 1995');

Map.addLayer(mB_final.first().select('classification_1995_forest_10y'), {min:-1, max:1}, 'forest 1995 - 10y');

Map.addLayer(aoi, {}, 'study area',true,0.3);
*/

// EXPORT 

// remove these layers for all years as not needed for export or for point value extraction
// 'classification_1995'
// 'classification_1995_10y'
// 'classification_1995_10k'
// 'classification_1995_10k_10y'
// 'classification_1995_30k'
// 'classification_1995_30k_10y'

// make list of these names for all years

// GET image ids - 
var do_rmID = function(z, strList){
  strList = ee.List(strList);
  z = ee.String(ee.Number(z).int());
  z = ee.String('classification_').cat(z); // /cat() can't take a list... so need to do indiviudally? must be better way, but anyawy...
  strList = strList.add(z);
  var z1 = z.cat('_10y');
  strList = strList.add(z1);
  var z2 = z.cat('_10k');
  strList = strList.add(z2);
  var z3 = z.cat('_30k');
  strList = strList.add(z3);
  var z4 = z.cat('_10k_10y');
  strList = strList.add(z4);
  var z5 = z.cat('_30k_10y');
  strList = strList.add(z5);
  return strList;
  };
  

var rmIDs = ee.List.sequence(1995, 2019, 1).iterate(do_rmID, ee.List([]));
print(rmIDs);

// remove these bands from the collection, 
// var exp = mB_final.toList(24,0);

var rmBands = function(image){
  
  var bNames = image.bandNames();
  bNames = bNames.removeAll(rmIDs);
  image = image.select(bNames);
  return(image);
  
};

// remove bands and convert collection to float 32
var exp = mB_final.map(rmBands)
                  .map(function(im) {return(im.toFloat())})
                  .map(function(im) {return(im.clip(aoi))});

/*
// try small raster to gauge size.
var exp = mB_final.first().select(  'classification_1995_forest',
                                    'classification_1995_forest_30k',
                                    'classification_1995_forest_10y',
                                    'classification_1995_forest_30k_10y'
                                    );

// change to same data type for export - 32bit float
exp = exp.toFloat();
*/

print(exp, 'export');

/*
Export.image.toDrive({
  image: exp.first(),
  description: 'mB_final_1k_1995_trial_clip',
  scale: 1000,
  crs: 'EPSG:5880',
  region: aoi,
  fileFormat: 'GeoTIFF',
  maxPixels: 250000000000,
  folder: 'HyacinthMacaw'
});
*/

// divide into groups for export

var exp1 = ee.ImageCollection(exp.toList(6, 0)).toBands(); // OJO renaming adds band_1 etc to each.. 
var exp2 = ee.ImageCollection(exp.toList(6, 6)).toBands();
var exp3 = ee.ImageCollection(exp.toList(6, 12)).toBands();
var exp4 = ee.ImageCollection(exp.toList(7, 18)).toBands();

print(exp1, 'exp1');
print(exp2, 'exp2');
print(exp3, 'exp3');
print(exp4, 'exp4');

Export.image.toDrive({
  image: exp1,
  description: 'mB_final_1k_1995_2000',
  scale: 1000,
  crs: 'EPSG:5880',
  region: aoi_exp, // same aoi as prjoect aoi in sirgas
  fileFormat: 'GeoTIFF',
  maxPixels: 250000000000,
  folder: 'HyacinthMacaw'
});


Export.image.toDrive({
  image: exp2,
  description: 'mB_final_1k_2001_2006',
  scale: 1000,
  crs: 'EPSG:5880',
  region: aoi_exp,
  fileFormat: 'GeoTIFF',
  maxPixels: 250000000000,
  folder: 'HyacinthMacaw'
});


Export.image.toDrive({
  image: exp3,
  description: 'mB_final_1k_2007_2012',
  scale: 1000,
  crs: 'EPSG:5880',
  region: aoi_exp,
  fileFormat: 'GeoTIFF',
  maxPixels: 250000000000,
  folder: 'HyacinthMacaw'
});


Export.image.toDrive({
  image: exp4,
  description: 'mB_final_1k_2013_2019',
  scale: 1000,
  crs: 'EPSG:5880',
  region: aoi_exp,
  fileFormat: 'GeoTIFF',
  maxPixels: 250000000000,
  folder: 'HyacinthMacaw'
});
