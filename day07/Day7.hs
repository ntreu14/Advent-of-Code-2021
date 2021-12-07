module Day7 where

input = [1101,1,29,67,1102,0,1,65,1008,65,35,66,1005,66,28,1,67,65,20,4,0,1001,65,1,65,1106,0,8,99,35,67,101,99,105,32,110,39,101,115,116,32,112,97,115,32,117,110,101,32,105,110,116,99,111,100,101,32,112,114,111,103,114,97,109,10,1425,266,740,842,335,1076,1125,108,728,131,553,757,316,361,475,1058,555,157,37,1501,287,61,22,394,886,535,235,734,1381,428,200,838,84,0,99,397,516,1260,1079,457,685,669,85,1161,851,1413,207,125,23,396,1024,637,712,942,320,507,32,686,1073,449,736,619,120,1092,674,769,519,26,42,366,187,261,389,583,170,700,695,531,57,263,1058,755,1215,413,201,617,311,443,694,285,677,722,1262,934,790,31,272,410,129,22,186,49,1040,399,19,624,132,1,35,515,423,1039,128,963,254,152,1306,33,360,484,463,483,254,741,284,14,155,6,16,1053,36,1299,637,985,470,476,383,717,304,31,209,263,70,1196,2,283,470,45,20,226,249,654,692,107,31,123,131,42,36,469,249,74,703,798,195,126,1699,135,143,1028,180,33,248,4,118,22,783,721,1033,1250,779,213,241,170,1026,0,124,709,672,349,286,494,134,361,938,985,539,267,240,951,496,431,449,242,804,422,24,202,76,947,414,396,681,142,366,342,256,978,373,677,1471,187,307,579,437,17,779,81,1380,241,69,61,758,1290,98,514,275,510,1427,185,139,816,1401,105,74,978,544,248,413,0,45,1107,223,332,723,745,71,70,330,727,261,1223,914,16,980,306,331,1011,132,70,1735,281,993,976,1,370,280,502,41,644,213,1191,518,464,693,446,44,930,1,23,1412,219,722,1028,84,552,1261,601,433,538,728,385,9,346,212,1017,7,80,88,336,480,1264,219,750,0,1080,711,1095,849,1270,175,20,314,452,620,1283,81,57,193,392,79,1330,220,396,184,922,921,902,199,56,107,32,67,275,91,202,49,4,312,372,262,49,172,493,1473,989,70,373,941,1116,798,709,865,105,442,555,1616,74,402,703,439,120,262,442,1704,1459,195,237,1763,376,734,28,867,370,6,1080,548,750,391,367,123,324,221,453,131,516,586,72,57,185,1667,468,439,225,1407,663,12,355,1320,595,60,59,158,279,365,670,505,14,240,1299,337,128,615,823,576,823,890,284,1196,717,955,1282,1002,20,176,32,222,33,248,634,885,703,543,368,585,1151,110,124,41,475,958,252,99,30,620,793,1021,540,154,635,1194,420,54,33,452,797,157,576,86,116,842,94,98,0,1162,38,483,138,949,316,1248,79,249,40,234,698,275,1239,573,649,815,348,48,78,1039,276,12,261,317,638,304,20,184,1152,711,1673,917,40,244,655,268,151,41,851,79,242,788,611,300,27,141,635,274,330,900,1023,498,269,267,46,436,844,1228,38,142,467,192,399,86,87,645,792,405,844,108,487,356,1251,332,146,128,383,1123,145,0,1148,688,127,316,579,15,215,293,73,1648,599,432,155,317,1054,205,155,451,1411,291,104,536,719,35,25,24,62,747,702,224,971,107,1210,114,41,472,29,286,4,920,0,197,135,112,308,191,1017,438,206,239,6,11,69,945,248,274,397,50,173,80,1349,268,585,590,1071,1127,351,929,106,989,396,209,691,17,149,1001,354,1296,473,179,152,141,1049,376,590,196,27,656,67,275,153,916,849,27,1093,73,156,30,1206,276,623,395,38,760,33,222,371,489,246,309,385,498,517,748,1384,1203,465,360,237,763,1173,94,431,48,770,491,132,564,84,472,1804,57,59,187,351,1340,265,1099,36,199,60,608,148,1209,1142,231,268,254,105,1020,200,1202,661,225,1313,55,808,770,80,522,185,129,36,476,815,1424,534,583,285,15,21,607,722,242,33,299,672,1253,1078,142,285,417,461,261,310,296,1934,271,144,1572,155,1039,881,1097,18,226,45,789,213,309,32,603,1102,5,81,511,672,314,7,1471,104,196,875,286,4,198,472,549,613,1453,139,596,270,164,417,709,437,27,86,758,1365,216,38,1047,124,96,255,72,67,1372,143,120,502,276,922,89,231,491,1330,245,473,25,944,266,1475,569,215,484,73,264,214,608,423,333,879,251,300,32,18,514,135,1349,80,493,569,784,2,794,846,596,30,862,318,207,546,551,1548,547,181,1219,354,650,791,53,20,629,52,105,98,312,140,111,1451,973,11,17,821,724,1836,376,82,248,86,730,1061,47,309,142,1039,114,157,26,307,1058,803,723,105,170,59,239,181,601,79,564,671,636,1465,530,533,75,261,1522,537,96,984,71,504,572,923,85,103,567,780,102,4,835,463,684,427,1091,1104,1163,626,1015,395,1881,43,490,906,1013,398,113,95,332,215,14,8,85,92,1579]

stepPart1 crabs position = sum $ map step crabs
  where
    step crab = abs (crab - position)

stepPart2 crabs position = sum $ map step crabs
  where 
    step crab =
      let diff = abs (crab - position)
      in
        diff * (diff + 1) `div` 2 -- O(1) formula for summing numbers 1 to n

minMove moveFn crabs = minimum $ map (moveFn crabs) [1..length crabs]

main = do
  -- Part 1
  print $ minMove stepPart1 input
  
  -- Part 2
  print $ minMove stepPart2 input 