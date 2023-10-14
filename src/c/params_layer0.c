#include "../../include/network.h"

double weights_layer0[16][30] = {{-0.13286117,0.038061853,0.2926125,-0.24049497,0.27757543,0.27134743,0.17145564,-0.23078561,-0.11318296,-0.0899038,-0.2600084,-0.18552692,0.34415036,0.35850185,0.26553792,-0.030042192,-0.23348929,0.09776852,0.07744686,-0.05903489,-0.2398278,-0.19741467,-0.09089929,-0.17370428,-0.26452288,-0.14896046,-0.318825,-0.25949362,0.1415747,0.40012825},
{0.32923475,0.31032607,0.16894326,0.011607292,-0.3261223,-0.16546713,0.022478873,-0.23569821,-0.12920527,-0.04651267,-0.47864795,0.036573473,-0.2048791,-0.34349403,-0.62700814,0.41524312,-0.32843307,-0.41487756,-0.3401842,-0.19945905,-0.35991278,-0.23377909,-0.020702418,-0.5674816,-0.49938315,-0.11129338,-0.01970232,-0.19241945,-0.22092168,0.28439826},
{0.25338858,-0.3127665,-0.27149853,-0.40051055,-0.38188437,0.30554053,0.12912303,0.10918987,0.10694899,0.12798314,-0.05739583,-0.13988996,0.06964645,-0.040632743,-0.2579029,0.25130132,0.43206266,-0.10452519,0.03456291,0.30972776,0.21057725,-0.5297356,-0.08105001,0.068441376,-0.3814565,-0.13873023,0.20063524,-0.20047839,-0.15308803,-0.30267224},
{-0.10626992,0.1512299,-0.35876846,-0.26271123,0.19987124,-0.27712995,-0.33156925,-0.1275509,-0.015465095,0.36148503,-0.45459527,-0.17126134,0.017419066,-0.38536412,0.17946297,0.3445426,-0.36065096,-0.07290558,0.19671965,0.23233445,-0.1380105,-0.3170172,-0.531208,-0.54265857,-0.10049084,0.26647657,-0.3160723,-0.3401752,0.2089031,-0.010847155},
{-0.048739757,-0.2603767,-0.31634918,-0.2245733,0.36622882,-0.6769333,-0.30434433,-0.34586468,-0.32576373,0.0565074,0.705246,-0.07330822,0.16115543,0.44679374,-0.3907024,-0.33300522,0.034312513,-0.11990615,-0.4184131,-0.23687834,0.37578523,0.35783195,0.025353309,-0.064446904,0.27500007,-0.14273359,0.07551661,-0.019531125,0.19611934,0.023318978},
{-0.34571826,-0.28392896,-0.48062152,0.1289167,-0.418971,-0.15049309,-0.018094685,0.04923076,0.24035743,0.3415951,-0.017056208,0.10718333,-0.570915,0.065232515,0.28781813,0.52895916,0.022298474,0.14876346,0.007381605,0.014556911,-0.4862213,-0.11861788,-0.3643854,0.06782225,-0.6100633,-0.22590718,-0.07238342,-0.41148221,-0.39525235,0.12833174},
{0.16548268,0.42037392,0.37123895,0.24179304,0.26121435,-0.063848,0.57393974,0.21230422,-0.011268252,-0.5103075,0.12951167,-0.12666036,0.42987645,0.046774074,-0.18775077,-0.17288822,0.050424024,-0.31340876,0.094304875,-0.34621587,0.07262291,0.08389923,0.3769975,0.13699605,0.5999872,-0.30274087,0.5207666,0.44348997,-0.061811358,0.20990255},
{-0.12217795,-0.06284081,0.2222718,0.3106469,-0.01992984,0.11240142,0.4805217,0.40012375,-0.123239525,-0.12798491,0.45703837,-0.070504874,-0.17854786,-0.074121945,0.21986109,-0.08596116,-0.19169481,-0.028564112,0.23555566,-0.042127937,-0.14708133,0.40952954,0.20326898,0.048343137,0.4710713,-0.010293542,0.054089125,0.3544851,0.20271197,0.3300584},
{0.0021237344,0.2925802,-0.23810287,-0.35783848,0.14002259,0.1582003,-0.4808022,-0.5392546,-0.31128773,0.3758267,-0.13150737,0.16342936,-0.3805138,-0.5992916,0.3354826,0.13348548,-0.25763577,-0.06379393,0.11323064,0.08623959,-0.0006996938,-0.1991676,0.11153616,-0.5070199,0.12824199,0.42599896,-0.22482176,-0.5628328,-0.11061555,-0.24120228},
{0.049020603,0.100941725,0.36814886,0.10316306,-0.26802477,-0.41435596,0.18481405,0.34265274,0.19482033,0.20975374,0.4566386,-0.23965569,-0.10739632,0.054996558,-0.13297468,-0.30116966,-0.17659764,-0.1921244,-0.55859935,-0.09994173,0.43457592,0.38582492,0.035109844,0.21691023,0.34885687,0.0567619,0.026783602,0.013876943,0.2637516,0.2929822},
{0.03535358,0.38923067,0.47012722,0.20788465,-0.0063615823,0.12394936,0.22465496,0.47406074,0.042338323,-0.33095014,0.16806054,-0.09674454,0.03189453,0.0013306725,0.08000911,0.22037557,0.16863425,0.29643944,0.18890008,-0.23998821,0.042920433,-0.044240795,0.5153428,0.044324547,0.5253685,-0.1087254,-0.035380654,0.083179295,0.37854365,0.1048009},
{-0.15614983,0.35042724,0.045259498,0.1905309,0.09976799,0.21386409,-0.019050786,0.20637693,-0.23503727,-0.25663182,0.5263713,-0.38527954,0.3394673,0.19252962,0.21246406,0.0091738915,0.20042232,0.2657129,-0.40545586,-0.5321634,0.16189907,0.17881337,0.05313995,0.4318322,0.50584775,0.36138496,0.30756536,0.060678005,-0.03483612,0.22638035},
{0.07203637,-0.50382626,0.08009284,0.05762292,0.30684626,0.19101776,-0.050486624,0.08004844,-0.2885049,-0.21648881,-0.47431237,-0.2858541,-0.07001652,-0.2855516,0.07752983,0.34453237,-0.10080657,-0.15903394,-0.19490896,-0.036609646,-0.122324206,-0.57291794,-0.44912714,-0.07265653,0.24541532,0.20917435,-0.24634218,-0.20492278,-0.048303142,-0.08201241},
{-0.12021479,-0.34514153,-0.047838103,-0.23818386,-0.14633046,0.15417802,-0.19392444,0.21013781,-0.19873767,0.10398156,0.13174061,0.17836465,-0.020810967,-0.2243565,-0.21991538,-0.2925117,0.19937906,0.05010183,-0.24680382,0.043352064,-0.12078341,-0.128479,0.1388267,-0.08614259,0.25580963,-0.095717065,0.09134251,-0.15420781,0.2660983,0.1681744},
{0.17590863,0.105136044,0.20262529,-0.13887438,-0.39707515,-0.07036349,0.14444388,0.0726534,-0.060459524,0.006497817,0.19500168,-0.23850144,-0.19790386,0.13296099,0.06192504,0.17016861,-0.26261294,-0.20773035,-0.3092438,-0.1318907,0.07308202,0.12039638,-0.25104085,0.04474043,0.21352528,0.20498522,-0.2738242,0.12594788,0.24510781,0.053824864},
{0.39682993,0.25156242,0.006834086,-0.19932948,-0.15230866,-0.229371,-0.20347989,0.080733426,0.08685007,-0.20354639,0.28312778,-0.2515069,0.09018541,-0.17673665,0.101962686,0.24388696,0.23325975,0.1236407,0.19035245,-0.073432416,-0.13351849,-0.23765658,0.051315807,-0.15320379,-0.27573228,0.19680269,0.11040089,-0.05131085,-0.2773734,0.019273248}};

double biases_layer0[16] = {-0.005827288,0.3743796,-0.01089534,0.3139195,0.073384896,0.30773762,0.27045417,0.19096601,0.13131897,0.233602,0.16778208,0.22492167,0.32171395,-0.037178516,-0.13694689,-0.08003925};

