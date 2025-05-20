library(readr)
data <- "state, abbreviation, st, decriminalization
Alabama, AL, 01, 0
Alaska, AK, 02, 1
Arizona, AZ, 04, 1
Arkansas, AR, 05, 0
California, CA, 06, 1
Colorado, CO, 08, 1
Connecticut, CT, 09, 1
Delaware, DE, 10, 1
District of Columbia, DC, 11, 1
Florida, FL, 12, 0
Georgia, GA, 13, 0
Hawaii, HI, 15, 1
Idaho, ID, 16, 0
Illinois, IL, 17, 1
Indiana, IN, 18, 0
Iowa, IA, 19, 0
Kansas, KS, 20, 0
Kentucky, KY, 21, 0
Louisiana, LA, 22, 0
Maine, ME, 23, 1
Maryland, MD, 24, 1
Massachusetts, MA, 25, 1
Michigan, MI, 26, 1
Minnesota, MN, 27, 1
Mississippi, MS, 28, 1
Missouri, MO, 29, 1
Montana, MT, 30, 1
Nebraska, NE, 31, 1
Nevada, NV, 32, 1
New Hampshire, NH, 33, 0
New Jersey, NJ, 34, 1
New Mexico, NM, 35, 1
New York, NY, 36, 1
North Carolina, NC, 37, 0
North Dakota, ND, 38, 1
Ohio, OH, 39, 1
Oklahoma, OK, 40, 0
Oregon, OR, 41, 1
Pennsylvania, PA, 42, 0
Rhode Island, RI, 44, 1
South Carolina, SC, 45, 0
South Dakota, SD, 46, 0
Tennessee, TN, 47, 0
Texas, TX, 48, 0
Utah, UT, 49, 0
Vermont, VT, 50, 1
Virginia, VA, 51, 1
Washington, WA, 53, 1
West Virginia, WV, 54, 0
Wisconsin, WI, 55, 0
Wyoming, WY, 56, 0"

state_cannabis <- read_csv(data)
