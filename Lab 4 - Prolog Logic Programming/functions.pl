/* 
Davie Truong
dtruong8@ucsc.edu
1525861
*/

not( X ) :- X, !, fail.
not( _ ). 

/* 
Convert degrees and minutes of arc to radians
*/
degminToRadian( degmin(Degree, Minute), Radian) :-
   Radian is (Degree + Minute / 60) * pi / 180.

/* 
Haversine Distance Formula used to Compute distance between airports 
(Does not account for radian, currently accepts any lat/lon)
*/
haversineDistance(Lat1, Lon1, Lat2, Lon2, Miles) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   Temp is (sin( Dlat/2 )) ** 2 + cos( Lat1 ) * cos( Lat2 ) * (sin( Dlon/2 )) ** 2,
   UnitDistance is 2 * atan2(sqrt(Temp), sqrt(1 - Temp)),
   Miles is 3961 * UnitDistance.

/*
Gets the correct Distance between two flights 
Calls the degminToRadian on two flights to get the radian of the lat/lon to
run it on the haversineDistance function
*/
distance(Airport1, Airport2, Miles) :-
   airport(Airport1, _, DegminLat1, DegminLon1),
   airport(Airport2, _, DegminLat2, DegminLon2),
   degminToRadian(DegminLat1, Lat1Radian),
   degminToRadian(DegminLon1, Lon1Radian),
   degminToRadian(DegminLat2, Lat2Radian),
   degminToRadian(DegminLon2, Lon2Radian),
   haversineDistance(Lat1Radian, Lon1Radian, Lat2Radian, Lon2Radian, Miles).

/*
Conditionl to print the time in correct format making sure there is always XX digit 
*/
printTime(Number) :-
   Number >= 10,
   print(Number).
printTime(Number) :-
   Number < 10,
   print(0),
   print(Number).

/*
Prints the Arivial Time from Departure Time
*/
printArivalTime(Duration) :-
   TotalMinutes is ceiling(Duration * 60),
   Hours is TotalMinutes // 60,
   Minutes is TotalMinutes mod 60,
   printTime(Hours),
   print(':'),
   printTime(Minutes).

/*
Get the time of the trip from the miles given
*/
convertMilesToHours(Miles, Hours) :-
   Hours is Miles / 500.

/*
Convert Hours and Mintues to only Hours for simpler time calculations
*/
makeHours( time(Hours, Minutes), TotalTimeInHours) :-
   TotalTimeInHours is Hours + Minutes / 60.

/*
Determines the flight path of the two destinations
*/
/*If the flight has arrived*/
getPath(EndLoc, EndLoc, _, [EndLoc], _).

/*If there is a flight from Start directly to Final*/
getPath(StartLoc, EndLoc, VisitedList, [[StartLoc, DepartureTimeInHours, ArrivalTime] | List], DepartureTime) :-
   flight(StartLoc, EndLoc, DepartureTime),    /*Does the flight path exist in the database?*/
   not(member(EndLoc, VisitedList)),     /*Did we visit EndLoc already?*/
   distance(StartLoc, EndLoc, TripInMiles),   /*How far is the trip?*/
   convertMilesToHours(TripInMiles, TravelTime),  /*Number of hours for the flight travel*/
   makeHours(DepartureTime, DepartureTimeInHours),       /*Convert DepartureTime to Hours*/
   ArrivalTime is DepartureTimeInHours + TravelTime, /*Calculate the final arrival time in Hours*/
   ArrivalTime < 24.0,     /*Can we make the flight within the day*/
   getPath(EndLoc, EndLoc, [EndLoc | VisitedList], List, _). /*We reached destination, Parse Data*/

/*Find Connecting Flights*/
getPath(StartLoc, EndLoc, VisitedList, [[StartLoc, DepartureTimeInHours, ArrivalTime] | List], DepartureTime) :-
   flight(StartLoc, MiddleLoc, DepartureTime),  /*Does the flight path exist in the database?*/
   not(member(MiddleLoc, VisitedList)),         /*Did we visit MiddleLoc already?*/
   distance(StartLoc, MiddleLoc, TripInMiles),  /*How far is the trip?*/
   convertMilesToHours(TripInMiles, TravelTime),   /*Number of hours for the flight travel*/
   makeHours(DepartureTime, DepartureTimeInHours),  /*Convert DepartureTime to Hours*/
   ArrivalTime is DepartureTimeInHours + TravelTime,   /*Calculate the arrival time in Hours*/
   ArrivalTime < 24.0,                          /*Can we continue to fly?*/
   flight(MiddleLoc, _, NextDepartureTime),     /*Find next Flight*/
   makeHours(NextDepartureTime, NextDepartureTimeInHours),  /*Convert DepartureTime to Hours*/
   (ArrivalTime + 0.5) =< NextDepartureTimeInHours,   /*Connecting Flights must be 30 min after arrival*/
   getPath(MiddleLoc, EndLoc, [MiddleLoc | VisitedList], List, NextDepartureTime).  /*Call again with new data*/

/*Output the data of the flight to the screen*/
/*Writes the path for when there is no connecting flights*/
writePath([[StartShortName, StartDepatureTime, EndArrivalTime], EndShortName | []]) :- /*Gets Data from the inputlist*/
   airport(StartShortName, StartFullName, _, _),                  /*Gets Name of the airport*/
   airport(EndShortName, EndFullName, _, _),                      /*Gets Name of the airport*/
   format('depart  %s  %s ', [StartShortName, StartFullName]),    /*Prints the Data*/
   printArivalTime(StartDepatureTime), nl,
   format('arrive  %s  %s ', [EndShortName, EndFullName]),
   printArivalTime(EndArrivalTime), nl.

/*Writes the path for when there are connecting flights*/
writePath([[StartShortName, StartDepatureTime, EndArrivalTime],               /*Gets Data from the inputlist*/
   [NextShortName, NextDepartureTime, NextArrivalTime] | NextFlight]) :-
   airport(StartShortName, StartFullName, _, _),                     /*Gets Name of the airport*/
   airport(NextShortName, EndFullName, _, _),                         /*Gets Name of the airport*/
   format('depart  %s  %s ', [StartShortName, StartFullName]),       /*Prints the Data*/
   printArivalTime(StartDepatureTime), nl,
   format('arrive  %s  %s ', [NextShortName, EndFullName]),
   printArivalTime(EndArrivalTime), nl,
   writePath([[NextShortName, NextDepartureTime, NextArrivalTime] | NextFlight]).   /*Recusive call with Next flight data*/

/*The main function that calls everything above to work*/
/*Check If same input for the two locations*/
fly(StartName, StartName) :-
   format('Error: Cannot Fly To Same Location %s', [StartName]), nl, !, fail.

/*Flying with correct inputs*/
fly(StartName, EndName) :-
   airport(StartName, _, _, _),
   airport(EndName, _, _, _),
   getPath(StartName, EndName, [StartName], PathList, _), !, nl,
   writePath(PathList),
   true.

/*Flight Not found*/
fly(StartName, EndName) :-
   airport(StartName, _, _, _),
   airport(EndName, _, _, _),
   format('Error: Flight %s to %s does not exist, or the flight cannot be completed in 24 hours', 
   [StartName, EndName]), nl, !, fail.

fly(_, _) :-
   write('Error: Input Does Not Exist.\n'), !, fail.