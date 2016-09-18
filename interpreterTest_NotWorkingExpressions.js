var notWorking = [ for (x of [0])  if (x < 10) for( y of [0])  x ];

var notWorking2 = [ for (x of [0])  if (x < 10) if( x === 0)  x ];