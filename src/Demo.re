Js.log("hhhh");
let dateStr = "1518-11-04 00:05";
let yearStr = Js.String.substring(~from=0,~to_=4, dateStr);
let monthStr = Js.String.substring(~from=5,~to_=7, dateStr);
let dayStr = Js.String.substring(~from=8,~to_=10, dateStr);
let hourStr = Js.String.substring(~from=11,~to_=13, dateStr);
let minStr = Js.String.substringToEnd(~from=14,dateStr);
let dt = Js.Date.utcWithYMDHM(~year= float_of_string(yearStr),
                               ~month=float_of_string(monthStr),
                               ~date=float_of_string(dayStr),
                               ~hours=float_of_string(hourStr),
                               ~minutes=float_of_string(minStr), ());
Js.log(Js.Date.fromFloat(dt));
let aStr = "[1518-04-28 23:59] Guard #1021 begins shift";
let d = Belt.Array.range(10,15);

Js.log(d);

let rec range = (i,j) => i > j ? [] : [i,...(range(i+1, j))];
let charRange = (x: char,y: char): list(string) => Belt.List.map(range(Char.code(x), Char.code(y)), Js.String.fromCharCode);
let stringRage = charRange('a','z');

let oppString = (x,y) => x != y && Js.String.toLowerCase(x) == Js.String.toLowerCase(y);
Js.log(Js.String.replace("a", "b","aabb"));
Js.String.replaceByRe(Js.Re.fromString(""), "", "aabb");
Js.log(Js.String.substring(~from=0,~to_=2, "absba") ++ "" ++ Js.String.substringToEnd(~from=2+1,"absba"));