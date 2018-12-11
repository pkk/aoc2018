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

let s1 = "a";
let s2 = "A";
let s3 = "a";
Js.log(s1 == s3? "equal": "notequal");
let charArr = Js.String.split("","abcd");
Js.log(charArr);