type action = | Guard(int) | Asleep(int) | Awake(int)

type timeSheetT = { id: int, totalSleep: int, minMap: Belt.HashMap.Int.t(int) };

type mapValT = {min: int, count:int};

let parseActions = (line)  => {
  let timeUnit = line |> Js.String.substring(~from=15,~to_=17) |> int_of_string;
  if (Js.String.indexOf("Guard", line) > -1) {
    Guard(line |> Js.String.substring(~from=Js.String.indexOf("#", line) + 1,~to_=Js.String.indexOf("begins", line) - 1) |> int_of_string);
  } else if (Js.String.indexOf("falls asleep", line) > -1) {
    Asleep(timeUnit)
  } else {
    Awake(timeUnit);
  };
};

let processNap = (sTime, eTime, timeSheetMap, currentGuard) => {
  let minsToFill = Belt.Array.range(sTime, eTime - 1);
  let timeSheet = switch (Belt.HashMap.Int.get(timeSheetMap, currentGuard)) {
  | Some(x) => x
  | _ => {id: currentGuard, totalSleep:0, minMap: Belt.HashMap.Int.make(~hintSize=60)}
  };
  Belt.Array.forEach(minsToFill, (x) => {
    Belt.HashMap.Int.set(timeSheet.minMap, x, Belt.Option.getWithDefault(Belt.HashMap.Int.get(timeSheet.minMap, x),0)+1);
  });
  Belt.HashMap.Int.set(timeSheetMap, currentGuard, {id: timeSheet.id, totalSleep: timeSheet.totalSleep + eTime - sTime, minMap: timeSheet.minMap});
};

let rec makeTimeSheetMaps = (currentGuard: int, startMin: int, endMin: int, arr: action, timeSheetMap: Belt.HashMap.Int.t(timeSheetT), arrs: list(action)) => {
  switch (arrs) {
  | [] => {
    processNap(startMin, endMin == -1 ? 59 : endMin, timeSheetMap, currentGuard);
    timeSheetMap;
  };
  | [first, ...rest] => {
      switch(first) {
      | Guard(x) => {
        if (currentGuard != -1) {
          processNap(startMin, endMin == -1 ? 60 : endMin, timeSheetMap, currentGuard);
        };
        makeTimeSheetMaps(x, -1, -1, first, timeSheetMap, rest);
      };
      | Asleep(x) => {
        if (endMin != -1) {
          processNap(startMin, endMin == -1 ? 60 : endMin, timeSheetMap, currentGuard);
        }
        makeTimeSheetMaps(currentGuard, x, -1, first, timeSheetMap, rest);
      };
      | Awake(x) => makeTimeSheetMaps(currentGuard, startMin, x, first, timeSheetMap, rest);
      };
    };
  };
}

let sortedTimeSheetArray = 
    Node.Fs.readFileAsUtf8Sync("./src/day4/input2.txt") 
    |> Js.String.split("\n")
    |> ArrayLabels.to_list
    |> ListLabels.sort(~cmp=compare)
    |> ListLabels.map(~f=parseActions)
    |> makeTimeSheetMaps(-1,-1,-1, Guard(-1), Belt.HashMap.Int.make(~hintSize=10))
    |> Belt.HashMap.Int.valuesToArray
    |> Js.Array.sortInPlaceWith((a,b) => compare(a.totalSleep, b.totalSleep))

let maxTimeSheet = Belt.Array.getExn(sortedTimeSheetArray, Js.Array.length(sortedTimeSheetArray) - 1);
let maxMinCount = Belt.HashMap.Int.reduce(maxTimeSheet.minMap, {min:0,count:0},(a,k,v) => a.count > v ? a : {min:k, count:v});
Js.log(string_of_int(maxTimeSheet.id * maxMinCount.min));