let lines = 
    Node.Fs.readFileAsUtf8Sync("./src/day1/input1.txt") 
|> Js.String.split("\n")

let resultsTable = Belt.HashSet.Int.make(~hintSize=Belt.Array.length(lines));

let rec findDuplicate = (allLines: array(string), num: int, accm: int) : bool => {
    let line = switch (Belt.Array.get(allLines, num)) {
    | Some(value) => value;
    | None => "";
    };
    let res = if (Js.String.indexOf("+", line) > -1) {
        accm + int_of_string(Js.String.substr(~from=1,line));

    } else {
        accm + int_of_string(line);
    }; 
    if (Belt.HashSet.Int.has(resultsTable, res)) {
        Js.log2("Found res", res);
        true;
    } else {
        Belt.HashSet.Int.add(resultsTable, res);
        let nextNum = if (num + 1 == Belt.Array.length(allLines)) {
            0;
        } else {
            num + 1;
        }
        findDuplicate(allLines, nextNum, res);
    }
};
findDuplicate(lines,0, 0);