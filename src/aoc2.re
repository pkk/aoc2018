/*
--- Part Two ---
You notice that the device repeats the same frequency change list over and over. To calibrate the device, you need to find the first frequency it reaches twice.

For example, using the same list of changes above, the device would loop as follows:

Current frequency  0, change of +1; resulting frequency  1.
Current frequency  1, change of -2; resulting frequency -1.
Current frequency -1, change of +3; resulting frequency  2.
Current frequency  2, change of +1; resulting frequency  3.
(At this point, the device continues from the start of the list.)
Current frequency  3, change of +1; resulting frequency  4.
Current frequency  4, change of -2; resulting frequency  2, which has already been seen.
In this example, the first frequency reached twice is 2. Note that your device might need to repeat its list of frequency changes many times before a duplicate frequency is found, and that duplicates might be found while in the middle of processing the list.

Here are other examples:

+1, -1 first reaches 0 twice.
+3, +3, +4, -2, -4 first reaches 10 twice.
-6, +3, +8, +5, -6 first reaches 5 twice.
+7, +7, -2, -7, -4 first reaches 14 twice.
What is the first frequency your device reaches twice?
*/
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