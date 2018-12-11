type twoAndThrees = {
    mutable twos: int,

    mutable threes: int
};

let lines = 
    Node.Fs.readFileAsUtf8Sync(Node.Fs.readFileAsUtf8Sync(Belt.Array.getExn(Node.Process.argv,2))) 
    |> Js.String.split("\n");

let makeTwosAndThrees = (charMap: Belt.HashMap.String.t(int)) => {
    let res = {twos: 0, threes: 0};
    let addToResult = (_, count) => {
        if (count == 2 && res.twos == 0) {
            res.twos = res.twos + 1;
        } else if (count ==3 && res.threes == 0) {
            res.threes = res.threes + 1;
        };
    };
    Belt.HashMap.String.forEach(charMap, addToResult);
    res;
};

let mapTwosAndThrees(line: string): twoAndThrees{
    let charMap = Belt.HashMap.String.make(~hintSize=10);
    let rec helper = (line, charMap) => {
        if (Js.String.length(line) == 0) {
            makeTwosAndThrees(charMap);
        } else {
            let c = Js.String.charAt(0, line);
            let count = switch (Belt.HashMap.String.get(charMap, c)) {
            | Some(x) => x;
            | _ => 0;
            };
            Belt.HashMap.String.set(charMap, c, count + 1);
            helper(Js.String.substr(~from=1, line), charMap);
        };
    };
    helper(line, charMap);
};

let twosAndThreesArr = Belt.Array.map(lines, mapTwosAndThrees);
let finalTowsAndThrees = Belt.Array.reduce(twosAndThreesArr, {twos:0, threes:0}, (a,b) => {
    {
        twos: a.twos + b.twos,
        threes: a.threes + b.threes
    };
});
Js.log(finalTowsAndThrees.threes * finalTowsAndThrees.twos);
let sortedLines = Belt.SortArray.String.stableSort(lines);
let rec compareStrings = (s1,s2,diff) => {
    if (diff == 1 && Js.String.length(s1) == 0) {
        true;
    } else if (diff > 1) {
        false;
    } else {
        let nDiff = diff + (Js.String.charAt(0,s1) === Js.String.charAt(0,s2) ? 0 : 1);
        compareStrings(Js.String.substr(~from=1,s1), Js.String.substr(~from=1,s2), nDiff);
    };
};
let rec findMostSimilar = (lines:array(string), len: int): bool => {
    if (len == Belt.Array.length(lines) - 1) {
        false;
    } else {
        let curStr = Belt.Option.getWithDefault(Belt.Array.get(lines, len),"");
        let nextStr = Belt.Option.getWithDefault(Belt.Array.get(lines, len + 1),"");
        if (compareStrings(curStr, nextStr,0)) {
            Js.log(curStr);
            Js.log(nextStr);
            true;
        } else {
            findMostSimilar(lines, len + 1);
        };
    };
};
findMostSimilar(sortedLines,0);
