
let oppPolarity = (x, y) => x != y && Js.String.toLowerCase(x) == Js.String.toLowerCase(y);

let rec takeOutOppString= (index, line) => {
   if (index + 2 >= Js.String.length(line)) {
       line;
   } else {
       if (oppPolarity(Js.String.get(line, index), Js.String.get(line, index + 1))) {
           let idx = index - 1 < 0 ? 0 : index - 1;
           takeOutOppString(idx, Js.String.substring(~from=0,~to_=index,line) ++ Js.String.substringToEnd(~from=index+2,line))
       } else {
           takeOutOppString(index+1, line);
       }
   }
}
let rec range = (i,j) => i > j ? [] : [i,...(range(i+1, j))];
let charRange = (x,y) => Belt.List.map(range(Char.code(x), Char.code(y)), Js.String.fromCharCode);
let allAlphaRange = charRange('a','z');
let input = Node.Fs.readFileAsUtf8Sync(Belt.Array.getExn(Node.Process.argv,2));
let part1 = (anInput) => anInput |> takeOutOppString(0) |> Js.String.length
let rec replaceAllCaseInsensitive = (stringToReplace, replaceWith, index, aString) => {
    if (index >= Js.String.length(aString)) {
        aString;
    } else {
        if (Js.String.toLowerCase(Js.String.get(aString,index)) == Js.String.toLowerCase(stringToReplace)) {
            replaceAllCaseInsensitive(stringToReplace, replaceWith, index , Js.String.substring(~from=0,~to_=index, aString) ++ replaceWith ++ Js.String.substringToEnd(~from=index+1, aString));
        } else {
            replaceAllCaseInsensitive(stringToReplace, replaceWith, index + 1, aString);
        };
    }
}
Js.log("Day 5 Part 1 = " ++ string_of_int(input |> part1));
let removeCharAndCount = (alpha) => {
    let interStr = input |> replaceAllCaseInsensitive(alpha,"",0);
    interStr |> part1
};
let part2 = (charArrRange) =>  Belt.List.map(charArrRange, removeCharAndCount) |> Belt.List.toArray |> Js.Array.sortInPlaceWith(compare) |> Js.Array.shift |> Belt.Option.getExn |> string_of_int
Js.log("Day 5 Part 2 = " ++ (allAlphaRange |> part2));