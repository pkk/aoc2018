let calcFreq = (accm, line) => {
    if (Js.String.indexOf("+", line) > -1) {
        accm + int_of_string(Js.String.substr(~from=1,line));

    } else {
        accm + int_of_string(line);
    }
};
let lines = 
    Node.Fs.readFileAsUtf8Sync("./src/day1/input1.txt") 
|> Js.String.split("\n")
let res= Belt.Array.reduce(lines, 0, calcFreq);
Js.log(res);