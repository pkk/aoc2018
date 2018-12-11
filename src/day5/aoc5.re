
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
let filePath = Belt.Array.getExn(Node.Process.argv, 2);
let part1 = 
Node.Fs.readFileAsUtf8Sync(filePath)
|> takeOutOppString(0)
|> Js.String.length

Js.log(part1);