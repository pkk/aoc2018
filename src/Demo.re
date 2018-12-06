Js.log("hhhh");
let b = [|[|1,2,3|], [|4,5,6|], [|7,8,9|]|];
let a = Belt.Array.makeBy(3, (_) => (Belt.Array.make(3,0)));
Js.log(a);
let nRow = Belt.Array.getExn(a, 1);
Js.log(nRow);
Belt.Array.setExn(nRow, 2, 7);
Js.log(nRow);
Belt.Array.setExn(a, 1, nRow)
Js.log(a);