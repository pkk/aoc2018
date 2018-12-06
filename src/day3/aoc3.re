type claimT = {
    leftEdge: int,
    topEdge: int,
    width: int,
    height: int
};
let lines = 
    Node.Fs.readFileAsUtf8Sync("./src/day3/input1.txt") 
    |> Js.String.split("\n");

let claimArea = Belt.Array.makeBy(1000, (_) => Belt.Array.make(1000,0));

let fillRows = (claim: claimT, arrs: array(array(int))) : array(array(int)) => {
    let rec eachRow = (rStart: int, claim: claimT, arrs: array(array(int))): array(array(int)) => {
        let rec eachColumn = (cStart: int, row: int, claim: claimT, currentRow: array(int)): array(int) => {
            if (cStart == claim.topEdge + claim.height) {
                currentRow;
            } else {
                let count = Belt.Array.getExn(currentRow, cStart);
                Belt.Array.setExn(currentRow, cStart, count + 1);
                eachColumn(cStart + 1, row, claim, currentRow);
            };
        };
        if (rStart == claim.leftEdge + claim.width) {
            arrs;
        } else {
            let newRow = eachColumn(claim.topEdge, rStart, claim, Belt.Array.getExn(arrs, rStart));
            Belt.Array.setExn(arrs, rStart, newRow);
            eachRow(rStart+1, claim, arrs);
        };
    };
    eachRow(claim.leftEdge, claim, arrs);
};


let rec getMaxInches = (lines: array(string), claimArea: array(array(int))) => {
    if (Belt.Array.length(lines) == 0) {
        claimArea;
    } else {
        let line = Belt.Array.getExn(lines, 0);
        let leftEdge = int_of_string(Js.String.substring(~from=Js.String.indexOf("@", line)+2,~to_=Js.String.indexOf(",", line),line));
        let topEdge = int_of_string(Js.String.substring(~from=Js.String.indexOf(",", line)+1,~to_=Js.String.indexOf(":", line),line));
        let width = int_of_string(Js.String.substring(~from=Js.String.indexOf(":", line) + 2, ~to_=Js.String.indexOf("x", line), line));
        let height = int_of_string(Js.String.substringToEnd(~from=Js.String.indexOf("x", line) + 1,line));
        let aClaim = {
            leftEdge : leftEdge,
            topEdge: topEdge,
            width: width,
            height: height
        };
        getMaxInches(Belt.Array.sliceToEnd(lines,1), fillRows(aClaim, claimArea));
    };
};
let countArea(claimArea: array(array(int))) = {
    let rec eachRow = (start, limit, claimArea, res) => {
        let rec eachCol = (start, row, limit, claimArea, res) => {
            if (start == limit) {
                res;
            } else {
                let count = Belt.Array.getExn(Belt.Array.getExn(claimArea, row), start) > 1 ? 1 : 0;
                eachCol(start + 1, row, limit, claimArea, res + count);
            }
        };
        if (start == limit) {
            res;
        } else {
            eachRow(start + 1, limit, claimArea, eachCol(0, start, Belt.Array.size(Belt.Array.getExn(claimArea,0)), claimArea, res));
        };
    };
    eachRow(0, Belt.Array.size(Belt.Array.getExn(claimArea,0)), claimArea, 0);
};
let hasAllOnes = (claim, claimArea) => {
    let rec getRow = (start, claim, claimArea) => {
        let rec checkRow = (colm, row, claim) => {
            if (colm == claim.topEdge + claim.height) {
                true;
            } else {
                if (Belt.Array.getExn(row, colm) == 1) {
                    checkRow(colm + 1, row, claim);
                } else {
                    false;
                };
            };
        };
        if (start == claim.leftEdge + claim.width) {
            true;
        } else {
            let currentRow = Belt.Array.getExn(claimArea, start);
            if (checkRow(claim.topEdge, currentRow, claim)) {
                getRow(start+1, claim, claimArea);
            } else {
                false;
            };
        };
    };
    getRow(claim.leftEdge, claim,claimArea);
};
let rec partTwo = (lines: array(string), claimArea: array(array(int))) => {
    if (Belt.Array.length(lines) == 0) {
        "None Found"
    } else {
        let line = Belt.Array.getExn(lines, 0);
        let leftEdge = int_of_string(Js.String.substring(~from=Js.String.indexOf("@", line)+2,~to_=Js.String.indexOf(",", line),line));
        let topEdge = int_of_string(Js.String.substring(~from=Js.String.indexOf(",", line)+1,~to_=Js.String.indexOf(":", line),line));
        let width = int_of_string(Js.String.substring(~from=Js.String.indexOf(":", line) + 2, ~to_=Js.String.indexOf("x", line), line));
        let height = int_of_string(Js.String.substringToEnd(~from=Js.String.indexOf("x", line) + 1,line));
        let aClaim = {
            leftEdge : leftEdge,
            topEdge: topEdge,
            width: width,
            height: height
        };
        if (hasAllOnes(aClaim, claimArea)) {
            Js.String.substring(~from=Js.String.indexOf("#", line) + 1, ~to_=Js.String.indexOf("@", line) - 1, line);
        } else {
            partTwo(Belt.Array.sliceToEnd(lines, 1), claimArea);
        }
    };
};
let filledClaimArea = getMaxInches(lines, claimArea);
Js.log("part1 = " ++ string_of_int(countArea(filledClaimArea)));
let newClaimArea = getMaxInches(lines, Belt.Array.makeBy(1000, (_) => Belt.Array.make(1000,0)));
Js.log("part2 = " ++ partTwo(lines, newClaimArea));