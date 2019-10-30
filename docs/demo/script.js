var tau = {session: pl.create()};

var src_path = "../../src/";
handleFile(src_path + "8puzzle.pl");

// upload file source
function handleFile(fileName) {
    var match;
    fetch(fileName).then(response => response.text()).then(text => {
        tau.code = ":- use_module(library(lists)).\n" + text;
        var regex = /:-\s*consult\((?:'|")(\S+)(?:'|")\)\./;
        match = tau.code.match(regex);
        var src = "";
        if (match.length > 1) src = src_path + match[1];
        return fetch(src);
    }).then(response => response.text()).then(text => {
        tau.code = tau.code.replace(match[0], text);
        tau.session.consult(tau.code);
    }).catch(error => {
        tau.session.consult(tau.code);
    });
}

// buttons
const newPuzzle = document.getElementById('new-puzzle');
const moveBtn = document.getElementById('move');
const backBtn = document.getElementById('back');

// initialize puzzle
var grid = new Grid('grid');
var path;
var step;
newPuzzle.addEventListener("click", e => {
    let puzzle = [1,2,3, 4,5,0, 7,8,6]; //testing..should be random
    path = getPath(puzzle);
    grid.data = puzzle;
    moveBtn.removeAttribute("style");
    backBtn.removeAttribute("style");
    moveBtn.removeAttribute("disabled");
    backBtn.setAttribute("disabled", "");
    step = 0;
});

function getPath(puzzle) {
    var varName = 'P';
    var query = 'naive_solve([' + puzzle.join(', ') + '], ' + varName + ').';
    tau.session.query(query);
    var path;
    tau.session.answer(x => path = x.links[varName].toJavaScript());
    return path;
}

// do steps
moveBtn.addEventListener("click", e => {
    if (step < path.length) {
        grid.data = move(path[step]);
        step++;
        backBtn.removeAttribute("disabled");
    }
    if (step >= path.length) {
        moveBtn.setAttribute("disabled", "");
    }
});

backBtn.addEventListener("click", e => {
    if (step > 0) {
        step--;
        grid.data = back(path[step]);
    }
    if (step == 0) {
        backBtn.setAttribute("disabled", "");
        moveBtn.removeAttribute("disabled");
    }
});

function move(direction) {
    var varName = 'S';
    var query = 'move([' + grid.data.join(', ') + '], ' + varName + ', ' + direction + ').';
    tau.session.query(query);
    var state;
    tau.session.answer(x => state = x.links[varName].toJavaScript());
    return state;
}

function back(direction) {
    var varName = 'S';
    var query = 'move(' + varName + ', [' + grid.data.join(', ') + '], ' + direction + ').';
    tau.session.query(query);
    var state;
    tau.session.answer(x => state = x.links[varName].toJavaScript());
    return state;
}