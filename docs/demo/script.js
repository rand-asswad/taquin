var tau = {session: pl.create()};

handleFile("main.pl");

// upload file source
function handleFile(fileName) {
    fetch(fileName).then(response => response.text()).then(text => {
        tau.code = text;
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
    let puzzle = [0,1,3, 4,2,5, 7,8,6]; //testing..should be random
    //let puzzle = randomPuzzle();
    path = getPath(puzzle);
    grid.data = puzzle;
    moveBtn.removeAttribute("style");
    backBtn.removeAttribute("style");
    moveBtn.removeAttribute("disabled");
    backBtn.setAttribute("disabled", "");
    step = 0;
});

function randomPuzzle() {
    var query = 'goal(P).';
    tau.session.query(query);
    var goal;
    tau.session.answer(x => path = x.links.P.toJavaScript());
    return shuffle(goal);
}

function getPath(puzzle) {
    var varName = 'P';
    var query = 'solve([' + puzzle.join(', ') + '], ' + varName + ').';
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

function shuffle(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;

    // While there remain elements to shuffle...
    while (0 !== currentIndex) {

        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array;
}