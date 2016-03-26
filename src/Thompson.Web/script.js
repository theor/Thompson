var eps = "\u03b5";

var d3 = window.d3;
var dagreD3 = window.dagreD3;
//var graph = {
//    start: 0,
//    ends: [3],
//    transitions: [
//      { from: 0, to: 3 },
//      { from: 0, to: 1 },
//      { from: 1, to: 2, label: "a" },
//      { from: 2, to: 3 },
//      { from: 2, to: 1 }
//    ]
//};



function draw(graph, svgid) {
    if (graph === null)
        return;
    // Set up zoom support
    var svg = d3.select(svgid),
        inner = svg.select("g"),
        zoom = d3.behavior.zoom().on("zoom", function () {
            inner.attr("transform", "translate(" + d3.event.translate + ")" +
                                        "scale(" + d3.event.scale + ")");
        });
    svg.call(zoom);
    var render = new dagreD3.render();

    // Left-to-right layout
    var g = new dagreD3.graphlib.Graph();
    g.setGraph({
        nodesep: 70,
        ranksep: 50,
        rankdir: "LR",
        marginx: 20,
        marginy: 20
    });
    //console.clear();
    //d3.json("/data.json", function(error, graph) {
    var t = graph.transitions;
    var nodes = new Set();
    for (var id in t) {
        nodes.add(t[id].from);
        nodes.add(t[id].to);
        console.log(t[id].from + " -> " + t[id].to);
    }
    for(let node of nodes) {
        var className = /* worker.consumers ? */"running"/* : "stopped"*/;
        var html = "<div>";
        html += "<span class=status></span>";
    //html += "<span class=consumers>" + node + "</span>";
        html += "<span class=name>" + node + "</span>";
    //html += "<span class=queue><span class=counter>" + node + "</span></span>";
        html += "</div>";
        g.setNode(node, {
            labelType: "html",
            label: html,
            rx: 5,
            ry: 5,
            padding: 0,
            class: className
        });
    }
    for (var tid in graph.transitions) {
        var t = graph.transitions[tid];
        g.setEdge(t.from, t.to, {
            label: t.label || eps,
            width: 40
        });
    }
    inner.call(render, g);
    fit(g, svgid, zoom);
    //});
}
function fit(g, id, zoom) {
    // Zoom and scale to fit
    var svg = d3.select(id);
    var graphWidth = g.graph().width + 80;
    var graphHeight = g.graph().height + 40;
    var width = parseInt(svg.style("width").replace(/px/, ""));
    var height = parseInt(svg.style("height").replace(/px/, ""));
    var zoomScale = Math.min(width / graphWidth, height / graphHeight);
    var translate = [(width / 2) - ((graphWidth * zoomScale) / 2), (height / 2) - ((graphHeight * zoomScale) / 2)];
    zoom.translate(translate);
    zoom.scale(zoomScale);
    let isUpdate = false;
    zoom.event(isUpdate ? svg.transition().duration(500) : svg);


}

//draw();

function sendRegex() {
    var input = document.getElementById("inputRegex");
    console.log("send", input.value);
    d3.json("/regex")
      .post(input.value, function (error, graph) {
          console.log("graph", graph);
          draw(graph.nfa, "#svg");
          draw(graph.dfa, "#svg2");
      });
    return false;
}
