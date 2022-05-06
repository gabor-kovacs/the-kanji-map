import * as d3 from "d3";

function drag(simulation) {
  function dragstarted(event) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    event.subject.fx = event.subject.x;
    event.subject.fy = event.subject.y;
  }

  function dragged(event) {
    event.subject.fx = event.x;
    event.subject.fy = event.y;
  }

  function dragended(event) {
    if (!event.active) simulation.alphaTarget(0);
    event.subject.fx = null;
    event.subject.fy = null;
  }

  return d3
    .drag()
    .on("start", dragstarted)
    .on("drag", dragged)
    .on("end", dragended);
}

function ticked(link, node) {
  link
    .attr("x1", (d) => d.source.x)
    .attr("y1", (d) => d.source.y)
    .attr("x2", (d) => d.target.x)
    .attr("y2", (d) => d.target.y);

  // container.attr("cx", (d) => d.x).attr("cy", (d) => d.y);
  node.attr("transform", (d) => {
    return "translate(" + d.x + "," + d.y + ")";
  });
}

export function forceGraph(
  {
    nodes, // an iterable of node objects (typically [{id}, …])
    links, // an iterable of link objects (typically [{source, target}, …])
  },
  {
    svgEl,
    nodeId = (d) => d.id, // given d in nodes, returns a unique identifier (string)
    nodeGroup, // given d in nodes, returns an (ordinal) value for color
    nodeGroups, // an array of ordinal values representing the node groups
    nodeTitle, // given d in nodes, a title string
    nodeFill = "currentColor", // node stroke fill (if not using a group color encoding)
    nodeStroke = "#fff", // node stroke color
    nodeStrokeWidth = 1.5, // node stroke width, in pixels
    nodeStrokeOpacity = 1, // node stroke opacity
    nodeRadius = 20, // node radius, in pixels
    nodeStrength,
    linkSource = ({ source }) => source, // given d in links, returns a node identifier string
    linkTarget = ({ target }) => target, // given d in links, returns a node identifier string
    linkStroke = "#999", // link stroke color
    linkStrokeOpacity = 0.6, // link stroke opacity
    linkStrokeWidth = 1.5, // given d in links, returns a stroke width in pixels
    linkStrokeLinecap = "round", // link stroke linecap
    linkStrength,
    linkDistance = 100,
    colors = d3.schemeTableau10, // an array of color strings, for the node groups
    width = 640, // outer width, in pixels
    height = 400, // outer height, in pixels
    invalidation, // when this promise resolves, stop the simulation
  } = {}
) {
  // Compute values.
  const N = d3.map(nodes, nodeId).map(intern);
  const LS = d3.map(links, linkSource).map(intern);
  const LT = d3.map(links, linkTarget).map(intern);
  if (nodeTitle === undefined) nodeTitle = (_, i) => N[i];
  const T = nodeTitle == null ? null : d3.map(nodes, nodeTitle);
  const G = nodeGroup == null ? null : d3.map(nodes, nodeGroup).map(intern);
  const W =
    typeof linkStrokeWidth !== "function"
      ? null
      : d3.map(links, linkStrokeWidth);

  // Replace the input nodes and links with mutable objects for the simulation.
  nodes = d3.map(nodes, (_, i) => ({ id: N[i], group: G[i] }));
  links = d3.map(links, (_, i) => ({ source: LS[i], target: LT[i] }));

  // Compute default domains.
  if (G && nodeGroups === undefined) nodeGroups = d3.sort(G);

  // Construct the scales.
  const color = nodeGroup == null ? null : d3.scaleOrdinal(nodeGroups, colors);

  // Construct the forces.
  const forceNode = d3.forceManyBody();
  const forceLink = d3.forceLink(links).id(({ index: i }) => N[i]);
  const forceCollide = d3.forceCollide((d) => (2.5 * nodeRadius) / d.group);
  if (nodeStrength !== undefined) forceNode.strength(nodeStrength);
  if (linkStrength !== undefined) forceLink.strength(linkStrength);
  forceLink.distance(linkDistance);

  const simulation = d3
    .forceSimulation(nodes)
    .force("link", forceLink)
    .force("charge", forceNode)
    .force("collide", forceCollide)
    .force("center", d3.forceCenter());
  // .on("tick", ticked);

  const svg = d3
    .select(svgEl)
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", [-width / 2, -height / 2, width, height])
    .attr("style", "width: 100%; height: auto; height: intrinsic;");

  const link = svg
    .append("g")
    .attr("stroke", linkStroke)
    .attr("stroke-opacity", linkStrokeOpacity)
    .attr(
      "stroke-width",
      typeof linkStrokeWidth !== "function" ? linkStrokeWidth : null
    )
    .attr("stroke-linecap", linkStrokeLinecap)
    .selectAll("line")
    .data(links)
    .join("line");

  const container = svg
    .append("g")
    .selectAll("g")
    .data(nodes)
    .enter()
    .append("g")
    .call(drag(simulation));

  const node = container
    .append("circle")
    .attr("fill", nodeFill)
    .attr("stroke", nodeStroke)
    .attr("stroke-opacity", nodeStrokeOpacity)
    .attr("stroke-width", nodeStrokeWidth)
    .attr("r", (d) => (2.5 * nodeRadius) / d.group);

  const label = container
    .append("text")
    // .attr("font-size", d => (1.3 * nodeRadius) / d.group)
    .attr("text-anchor", "middle")
    .attr("cursor", "default")
    .attr("x", 0)
    .attr("dy", (d) => (2.5 * nodeRadius) / d.group + nodeRadius)
    .style("fill", "#000")
    .text((d) => d.id);

  if (W) link.attr("stroke-width", ({ index: i }) => W[i]);
  if (G) node.attr("fill", ({ index: i }) => color(G[i]));
  if (T) node.append("title").text(({ index: i }) => T[i]);
  if (invalidation != null) invalidation.then(() => simulation.stop());

  function intern(value) {
    return value !== null && typeof value === "object"
      ? value.valueOf()
      : value;
  }

  simulation.on("tick", () => ticked(link, container));

  return Object.assign(svg.node(), { scales: { color } });
}

let defaultRadius = 20;

// handling circle size based on node hierarchy
const depthToRadius = (depth) => {
  return (2.5 * defaultRadius) / (depth + 1);
};

// handling node colors based on node hierarchy
const mapColors = (depth) => {
  const colors = d3.schemeTableau10;
  return colors[depth + 1];
};

export function forceDirectedGraph(data, element) {
  const root = d3.hierarchy(data);
  const links = root.links();
  const nodes = root.descendants();

  const simulation = d3
    .forceSimulation(nodes)
    .force(
      "link",
      d3
        .forceLink(links)
        .id((d) => d.id)
        .distance(80)
        .strength(1)
    )
    .force(
      "collide",
      d3.forceCollide((d) => depthToRadius(d.depth) + 3)
    )
    .force("charge", d3.forceManyBody().strength(-400))
    .force("center", d3.forceCenter())
    .force("x", d3.forceX())
    .force("y", d3.forceY());

  let width = 400;
  let height = 400;

  const svg = d3
    .select(element)
    .attr("viewBox", [-width / 2, -height / 2, width, height])
    .style("width", "100%")
    .style("height", "auto");

  const link = svg
    .append("g")
    .attr("stroke", "#999")
    .attr("stroke-opacity", 0.6)
    .selectAll("line")
    .data(links)
    .join("line");

  // Wrapper for circle and text label
  const nodesContainer = svg
    .append("g")
    .selectAll("g")
    .data(nodes)
    .enter()
    .append("g")
    .call(drag(simulation));

  // Adding circles
  nodesContainer
    .append("circle")
    .attr("fill", (d) => mapColors(d.depth))
    .attr("stroke", (d) => (d.children ? "#000" : "#fff"))
    .attr("stroke-width", 1.5)
    .attr("r", (d) => depthToRadius(d.depth));

  // Adding Labels
  nodesContainer
    .append("text")
    .attr("font-size", (d) => (d.index === 0 ? 10 : d.children ? 8 : 6))
    .attr("font-weight", 900)
    .attr("text-anchor", "middle")
    .attr("cursor", "default")
    .attr("x", 0)
    .attr("dy", ".35em")
    .style("fill", (d) => (d.children ? "#ddd" : "#000"))
    .text((d) => d.data.name);

  nodesContainer.append("title").text((d) => d.data.name);

  simulation.on("tick", () => ticked(link, nodesContainer));

  return svg.node();
}
