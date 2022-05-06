import { useEffect, useRef } from "react";
import { forceGraph } from "../lib/d3ForceGraph";

const FirstChart = ({ data }) => {
  let svgRef = useRef(null);

  useEffect(() => {
    forceGraph(data, {
      svgEl: svgRef.current,
      nodeId: (d) => d.id,
      nodeGroup: (d) => d.group,
      nodeTitle: (d) => `${d.id}\n${d.group}`,
      nodeStrength: -200,
      linkStrokeWidth: (l) => Math.sqrt(l.value),
      height: 600,
    });
  }, []);

  return <svg ref={svgRef} style={{ background: "#ddd" }} />;
};

export default FirstChart;
