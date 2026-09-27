"use client";

import kanjilist from "@/../data/kanjilist.json";
import { buildKanjiHref, type MobileTabKey } from "@/lib/kanji-routing";
import { escapeHtml } from "@/lib/utils";
import {
  NODE_SELECTED,
  NODE_JOYO,
  NODE_JINMEIYO,
  NODE_OTHER,
} from "@/lib/graph-colors";
import { useTheme } from "next-themes";
import { useRouter } from "next/navigation";
import * as React from "react";
import type { ForceGraphMethods, GraphData } from "react-force-graph-3d";
import ForceGraph3D, { LinkObject, NodeObject } from "react-force-graph-3d";
import type { RectReadOnly } from "react-use-measure";
import * as THREE from "three";
import SpriteText from "three-spritetext";

type NodeObjectWithData = NodeObject & { data: GraphNodeData | null };

interface Props {
  kanjiInfo: KanjiInfo;
  graphData: BothGraphData | null;
  showOutLinks: boolean;
  triggerFocus: number;
  bounds: RectReadOnly;
  autoRotate: boolean;
  showParticles: boolean;
  navigationTab?: MobileTabKey;
  enableNodePreview?: boolean;
  onPreviewNode?: (node: GraphNode) => void;
  onClosePreview?: () => void;
  onWebglBroken?: () => void;
}

const KANJI_SPRITE_OFFSET_Y = 2.0;

const NODE_RADIUS = 8;

const NODE_GEOMETRY = new THREE.SphereGeometry(NODE_RADIUS, 32, 32);

// Never frame the camera closer than this, so tiny graphs (a single node or
// a couple of links) stay at a comfortable viewing distance.
const MIN_FIT_DISTANCE = 120;

// How many consecutive watchdog rebuilds are allowed before the component
// gives up and asks its parent to fall back to the 2D view. Without the cap
// a permanently broken (or unavailable) WebGL context would rebuild the
// engine forever, once every few seconds, each rebuild allocating a fresh
// WebGL context.
const MAX_WEBGL_REBUILD_ATTEMPTS = 3;

const KANJI_GROUPS = kanjilist.reduce(
  (groups, entry) => {
    if (entry.g === 1) {
      groups.joyo.add(entry.k);
    } else if (entry.g === 2) {
      groups.jinmeiyo.add(entry.k);
    }

    return groups;
  },
  {
    joyo: new Set<string>(),
    jinmeiyo: new Set<string>(),
  },
);

const getNodeDefaultColor = (nodeId: string, selectedId: string) => {
  if (nodeId === selectedId) {
    return NODE_SELECTED;
  }
  if (KANJI_GROUPS.joyo.has(String(nodeId))) {
    return NODE_JOYO;
  }
  if (KANJI_GROUPS.jinmeiyo.has(String(nodeId))) {
    return NODE_JINMEIYO;
  }

  return NODE_OTHER;
};

const resetNodeColor = (node: any, selectedId: string) => {
  const defaultColor = getNodeDefaultColor(node.id, selectedId);
  if (node?.__threeObj?.children[1]?.material?.color) {
    node.__threeObj.children[1].material.color.set(defaultColor);
  }
};

const highlightNode = (node: any) => {
  if (node?.__threeObj?.children[1]?.material?.color) {
    const color = node.__threeObj.children[1].material.color;
    node.__threeObj.children[1].material.color.setRGB(
      color.r * 0.8,
      color.g * 0.8,
      color.b * 0.8,
    );
  }
};

const getLinkDirectionalArrowRelPos = ({ source, target }: LinkObject) => {
  if (
    typeof source === "object" &&
    typeof target === "object" &&
    source.x &&
    target.x &&
    source.y &&
    target.y &&
    source.z &&
    target.z
  ) {
    const linkLength = Math.hypot(
      target.x - source.x,
      target.y - source.y,
      target.z - source.z,
    );
    if (!linkLength) {
      return 0.8;
    }

    // Clamp: while the layout is still settling, short links would
    // otherwise place the arrow head outside the line
    return Math.max(0, Math.min(1, (linkLength - 8) / linkLength));
  }

  return 0.8;
};

// Captures the current camera framing so it can be restored when the
// engine is rebuilt.
const captureEngineCamera = (
  engine: ForceGraphMethods,
): { pos: THREE.Vector3; target: THREE.Vector3 } | null => {
  try {
    const camera = engine.camera();
    const controls = engine.controls() as
      | { target: THREE.Vector3 }
      | undefined;
    if (camera?.position && controls?.target) {
      return {
        pos: camera.position.clone(),
        target: controls.target.clone(),
      };
    }
  } catch {
    // Engine not fully ready.
  }
  return null;
};

// Frames the whole graph: move the orbit target to the node bounding-box
// center and pull the camera back along its current viewing direction until
// the box fits the frustum. Unlike the library's zoomToFit, the target is
// the actual bbox center (the layout drifts off origin) and the distance has
// a floor, so both tiny and sprawling graphs frame sensibly.
const fitGraphCamera = (engine: ForceGraphMethods, nodes: NodeObject[]) => {
  let minX = Infinity;
  let minY = Infinity;
  let minZ = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;
  let maxZ = -Infinity;
  for (const node of nodes) {
    const x = node.x;
    const y = node.y;
    const z = node.z;
    // Skip nodes the layout has not placed yet (or that went NaN).
    if (
      x === undefined ||
      y === undefined ||
      z === undefined ||
      !Number.isFinite(x) ||
      !Number.isFinite(y) ||
      !Number.isFinite(z)
    ) {
      continue;
    }
    minX = Math.min(minX, x);
    maxX = Math.max(maxX, x);
    minY = Math.min(minY, y);
    maxY = Math.max(maxY, y);
    minZ = Math.min(minZ, z);
    maxZ = Math.max(maxZ, z);
  }
  if (!Number.isFinite(minX)) {
    return; // no placed nodes yet
  }
  const camera = engine.camera() as THREE.PerspectiveCamera | undefined;
  const controls = engine.controls() as
    | { target: THREE.Vector3 }
    | undefined;
  if (!camera || !controls) {
    return;
  }
  const center = new THREE.Vector3(
    (minX + maxX) / 2,
    (minY + maxY) / 2,
    (minZ + maxZ) / 2,
  );
  const radius =
    Math.hypot(maxX - minX, maxY - minY, maxZ - minZ) / 2 + NODE_RADIUS;
  const fovY = ((camera.fov ?? 75) * Math.PI) / 180;
  const halfFovY = fovY / 2;
  const aspect =
    Number.isFinite(camera.aspect) && camera.aspect > 0 ? camera.aspect : 1;
  const halfFovX = Math.atan(Math.tan(halfFovY) * aspect);
  const distance = Math.max(
    radius / Math.tan(Math.min(halfFovY, halfFovX)),
    MIN_FIT_DISTANCE,
  );
  const dir = camera.position.clone().sub(controls.target);
  if (dir.lengthSq() === 0) {
    dir.set(0, 0, 1);
  }
  dir.normalize().multiplyScalar(distance);
  engine.cameraPosition(
    {
      x: center.x + dir.x,
      y: center.y + dir.y,
      z: center.z + dir.z,
    },
    { x: center.x, y: center.y, z: center.z },
    500,
  );
};

const getNodeLabel = (n: NodeObject, enableNodePreview: boolean) => {
  if (enableNodePreview) {
    return "";
  }

  const node = n as NodeObjectWithData;
  if (!node.data) {
    return "";
  }

  const kunyomi = node.data.kunyomi.join(", ");
  const meaning = node.data.meaning;
  if (!kunyomi && !meaning) {
    return "";
  }

  return `<div style="color: #ffffff; background: #000000a6; padding: 4px; border-radius: 4px;">
            <span>${escapeHtml(kunyomi)}</span>
            <br/>
            <span>${escapeHtml(meaning)}</span>
          </div>
         `;
};

const createNodeThreeObject = (node: NodeObject, selectedId: string) => {
  const ball = new THREE.Mesh(
    NODE_GEOMETRY,
    new THREE.MeshLambertMaterial({
      color: getNodeDefaultColor(String(node.id), selectedId),
      transparent: true,
      depthWrite: false,
      opacity: 0.8,
    }),
  );

  const sprite = new SpriteText(String(node.id));
  sprite.fontFace =
    "Iowan Old Style, Apple Garamond, Baskerville, Times New Roman, Droid Serif, Times, Source Serif Pro, serif";
  sprite.color = "#000";
  sprite.textHeight = 10;
  sprite.fontSize = 120;
  sprite.padding = 3;
  sprite.offsetY = KANJI_SPRITE_OFFSET_Y;

  const group = new THREE.Group();
  group.add(sprite);
  group.add(ball);
  return group;
};

const createLinkThreeObject = (
  linkLabel: string,
  resolvedTheme: string | undefined,
) => {
  if (!linkLabel) {
    return null;
  }

  const sprite = new SpriteText(linkLabel);
  sprite.color = resolvedTheme === "dark" ? "#ffffff" : "#000000";
  sprite.textHeight = 6;
  return sprite;
};

const updateLinkPosition = (
  sprite: THREE.Object3D | SpriteText | null | undefined,
  { start, end }: { start: { x: number; y: number; z: number }; end: { x: number; y: number; z: number } },
) => {
  const middlePos = {
    x: start.x + (end.x - start.x) / 2,
    y: start.y + (end.y - start.y) / 2,
    z: start.z + (end.z - start.z) / 2,
  };

  sprite?.position && Object.assign(sprite.position, middlePos);
  return null;
};

const Graph3D = ({
  kanjiInfo,
  graphData,
  showOutLinks,
  triggerFocus,
  bounds,
  autoRotate,
  showParticles,
  navigationTab,
  enableNodePreview = false,
  onPreviewNode,
  onClosePreview,
  onWebglBroken,
}: Props) => {
  const { resolvedTheme } = useTheme();
  const { push, prefetch } = useRouter();

  const fg3DRef: React.MutableRefObject<ForceGraphMethods | undefined> =
    React.useRef(undefined);

  // Keep a copy of the most recent engine instance. The visibility effect
  // reads it on document visibilitychange, which can fire after a watchdog
  // remount (a new engine) or an unmount (ref already null), so a plain
  // fg3DRef read would be stale.
  const latestFgRef = React.useRef<ForceGraphMethods | undefined>(undefined);

  // The current graph data, readable from callbacks whose dep arrays
  // deliberately omit it (the camera capture for restoredCameraRef).
  const dataRef = React.useRef<GraphData | null>(null);

  const data = React.useMemo<GraphData | null>(
    () => (showOutLinks ? graphData?.withOutLinks : graphData?.noOutLinks) ?? null,
    [graphData?.noOutLinks, graphData?.withOutLinks, showOutLinks],
  );

  // The engine ref is attached during commit, which the visibility effect
  // (registered once per hasGraph/engineKey) may not have seen yet. Syncing
  // latestFgRef on every commit (no dep array) keeps it current.
  React.useEffect(() => {
    latestFgRef.current = fg3DRef.current;
    dataRef.current = data;
  });

  // The container reports 0x0 before the first layout and while the graph
  // layer is hidden on mobile, so the engine mounts only once it has real
  // dimensions. Tracked as a value (not via the engine ref, which is only
  // assigned during commit) so the mount and fit effects below can tell
  // when the engine appears on a mobile reveal.
  const boundsReady = bounds.width > 0 && bounds.height > 0;

  // Precompute the shared onyomi label for every link once per data change,
  // so rebuilding link objects is a map lookup instead of a node-list scan
  const linkLabelByLink = React.useMemo(() => {
    const onyomiById = new Map<string, string[]>();
    data?.nodes?.forEach((node) => {
      const onyomi = (node as NodeObjectWithData).data?.onyomi;
      if (onyomi?.length) {
        onyomiById.set(String(node.id), onyomi);
      }
    });

    const labels = new Map<LinkObject, string>();
    data?.links?.forEach((link) => {
      const source =
        typeof link.source === "object" ? link.source.id : link.source;
      const target =
        typeof link.target === "object" ? link.target.id : link.target;
      const on1 = onyomiById.get(String(source));
      const on2 = onyomiById.get(String(target));
      const shared =
        on1 && on2 ? on1.filter((value) => on2.includes(value)) : [];
      labels.set(link, shared.join(", "));
    });
    return labels;
  }, [data]);

  const buildNodeHref = React.useCallback(
    (id: string) =>
      buildKanjiHref(id, {
        tab: navigationTab ?? null,
      }),
    [navigationTab],
  );

  const resumeRotateTimeout = React.useRef<ReturnType<typeof setTimeout>>(
    undefined,
  );
  const hasGraph = Boolean(data);
  const [engineKey, setEngineKey] = React.useState(0);
  const lastHealthyAt = React.useRef(0);
  // Consecutive watchdog rebuilds without a healthy frame; the watchdog
  // gives up after MAX_WEBGL_REBUILD_ATTEMPTS and asks the parent to fall
  // back to the 2D view instead of rebuilding a broken context forever.
  const webglFailuresRef = React.useRef(0);
  // Latest onWebglBroken, callable from the watchdog interval without
  // re-arming the effect on every render.
  const onWebglBrokenRef = React.useRef(onWebglBroken);
  onWebglBrokenRef.current = onWebglBroken;
  // Restarts the watchdog after it gave up; assigned by the watchdog effect.
  const rearmWatchdogRef = React.useRef<(() => void) | undefined>(undefined);
  const pausedByObserver = React.useRef(false);
  const hiddenByVisibility = React.useRef(false);
  // Remembers that a webglcontextlost fired on the current engine. The reveal
  // and watchdog handlers rebuild when this is set even if the browser has
  // already auto-restored the context (isContextLost() would read false then).
  const contextLostRef = React.useRef(false);
  // Camera framing captured just before the engine goes away (a rebuild, or
  // the mobile layer hiding it on a tab switch), written back on remount so
  // the user's view survives. Tagged with the data it was captured for, so
  // a data swap while the engine was away makes the remount frame the new
  // graph instead of restoring a framing that belongs to the old one.
  const restoredCameraRef = React.useRef<{
    pos: THREE.Vector3;
    target: THREE.Vector3;
    data: GraphData | null;
  } | null>(null);
  const skipIntroFocusRef = React.useRef(false);
  // The user has taken over the camera since the last fit request; a
  // delayed re-fit must not override their view.
  const userMovedRef = React.useRef(false);
  const lastTriggerFocusRef = React.useRef(triggerFocus);

  // Frames the current graph contents (intro focus and the fit button both
  // go through here).
  const fitRef = React.useRef<() => void>(() => {});
  fitRef.current = () => {
    const engine = fg3DRef.current;
    if (!engine || !kanjiInfo.id || !data?.nodes?.length) {
      return;
    }
    userMovedRef.current = false;
    fitGraphCamera(engine, data.nodes);
  };

  // Reset per-session state when the graph data goes away (a non-kanji route,
  // a 404 clear, or the component unmounts) — not on a mobile tab switch,
  // which only hides the layer (boundsReady) while the data stays.
  React.useEffect(() => {
    if (!hasGraph) return;
    return () => {
      clearTimeout(resumeRotateTimeout.current);
      restoredCameraRef.current = null;
    };
  }, [hasGraph]);

  // Dispose the engine's WebGL context whenever the engine instance goes
  // away. The mobile layer unmounts the engine on every tab switch (bounds
  // collapse to 0x0) and creates a fresh one on reveal, so without this each
  // switch leaks a context into the browser's WebGL pool. The engine is
  // captured at run time (not read from the ref in the cleanup) because this
  // effect re-runs on every mount (boundsReady / engineKey), so the captured
  // instance is always the live one.
  React.useEffect(() => {
    if (!hasGraph || !boundsReady) return;
    const fg = fg3DRef.current;
    if (!fg) return;
    return () => {
      // Remember the framing so a later reveal (mobile tab switch) restores
      // the exact view instead of re-fitting; the mount effect applies it
      // only when the data is unchanged.
      const saved = captureEngineCamera(fg);
      if (saved) {
        restoredCameraRef.current = { ...saved, data: dataRef.current };
      }
      fg.pauseAnimation();
      // Dispose GPU resources and lose the context of the engine that is
      // going away so its resources are released immediately.
      const renderer = fg.renderer();
      renderer.dispose();
      renderer.forceContextLoss();
    };
  }, [hasGraph, boundsReady, engineKey]);

  // The canvas can stay black in two ways: the engine's rAF render loop dies
  // (uncaught error inside its animation cycle, or a context lost while the
  // loop is paused), or the WebGL context is lost (even at birth) while the
  // loop keeps ticking — every render is then a silent no-op. Watch for a
  // missing *healthy* frame (a render while the context is alive) and rebuild
  // the engine from scratch.
  React.useEffect(() => {
    if (!hasGraph || !boundsReady) {
      return;
    }
    const fg = fg3DRef.current;
    if (!fg) {
      return;
    }
    lastHealthyAt.current = performance.now();
    // A freshly mounted engine owns a fresh context; drop any loss latched by
    // a previous instance so it doesn't trigger a spurious rebuild.
    contextLostRef.current = false;

    const saved = restoredCameraRef.current;
    if (saved) {
      restoredCameraRef.current = null;
      // Only restore when the graph is unchanged since the capture. If the
      // data swapped while the engine was away (a kanji navigation on a
      // hidden mobile tab), the old framing belongs to the old graph, so
      // let the fit effect below frame the new one instead.
      if (saved.data === dataRef.current) {
        // The engine was just rebuilt: write the pre-rebuild framing back
        // directly (a zero-duration cameraPosition transition is unreliable)
        // and suppress the intro focus that would run on this mount.
        skipIntroFocusRef.current = true;
        try {
          const camera = fg.camera();
          const controls = fg.controls() as
            | { target: THREE.Vector3; update: () => void }
            | undefined;
          if (camera) {
            camera.position.copy(saved.pos);
          }
          if (controls?.target) {
            controls.target.copy(saved.target);
            controls.update();
          }
        } catch {
          // Engine not fully ready; the watchdog recovers it.
        }
      }
    }

    const renderer = fg.renderer();
    const canvas = renderer.domElement;
    const gl = renderer.getContext();

    // three.js reinitializes its GL state on context restore but does not
    // draw a frame, so force the loop back on to un-black the canvas.
    const onContextRestored = () => {
      fg.resumeAnimation();
    };
    canvas.addEventListener("webglcontextrestored", onContextRestored, false);

    // A lost context renders nothing. If the view is visible, rebuild now
    // instead of waiting out the 5-second watchdog grace; while hidden the
    // reveal handler's isContextLost() check covers the same recovery.
    const onContextLost = (event: Event) => {
      // Take over recovery: stop the browser silently auto-restoring the
      // context, otherwise isContextLost() reads false again before the view
      // is shown and the reveal check can't force a rebuild.
      event.preventDefault();
      contextLostRef.current = true;
      if (pausedByObserver.current || hiddenByVisibility.current) {
        return;
      }
      const saved = captureEngineCamera(fg);
      if (saved) {
        restoredCameraRef.current = { ...saved, data: dataRef.current };
      }
      lastHealthyAt.current = performance.now();
      setEngineKey((key) => key + 1);
    };
    canvas.addEventListener("webglcontextlost", onContextLost, false);

    // OrbitControls dispatches 'start' only for real user gestures (drag,
    // wheel, pinch); programmatic cameraPosition tweens never do. Track it
    // so a delayed re-fit never overrides a view the user has taken over.
    const controls = fg.controls() as
      | {
          addEventListener?: (type: string, listener: () => void) => void;
          removeEventListener?: (type: string, listener: () => void) => void;
        }
      | undefined;
    const onControlsStart = () => {
      userMovedRef.current = true;
    };
    controls?.addEventListener?.("start", onControlsStart);

    const origRender = renderer.render.bind(renderer);
    renderer.render = ((
      scene: THREE.Object3D,
      camera: THREE.Camera,
    ) => {
      // A render into a lost context draws nothing, so only a render into a
      // live context counts as the engine being healthy.
      if (!gl.isContextLost()) {
        lastHealthyAt.current = performance.now();
        // A live render proves the GL state is fine; clear any latched loss
        // and reset the rebuild-failure count.
        contextLostRef.current = false;
        webglFailuresRef.current = 0;
      }
      return origRender(scene, camera);
    }) as typeof renderer.render;

    let watchdog = 0;
    const watchdogTick = () => {
      // A hidden tab stalls the engine's rAF loop, so a missing healthy
      // frame is expected there, not a failure — never rebuild while hidden.
      if (pausedByObserver.current || hiddenByVisibility.current) {
        return;
      }
      // A latched loss is an immediate failure: the render hook only stops
      // marking healthy once a frame lands, so don't wait out the grace.
      if (
        !contextLostRef.current &&
        performance.now() - lastHealthyAt.current < 5000
      ) {
        return;
      }
      if (webglFailuresRef.current >= MAX_WEBGL_REBUILD_ATTEMPTS) {
        // Rebuilding is not helping (WebGL broken or unavailable); stop the
        // loop and let the parent fall back to the 2D view.
        window.clearInterval(watchdog);
        onWebglBrokenRef.current?.();
        return;
      }
      webglFailuresRef.current += 1;
      const saved = captureEngineCamera(fg);
      if (saved) {
        restoredCameraRef.current = { ...saved, data: dataRef.current };
      }
      renderer.dispose();
      renderer.forceContextLoss();
      lastHealthyAt.current = performance.now();
      setEngineKey((key) => key + 1);
    };
    const armWatchdog = () => {
      window.clearInterval(watchdog);
      watchdog = window.setInterval(watchdogTick, 2000);
    };
    armWatchdog();
    // The reveal handler calls this after a cap: reset the failure count
    // and restart the watchdog so the broken engine gets a fresh attempt
    // budget (and another fallback if its rebuilds fail again).
    rearmWatchdogRef.current = () => {
      if (webglFailuresRef.current < MAX_WEBGL_REBUILD_ATTEMPTS) {
        return;
      }
      webglFailuresRef.current = 0;
      armWatchdog();
    };

    return () => {
      canvas.removeEventListener("webglcontextrestored", onContextRestored, false);
      canvas.removeEventListener("webglcontextlost", onContextLost, false);
      controls?.removeEventListener?.("start", onControlsStart);
      renderer.render = origRender;
      rearmWatchdogRef.current = undefined;
      window.clearInterval(watchdog);
    };
    // The engine only mounts once the container reports real dimensions (the
    // bounds gate in the render below) and unmounts when the mobile layer is
    // hidden, so keying on boundsReady arms the watchdog on reveal and clears
    // its interval when the engine goes away.
  }, [hasGraph, boundsReady, engineKey]);

  // Browsers stall rAF in hidden tabs, so while the tab is hidden the engine
  // produces no healthy frames and the watchdog would rebuild it (each
  // rebuild force-losing the WebGL context). Pause the engine while hidden,
  // and on return start a fresh grace period before the watchdog may act.
  React.useEffect(() => {
    if (!hasGraph) {
      return;
    }
    const onVisibilityChange = () => {
      const engine = latestFgRef.current;
      hiddenByVisibility.current = document.hidden;
      // The IntersectionObserver may have already paused/resumed the engine
      // (graph tab off-screen on mobile); don't fight it in that case.
      if (document.hidden) {
        if (!pausedByObserver.current) {
          try {
            engine?.pauseAnimation();
          } catch {
            // Engine not fully ready; the watchdog recovers it.
          }
        }
      } else {
        lastHealthyAt.current = performance.now();
        if (!pausedByObserver.current) {
          try {
            engine?.resumeAnimation();
          } catch {
            // Engine not fully ready; the watchdog recovers it.
          }
        }
      }
    };
    onVisibilityChange();
    document.addEventListener("visibilitychange", onVisibilityChange, false);
    return () => {
      document.removeEventListener("visibilitychange", onVisibilityChange, false);
    };
  }, [hasGraph, engineKey]);

  // Stop the render loop when the graph is not on screen (e.g. its tab is
  // off-screen on mobile) and resume it when it comes back
  const containerRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    const container = containerRef.current;
    if (!container || typeof IntersectionObserver === "undefined") {
      return;
    }

    const observer = new IntersectionObserver(
      ([entry]) => {
        if (!entry) {
          return;
        }
        // Update the flag before the engine check: a tab reveal can fire
        // here before the new engine mounts, and an early return would
        // leave the flag stale (true), which makes the watchdog skip every
        // tick and never recover a black canvas.
        pausedByObserver.current = !entry.isIntersecting;
        const engine = fg3DRef.current;
        if (!engine) {
          return;
        }
        if (entry.isIntersecting) {
          // After a cap, the watchdog's interval is cleared and its effect
          // does not re-run, so a dead engine would stay black. Restart it
          // with a fresh attempt budget.
          rearmWatchdogRef.current?.();
          // The context can be lost while the view is hidden (e.g. a GPU
          // reset). Rebuild the engine the moment the view is shown again
          // instead of rendering black until the watchdog grace expires.
          try {
            // Rebuild when the context is lost, or a loss was latched and the
            // browser already auto-restored it (isContextLost() is false then).
            if (
              contextLostRef.current ||
              engine.renderer().getContext().isContextLost()
            ) {
              const saved = captureEngineCamera(engine);
              if (saved) {
                restoredCameraRef.current = { ...saved, data: dataRef.current };
              }
              lastHealthyAt.current = performance.now();
              setEngineKey((key) => key + 1);
              return;
            }
          } catch {
            // Engine not fully ready; the watchdog rebuilds it.
          }
          // The engine may be mid-teardown (key remount); the watchdog
          // recovers, so swallow the throw instead of killing the callback.
          try {
            engine.resumeAnimation();
            // The engine may have been hidden longer than the watchdog's
            // grace period; start a fresh clock so the first ticks after
            // reveal don't immediately rebuild a healthy engine.
            lastHealthyAt.current = performance.now();
          } catch {
            // Engine not fully ready; the watchdog rebuilds it.
          }
        } else {
          try {
            engine.pauseAnimation();
          } catch {
            // Engine not fully ready; the watchdog rebuilds it.
          }
        }
      },
      { threshold: 0 },
    );

    observer.observe(container);
    return () => observer.disconnect();
  }, [hasGraph]);

  const handleClick = (node: NodeObject) => {
    const nodeId = String(node?.id);

    if (enableNodePreview && onPreviewNode) {
      onPreviewNode({
        id: nodeId,
        data: (node as NodeObjectWithData).data ?? null,
      });
      return;
    }

    void push(buildNodeHref(nodeId));
  };

  React.useEffect(() => {
    const controls = fg3DRef?.current?.controls();
    if (controls) {
      //@ts-ignore
      controls.autoRotate = autoRotate;
    }
  }, [autoRotate, fg3DRef?.current]);

  // FRAME THE GRAPH — when the graph content (or an explicit fit request)
  // changes, frame the whole graph in view. The old fixed 160-unit main-node
  // focus overflowed big graphs and left small ones floating in empty space.
  // A bare mobile reveal does not re-fit: the mount effect has restored the
  // user's framing (skipIntroFocus below) unless the data changed while the
  // engine was hidden.
  React.useEffect(() => {
    // An explicit focus request (the fit button) always wins over a view
    // the user has taken over; any other trigger (a data swap, a reveal of
    // new data) re-arms the fit so the new framing runs unless the user
    // grabs the camera within the delay window.
    const explicit = triggerFocus !== lastTriggerFocusRef.current;
    lastTriggerFocusRef.current = triggerFocus;
    if (skipIntroFocusRef.current) {
      // This mount followed a rebuild; the pre-rebuild camera framing was
      // restored by the mount effect, so no fit is requested and the
      // taken-over state is left alone.
      skipIntroFocusRef.current = false;
      return;
    }
    userMovedRef.current = false;
    const focusMain = setTimeout(() => {
      if (!explicit && userMovedRef.current) {
        return;
      }
      if (kanjiInfo.id && data && data.nodes.length > 0 && fg3DRef.current) {
        fitRef.current();
      }
    }, 100);

    return () => {
      clearTimeout(focusMain);
    };
  }, [data, kanjiInfo.id, triggerFocus, engineKey, boundsReady]);

  const handleHover = (node: NodeObject | null, prevNode: NodeObject | null) => {
    if (node) {
      void prefetch(buildNodeHref(String(node.id)));
    }

    // Pause autoRotate while hovering, resume shortly after leaving a node
    const controls = fg3DRef.current?.controls() as
      | { autoRotate: boolean }
      | undefined;
    if (autoRotate && controls) {
      clearTimeout(resumeRotateTimeout.current);
      if (node) {
        controls.autoRotate = false;
      } else {
        resumeRotateTimeout.current = setTimeout(() => {
          controls.autoRotate = true;
        }, 500);
      }
    }

    // Reset the previous node's color to its default
    if (prevNode) resetNodeColor(prevNode, kanjiInfo.id);

    // Apply hover effect to the currently hovered node
    if (node) highlightNode(node);
  };

  const linkColor = React.useCallback(
    () => (resolvedTheme === "dark" ? "#ffffff" : "#000000"),
    [resolvedTheme],
  );
  const nodeLabel = React.useCallback(
    (node: NodeObject) => getNodeLabel(node, enableNodePreview),
    [enableNodePreview],
  );
  const nodeThreeObject = React.useCallback(
    (node: NodeObject) => createNodeThreeObject(node, kanjiInfo.id),
    [kanjiInfo.id],
  );
  const linkThreeObject = React.useCallback(
    (link: LinkObject) =>
      createLinkThreeObject(linkLabelByLink.get(link) ?? "", resolvedTheme),
    [linkLabelByLink, resolvedTheme],
  );
  const handleBackgroundClick = React.useCallback(() => {
    if (enableNodePreview) {
      onClosePreview?.();
    }
  }, [enableNodePreview, onClosePreview]);

  if (!graphData || !kanjiInfo || !data) return <></>;

  // The engine is only mounted once the container has real dimensions (see
  // the boundsReady value above) — a 0x0 birth yields a degenerate WebGL
  // canvas that cannot render until it is resized.
  return (
    <div ref={containerRef} className="size-full">
      {boundsReady && (
      <ForceGraph3D
      key={engineKey}
      controlType={"orbit"}
      width={bounds.width}
      height={bounds.height}
      backgroundColor={"#00000000"}
      graphData={data}
      linkColor={linkColor}
      linkDirectionalArrowLength={5}
      linkDirectionalArrowRelPos={getLinkDirectionalArrowRelPos}
      linkDirectionalArrowResolution={8}
      linkDirectionalParticles={showParticles ? 3 : 0}
      linkDirectionalParticleSpeed={0.004}
      linkDirectionalParticleWidth={1}
      linkDirectionalParticleResolution={8}
      enableNavigationControls={true}
      showNavInfo={false}
      ref={fg3DRef}
      warmupTicks={60}
      onNodeClick={handleClick}
      onBackgroundClick={handleBackgroundClick}
      onNodeHover={handleHover}
      nodeLabel={nodeLabel}
      nodeThreeObject={nodeThreeObject}
      // ADD ONYOMI TO LINKS
      linkThreeObjectExtend={true}
      // @ts-ignore
      linkThreeObject={linkThreeObject}
      linkPositionUpdate={updateLinkPosition}
      />
      )}
    </div>
  );
};

export default Graph3D;
