import * as fs from "fs";
import * as path from "path";
import { Parser } from "xml2js";

const rawXML = fs.readFileSync("./kanjivg.xml", "utf-8");

// Load radicals list
const radicalsPath = path.join(
  path.dirname(__dirname),
  "data",
  "radicallist.json"
);
// Strictly validate radicallist.json contents
interface RadicalEntry {
  radical: string;
  original?: string;
}

const parseRadicals = (raw: unknown): RadicalEntry[] => {
  if (!Array.isArray(raw)) return [];
  const result: RadicalEntry[] = [];
  for (const item of raw) {
    if (item && typeof item === "object") {
      const radical = (item as Record<string, unknown>)["radical"];
      const original = (item as Record<string, unknown>)["original"];
      if (typeof radical === "string" && radical.length > 0) {
        if (
          original === undefined ||
          (typeof original === "string" && original.length >= 0)
        ) {
          result.push({ radical, original: original as string | undefined });
        }
      }
    }
  }
  return result;
};

const radicalsData: RadicalEntry[] = parseRadicals(
  JSON.parse(fs.readFileSync(radicalsPath, "utf-8"))
);
const radicalsSet = new Set<string>();
const radicalOriginalMap = new Map<string, string>();

// Extract all radical characters
radicalsData.forEach((entry) => {
  radicalsSet.add(entry.radical);
  if (entry.original && entry.original.length > 0) {
    radicalOriginalMap.set(entry.radical, entry.original);
  }
});

const parser = new Parser();

interface KanjiComponent {
  $: {
    "kvg:element"?: string;
    "kvg:variant"?: string;
    "kvg:original"?: string;
  };
  g?: KanjiComponent[];
}

interface KanjiEntry {
  $: {
    id: string;
  };
  g?: KanjiComponent[];
}

interface KanjiGraph {
  in: string[];
  out: string[];
}

// Check if an element is an IDS (Ideographic Description Sequence) notation
const isIDSNotation = (element: string): boolean => {
  return (
    element.startsWith("⿰") ||
    element.startsWith("⿱") ||
    element.startsWith("⿲") ||
    element.startsWith("⿳") ||
    element.startsWith("⿴") ||
    element.startsWith("⿵") ||
    element.startsWith("⿶") ||
    element.startsWith("⿷") ||
    element.startsWith("⿸") ||
    element.startsWith("⿹") ||
    element.startsWith("⿺") ||
    element.startsWith("⿻") ||
    element.startsWith("⿼") ||
    element.startsWith("⿽") ||
    element.startsWith("⿾") ||
    element.startsWith("⿿")
  );
};

// Recursively extract all elements from a component tree
const extractAllElements = (
  comp: KanjiComponent,
  elements: Set<string>
): void => {
  if (comp.$?.["kvg:element"]) {
    const element = comp.$["kvg:element"];
    // Skip IDS notation elements like ⿱穴㒸
    if (!isIDSNotation(element)) {
      elements.add(element);
    }
  }
  if (comp.g) {
    comp.g.forEach((child) => extractAllElements(child, elements));
  }
};

// Get direct child elements (in-nodes) for a given element, recursively searching the tree
const getDirectChildren = (comp: KanjiComponent, element: string): string[] => {
  const children: string[] = [];

  // Collect the first descendant elements beneath a node, treating wrapper groups
  // (that lack kvg:element, e.g. groups with only kvg:position/phon) as transparent.
  // - If a radical is encountered, include it and stop descending that branch.
  // - If an IDS node is encountered, include its children elements (radicals kept,
  //   no further expansion), mirroring existing behavior.
  const collectDirectChildElements = (
    node: KanjiComponent,
    acc: string[]
  ): void => {
    const nodeElement = node.$?.["kvg:element"];
    if (nodeElement) {
      if (radicalsSet.has(nodeElement)) {
        acc.push(nodeElement);
        return;
      }
      if (isIDSNotation(nodeElement)) {
        if (node.g) {
          node.g.forEach((grandchild) => {
            const grandEl = grandchild.$?.["kvg:element"];
            if (grandEl) {
              if (radicalsSet.has(grandEl)) {
                acc.push(grandEl);
                return;
              }
              acc.push(grandEl);
            }
          });
        }
        return;
      }
      acc.push(nodeElement);
      return;
    }
    // No element on this node (likely a structural wrapper) → descend
    if (node.g) {
      node.g.forEach((grandchild) =>
        collectDirectChildElements(grandchild, acc)
      );
    }
  };

  // If this component matches the element we're looking for
  if (comp.$?.["kvg:element"] === element) {
    // If the matched element is a radical, stop recursion into drawing children
    // and, if an original form exists for this radical, include it as a direct child.
    if (radicalsSet.has(element)) {
      const original = radicalOriginalMap.get(element);
      if (original) {
        children.push(original);
      }
      return children;
    }

    // For non-variants, add all direct children
    if (comp.g) {
      comp.g.forEach((child) => {
        collectDirectChildElements(child, children);
      });
    }
  }

  // Continue searching in children, but stop if this component is a variant or IDS notation
  if (comp.g) {
    comp.g.forEach((child) => {
      // Skip recursing into IDS notations
      const childElement = child.$?.["kvg:element"] || "";
      // Stop recursion entirely when encountering any radical node
      if (radicalsSet.has(childElement)) {
        return;
      }
      if (!isIDSNotation(childElement)) {
        children.push(...getDirectChildren(child, element));
      }
    });
  }

  return children;
};

// Removed XML-based variant handling; variants/originals are resolved via radicallist.json

parser
  .parseStringPromise(rawXML)
  .then((parsedXML) => {
    console.log("Extracting all elements...");

    // Step 1: Extract all unique elements from the entire XML
    const allElements = new Set<string>();
    parsedXML.kanjivg.kanji.forEach((entry: KanjiEntry) => {
      if (entry.g) {
        entry.g.forEach((comp) => extractAllElements(comp, allElements));
      }
    });

    console.log(`Found ${allElements.size} unique elements`);

    // Step 2: Build in-nodes for each element
    console.log("Building in-nodes...");
    const inNodes: Record<string, Set<string>> = {};

    allElements.forEach((element) => {
      inNodes[element] = new Set<string>();
    });

    parsedXML.kanjivg.kanji.forEach((entry: KanjiEntry, idx: number) => {
      if (idx % 1000 === 0) {
        console.log(
          `Processing entry ${idx}/${parsedXML.kanjivg.kanji.length}`
        );
      }

      if (entry.g) {
        entry.g.forEach((comp) => {
          // For each element, find its direct children
          allElements.forEach((element) => {
            const children = getDirectChildren(comp, element);
            children.forEach((child) => {
              inNodes[element].add(child);
            });
          });
        });
      }
    });

    // Step 3: Build out-nodes from in-nodes (reverse relationships)
    console.log("Building out-nodes...");
    const outNodes: Record<string, Set<string>> = {};

    allElements.forEach((element) => {
      outNodes[element] = new Set<string>();
    });

    // For each element and its in-nodes, add this element to the out-nodes of its children
    allElements.forEach((element) => {
      inNodes[element].forEach((child) => {
        if (!outNodes[child]) {
          outNodes[child] = new Set<string>();
        }
        outNodes[child].add(element);
      });
    });

    // Step 4: Build the final database, excluding elements with no in-links AND no out-links
    console.log("Building final database...");
    const db: Record<string, KanjiGraph> = {};

    allElements.forEach((element) => {
      let inList = Array.from(inNodes[element]);
      const outList = Array.from(outNodes[element]);

      // If this element is a radical, override its in-nodes to only include its original (if any)
      if (radicalsSet.has(element)) {
        const original = radicalOriginalMap.get(element);
        inList = original ? [original] : [];
      }

      // Only include elements that have at least one in-link OR out-link
      if (inList.length > 0 || outList.length > 0) {
        db[element] = {
          in: inList,
          out: outList,
        };
      }
    });

    console.log(
      `Final database has ${Object.keys(db).length} elements (filtered from ${
        allElements.size
      })`
    );

    // Ensure the directory exists
    const dirPath = path.join(path.dirname(__dirname), "data");
    if (!fs.existsSync(dirPath)) {
      fs.mkdirSync(dirPath);
    }
    // Write to the file
    const filePath = path.join(dirPath, "composition.json");
    fs.writeFileSync(filePath, JSON.stringify(db, null, 2), "utf-8");

    console.log("File written successfully to", filePath);
  })
  .catch((err) => {
    console.error("Error reading or processing XML:", err);
  });
