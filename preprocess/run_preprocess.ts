import { spawn } from "child_process";
import path from "path";
import { fileURLToPath } from "url";

type Step = {
  label: string;
  command: string;
  args: string[];
};

const preprocessDir = path.dirname(fileURLToPath(import.meta.url));
const bunPath = process.execPath;

const steps: Step[] = [
  {
    label: "Normalize kanjivg.xml",
    command: "go",
    args: ["run", "./unify_kangxi_cjk.go", "./kanjivg.xml"],
  },
  {
    label: "Create composition",
    command: bunPath,
    args: ["run", "1_create_composition.ts"],
  },
  {
    label: "Create searchlist",
    command: bunPath,
    args: ["run", "2_create_searchlist.ts"],
  },
  {
    label: "Create kanji data",
    command: bunPath,
    args: ["run", "3_create_data.ts"],
  },
];

const runStep = (step: Step): Promise<void> =>
  new Promise((resolve, reject) => {
    console.log(`\n=== ${step.label} ===`);

    const child = spawn(step.command, step.args, {
      cwd: preprocessDir,
      env: process.env,
      stdio: "inherit",
    });

    child.on("error", (error) => {
      reject(new Error(`${step.label} failed to start: ${error.message}`));
    });

    child.on("exit", (code, signal) => {
      if (signal) {
        reject(new Error(`${step.label} terminated by signal ${signal}`));
        return;
      }

      if (code !== 0) {
        reject(new Error(`${step.label} failed with exit code ${code ?? 1}`));
        return;
      }

      resolve();
    });
  });

const main = async (): Promise<void> => {
  console.log("Starting preprocess pipeline...");
  console.log(`Working directory: ${preprocessDir}`);

  for (const step of steps) {
    await runStep(step);
  }

  console.log("\nPreprocess pipeline completed successfully.");
};

main().catch((error) => {
  const message = error instanceof Error ? error.message : String(error);
  console.error(`\nPreprocess pipeline aborted: ${message}`);
  process.exit(1);
});
