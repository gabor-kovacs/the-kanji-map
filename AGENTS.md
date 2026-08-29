<!-- BEGIN:nextjs-agent-rules -->

# This is NOT the Next.js you know

This version has breaking changes — APIs, conventions, and file structure may all differ from your training data. Read the relevant guide in `node_modules/next/dist/docs/` (resolved from this file's directory; in monorepos the `next` package may not be visible from the repo root) before writing any code. Heed deprecation notices.

This block is written and re-added by `next dev` — verify at `node_modules/next/dist/server/lib/generate-agent-files.js`. Removing it from a diff only re-creates the uncommitted change; committing it with your work keeps the tree clean.

<!-- END:nextjs-agent-rules -->

## Project instructions

- This is a Bun-first Next.js App Router project. Use `bun install` and `bun run` for local work.
- Production builds use a static export (`out/`); run `bun run build` to verify changes.
- Run `bun run lint` after code changes. Keep `bun.lock` and `package-lock.json` synchronized when dependency changes require lockfile updates.

## Project overview

The Kanji Map is a Japanese language learning tool that visualizes kanji characters and their decomposition relationships in interactive graph form. It is deployed at thekanjimap.com.

## Commands

```bash
bun run dev       # Start the development server (http://localhost:3000)
bun run build     # Build for production (static export)
bun run lint      # Run ESLint
bun run serve     # Serve the pre-built `out` directory locally
```

## Architecture

### Tech stack

- Next.js 16 with App Router (static export in production)
- React 19, TypeScript, Tailwind CSS 4
- Jotai for state management
- react-force-graph-2d/3d with Three.js for visualization
- shadcn/ui (Radix UI) for components

### Key directories

- `src/app/` - Next.js pages: home (`page.tsx`), dynamic kanji pages (`[id]/`)
- `src/components/` - UI components, graph visualizations (`graph-2D.tsx`, `graph-3D.tsx`), and kanji display
- `src/lib/` - Server functions (`index.ts`) and Jotai atoms (`store.tsx`)
- `data/` - Static kanji data (`composition.json` and individual kanji JSON files)
- `data/animCJK/` - Git submodule with stroke animation SVGs
- `preprocess/` - Scripts that generate kanji data from KanjiVG, Jisho.org, and KanjiAlive

### Data flow

1. **Preprocessing:** Scripts in `preprocess/` fetch from KanjiVG, Jisho.org, and KanjiAlive, then generate `data/*.json`.
2. **Build:** Next.js statically generates all kanji pages using `generateStaticParams()`.
3. **Runtime:** Pages load `composition.json` and specific kanji data; the client renders the force graph.

### Key patterns

- Server functions in `src/lib/index.ts` use the `"server-only"` module.
- Graph components use `next/dynamic` with SSR disabled.
- UTF-16 handling: use `Array.from()` and `codePointAt()` for kanji with surrogate pairs.
- Mobile and desktop layouts are determined by the `useMediaQuery` hook.
- Stroke animation SVGs are fetched from animCJK with a fallback chain (`svgsJa` → `svgsZhHans` → etc.).

## TypeScript path alias

```text
@/* → ./src/*
```

## Environment variables

Required in `.env`:

- `NEXT_PUBLIC_BASE_URL` - Public domain for SEO
- `KANJIALIVE_API_KEY` - RapidAPI key for kanji data
