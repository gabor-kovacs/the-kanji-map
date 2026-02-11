# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

The Kanji Map is a Japanese language learning tool that visualizes kanji characters and their decomposition relationships in interactive graph form. Deployed at thekanjimap.com.

## Commands

```bash
npm run dev       # Start development server (http://localhost:3000)
npm run build     # Build for production (static export)
npm run lint      # Run ESLint
npm run serve     # Serve pre-built 'out' directory locally
```

## Architecture

### Tech Stack
- Next.js 16 with App Router (static export in production)
- React 19, TypeScript, Tailwind CSS 4
- Jotai for state management
- react-force-graph-2d/3d with Three.js for visualization
- shadcn/ui (Radix UI) for components

### Key Directories
- `src/app/` - Next.js pages: home (`page.tsx`), dynamic kanji pages (`[id]/`)
- `src/components/` - UI components, graph visualizations (`graph-2D.tsx`, `graph-3D.tsx`), kanji display
- `src/lib/` - Server functions (`index.ts`), Jotai atoms (`store.tsx`)
- `data/` - Static kanji data (composition.json, individual kanji JSON files)
- `data/animCJK/` - Git submodule with stroke animation SVGs
- `preprocess/` - Scripts to generate kanji data from KanjiVG, Jisho.org, KanjiAlive

### Data Flow
1. **Preprocessing**: Scripts in `preprocess/` fetch from KanjiVG, Jisho.org, KanjiAlive → generate `data/*.json`
2. **Build**: Next.js statically generates all kanji pages using `generateStaticParams()`
3. **Runtime**: Pages load composition.json + specific kanji data, client renders force graph

### Key Patterns
- Server functions in `src/lib/index.ts` use `"server-only"` module
- Graph components use `next/dynamic` with SSR disabled
- UTF-16 handling: use `Array.from()` and `codePointAt()` for kanji with surrogate pairs
- Mobile/desktop layouts determined by `useMediaQuery` hook
- Stroke animation SVGs fetched from animCJK with fallback chain (svgsJa → svgsZhHans → etc.)

### TypeScript Path Alias
```
@/* → ./src/*
```

## Environment Variables

Required in `.env`:
- `NEXT_PUBLIC_BASE_URL` - Public domain for SEO
- `KANJIALIVE_API_KEY` - RapidAPI key for kanji data
