// Shared channel between the mobile carousel (which lives in the [id] page
// subtree) and MobileGraphLayer (which lives in the root layout). The layer
// cannot read the carousel's api, so the carousel publishes its scroll
// progress here every animation frame and the layer maps it to its
// horizontal position, mirroring the graph slide's own motion.
// Plain module state (no React, no window), so importing it during prerender
// is safe; it simply stays at its initial value until the client publishes.
//
// Singleton contract: at most one publisher is mounted at a time (the page
// subtree's MobileLayout — both the home page and the [id] page have one,
// but only one page is mounted at a time) and at most one subscriber
// (MobileGraphLayer, which only subscribes on mobile while graph data is
// present). The module stores a single value with no notion of which
// carousel wrote it, so two concurrently mounted publishers would
// overwrite each other frame by frame — adding a new mobile carousel
// requires extending this module, not reusing it.

let progress = 0;
const listeners = new Set<(value: number) => void>();

export function setMobileCarouselProgress(value: number) {
  progress = value;
  for (const listener of listeners) {
    listener(value);
  }
}

export function onMobileCarouselProgress(
  listener: (value: number) => void,
): () => void {
  listeners.add(listener);
  // Sync with the current position immediately, in case the carousel has
  // already moved before the listener subscribed.
  listener(progress);
  return () => listeners.delete(listener);
}
