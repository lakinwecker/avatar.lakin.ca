/* Copyright 2019 Lakin Wecker
   Licensed under the GPL v3.0
   See LICENSE for more information. */
import "animate.css";
import { Elm } from "./Main.elm";

const DEFAULT_GRID_SIZE = 32;

export function init(node, opts = {}) {
  const width = opts.width || node.clientWidth || window.innerWidth;
  const height = opts.height || node.clientHeight || window.innerHeight;

  let gridSize;
  if (opts.cellSize) {
    // Round down to nearest even — the LW bounding box is even-width, so an
    // odd-sized grid can never center it exactly.
    const raw = Math.floor(Math.min(width, height) / opts.cellSize);
    gridSize = raw - (raw % 2);
  } else {
    gridSize = opts.gridSize || DEFAULT_GRID_SIZE;
  }

  // Elm replaces the mount node, so use a child div to preserve the host's styling.
  const mount = document.createElement("div");
  node.appendChild(mount);

  const app = Elm.Main.init({
    node: mount,
    flags: { size: { width, height }, gridSize },
  });

  const observer = new ResizeObserver((entries) => {
    for (const entry of entries) {
      const r = entry.contentRect;
      app.ports.resize.send({
        width: Math.round(r.width),
        height: Math.round(r.height),
      });
    }
  });
  observer.observe(node);

  return app;
}

function initFromAttrs(node) {
  if (node.dataset.avatarInited) return;
  node.dataset.avatarInited = "true";
  const opts = {};
  if (node.dataset.gridSize) opts.gridSize = parseInt(node.dataset.gridSize);
  if (node.dataset.cellSize) opts.cellSize = parseInt(node.dataset.cellSize);
  init(node, opts);
}

// Initial pass: any [data-avatar] elements already in the DOM.
document.querySelectorAll("[data-avatar]").forEach(initFromAttrs);

// Watch for [data-avatar] elements added later (e.g. by React/Astro islands).
new MutationObserver((mutations) => {
  for (const m of mutations) {
    for (const node of m.addedNodes) {
      if (node.nodeType !== 1) continue;
      if (node.matches?.("[data-avatar]")) initFromAttrs(node);
      node.querySelectorAll?.("[data-avatar]").forEach(initFromAttrs);
    }
  }
}).observe(document.body, { childList: true, subtree: true });

// Fallback: init on #root if it exists and hasn't been initialized.
const root = document.getElementById("root");
if (root && !root.dataset.avatar && !root.children.length) {
  init(root);
}
