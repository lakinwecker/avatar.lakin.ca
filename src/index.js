/* Copyright 2019 Lakin Wecker
   Licensed under the GPL v3.0
   See LICENSE for more information. */
import { Elm } from "./Main.elm";

const DEFAULT_GRID_SIZE = 32;

export function init(node, opts = {}) {
  const width = opts.width || node.clientWidth || window.innerWidth;
  const height = opts.height || node.clientHeight || window.innerHeight;

  let gridSize;
  if (opts.cellSize) {
    gridSize = Math.floor(Math.min(width, height) / opts.cellSize);
  } else {
    gridSize = opts.gridSize || DEFAULT_GRID_SIZE;
  }

  // Elm replaces the mount node, so use a child div to preserve the host's styling.
  const mount = document.createElement("div");
  node.appendChild(mount);

  return Elm.Main.init({
    node: mount,
    flags: { size: { width, height }, gridSize },
  });
}

// Auto-init: find all elements with data-avatar attribute
document.querySelectorAll("[data-avatar]").forEach((node) => {
  const opts = {};
  if (node.dataset.gridSize) opts.gridSize = parseInt(node.dataset.gridSize);
  if (node.dataset.cellSize) opts.cellSize = parseInt(node.dataset.cellSize);
  init(node, opts);
});

// Fallback: init on #root if it exists and hasn't been initialized
const root = document.getElementById("root");
if (root && !root.dataset.avatar && !root.children.length) {
  init(root);
}
