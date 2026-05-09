import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  publicDir: "public",
  build: {
    outDir: "build",
    emptyOutDir: true,
  },
});
