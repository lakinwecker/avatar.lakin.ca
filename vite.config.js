import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  publicDir: "public",
  base: "/avatar/",
  build: {
    outDir: "build",
    emptyOutDir: true,
    rollupOptions: {
      output: {
        entryFileNames: "avatar.js",
        chunkFileNames: "avatar-[name].js",
        assetFileNames: (info) => {
          const ext = info.name.split(".").pop();
          return ext === "css" ? "avatar.css" : "assets/[name][extname]";
        },
      },
    },
  },
});
