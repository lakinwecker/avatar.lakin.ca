{ pkgs, ... }:

{
  packages = with pkgs.elmPackages; [
    elm
    elm-format
    elm-test
  ];

  languages.javascript = {
    enable = true;
    bun = {
      enable = true;
      install.enable = true;
    };
  };

  scripts = {
    dev.exec = "bunx vite";

    build.exec = "bunx vite build";

    build-opt.exec = ''
      ELM_FLAGS="--optimize" bunx vite build
    '';

    preview.exec = ''
      build-opt
      bunx vite preview
    '';

    # Build optimized assets and copy the embed bundle into the lakin.ca site.
    # Only avatar.js / avatar.css are copied — the demo HTML is served by the
    # Astro page at /avatar.
    copy-to-lakin.exec = ''
      build-opt
      DEST="$HOME/personal-repos/lakin.ca/public/avatar"
      mkdir -p "$DEST"
      cp build/avatar.js build/avatar.css "$DEST/"
      echo "Copied avatar.js and avatar.css to $DEST"
    '';
  };
}
