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

    # Build optimized assets and copy them into the lakin.ca personal site,
    # replacing the existing avatar embed entirely.
    copy-to-lakin.exec = ''
      build-opt
      DEST="$HOME/personal-repos/lakin.ca/public/avatar"
      mkdir -p "$DEST"
      rsync -a --delete build/ "$DEST/"
      echo "Synced build/ to $DEST"
    '';
  };
}
