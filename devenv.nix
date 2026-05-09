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

    deploy.exec = ''
      build-opt
      rsync -azP ./build/ lakin@lakin.ca:~/webapps/www.lakin.ca/source/avatar/
    '';
  };
}
