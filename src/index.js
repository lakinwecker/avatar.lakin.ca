/* Copyright 2019 Lakin Wecker
   Licensed under the GPL v3.0
   See LICENSE for more information. */
import { Elm } from "./Main.elm";

Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    width: window.innerWidth,
    height: window.innerHeight
  }
});
