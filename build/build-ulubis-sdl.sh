#!/bin/bash
cd "${0%/*}"
cat build-ulubis-sdl.lisp | sbcl
