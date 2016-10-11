#!/bin/bash
cd "${0%/*}"
cat build-ulubis-drm-gbm.lisp | sbcl
