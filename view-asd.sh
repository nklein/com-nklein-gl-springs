#!/bin/sh

asd="$1"

sbcl \
    --noinform \
    --end-runtime-options \
    --eval '(require :com.nklein.gl-springs)' \
    --eval '(use-package :com.nklein.gl-springs)' \
    --eval "(gl-springs (make-spring-system (springs-from-asd \"${asd}\")))" \
    --eval '(sb-ext:quit)'
