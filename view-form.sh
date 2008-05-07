#!/bin/sh

form_file="$1"

sbcl \
    --noinform \
    --end-runtime-options \
    --eval '(require :com.nklein.gl-springs)' \
    --eval '(use-package :com.nklein.gl-springs)' \
    --eval "(gl-springs (read-spring-system-from-file \"${form_file}\"))" \
    --eval '(sb-ext:quit)'
