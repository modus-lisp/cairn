#!/usr/bin/env sh
# cairn's hermetic gates — the ones that build what they need and leave nothing behind.
#
# WHY A LIST AND NOT inspect/*.lisp.  This directory holds gates and DEMOS side by
# side, and the difference between them is intent rather than anything a tool can see.
# verify-objects.lisp wants an existing repository at /tmp/gitdelta; clone.lisp,
# fetch.lisp, serve.lisp and ssh.lisp reach the network or a running server.  Those are
# programs somebody runs by hand against something they already have, and wiring CI to
# them produces a red badge that means nothing.
#
# The gates below create their own scratch repositories under /tmp, assert, and exit
# non-zero on failure.  Adding one here is the act that says "this is a gate".
#
#   sh inspect/run-all.sh
set -u
here=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
# The tree is rooted at the workspace so sibling repos resolve — cairn reaches cabinet,
# conch, cram, pagetree and seal — and NOT at a local-projects symlink, so the gate
# tests the code in front of it.
export CL_SOURCE_REGISTRY="(:source-registry (:tree \"$here/../\") (:exclude \"vendor\") (:exclude \"deps\") :inherit-configuration)"

fails=0
for g in sha256 status-diff commit cabinet-backend pagetree-backend fetch-http; do
  echo "### $g ###"
  if sbcl --dynamic-space-size 3072 --non-interactive \
          --eval '(require :asdf)' --load "$here/inspect/$g.lisp"; then
    :
  else
    echo "  -> $g FAILED"
    fails=$((fails + 1))
  fi
done

echo "==============================="
if [ "$fails" -eq 0 ]; then
  echo "cairn: all hermetic gates green"
else
  echo "cairn: $fails gate(s) failed"
fi
exit "$fails"
