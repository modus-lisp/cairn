# cairn

**A from-scratch git implementation in pure Common Lisp.** Clean-room — no
libgit2, no shelling out to `git`, no FFI to any C library. It reads and writes
repositories, and clones, fetches, pushes and pulls over both HTTPS and SSH.

A cairn is a stack of stones raised to mark a trail — each stone set on the last,
the line of them showing where the path has gone. A git history is the same
shape: every commit a stone placed on the one before, the chain of them marking
where the work has been. And a git object *is* a stone — content-addressed, named
by the hash of what's inside it — so a repository is quite literally a cairn of
them. You build it one stone at a time.

cairn implements the git object model, the pack format, the index, and the smart
transfer protocol itself; the pieces it would otherwise reinvent it borrows from
its siblings instead — SHA-1 from [`seal`](https://github.com/modus-lisp/seal)
(the classical-crypto home), DEFLATE from `chipz`/`salza2` (pure-CL inflate and
deflate). Its network transports ride the rest of the ecosystem: HTTPS on
`seal`'s TLS, SSH on [`conch`](https://github.com/modus-lisp/conch) — and both,
underneath, on [`natrium`](https://github.com/modus-lisp/natrium)'s crypto. A
clone travels **cairn → seal → natrium**; a push travels **cairn → conch →
natrium**. No OpenSSL, no libcurl, no libssh; the whole stack is Common Lisp.

## What it does

```lisp
;; --- read any repository (loose objects + packfiles, delta-compressed) ---
(cairn:with-repository (r "/path/to/repo")
  (cairn:head-commit r)                                  ; the HEAD commit id
  (dolist (c (cairn:log-commits r :limit 10))            ; first-parent history
    (format t "~a ~a~%" (subseq (car c) 0 8) (cairn:commit-summary (cdr c))))
  (cairn:cat-file-string r "HEAD")                       ; pretty-print an object
  (cairn:ls-tree r "HEAD"))                              ; tree entries

;; --- clone over HTTPS (cairn -> seal TLS -> natrium), checkout included ---
(cairn:clone "https://github.com/modus-lisp/natrium" "/tmp/natrium")

;; --- clone over SSH (cairn -> conch -> natrium) ---
(cairn:clone-ssh "ssh://git@host/srv/repo.git" "/tmp/repo"
                 :identity "~/.ssh/id_ed25519")

;; --- the write side: stage, commit (git accepts the result byte-for-byte) ---
(let ((r (cairn:open-repository "/tmp/repo")))
  (cairn:add r "README.md" "src/new.lisp")
  (cairn:commit r :message "a change" :author "you <you@example.com>")
  (cairn:print-status r)                                 ; staged / unstaged / untracked
  (cairn:diff r)                                         ; unified diff, git format
  (cairn:push-ssh r "ssh://git@host/srv/repo.git" :identity "~/.ssh/id_ed25519"))

;; --- fetch new objects and fast-forward ---
(cairn:fetch r)                                          ; url from the origin remote
(cairn:pull r :identity "~/.ssh/id_ed25519")             ; fetch + merge (ff or three-way)

;; --- three-way merge (canonical git; conflict handling is pluggable) ---
(cairn:merge r "refs/heads/feature")                     ; merge commit, or git-style conflict markers
(let ((cairn:*merge-resolver*                            ; swap in a smarter strategy…
        (lambda (ours base theirs) (declare (ignore base theirs)) (values ours nil))))
  (cairn:merge r "refs/heads/feature"))                  ; …e.g. always-take-ours, resolves cleanly
```

## Conformance

Every layer is checked by handing cairn's output to real `git` and confirming it
agrees — the strongest test available:

- **Reads** are verified by re-hashing: for every object `git` reports, cairn's
  `read-object` + `hash-object` reproduces its id (loose, packed, and
  delta-compressed objects, delta chains resolved to depth 8+).
- **Clone** produces an object set byte-identical to `git clone`; `git fsck
  --full --strict` and `git verify-pack` pass on what cairn writes.
- **Checkout** yields a working tree byte-identical to a reference clone —
  regular files, executables, and symlinks — and `git status` is clean.
- **Commits** hash-match: cairn's computed commit id equals `git rev-parse
  HEAD`, so its commit/tree/blob serialization is byte-exact.
- **status** / **diff** match `git status` / `git diff` (hunk headers,
  `/dev/null` ranges, and all).
- **Push** and **pull** round-trip through a real `git` server: after a push the
  remote's HEAD is cairn's commit and `git fsck` is clean; an incremental fetch
  transfers only the new objects and fast-forwards cleanly.
- **Merge** matches git: a clean three-way merge produces a tree byte-identical
  to `git merge`'s (same two parents); a conflict produces identical
  `<<<<<<<`/`=======`/`>>>>>>>` markers and an unmerged index (stages 1/2/3) that
  `git status` reads as `UU`. **Pull** is fetch + merge — a divergent pull
  three-way-merges (tree byte-identical to a pure-git reproduction) or leaves
  git-identical conflict markers labelled `origin/<branch>`.

The `inspect/` scripts reproduce each of these.

## Architecture

`src/`: `util` (the borrowed primitives — SHA-1←seal, zlib←chipz/salza2 — plus
CRC-32 and byte helpers) · `objects` (blob/tree/commit/tag) · `refs` ·
`pack` (v2 `.idx` + ofs/ref-delta resolution) · `repository` · `plumbing`
(cat-file/log/ls-tree) · `write` (the loose-object store) · `index` (the DIRC
staging area) · `index-pack` (turn a received pack into a stored one) ·
`pack-write` (build a pack to send) · `checkout` · `commit` (add/write-tree/
commit) · `status` · `diff` · `merge` (three-way, diff3, pluggable resolver) ·
`pktline` · `http` (a minimal HTTPS client on seal) · `clone` · `ssh` (git over a
conch channel) · `fetch`.

The ecosystem underneath: **natrium** is the constant-time modern crypto floor;
**seal** is a pure-CL TLS 1.3 client (and the home of the classical hashes);
**conch** is a pure-CL SSH client. cairn is the git that stands on all three —
each library built so the next could exist.

## Not yet

`merge` (and so `pull`) finds a single merge base — the recursive merge of
multiple bases (criss-cross histories) is future work. Also: HTTP push (needs
credential auth; SSH push works), delta compression in *written* packs (they're
correct but larger than git's), SHA-256 repositories, the commit-graph, and
shallow/partial clone. Contributions welcome.

The conflict resolver is a single seam — bind `*merge-resolver*` to change how
regions both sides edited are reconciled (the default reproduces git's markers).

## Running the tests

The `inspect/` scripts each load the system and check a layer against real `git`:
`verify-objects.lisp` (read + re-hash any repo), `clone.lisp` (clone over HTTPS),
`commit.lisp` (clone → edit → commit → `git fsck`), `status-diff.lisp` (next to
`git status`/`git diff`), `merge.lisp` (clean + conflict merge vs `git merge`),
`ssh.lisp` (clone + push over a local `sshd`), and `fetch.lisp` (incremental
fetch + fast-forward pull). The SSH scripts need a local `sshd`; setup is in each
file's header comment.

MIT. Research / educational; **not audited** — do not trust it with anything that
matters without an independent review.
