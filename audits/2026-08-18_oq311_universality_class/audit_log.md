# Audit log — OQ-311 Item 1 (withdrawal + tracker repair) and Item 2 pre-registration

Append-only. Times are session-order, not wall-clock.

## HEAD stamp — OPEN

    $ git rev-parse HEAD
    d96873819c8deda20f948f432c72fad5e23d03c7   # d9687381 (session start, before any edit)

Session-start HEAD is `d9687381` ("Live-leg counts are never published; static-leg counts
are (operator ruling)"). All recon reads below were taken at that HEAD.

## Ordering note — a declared deviation from the commit-log-first rule

`audits/README.md` requires `audit_log.md` be committed BEFORE any code commit, so git can
testify to the ordering of pre-edit reads. This log lands AFTER commit `5d548413`
(`range_sweep.py` path repair). Declared rather than narrated around:

- The deviation is bounded to `python/sweeps/range_sweep.py`. That commit's witnesses (the
  R100 rename listing and the corpus load check) are pasted in full **in the commit message
  itself**, which is a git-testified artifact created at the same time as the edit, so the
  ordering for that file is recoverable without this log.
- Every read that commit 2 depends on — the `observers_not_humans_v6.md` baseline propagation
  sweep, the quoted §2.3 text, the `lawvere_glossary.md` and `project_orientation.md` pins —
  is recorded below and lands in a commit BEFORE those files are edited. For the edits that
  are still ahead, the rule is satisfied.

## Pre-edit reads recorded (all at HEAD d9687381)

- `evidence/rename_r100.txt` — prolog_v5 → original_v6 at R100.
- `evidence/range_sweep_output_keys.txt` — the `results_out` dict literal, pre-repair text.
- `evidence/tracked_witness.txt` — `git ls-files` + the JSON's own `total`/`jaccard_vs_sigmoid`.
- `evidence/arithmetic.txt` — three corpus counts + the 3,414 contradiction + the load check.
- `evidence/propagation_sweep.txt` — BASELINE sweep, taken before any edit (v6: 7 hits).
- `evidence/feasibility_crosstab.tsv` — design-feasibility proxy, NOT a result.

## Prereg freeze

    $ md5sum PREREGISTRATION.md
    3f53bb8e0daf2cba1fdd5c22f746163d  PREREGISTRATION.md

No run is funded by this audit, so the md5 witnesses the authored-and-frozen state only;
the first result line that would sit below it does not exist yet by design.

## HEAD stamp — CLOSE

    $ git rev-parse HEAD
    2a52e1f0db229bdaf4ead0f88a3a0931b753d626   # 2a52e1f0

**Comparison of the pair.** OPEN `d9687381` != CLOSE `2a52e1f0` — the stamps DIFFER, so the
question "was there a concurrent writer?" is answered by diff, not inference. The three
intervening commits are all this audit's own:

    $ git log --format='%h %an %s' d9687381..HEAD
    2a52e1f0 cafebedouin OQ-311 Item 1: withdraw §2.3's type-concentration claim; ...
    ec860a2e cafebedouin OQ-311 audit dir: audit_log + evidence, landed BEFORE the doc edits
    5d548413 cafebedouin range_sweep.py: repair dead corpus path + mark the unstratified-output site

**Blast radius on this audit's own read-set: none.** No commit in that range was authored by
another writer, so no file this audit read was changed under it. Recorded as DETECTION, not
prevention — this workflow has no lock file, and a clean result here is a different thing from
never having looked.

## Post-close amendment (recorded rather than silently applied)

After commit `2a52e1f0`, `scripts/gate.sh` went RED on the `apparatus` row: the `Fired:` line in
`WRITEUP.md` was written `**Fired:** **live** — ...` with the token bolded, and the checker's
`FIRED_RE` requires a bare `live|latent|no` immediately after the label. It matched `FIRED_ANY_RE`
but not `FIRED_RE`, so it registered as a **malformed** bit rather than a live one — which is why
the tally read 14 bits over 64 writeups when it had read 14 over 63 before the audit dir existed.
Corrected to `**Fired:** live — ...`; the row then reads 14L/1l/0n of 15 bits, GREEN.

Worth noting as the instrument working: a malformed catch bit is exactly the failure the
apparatus row exists to catch, and it caught it on its author.

## Verification summary (all steps, pasted witnesses in WRITEUP.md §4)

    V1 issues_status --check      319 parsed, 0 malformed
    V2 omega check / index        0 problems; index fresh (319 rows); selftest 10/10
    V3 audit_writeup_gate --check OK (189 dirs, 21 enforced, 0 problems); selftest 8 controls
    V4 scripts/gate.sh            GATE: GREEN (24 rows; python env row read first)
    V5 load check                 N = 3380 measured -> branch N < 3,414 -> ground (ii) survives
                                  discrimination: repaired exit 0 / pre-repair exit 2 corpus_empty
    V6 propagation sweep          v6 7 -> 15 (property gate, per line); lawvere 1 -> 0; else 0
                                  control: BRE 0 vs ERE 7
    V7 readback                   withdrawn numbers present and marked, not deleted
