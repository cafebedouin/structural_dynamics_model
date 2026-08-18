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
    <recorded below, at the position it was frozen>

No run is funded by this audit, so the md5 witnesses the authored-and-frozen state only;
the first result line that would sit below it does not exist yet by design.

## HEAD stamp — CLOSE

    (recorded at close, below)
