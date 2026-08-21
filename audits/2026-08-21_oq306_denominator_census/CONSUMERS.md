# OQ-306 D5 — consumer sweep, with per-consumer dispositions

**Executed:** 2026-08-21. Roster is CHECKABLE, not received: it is the union of five
`/usr/bin/grep` sweeps, each re-run at execution with a positive control.

## Derivation and its boundary

Sweeps: (a) `n_constraints`, (b) `corpus_constraint`, (c) `per_constraint`,
(d) `_contradictions`, (e) `schema_version` (conditional on R-C's bump).
Trees: `python/ prolog/ docs/ agent/ audits/ scripts/`, plus repo-root `*.md`
(`ISSUES.md`, `KNOWN_STATE.md`, `AGENTS.md`, `README.md`, `CLAUDE.md` — plausible carriers of an
`n_constraints` claim) and `json/`.

Excluded, with reasons: `.claude/` (no code consumers — the hooks invoke python scripts already
in-tree), `outputs/` (gitignored, regenerable), `prolog/archives/` and `prolog/testsets*/` (data,
not consumers).

**Boundary justification.** A positive control inside the union cannot reveal that the union is
too small — it can only show the instrument fires within it. So the boundary carries this stated
justification rather than a control it cannot have.

### Positive controls (each chosen OUTSIDE the narrowest tree, so the control tests COVERAGE)

| sweep | control site | result |
|---|---|---|
| (a) `n_constraints` | `audits/2026-06-13_twin_comparison/RESULTS.md` | 1 hit ✓ |
| (b) `corpus_constraint` | `python/run_pipeline.py` (the in-Prolog gate string) | 8 hits ✓ |
| (c) `per_constraint` | `prolog/json_report.pl` (the writer, outside `python/`) | 16 hits ✓ |
| (d) `_contradictions` | `agent/generate_kernel_corpus.py` | 5 hits ✓ |
| (e) `schema_version` | ~~`prolog/`~~ **NO VALID CONTROL** | see below |

**Control (e) is WITHDRAWN (post-implementation evaluation, 2026-08-21).** It claimed
`prolog/` as an outside-`python/` control with "hits ✓" and no hit count — the only row in the table
lacking one. `grep -rn schema_version prolog/` returns 3 hits, **all under `prolog/archives/`, a
subtree this very document excludes from the sweep**; there are ZERO occurrences in any
`prolog/*.pl`. So the control either fired inside excluded territory (demonstrating nothing about
the sweep as run) or was never run. **Sweep (e) is therefore UNCONTROLLED**, and its result is
reported at that altitude: it found the `twin_comparison.py` breakage, which is a real catch, but
it carries no evidence that it found *everything*. `schema_version` is a python-side manifest key
with no Prolog reader, so no valid outside-`python/` control exists — which is itself the honest
finding, not a gap to paper over.

**Control (a) was CORRECTED at execution.** The plan named
`audits/oq140_divergence_extract.py`; that file's real path is
`python/audits/oq140_divergence_extract.py`. At its real path it sits *inside* `python/`, the tree
the sweep is most certain to cover, so it tested firing rather than coverage — destroying the
property it was chosen for. Replaced with a genuinely-outside-`python/` site. A missing site is a
finding, not a silent drop.

## Dispositions

### By-design members — inline comment, no change

- `prolog/data_validation.pl` (orphan-cid check) — enumerates the MEMBER population deliberately:
  it asks "is this authored fact keyed on something that is not a corpus member", and a non-story
  member is still a member. Correct as written.

### Identity keys — unchanged, and deliberately so

These use `n_constraints` / `per_constraint` as a same-run IDENTITY KEY, not as a semantic count.
This is exactly why R1 kept the name.

- `python/run_pipeline.py` three-way gate (`per_constraint == glob == n_constraints`)
- `python/enhanced_report.py`, `python/w1_sheaf_join.py`, `python/orbit_operator.py`,
  `python/audits/oq140_divergence_extract.py`

The three-way gate is the one thing that catches corpus/manifest divergence; every OQ-306 choice
preserved it, and it still passes.

### Routed to owning OQs — site comment naming `corpus_story/1`

Rate-computing consumers whose denominator SHOULD be stories, but whose restatement belongs to the
OQ that owns the arithmetic:

- `prolog/commentary_census.pl` → OQ-136 / OQ-202
- `prolog/kernel_orbit_export.pl`
- `prolog/probe_oq197_controls.pl`
- `python/batch_claim_reconciliation.py`, `python/q_provenance_readout.py`
- `prolog/cs_kernel_registry.pl`
- `python/run_pipeline.py` provenance-coverage gate → OQ-202

### Verified unaffected

- `python/golden_file_check.py` — its extractor projects `per_constraint` to
  `{id: [4 perspectives]}` and is structurally blind to added entry keys and added manifest keys.
  This is the evidence that dissolved R3(b)'s NECESSITY half; the PROCESS half survives (R-F), so
  C3 kept its own read step with baseline provenance. **Cataloged, not fixed:** its docstring calls
  `outputs/golden_classifications.json` a "committed baseline" while `outputs/` is gitignored —
  pre-existing, and in a file this change only RUNS, so the stale-prose rule leaves it alone.
- `python/omega_resolver.py:1069` — compares `schema_version` against its OWN
  `SCHEMA_VERSION = "omega-resolver/1"`. Different schema, different namespace (a string, not an
  int). Unaffected by the pipeline manifest bump.

### FIXED — the R-C sweep's one real catch

- **`python/audits/twin_comparison.py:587`** asserted `schema_version != 2` as a refuse-to-join
  guard. The 2 → 3 bump would have made it **refuse every freshly generated pipeline output**.
  It fails loud rather than silently, but it was still broken by this change.
  Fixed to `JOINABLE_SCHEMA_VERSIONS = (2, 3)` — named as *the set of versions whose
  `per_constraint` shape this join actually reads*, not "the current version", with a comment
  saying to widen it only after checking a new version against what the join READS (a bare
  `!= <latest>` would refuse every older artifact — the opposite failure, equally wrong).
  Docstring guard statement updated in the same change.
  **Two-sided control:** accepts 2 and 3; still REFUSES 1, 4 and `None` — it did not become a
  pass-everything.
- `python/audits/five_leg_twin_comparison.py` — checked, carries no `schema_version` reference.

### Provenance echoes — enumerated after the evaluation (sweep completeness was over-claimed)

The post-implementation evaluation found **at least nine** `manifest["n_constraints"]` readers with
no disposition anywhere in this document. **Nothing is broken** — every one is a provenance echo or
an identity comparison, not a rate denominator — but "consumers swept with per-consumer
dispositions" over-claimed the sweep's completeness, so they are named here:

`python/audits/schema_sieve.py:118` (compares its own row count against it),
`python/audits/g_beneficiary_channel_audit.py:195` (prints *"Corpus: N constraints"* — now a
MISLABEL, since N counts members; harmless but worth fixing if that file is touched),
`python/container_typology_analysis.py:495`, `python/run_drift_mismatch.py:96`,
`python/epsilon_authorship_readout.py:148`, `python/tensions_ledger.py:385`,
`python/audits/audit3_synthesis.py:447`, `python/audits/oq151_dual_gauge_crosstab.py:85`,
`python/audits/oq88_false_mountain_detector.py:260`.

**The lesson is about the claim, not the sites:** a sweep whose controls all fire still cannot
support "complete" — controls establish that the instrument reaches, never that the roster is
closed. This document should have said "the sites that compute RATES are dispositioned", which is
what was actually done.

### Cataloged, left as-is

- The five filename-suffix local exclusions (`*_contradictions` string matches) — each a *local*
  exclusion by a different consumer. These are the seed sites the OQ named; they are now
  redundant with `corpus_member_kind/2` but harmless, and rewriting five call sites is not this
  commit's scope.
- `prolog/validation_suite.pl` — generated; carries the `unknown_interval` sentinel.
- ISSUES.md orbit-holes row.
- `n_sotu_constraints` — checked path is `<repo>/prolog/testsets_sotu/` (`run_pipeline.py:31-34`
  constant), absent on disk 2026-08-21 by direct `ls`; the archive sibling
  `prolog/archives/datasets/testsets_sotu/` EXISTS and is a DIFFERENT path. Both paths and the
  instrument recorded so a later reader does not conflate them.

## Seed sites from the OQ body

The OQ named `ISSUES.md:608 / :4038 / :5345 / :6208` as the four local exclusions whose
un-consolidated existence was half the reason the defect persisted. They had drifted to
`:609 / :4129 / :5489 / :6352` by 2026-08-20 (ISSUES line drift is normal and expected). Each is
retired or re-routed by the close, so closure back-maps to the OQ.
