# Phase 4 — `domain_priors` D3 retirement (OQ-296)

Executed 2026-08-18. Retired `should_be_natural_law/1`, `expected_signature/2`,
`validate_signature/2` **and their exports only**. `get_prior/3`, `is_known_domain/1`,
`flag_novelty/1`, `category_of/2` untouched — `category_of/2`'s disposition is OQ-316's, not
something to settle while holding the delete key.

## Witness rule discharged — prove before you replace

**Deadness 1 — 0 firings** (measured on the live leg, n=279):
```
[retire] should_be_natural_law/1 firings on live leg: 0
[retire] reachable category_of values: [unknown_novel]
[retire] expected_signature rows: 7 total, 6 UNREACHABLE
         [physical_natural, formal_logic, election_cycle, statutory_formal,
          extractive_market, narrative_history]
[retire] constructed_constraint: NOT in live signature vocabulary
```

**Deadness 2 — 0 consumers.** All 17 references repo-wide were inside `domain_priors.pl`.
Control for the sweep shape: the same grep returns **9 external hits** for the sibling
`category_of/2` — which is why that one was kept. A sweep that finds nothing everywhere would
prove nothing; this one discriminates.

**Correction to the plan's figure.** The plan said "5 of 7 rows unreachable." The precise
breakdown is three-layered, and the third layer is the one that settles it:
- **5/7 unreachable by construction** — `category_of/2` has two clauses and cannot emit those
  five `domain_registry`-era categories at all.
- **6/7 unreachable on any authored corpus** — adding `physical_natural`, reachable by dispatch
  but never fed (claim vocabulary authored 0 times in ~5,311 files).
- **The surviving row is the least informative one**, and `constructed_constraint` — the
  expectation for four of seven rows — **is not in the live signature vocabulary at all**. So even
  a repaired classifier would have compared against an atom the engine no longer emits.

## Old-vs-new diff

```
$ python3 python/run_pipeline.py     → exit 0, output mtime 2026-08-18 15:36:01 (advanced)

per_constraint md5 PRE-retirement : 6bab9cdc3880b6b7d21f95ad453b33cd
per_constraint md5 POST-retirement: 6bab9cdc3880b6b7d21f95ad453b33cd
BYTE-IDENTICAL: True
top-level keys differing: ['manifest']
```
Exit 0 AND mtime advanced, so this is a real rewrite, not a stale-file false pass.

```
$ swipl -g "[stack], corpus_loader:load_all_testsets, halt" | python3 python/load_warning_gate.py
[WARNING-GATE] 3 load warnings: 3 allowlisted, 0 unexpected     (gate exit 0)
```
Allowlist channel, not `grep -v Warning`. Removing three exported predicates produced no new
load warning — nothing referenced them.

**Residual references after deletion: 2, both inside the retirement note itself.**

## Content preserved

`docs/design/design_gaps.md` → **GAP-38**, quoting the 7-row table verbatim and stating **both**
deadnesses. The routing_sink dark-declaration model covers unpowered sockets that something still
reads; it does not cover consumer-less code, which is why this is a gap entry rather than a site
comment. Per *Unwired ≠ worthless*, the entry records that the retirement judged **this
implementation** unable to deliver the capability — not the idea worthless — and names what
reviving it would require (a working classifier AND a re-derivation against the current signature
vocabulary; reviving the predicates alone reproduces the dead state).
