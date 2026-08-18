# Phase 2 — the reported Python surfaces (OQ-296)

Executed 2026-08-18. Sites located by content. Phase 2 carries **two acts**: the
`is_constructed` provenance sibling, and an input-provenance stamp on
`container_candidates.json`. Site 1's originally-planned act was withdrawn — see
`PHASE2_SITE1_BLOCKER.md` and the operator ruling recorded below.

## Consumer checks (read-only, ran BEFORE editing — they gated both shapes)

**`is_constructed` — NOT consumer-less.** Live readers:
```
python/query.py:95,396                       prints "Is Constructed :" to a human
python/reports/queries/pattern_mining.py:190 carries it into mined patterns
python/boolean_independence.py:114,165-169   bool(c.get("is_constructed"))  [OUT OF SCOPE per ruling]
```
This is what decided drop-vs-flag. Dropping the field makes `.get()` return None in the
out-of-scope consumer, silently converting a constant-True feature into constant-False —
a worse defect than the one being fixed, inside the untouched set.

**`container_candidates.json` — zero readers.**
```
grep -rn "container_candidates" . (excl .venv/.git)
  → python/container_typology_analysis.py:26   (the writer)
  → this audit dir                              (my own notes)
```
Positive control for the sweep shape: the identical sweep for `corpus_data.json` returns
**109** hits, so it finds readers when they exist. A top-level schema addition is therefore
not a breaking change here.

## Act 1 — `extract_corpus_data.py`: is_constructed kept, flagged

Operator ruling 2026-08-18: **keep flagged; do not drop; do not make tri-valued.**

The tri-valued option was rejected on a sharper ground than "smaller blast radius":
`bool(None)` is `False`, so emitting None for the abstaining rows would deliver them to
`boolean_independence.py:169` as **asserted negatives**, not as abstains. The honesty is
destroyed at the read. Half a tri-state, delivered into a consumer that can only see two
values, is worse than not starting.

Emitted `analysis.is_constructed_provenance` states BOTH defects:
1. **Detector dark** — the only value that could yield False is `natural_law`, 0-firing on
   every corpus (dead-by-range; GAP-08 §7 to power). Measured 279/279 True.
2. **Abstain read as assertion** — `sig not in ('natural_law',)` is also True for the
   `unknown` signature (26/279 live rows), so those rows assert "constructed" on the
   strength of having *no data*. Independent of the dark detector; **not fixed here**,
   routed to its own OQ where `boolean_independence.py` is in scope and gets a real
   before/after diff.

DIFF (`outputs/corpus_data.json`, regenerated from `cwd=python/`, exit 0):
```
rows before/after: 279 279 | same ids: True
  ADDED .analysis.is_constructed_provenance: 279 rows
is_constructed value set: {True}   (unchanged)
```
Purely additive. Every difference justified: one added key, no value changed, no consumer
breaks.

## Act 2 — `container_typology_analysis.py`: input-provenance stamp

The planned NL-darkness flag was **withdrawn**: this site does not read a constant zero. It
serves `natural_law_pct` up to 0.9808 from a 2026-05-16 recon artifact over the chimera-era
corpus (n=3369). Full evidence in `PHASE2_SITE1_BLOCKER.md`. The drafted edit was written,
run, diffed, and reverted unshipped because its provenance text asserted "structurally zero
on every corpus," which is false for the values actually served.

Operator ruling: **stamp, then spawn**, under three constraints, all honoured:

1. **Measurements only, no regime explanation.** The stamp records the recon file's mtime,
   its `total_constraints`, and the live manifest's `n_constraints`/`pipeline_run_at` for
   contrast. It does NOT assert that the recon-era firings are pre-OQ-44 pass-open
   artifacts. That reading of `signature_detection.pl:249-255` is a reconstruction of a
   regime that no longer governs, applied to an artifact generated before the change —
   probably right, not witnessed. It goes to the spawned OQ as its lead hypothesis.
2. **States that this is not a falsifier hit, and why.** The artifact reports non-zero
   `natural_law_pct` and would otherwise read as OQ-296's own halt condition sitting in
   `outputs/`. The stamp says: pre-reset values over a different corpus, not a HEAD firing;
   Phase 0's live-leg census returned 0 over a non-degenerate 7-signature histogram.
3. **Reader check first.** Done above — zero readers, positive control at 109.

The stamp's `scope` field covers **every** field in the artifact, not just the NL component.
Flagging only NL would have certified the rest of the record as current, which is the more
dangerous claim.

DIFF (`outputs/container_candidates.json`, exit 0):
```
ADDED top-level keys: {'_input_provenance'}
REMOVED top-level keys: set()
pre-existing domain records CHANGED: 0
```
Purely additive.

## Superseded rationale — for the close note

**D2's site-1 half is superseded by this evidence.** D2 held the `formalization` axis was
*biased* — textual silently absorbing would-be formal — and required provenance to
propagate to the derived label. That analysis assumed the axis was computed on live data.
It is not computed at all: it is served from a frozen May-16 file. The site-2 half of D2
(provenance to the derived label, `is_constructed` flagged not tri-valued) stands unchanged
and was implemented as written.

**Roster reliability.** This is the **second** reclassification out of ~20 consumer sites —
`classification_audit.py:61` was the first (validates authored `constraint_claim` values, a
legitimately non-empty surface). Both were found only by inspecting a site someone was about
to edit. Consequence carried into Phase 3, which is comment-only and therefore verifies
nothing by the act of editing: **each Phase 3 annotation must state what was confirmed at
that site at edit time, and any site that turns out not to read the predicate is
reclassified out with a note rather than annotated.** A dark declaration on a site that does
not read the constant is a false annotation in the `MISSING_NL_PROFILE` class.
