# Stage-2 dominance-ordering clause — PROPOSAL (pre-registered before implementation + re-test)

**Date:** 2026-07-13. **Origin:** OQ-219 routing **outcome (a)** — floor-recovery tracks
dominance; the fault was dual-grain competition, not a missing protocol socket → **no v0.3**;
the fix is seed-side. Operator go granted, exact content spec supplied.

## What is implemented (R3(b) architecture, third application)

1. **stage0.md** authors a `primary="yes/no"` flag on each `<invariant_contract>` invariant — at
   most one marked primary (the world's Tier-1 real, tied to the foundational/highest-centrality
   constraint). AUTHORED at the only source-sighted stage; never inferred downstream.
2. **orchestrator** injects the **DOMINANCE ORDERING clause** (operator's verbatim text) into the
   Stage-2 prompt **iff** the Stage-0 contract marks `missing_floor` `present="yes"` AND
   `primary="yes"` — a **structural** gate (`_contract_marks_floor_primary` / `_stage2_dominance_suffix`),
   never model-inferred. INERT on grain-primary / no-primary sources.

## Pre-registered verification (three pieces; do FIRST, before any spend)

### (1) Negative-control fixture (FREE — structural gate, no API)

A fixture test asserting the clause text reaches the Stage-2 prompt **iff** the floor-primary flag
is set:
- floor-primary contract (`<missing_floor present="yes" primary="yes">`) → `_stage2_dominance_suffix`
  returns the clause.
- grain-primary contract (`<missing_floor present="yes" primary="no">` /
  `<untranslatable_real ... primary="yes">`) → returns `""` (INERT).
- floor present but no primary marked → `""`.
- missing_floor absent → `""`.
**Failure mode this guards (the hard-ban mistake relocated): over-firing** — the clause suppressing
the grain globally, flattening the legitimate dual-real richness of Margins-class (grain-primary)
stories. Structural gate ⇒ fixture ⇒ **no cost excuse for skipping it.**

### (2) Positive: the paired re-run (the no-clause Datum Stone run is already the control)

Re-run Datum Stone with the clause ON (same source, `primary="yes"` authored on its floor; the
committed no-clause run `a02246f7` is the paired control). **Success:**
- the **subordination beat is nameable** in the improver read (a beat where the grain's question
  resolves/recedes while the floor's stands), AND
- **cold recovery ≥ the 2.5/3 baseline** on the SAME three arms (Sonnet/Gemini/Haiku), with the
  **Haiku-class partial as the sensitive indicator** — if the clause is doing work, that is the
  reader it rescues (partial → full floor).

### (3) Kill condition

If the **grain disappears entirely** from the output, the clause is **too strong** → **soften the
subordination language** (not "resolve/recede" but "quiet for one beat"); **do NOT reach for a floor
term** — the term stays dead per routing (a). Re-test.

### Pre-stated acceptance (so it can't be "fixed" later)

The clause is **INERT on grain-primary seeds by design.** Margins-class fragility — floor as
break-rider, reader-dependent recovery — is **accepted structural residue** (the presuppositional-
prior legibility handicap), **filed, not a bug awaiting a pass.** No future pass may "fix" Margins
by making the clause fire on grain-primary sources.

## Methodological note (exported to OQ-220 before its spend)

Hold the arms constant across sources — a flip is only data if the reader is held constant (this
arc's Sonnet flip was readable *because* Sonnet was the same reader on both seeds).
