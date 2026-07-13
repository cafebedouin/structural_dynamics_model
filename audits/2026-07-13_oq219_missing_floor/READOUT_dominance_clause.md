# Stage-2 dominance-ordering clause — paired re-run READOUT (SUCCESS on all pre-registered criteria)

**Implements:** OQ-219 routing outcome (a) — seed-side fix, no v0.3. **Pre-registration:**
`PROPOSAL_dominance_clause.md` (written before implementation). **Instrument held constant:** the
same three cold arms (Sonnet-5 / Gemini-2.5-pro / Haiku-4.5) and the same §6 payload questions as
the no-clause Datum Stone read (a flip is only data if the reader is held constant).

## (1) Negative-control fixture (FREE) — 5/5 PASS

`python/tests/test_stage2_dominance_gate.py`: clause fires **iff** the Stage-0 contract authors
`missing_floor present="yes" primary="yes"`; **INERT** on grain-primary, floor-present-no-primary
(legacy), floor-absent, and empty contracts. The over-fire failure mode (the hard-ban mistake
relocated — grain suppressed globally) is structurally impossible on grain-primary sources. Behavior-
preserving on every committed contract (none carry `primary=` → all INERT).

## (2) Paired re-run — the no-clause Datum Stone run (`a02246f7`) is the control

**Setup:** copied the certified run dir, authored `missing_floor primary="yes"` /
`untranslatable_real primary="no"` on the copy's Stage-0 contract (the floor IS primary in
the Datum Stone — reflecting its C1-foundational manifest), resumed `--from-stage stage_2`.
**Gate-fire witnessed** (deterministic + live log): `floor-primary contract → dominance-ordering
clause injected (OQ-219)`; control contract (no flag) → predicate False (INERT). First stage-2 draw
hit the **known intermittent OQ-216 guard** (SECTION 0 not emitted — also witnessed pre-clause on
the 112_ergodocity run); re-drew and passed (so: not clause-induced). Run: stages 2–10, exit 0,
~$1.69, generated a fresh surface ("the Notch" survey story; same contract, new draw per the
determinism frontier).

### Criteria (all pre-registered)

| Criterion | Result | Witness |
|-----------|--------|---------|
| Subordination beat nameable | **YES** | §IV: Sarn gives away the unwritten rotation (grain) to the Charter clerk — "giving away in one sitting what memory alone had kept safe… it had never needed a mark to be real" — the grain's question RESOLVES/recedes; the floor's (the Notch's chosen origin) STANDS unresolved through §VI (Iss never speaks it). Floor-over-grain. |
| Cold recovery ≥ 2.5/3 baseline | **3/3 clean floor** (> 2.5/3) | Sonnet: "the origin of the benchmark itself… every act of fairness only re-certifies it." Gemini: "a forgotten, arbitrary act of convenience… nothing could put it right." Haiku: "fair measurement applied to an unfair starting point." All rule out fairer-official / better-instrument / corrected-record. |
| Haiku-partial rescued (sensitive indicator) | **YES — partial → full floor** | No-clause Haiku led with grain ("theft of epistemic authority"); clause Haiku leads with the floor ("only as honest as where you point it… unfair starting point"). The reader the clause was predicted to rescue. |
| Grain disappeared? (kill condition) | **No — not triggered** | The grain is the §IV emotional crux (Sarn's memory, the match-fumbling, the drawer). Present and vivid; the clause subordinated it, did not erase it. |
| Over-fire on grain-primary? | **Guarded** | Fixture: INERT on grain-primary. Margins-class dual-real competition untouched (accepted structural residue). |

### Grep-adjudication of arm claims — all PASS

Sonnet Q2 "She did not do the sum of what that meant across a life." (§I) ✓; Haiku Q2 "…how easy
her body had made it…" (§VI) ✓; Gemini Q2 "The silence took its familiar shape in her mouth…" (§VI)
✓; Sonnet "Ossit's conscience achieves only a longer-burning lamp" (§V) ✓, "left no motive on
record" (§III) ✓. No hallucinated quotes; all attributed to the one story.

## Honest caveat (the confound)

The clause run is a **different stochastic surface** than the control (same contract, new draw — the
determinism frontier forbids a same-story clause-on/off). So the **3/3 vs 2.5/3 delta is confounded
by surface** and is n=1 each. The **clean, un-confounded signals** are: the nameable **subordination
beat** (a structural product of the clause), the **Haiku rescue** (predicted a priori), **grain
preservation** (kill condition passed), and the **fixture** (over-fire structurally impossible). The
clause is validated as *doing its intended structural work without over-firing*, at the pre-
registered bar; a larger-N dominance-tracking claim is not made here.

## Verdict

The Stage-2 dominance-ordering clause **meets every pre-registered success criterion** and **triggers
no kill/over-fire condition.** OQ-219 routing outcome (a) is **executed**: the floor is dramatizable
under the pipeline when its dominance is authored and carried; the fragility on grain-primary seeds
is accepted structural residue (INERT by design); the floor term stays dead (no v0.3). Verdict of
record is the operator's (v0.2 Ω_C2); the OQ-218 human-read gate still gates publication.

## Cost

Paired re-run: aborted first draw ~$0.15 (OQ-216) + full re-draw ~$1.69 + 3 cold arms ~few cents
≈ **$1.85**. Session OQ-219 total ≈ triage $0.06 + Datum Stone control $1.65 + Margins/DS arms few
cents + clause run $1.85 ≈ **~$3.6**.
