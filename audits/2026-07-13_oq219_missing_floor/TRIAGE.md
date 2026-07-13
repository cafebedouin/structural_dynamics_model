# OQ-219 Step-0 — Leg-B triage (corpus-drawn only; pause-if-none-pure)

**Date:** 2026-07-13. **Operator ruling driving this step:** "Triage only, then pause" +
"Corpus-drawn only — if none certifies pure, pause and report rather than authoring one."
**Goal:** find a corpus source whose Stage-0 `<invariant_contract>` certifies
`missing_floor present="yes"` **AND** `untranslatable_real present="no"`/weak — the pure-Detector-B
**positive control** the pre-registration (`PROPOSAL.md`) requires for the naming probe.

**Verdict: NO pure-Detector-B source found in the corpus.** Two arms of evidence below. Paused per
ruling; no source authored. This is itself a finding (see §3).

---

## 1. Prose pre-screen (free; a HYPOTHESIS, not the engine's verdict)

Read the head/spine of the strongest Detector-B-titled corpus sources
(`agent/narrative_transform/originals/`). Every purpose-built source pairs a **Detector-B**
codifying instrument with a **Detector-A** untranslatable-real — and the untranslatable-real is
typically *dominant*:

| Source | Detector-B element (floor) | Detector-A element (untranslatable real) | Screen |
|--------|----------------------------|-------------------------------------------|--------|
| `the_datum_stone` (leg A) | the crown datum / the Stone — arbitrary cut line, "no bottom, a hand" | "the walking of the water" — bound all equally, destroyed by codification | dual, **A-dominant** (source: "the others being only money") |
| `the-empty-pan` | the posture-index template (soldiers' bodies) | the body's tacit bridge-knowledge "the words don't exist in the language of scores" | dual (per OQ-219 #6 note) |
| `the_table_of_winters` | the Table prices a class; "a death priced correctly balances" | the boat-grammar "the knowing lived in the trimming, and nowhere else… a scale cannot weigh the wind" | dual, **A-dominant** |
| `the_good_name_book` | the 1–12 standing mark; "nobody decreed this" | Isa's faith "only itself while nobody could read it… it is not a figure" | dual, **A-dominant** |
| `the_eighth_commentary` | the Seven sanctioned readings / the list the levy reads | live judgment "does not survive being sanctioned" | dual, **A-dominant** |
| `rift1/4/5/6` | baselines, thresholds, quota pulse, the archive | palm-to-stone communion, the murmur, "Ama, igi-bar? — Mother, do you see?" | dual, **A grain strong** |
| `the_miller_his_son_and_their_ass` | (no central Snare; shifting crowd opinion) | (none) | **neither** — weak Detector-B fit |

**Prose conclusion:** the corpus is **architected dual-grain**. `rotation_seven`/`faint_blue`
(the plan's named leg-B candidates) were already pre-screened NOT purer. No obvious pure-B.

## 2. Engine triage (the witness — Stage-0 `--dry-run`, ~$0.03/source, gemini-2.5-pro, exit 0)

Ran the two best remaining pure-B shots — the most instrument/baseline-centric purpose-built
sources, where prose alone could not call the grain balance. **Both certify dual-grain:**

**`rift3` "Insufficient"** (`agent/narrative_transform/uke/rift3_1783965049/stage_0_output.md`):
```xml
<untranslatable_real present="yes">An organism's emergent, embodied adaptation to a chronic stressor is a form of knowledge that cannot be captured by a system designed only to measure deviation from a static ideal.</untranslatable_real>
<missing_floor present="yes">A system of control founded on a sharp, binary partition of a continuous reality will inevitably collapse into injustice when faced with phenomena that exist at the boundary.</missing_floor>
```

**`rift2` "Load-Bearing"** (`agent/narrative_transform/uke/rift2_1783965139/stage_0_output.md`):
```xml
<untranslatable_real present="yes">A physical process generates a signal that, while measurable by the system's instruments, is irreducible to the system's explanatory categories.</untranslatable_real>
<missing_floor present="yes">The obligation to produce is treated as a natural state of being, obscuring the founding choice that partitioned a class of people as fuel for a system.</missing_floor>
```

rift2's floor is textbook Detector-B ("the founding choice that partitioned a class of people as
fuel") — yet `untranslatable_real` is still `present="yes"`. The engine confirms the prose screen:
**even the most baseline-centric corpus source is dual-grain.**

## 3. The finding (why this matters beyond leg-B selection)

The UKE originals corpus appears to have **no pure-Detector-B source by construction** — every
source authored for the detector schema carries a live untranslatable-real. This is the same
structure OQ-215 arm-3 observed ("every arm-3 source led with a Detector-A grain") and is likely
**the reason for it**: the corpus is authored dual-grain, so the missing-floor path is never the
*sole* primary invariant. This bears directly on OQ-219: the pure-B **positive control** the
pre-registration needs (to prove the naming probe *can* flag a floor at all, operator sharpening #2)
**cannot be sourced corpus-drawn.** The design's positive control is unavailable under the
corpus-drawn-only constraint.

## 4. Cost + provenance

2 dry-runs × ~$0.03 = **~$0.06** (Stage-0 only, gemini-2.5-pro; no graded stages, no Claude
stages). Leg-A Stage 0 already spent (commit `434ec74d`). Raw artifacts:
`agent/narrative_transform/uke/{rift3_1783965049,rift2_1783965139}/`. The two graded runs remain
HELD.

## 5. Paused — decision returns to the operator (recorded in the entry / this turn)

Options surfaced (no default taken; the corpus-drawn-only ruling forecloses authoring without a
new ruling):
- **(a) Leg-A-only, dual-grain, discrimination-rule-as-whole-probe** — run leg A (`the_datum_stone`)
  and let the pre-registered *discrimination rule* (floor's work vs the walking's) carry the probe.
  Cost: no pure-B positive control ⇒ a leg-A null stays **uninterpretable** per #2 (probe-invalid
  risk is un-retired). Corpus-honest but structurally weaker.
- **(b) Operator authors (or hand-picks) leg B** — overrides "pause rather than author"; the operator
  authors a pure-B source blind to the load-bearing criteria (keeps the naming probe honest; the
  executing instance stays out).
- **(c) Reframe the probe** — treat the dual-grain finding as *itself* evidence: R14's floor may be
  inherently a **co-presence** grain in this corpus (never authored standalone-primary), which
  routes OQ-219 toward the v0.2-*repair* question (can v0.2 dramatize a contract-only floor?) rather
  than a pipeline-generation existence claim.
- **(d) Hold** — bank the pre-registration + this triage; resume later.
