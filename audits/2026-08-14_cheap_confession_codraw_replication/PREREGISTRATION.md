# PREREGISTRATION — cheap_confession co-draw replication

**Written:** 2026-08-14, before any draw was generated.
**OQ:** OQ-264 (same-input redraw stability), applied to the `positional_disagreement_as_evidence` kernel.
**Frozen manifest:** `agent/decompose_manifests/flat/cheap_confession_2026_20260814_151329.manifest.json`

## The question

`standpoint_reading` was generated ~4h after its three siblings, as a single fresh draw
against a set that was not drawn with it. It came back as the only reading sharing any
overlap with `instrumentalist_reading` — the essay's own thesis. That twinning now carries
§6's final bullet and the closing section of `cheap-confession-v4.md`.

**Is the twinning a property of the kernel, or an artifact of one draw?**

## The claim under test — ORDINAL, not the ratio

> Of the 6 reading pairs, **exactly one** has non-zero overlap, and it is
> (standpoint_reading, instrumentalist_reading).

**Why not the number.** `Jaccard = AgreeN / (2·NCtx − AgreeN)` (`cs_kernel_registry.pl:130`).
The baseline 0.270 = 62/(2·146−62), where NCtx=146 rather than 156 *only because
instrumentalist abstained in 10 contexts*. The denominator moves with abstention,
independently of agreement — the unit-built-denominator hazard OQ-264 names explicitly
("never gate finer than the denominator's own churn"). The ordinal claim is invariant to
that; the ratio is not. **No verdict in this audit may be stated on the magnitude of J.**

## Baseline (draw 0 = the committed live set), pinned

| pair | J | agree | diverge |
|---|---|---|---|
| standpoint × instrumentalist | 0.2696 | 62 | 84 |
| standpoint × proceduralist | 0.000 | 0 | 156 |
| standpoint × pragmatist | 0.000 | 0 | 153 |
| proceduralist × instrumentalist | 0.000 | 0 | 146 |
| proceduralist × pragmatist | 0.000 | 0 | 153 |
| instrumentalist × pragmatist | 0.000 | 0 | 146 |

Agreement decomposition: 23 tangled_rope + 21 rope + 18 naturalized = 62.

## Design

- **k = 3** independent co-draws. OQ-264 standard: replicated iff present in **all three**;
  1–2 of 3 is an *observation*, not a replication. n=1 is explicitly insufficient.
- Each draw regenerates the **whole frozen seed set together** (7 stories), so every reading
  is drawn in the presence of its siblings. Manifest is byte-frozen across draws; the only
  varying input is sampling (no seed, no temperature — Sonnet-5 rejects it; the churn is the
  production regime's own).
- Isolation: `--run-tag codraw_0N` → `json/codraw_0N/`, `prolog/testsets/codraw_0N/`.
  Non-recursive corpus glob means these never enter the live corpus. Artifacts move to the
  audit dir on completion; **nothing lands in `prolog/testsets/`**.

## Read path — verified before spend

`cs_kernel_registry:compare_kernel_readings(positional_disagreement_as_evidence, _, PairStats)`
under `[stack], corpus_loader:load_all_testsets` with `asserta(config:param(corpus_path, <dir>))`.

Two controls run **before** any draw:
1. **Live corpus (n=273)** → reproduced 0.26956521739130435 / 62 / 84 and five exact zeros,
   matching `standpoint_reading_report.md`. The predicate call is a valid read path.
2. **4-story isolate (n=5 incl. contradictions)** → byte-identical to (1). Two-sided: the
   signature layer carries corpus-relative inputs (twin-agreement 0.722), so corpus reduction
   *could* have moved this and did not. Licenses reading co-draws as isolates.

## Verdict rule — committed before the run

| Result | Verdict | Consequence for the essay |
|---|---|---|
| Ordinal claim holds **3/3** | **REPLICATED** | §6 bullet 5 and the closing stand as written |
| Holds **1–2 of 3** | **OBSERVATION** (not replication) | v4's existing "suggestion, not a measurement" hedge is *correct and must stay*; no strengthening |
| Holds **0/3** | **COLLAPSED** | twinning was a draw artifact; §6 bullet 5 is struck and "Who this is for" reverts toward v3 |

**Null-vs-zero trap (pre-committed).** `pair_reading_agreement/7` yields `J = null` for a pair
with no comparable context — deliberately *not* 1.0, and equally not 0. A draw where a reading
abstains everywhere produces null, which is **undetermined, not "no overlap."** A null in the
standpoint×instrumentalist cell makes that draw **uncountable toward either limb**, not a
falsification. Record it and report k as reduced.

## Secondary read — the relocating confound

Both standpoint and instrumentalist carry **‡ on the institutional seat** (OQ-188: that seat's
rope/not-rope verdict is role-authored, sitting on the `agenda_setter 0.12 ↔ beneficiary 0.25`
straddle of the f(d) sign root), and both read `rope` there. So their overlap may be an artifact
of how one stakeholder role was authored, twice — a confound a co-draw does **not** close, since
each draw re-authors the role.

Operational secondary read: **partition AgreeN by agreed-type** and report the non-`rope`
residual separately (baseline: 41 of 62 = 23 tangled_rope + 18 naturalized).

- Pre-committed: primary holds AND non-rope residual > 0 in ≥2 of 3 draws
  ⇒ the twinning is **not solely** role-authored.
- If the overlap collapses to rope-only agreement, the finding is **a fact about stakeholder-role
  authoring, not about the readings**, and must be reported at that altitude.

*Unverified mapping, stated as such:* I have **not** established that the 21 rope-agreement
contexts are the institutional-seat contexts. The partition above is by **agreed type**, which is
directly observable; any claim that it isolates the institutional seat requires a separate check
and may not be asserted from this partition alone.

## What this audit does NOT close

- Whether the kernel's four readings are the right decomposition (manifest-fixed, not tested).
- Whether `expected_structural_delta` in the frozen manifest pre-determines part of the overlap —
  the manifest is held constant by design, so this is *controlled for*, not *measured*.
- Anything about the corpus beyond these 7 constraints, all peripheral, none in the giant component.
- Whether v4's closing repair took. Different question, different run.
