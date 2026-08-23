# PREREGISTRATION — OQ-120 Phase 0 **v2**

> ## ⚠ THIS PREREG WAS NOT FROZEN BEFORE RESULTS. READ THIS FIRST.
>
> **v1 ran to completion on 2026-08-23** (`audits/2026-08-21_oq120_epsilon_boundary/`) and produced
> branch **G1b**. Its `PREREGISTRATION.md` *was* genuinely prior — md5
> `b181e1a2a9cd42b86d190be09f61d400`, recorded above the first result line before any sweep code
> was written — and it survives **unedited**, in its own directory, as the real ordering witness.
>
> **This document is weaker than that one and must not be described as frozen in advance.** It was
> authored on 2026-08-23 *after* seeing v1's output, in response to defects v1 surfaced. Every
> change below is a repair to a criterion that v1 proved unsatisfiable, vacuous, non-total, or
> underspecified — not a re-aim at a result someone wanted.
>
> **Mitigations, stated so a reader can discount appropriately rather than take assurances:**
> 1. v1's prior prereg is preserved intact and v1's numbers are published either way.
> 2. v2 lives in a **new directory**; nothing in v1's is re-created, re-stamped, or amended.
> 3. Both scorings (MOVED and DECISIVE) are retained so the branch can be re-derived either way.
> 4. **The substrate is identical to v1's** (same 18 legs, same file counts, HEAD unmoved at
>    `f88c8c3c`), so v2's sweep is a **re-derivation under final code plus new per-stratum
>    quantities**, not an independent measurement. If the transition data differs from v1's at all,
>    that is a determinism bug and the finding, not a new result.
> 5. **The branch outcome was substantially predictable before running**: v1 measured 1 decisive
>    case, and G0 requires 0. Saying so in advance is the honest version of a prereg written after
>    the fact.

**Executed:** 2026-08-23 · **OQ:** OQ-120 · **Scope:** Phase 0 only, local, zero API spend.

---

## What changed from v1's prereg, and what forced each change

| # | change | what forced it |
|---|---|---|
| 1 | **"attributes to" is now defined: DECISIVE** — the bit changed **and** the type that gate's clause produces is an MT endpoint (read on MT, since these gates live in `classify_from_metrics/6`). | v1: crossing 0.46 flips `snare_epsilon_floor` **by construction**, so a MOVED-based G0 test cannot come out false wherever any transition exists at 0.46. Operator ruled DECISIVE at the v1 checkpoint. |
| 2 | **G1 subtypes are exhaustive by construction**: G1a underpowered / G1b uncorroborated / G1c pair-falsified. **G1b keyed on ≥2 distinct MODELS**, not legs. | v1: G1b said "single leg" while G2 counted models, so a transition on two same-model redraw legs was *neither* subtype. Same-model redraws **are the floor** (OQ-347), not corroboration. |
| 3 | **The pooled `N_rail ≥ 10` floor is RETIRED as vacuous.** | v1 observed **9191** — three orders of magnitude clear. Authored for ~1,300 stories, run against ~17,100. Second unsatisfiable-by-construction test in the same gate. |
| 4 | **Pooling pinned per quantity**; per-stratum is primary, pooled is a memo line. | v1 reported single pooled totals over a corpus that is ~7 models × {redraw, regime} cells on one shared seed set, i.e. triple-counted seeds. |
| 5 | **Stratum = (model, regime, prompt_hash, schema_hash)**, per story. | Several legs are **backfilled** — a second generation event inside one leg at a different prompt/schema. `testsets_haiku` is **47% re-authored 70 days later under a different prompt AND schema**. |
| 6 | **C1–C4 given explicit multi-leg semantics**: PASS / SKIPPED-precondition / FAIL, three-way. | v1 had to invent this at runtime; its C4 reported an absent precondition as `FAIL` on 8 legs. |
| 7 | S13 brought inside the verified S1–S13 range. | It is load-bearing for the corroboration rule and sat outside the range the executor was told to check. |

## The gate

**Which type the pair is read on: `FT`,** the signature-resolved type — OQ-120 asks whether the
engine's *seat-constituted* type moves. MT recorded at every point regardless; **MT-invariant /
FT-only** transitions reported as their own named category.

**Definitions.** `N_eps` — (story × seat) cells with ≥1 located transition that is
**ε-gate-DECISIVE** and whose FT pair has **at least one endpoint** in
{rope, snare, tangled_rope, naturalized}; `unknown`-endpoint transitions counted but reported
separately. `N_reach` — of those, transition ε within that story's **own stratum's** authored-ε
[min,max] over the claimed-rope-or-snare population. `N_rail` — of those, visible at 0.01 rail
resolution.

**THE FLOOR (replacing `N_rail ≥ 10`).** Its original job — powering a two-arm generation design —
now belongs to **OR-5**. Its remaining job: *is the located effect big enough, in the stratum where
it lives, not to be a handful of boundary stories?* Per stratum, never pooled:

> `N_rail(stratum) ≥ 10` **AND** `N_rail(stratum) / (stories × 4 seats) ≥ 0.5%`,
> **in at least 2 distinct MODEL strata.**

The **rate is the load-bearing half** — a count scales with the corpus, a rate does not, which is
exactly what retired the old floor.

**Branches, tested in this order:**

- **G1a — UNDERPOWERED.** Floor not met in ≥2 model strata.
- **G0 — CLOSES OQ-120, no spend** (OR-1). Floor met **and ZERO** `snare_epsilon_floor`-DECISIVE
  transitions on the live legs. *Zero means zero* — v1's single non-replicating case, read
  substantively as a draw artifact, is still not zero, and this branch has no "close enough"
  reading. `snare_chi_floor` is deliberately **not** named as an expected attributor: v1 observed
  it in 0 qualifying transitions.
- **G1b — UNCORROBORATED.** Floor met, ≥1 decisive transition, but all from a single **model**
  stratum.
- **G1c — PAIR-FALSIFIED.** Floor + ≥2 models met, but no observed FT pair is exactly
  `{rope, snare}`. The gate is reachable and corroborated; the *label* is still wrong.
- **G2 — spend-go worth requesting.** Floor + ≥2 models + ≥1 decisive transition whose FT pair
  **is exactly `{rope, snare}`**. `kernel_v1` never counts toward the ≥2.

*Exhaustiveness:* G2 = floor ∧ models ∧ pair. G0 = floor ∧ zero-decisive. Any other result fails
the floor (G1a), or meets it with ≥1 decisive and fails models (G1b), or meets both and fails the
pair (G1c). The remaining cell — floor met, zero decisive — is G0. No gap.

**All branches are conditional on C1 having fired and C2 having declined.** If they did not
discriminate the gate is uninterpretable and that is the output, not a branch.

## Controls — three-way per corpus

**PASS** (precondition held, discriminated) / **SKIPPED-precondition** (precondition absent —
neither pass nor fail, named and counted) / **FAIL** (precondition held, did not discriminate).
An absent precondition is not a failure; a skipped control is never a passed one.

C1 positive plant (coalition-ineligible carrier, analytical d=1.0 override) must show a transition
at exactly 0.46 carrying `snare_epsilon_floor`. C2 same carrier with the floor overlaid to 0.90 —
C1's transition must **vanish**. C3 the `carbon_tax_2026` shadow guard, engine-level, must PASS on
every invocation. C4 the naturally-arising two-sided arm, on any corpus where the carrier's
suppression is below the snare floor (**verify per leg — the carrier is a redraw**). C1/C2 are
per-corpus blocking; C4 passes overall iff it PASSES on ≥1 corpus and FAILS on none.

**The DECISIVE predicate is itself an introduced instrument** and asserts its own two-sided control
in code: fires on `tangled_rope → snare` + `snare_epsilon_floor`, declines on
`tangled_rope → naturalized` + `snare_epsilon_floor`.

## Non-authoring-facing

OQ-78 ruling 3 binds on this phase's **output**. The transition map holds exact ε values at which
types flip. It must never feed a prompt, a seed file, or `epsilon_bin`.
