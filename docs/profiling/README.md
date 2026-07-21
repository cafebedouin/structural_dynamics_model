# `docs/profiling/` — cross-model behavioral profiling (reference / prior art)

Reference material imported from the operator's PoTM work (CC0). These are **prompt-based
behavioral fingerprints** of LLMs — a *different* instrument from this repo's engine, kept here so
the two methods can be read side by side. They are **not** wired into the pipeline and describe no
DR-engine behavior; treat them as prior art, not as engine documentation.

## Contents

| file | method | what it measures |
|------|--------|------------------|
| `architectural_profiling_v1.2.md` | one forced-certainty stress prompt across models | Authority-Gradient Resistance, uncertainty signaling, conflict-handling style |
| `correlation_matrix_and_fingerprints.md` | the "Blind Mirror Battery" (11 models) | dimensional scores + correlation matrix (authority-resistance × fabrication r = −0.81), phenotype taxonomy, χ² strategy×provider |
| `hyperstition.md` | feed models the DR philosophy, read which "hook" each latches | "Epistemic Tribes" (Technocrats / Humanists / Critics); frames DR as a *memetic mirror* / "Personality Test for LLMs" |

## Why they live next to the five-leg corpus audit

These and `audits/2026-07-20_five_leg_twin_comparison/` are **the same projective instrument with
opposite read-outs.** The DR framework is the Rorschach blot in both; each model projects its priors
onto it. What differs is how the projection is read back:

|  | **Profiling (here)** | **Constraint-story method (`audits/2026-07-20…`)** |
|--|----------------------|----------------------------------------------------|
| stimulus | 1 rich provocation | 1005 structured seeds |
| response | free prose | schema-constrained artifact (authored fields) |
| read-out | **interpretive** — analyst/LLM reads prose → labels | **mechanical** — deterministic engine → structural metrics |
| N | 1–few per model | 1005 per model |
| attribution | confounded (self-report, context contamination, provenance) | clean (matched seeds + single-model fingerprint) |
| reproducibility | low (docs self-report confidence ≈0.68; "cannot distinguish genuine from performed") | high (byte-deterministic from committed JSON) |
| sees | the model's **self-narrated stance** toward the frame | the model's **authored structural priors** |

**Trade the profiling method makes:** it captures the meta-stance (refusal, fabrication-with/without-
acknowledgment, authority drift, "tribe") that only appears when a model answers *as itself* — but
pays the observer-effect / self-report confounds the Blind Mirror doc flags as its central weakness.

**Trade the constraint-story method makes:** it removes that confound (the model authors a *task*
artifact, not a self-report, so priors leak unperformed) — but sees only what the DR ontology
encodes, and has no ground truth, so it reads *disposition*, not *capability*.

**They triangulate.** The five-leg study can say "kimi-k2.6 authors the least-differentiated observer
structure" but not "therefore weaker" — it lacks an external anchor. The profiling battery **is** a
candidate anchor: run forced-certainty + hyperstition on kimi-k2.6 and test whether its *stance*
fingerprint (authority-resistance, tribe-hook thinness, unacknowledged fabrication) converges with
its *structural* fingerprint. Cross-method convergence is the calibration neither method has alone.
(Caveat carried from the audit: that leg is **kimi-k2.6**, not K3/"3.0" — neither dataset yet
contains the hyped model.)

## Provenance caveat

The fingerprints in `correlation_matrix_*` and `hyperstition.md` were partly **authored by the
models about themselves** (e.g. Claude scoring itself "Very High meta-awareness / The Adversary").
That self-authored, self-flattering read is exactly the failure mode the mechanical method avoids —
so the two are complementary checks, not interchangeable evidence.

See also: `audits/2026-07-20_five_leg_twin_comparison/` (WRITEUP.md + DEEPER_CUTS.md). Note
`architectural_profiling_v1.2.md` was moved here from `docs/` (2026-07-20) to consolidate the
profiling set — it is the canonical copy and is cross-referenced by `docs/llm_presheaf_diagnostic_plan.md`.
