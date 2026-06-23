# OQ-112 close-out — pre-registered bite-definition ruling (recorded BEFORE the data)

**Date:** 2026-06-23. **Substrate:** `main` @ `5e5e2392`, live corpus 92 testsets,
manifest `pipeline_run_at=2026-06-23T05:39:44Z code_commit_short=a5593f7 n_constraints=92`.

This file is written and committed-to-the-audit-dir *before* the Part A / Part B probes run.
It is the RULING the result is sorted against — not a prior the result is bent to confirm.

## The bite-definition (Part A)

> A **bite** = **any field a consumer reads whose value changed on the live 92 due to the fix —
> FIELD-LEVEL, not headline-level.**

Consequences, fixed in advance:

- Item 1's `:198` fix (13/92 abductive signal `agrees` → `unavailable`, changing the
  diagnostic-summary **agreements list**) **IS a bite** under this definition — even though the
  `verdict_join` headline held for all 92. Drawing "bite" at headline-level would silently
  recategorize item 1 from live-touching to "no bite": a definitional escape, not a finding.
- A fix whose only on-92 effect is on a forced/constructed control (not on any of the 92
  live constraints) is **NOT a live bite** — it is latent-hardening, witnessed-latent.
- "Field a consumer reads" = a field serialized into `pipeline_output.json` / enriched output /
  a diagnostic-summary structure that a downstream report or join consumes. Internal intermediate
  values that no consumer reads do not count.

## The positive control obligation (Part A) — or "no bite" is unfalsifiable

The Part-A method compares consumer-read fields (including the `verdict_join` headline) before
(fix reverted) vs after (fix applied) on the 92. Before any "headline held / no live bite on 92"
may be asserted, the comparison apparatus must be shown able to **flag a constructed
headline-flip**. If a constructed input that *should* flip a join headline is NOT flagged by the
comparison, then "no live bite on 92" is the clean-grep that never looked → **HALT**, do not
report latency.

## Part B — four items, four positive controls, no shared generalization

Items 3 (A6 absence-certifies-clean), 5 (C4b blind=stable), 6 (A2 statistic-on-empty), and the
item-8 low cluster (C4c/A7/B2) are **different absence shapes**. A control proving the probe
detects A6's absence-branch says nothing about whether it detects A2's empty-statistic. Each item
gets:

1. a live-fire status on 92 (does its absence-gate fire on any of the 92?), AND
2. its **own** positive control showing that item's probe *would* detect a firing (construct the
   case the gate must flag; show it flags it), AND
3. its fix-shape recorded (so a declared-latent item carries its known remedy).

A single control wearing four hats makes "all latent on 92" an unwitnessed batch verdict. One
probe-shown-to-detect *per item*.

## Branches (pre-registered — the result sorts the remainder, not the prior)

- **Any item bites live** (Part A field-level change on a *live* 92 constraint, or Part B gate
  fires on any of the 92) → that item gets full land-alone treatment (Round-0 + fix per the OQ-44
  statute). Highest-value outcome; earns its round.
- **Remainder all-latent** (the preliminary expectation) → **DECLARE-AND-STOP.** File 3/5/6/8 as
  *witnessed latent on 92, fix-shape known, NOT landed — latent-hardening judged not to earn its
  spend pre-rebuild.* No hardening commit.

## Pre-registered expectation (the prior, named so the result can contradict it)

Preliminary read from Rounds 1–3: item 1 bites field-level (13/92 abductive flip, headline-neutral);
items 2/4/7 are witnessed-latent on 92 (void/sentinel/errored arms 0-firing); items 3/5/6/8 expected
latent (synchronic single-snapshot corpus → no temporal series for C4b; all-metrics-present →
no statistic-on-empty / no absence-certifies-clean firing). **This expectation does not license a
no-bite verdict — each leg is witnessed below with its own control, and a firing overrides it.**
