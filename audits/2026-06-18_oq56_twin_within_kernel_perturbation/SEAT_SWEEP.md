# Seat-sweep — what each fixed position sees across the corpus (OQ-56 headline)

The question OQ-56 opened with, finally run: **hold each role-position fixed and show what it
sees across the whole corpus.** Per-seat (no modal washing), fail-open on `unknown`, both twins
(`pipeline_output.{haiku,flash}.json`, n=960, `8126231`). Sharpened by a three-model review
(Gemini / Perplexity / Claude Code web) + two substrate kill-conditions run here.

## The exact table (% extractive = snare/tangled_rope/piton)

| seat | haiku | flash | source of the seat's `d` |
|---|---|---|---|
| **beneficiary** | **15.5** | **14.6** | `role→d` (low) — config |
| payer | 90.0 | 81.0 | `role→d` (high) — config |
| excluded | 80.9 | 72.2 | `role→d` (high) — config |
| observer | 81.6 | 72.3 | `role→d` — config |
| analytical (canonical ctx) | 82.6 | 71.7 | `context→(d,σ)` — config, **no role→d** |

Cluster spread 9 pts both twins; beneficiary sits ~65 pts below the cluster floor.

## Read it as geometry: 1-vs-4, not a spectrum

beneficiary vs {payer, excluded, observer, analytical}. Two points — beneficiary and
not-beneficiary — not five graded seats with a missing midpoint (a true midpoint is ~53% haiku /
~48% flash; nothing sits there). The cover-story orbit `[naturalized, snare, snare, snare]` is just
the 1-vs-4 baseline as a vector; the **informative orbits are the departures**:
`[snare,snare,snare,snare]` (beneficiary falls into the cluster — extraction with no cover story)
and the excluded-dissent cell (manufactured consensus).

## What is FORCED (mechanism, not finding)

The whole benign-vs-extractive split is **largely a consequence of the declared `role→d`
calibration** (beneficiary→low d, payer/excluded→high d; `dr_type` is a function of d). This is
authored-input circularity — do not present it as discovery.

- **The 70-point beneficiary↔payer swing** — forced direction; demoted to a **mechanism-stability
  check**. Its replication across both twins is *weak* evidence (both runs share the same `role→d`
  config), unlike the twin_comparison classification agreement.
- **excluded ≈ payer** — FORCED (config + coverage dilution), corrected from an earlier draft that
  mis-sorted it as additional. Both seats get high d; the arithmetic confirms pure dilution:
  payer 90% @ 8% unknown, thinned by excluded's extra 7 unknown points → 90×(85/92) ≈ 83 ≈ 81.
  The excluded seat's *only* non-forced content is the coverage gap itself (15% unknown,
  seat-localized) — its contribution is about absence, not reading (same conclusion as the
  cohomology turn).

## What SURVIVES every circularity deduction (the two residuals)

Both checked by substrate kill-conditions (parallel to the prior turn's cochain test):

1. **`naturalized` is the beneficiary seat's reading of high-ε constraints — ε/χ-selected, not
   d-fixed.** Trace of `classify_from_metrics` (`drl_core.pl:416`):
   ```
   naturalized :- BaseEps > rope_epsilon_ceiling(0.45),   % real extraction
                  Chi < tangled_rope_chi_floor(0.35).       % compressed below detection by low d
   ```
   It fires only when ε is genuinely high AND χ is compressed — the engine's literal cover-story
   definition. Confirmed non-forced: the same beneficiary low-d seat reads 5 types across the
   corpus (naturalized 634 / rope 128 / tangled_rope 97 / piton 57 / scaffold 38); a low-ε
   constraint reads `rope`, a high-ε-uncompressed one reads `tangled_rope`. So `naturalized` is
   the seat-relative cover-story *type*, ε-gated. (Survives.)

2. **The analytical seat sits at the payer's pole, not between the parties.** Recounted exactly
   (no estimate): haiku (293+98+17)/494 = **82.6%**, flash (371+152+13)/748 = **71.7%** — top of
   the cluster, nowhere near the ~53%/~48% midpoint. Analytical carries no `role→d` (only a
   declared `context→(d,σ)`), so this owes nothing to the role calibration. (Survives.)

## The headline claim

NOT "beneficiaries naturalize" (forced, contentless-by-Coupling). It is:

> **There is no view from nowhere in this corpus. The seat that names itself *analytical* is a
> declared seat (a `context→(d,σ)` calibration, not a god's-eye view), and as declared it sits at
> the payer's pole — beside the parties who pay, not between them.**

The Seat Theorem turned from axiom into measurement: even the auditor's own stance is positioned.
That is the part a hostile reader cannot wave off as built-in (the `role→d` deductions don't touch
it), and it is the more interesting essay than the (old) cover-story finding.

## Reproduce
Per-seat histogram over `dr_type_for_stakeholder` per role + the canonical analytical context, on
each twin via `corpus_path` overlay. (Scratch probe `probe_seat_sweep.pl` removed after the run;
counts above are the witnessed output.)
