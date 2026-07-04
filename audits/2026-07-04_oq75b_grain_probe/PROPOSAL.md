# OQ-75(b) grain-sensitivity precursor probe — PROPOSAL (pre-registration)

Written 2026-07-04, BEFORE any perturbed run. Zero API spend; unratified mechanical arms
(the OQ-75(b) ruling's Stage-1 KIND, at pilot scale). Substrate: the OQ-72 tranche-1
registry + the 10-kernel pilot (42 within-kernel pairs, both legs), baseline already
witnessed in `audits/2026-07-03_oq72_concept_key_pilot/sweep_results.tsv`.

## What this probe is and is NOT

- IS: the magnitude measurement the future §7.1 correlation-statistic spec needs — how much
  do the axiom-leg concept-key partitions move per grain step on real substrate?
- IS NOT: the ruled Stage-1 check itself. The ruling's kill condition tests stability of the
  CORRELATION, which does not exist yet (full-go (b) build). Nothing here discharges it.
- Direction disclosure (theorem, not finding): coarsening merges vantage atoms and can only
  preserve-or-increase alignment (a coarser partition's equivalence relation is a superset);
  arbitrary refinement can only preserve-or-decrease it. The MEASUREMENT is the magnitude;
  reporting the direction as an empirical result would be selling a theorem.

## Arms (deterministic, machine-generated from the ratified registry; no ratification)

- **A0 baseline** — tranche-1 registry as committed (`prolog/axiom_concept_registry.pl`).
  Read from the existing OQ-72 sweep output; re-run only if needed for format parity.
- **A1 coarsen-max** — every kernel's slots merged to one atom `<kernel>__all` (the
  within-kernel coarsening limit; cross-kernel merges stay impossible by namespacing).
- **A2 coarsen-2** — per kernel, the two slots with the most mapped axioms merged (tie →
  alphabetical; 2-slot kernels: identical to A1 by construction — reported, not hidden).
- **A3 refine-arbitrary** — every slot with ≥2 mapped axioms split alphabetically into
  `<slot>_r1`/`<slot>_r2` halves (deterministic arbitrary refinement — the maximally
  unratified move; a ratified refinement would split by subject-merit, this one cannot).
- **A4 refine-limit** — every axiom its own concept = structurally the `exact_name` key;
  KNOWN all-blind (0/935; witnessed corpus-wide). Cited from baseline, not re-run.

## Pre-registered observables (per arm × leg × kernel, over the 42 pilot pairs)

1. N_cells = concept-key (agreements + disparities) per pair; kernel and pooled totals.
2. Conversions: kernels with ≥1 cell (baseline = 10/10).
3. Contained-C2-pair co-slotting: the 3 live contradiction pairs same-atom under the arm's
   mapping (mechanical from the mapping; coarsening can never separate, refinement can).
4. Verdict distribution: `ax_stability_verdict` counts per arm.

Headline = the A0→A1 and A0→A3 deltas in pooled N_cells (the grain lever's throw on this
substrate). No pass/fail bar is pre-registered — this probe FEEDS the statistic spec's bar,
it does not enforce one. Interpretation grid (written before the run): large throw ⇒
grain-normalization in the statistic is load-bearing, Stage-1-proper likely swings and the
staged escalation earns its keep; small throw ⇒ the partition layer is grain-tolerant on
this substrate and the statistic's sensitivity will come from elsewhere if anywhere.

## Controls

- **Overlay-took-effect (per arm, per leg):** after loading an arm's registry overlay, the
  in-image `axiom_concept/2` fact count must equal the arm file's fact count, and A1's
  atom set must be exactly the 10 `<kernel>__all` atoms — else halt (a silent no-op overlay
  reading as "stable" is the exact false-stability this probe must not produce).
- **Known-changer (probe positive control):** A1 on digital_money_legitimacy must merge the
  baseline `issuance_legitimacy_basis` and `transaction_visibility` vantages into one atom —
  if A1's output equals baseline byte-for-byte anywhere a merge applies, the pipeline did
  not perturb and the run is VOID (census-sees ≠ diff-detects-change).
- Serial runs, one arm × leg per swipl process (no shared-image contamination).

## Files

New: this dir (PROPOSAL, generated arm registries, per-arm results, WRITEUP);
`python/audits/oq75b_grain_probe.py` (generator + runner + comparison).
No canonical file is touched: arms are OVERLAYS loaded after retracting the baked facts
in-process; `prolog/axiom_concept_registry.pl` is never edited.
