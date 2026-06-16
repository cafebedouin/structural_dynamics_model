# Census × perturb adapter — config-sensitivity of the commentary census

**Date:** 2026-06-16  **Tool:** `python/sweeps/census_sweep.py`  **Corpus:** live `testsets` (n=72, hash 790de8638207)

Pairs the perturb.py overlay method (retract/asserta a `config:param`, run a goal, diff vs baseline)
with the commentary census as the measurement surface. Each perturbation re-runs
`run_commentary_census` under the overlaid param and diffs per-source bucket histograms + the three
quantities the census separates: `n_in_domain`, `coverage`, `prevalence`.

## Harness validity

- **Null control** (`snare_epsilon_floor` → its baseline 0.46): census inert, all Δ=0. The tool
  asserts this and fails loud otherwise — proof the overlay/parse/diff machinery does not manufacture
  diffs (the perturb.py inertness discipline).

## Findings (the test was designed to reveal what a single rate would hide)

### 1. The two census surfaces have ORTHOGONAL config-sensitivity
- `snare_epsilon_floor`↑ and `snare_chi_floor`↑ MOVE q6 (snare↔tangled redistribution: e.g.
  live_snare 8→2, live_tangled 6→12) but leave **extraction inert** — because snare and tangled are
  *both* `extractive_type`, so the extractive domain is invariant to the snare/tangled boundary.
- q6 is sensitive to *within-extractive* boundaries; extraction is sensitive only to the
  *extractive ↔ non-extractive* boundary. A param moves one, the other, or both — and the census
  shows which.

### 2. Prevalence can rise PURELY by domain-shrink — zero new blindspots (the headline)
`tangled_rope_chi_floor` 0.35 → 0.85:
- `extraction_blindspot_fired`: **3 → 3 (unchanged)**
- `extraction_clear`: 42 → 37; `extraction_out_of_domain`: 22 → 27; `n_in_domain`: 50 → 45
- **`prevalence`: 0.060 → 0.067 (+12%)**

5 extractive-with-victim constraints fell out of the extractive domain (became non-extractive when
the tangled χ-floor rose), shrinking the denominator. The blindspot rate ROSE with **not a single new
blindspot firing**. A single "blindspot prevalence" number would read this as a signal; the census's
separation of `fired` / `n_in_domain` / `prevalence` shows it is a denominator artifact. **This is the
thing the test was built to catch and it is real on the live corpus.**

### 3. q6 coverage decomposes into a config-INVARIANT and a config-VARIANT absence
Same perturbation: q6 `coverage` 0.611 → 0.583, driven ENTIRELY by:
- `q6_signature_unknown`: 8 → 10 (computed-side absent — dr_type became `unknown`: **config-variant**)
- `q6_unmeasured`: 20 → 20 (authored-side absent — **config-invariant**, a pure authoring fact)

So q6's "coverage" is not one number: its authoring component is a fixed corpus property, its
computational component moves with thresholds. Reading q6 coverage as a single authoring-completeness
figure is wrong when thresholds change.

### 4. config_validation bounds the reachable sweep surface (relational invariants)
Two perturbations were CONFIG-REJECTED (config_validation halts on load):
- `snare_epsilon_floor` 0.20 — "rope_epsilon_ceiling must be < snare_epsilon_floor"
- `tangled_rope_extraction_floor` 0.95 — "must be < tangled_rope_extraction_ceil"
A single threshold cannot be perturbed past its neighbor; the tool records the rejection (a fact about
config geometry) and continues rather than aborting.

### 5. On this corpus the extractive domain's binding edge is the χ-floor, not the ε-floor
`tangled_rope_extraction_floor` 0.16 → 0.50 was INERT (extractive constraints all sit at ε ≥ 0.50),
while `tangled_rope_chi_floor`↑ shrank the domain by 5. The extractive domain is robust to the ε-floor
but sensitive to the χ-floor here.

## Why it matters (for OQ-136)

OQ-136 will read census bucket counts/rates to decide authoring-gap vs genuine-category. Finding 2 is
a standing caveat for that work: **a prevalence/rate comparison across any config (or schema-refit)
change must hold the domain fixed, or report `fired` count and `n_in_domain` separately** — otherwise a
denominator shift masquerades as a finding. Finding 3 sharpens it: q6 coverage must be read as two
components, not one.

## Artifacts

- `census_sweep.json` — full machine output (baseline + 7 perturbations, with config-rejections).
- `sweep_run.txt` — the console run.
- Tool: `python/sweeps/census_sweep.py` (`--param`/`--to` for one-offs; `--corpus` to overlay a twin).
