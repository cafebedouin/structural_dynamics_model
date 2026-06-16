# Census perturbation sweep — using the commentary census as a perturb measurement surface

Added 2026-06-16. Documents a reusable technique and the one measurement trap it exists to
catch. Scope: how to perturb a `config:param` and read the effect through the commentary census
(`commentary_census.pl`, OQ-134/OQ-121) instead of the product-site chi export, and the
denominator confound that makes a naive reading wrong. Tool: `python/sweeps/census_sweep.py`.
Companion: `perturb_substrate.md` (the chi-export side of the same overlay method).

---

## The technique

`perturb.py`'s method is corpus-independent of its *measurement* surface: write an overlay that
`retract`/`asserta`s a `config:param`, run a Prolog goal, diff the output against a baseline.
`census_sweep.py` reuses that overlay method but swaps the goal to `run_commentary_census` and
diffs the **per-source bucket histograms** plus the three quantities the census separates:
`n_in_domain`, `coverage`, `prevalence`.

```
python3 python/sweeps/census_sweep.py                          # curated default sweep
python3 python/sweeps/census_sweep.py --param snare_epsilon_floor --to 0.85
python3 python/sweeps/census_sweep.py --corpus testsets_haiku  # overlay a twin
```

Why the census is a good surface (not just a possible one):
- **Commentary-grade** — it never feeds classification, so the sweep is pure observation of how
  config moves the *reading*. No feedback to reason about.
- It **already carries `coverage`/`n_in_domain`** — `perturb.py`'s blind-vs-stable disambiguator
  (OQ-29). A flat census *with coverage reported* is INERT, not a false "stable".
- It **separates coverage / prevalence / domain size**, which is the whole point (see the trap).

Overlay mechanics (same as `perturb.py`): `retractall(config:param(P,_)), asserta(config:param(P,V))`.
One swipl process per perturbation (the serialization rule); each run self-loads the corpus via
`run_commentary_census -> ensure_corpus_loaded`. The overlaid param takes effect because `dr_type`
(and hence q6's named cells and extraction's domain) is computed at query time, after the overlay.

**Built-in positive control:** the default sweep's first entry is a *null perturbation* (re-apply
the baseline value). It MUST produce a zero diff; the tool raises if it doesn't. A non-zero
null-control diff means the overlay/parse/diff machinery is manufacturing differences — the same
discipline `perturb.py` enforces with its inertness controls.

---

## The trap this exists to catch: a rate can move purely by domain-shrink

**This is the reason to use the census's three-quantity split instead of a single rate.** A
measurement surface whose *domain* is config-variable (extraction applies only to
`extractive_type` constraints) lets a config change move a **rate** with no change in the
underlying **finding**.

Witnessed (live corpus, n=72): `tangled_rope_chi_floor` 0.35 → 0.85
- `extraction_blindspot_fired`: **3 → 3 (unchanged)** — not one new blindspot.
- `extraction_clear` 42 → 37; `extraction_out_of_domain` 22 → 27; `n_in_domain` 50 → 45.
- `prevalence`: 0.060 → 0.067 (**+12%**).

Five extractive-with-victim constraints fell out of the extractive domain when the χ-floor rose,
shrinking the denominator. The blindspot *rate* rose while the blindspot *count* held at 3. A
single "prevalence" number reads this as a signal; the census shows it is a denominator artifact.

**Rule (applies to ANY config / schema-refit / cross-corpus comparison of census numbers):**
report the raw `fired` count and `n_in_domain` ALONGSIDE any rate, or hold the domain fixed.
Never compare bare rates across a change that can move the domain. (This is the standing caveat
on OQ-136, whose clustering test must use raw counts per model/run-tag/topic, not rates.)

---

## Other facts the sweep surfaced (useful when reading its output)

- **q6 `coverage` is two components, not one.** `q6_unmeasured` (authored-side absent) is
  config-INVARIANT — a fixed corpus authoring fact. `q6_signature_unknown` (computed-side absent,
  `dr_type` became `unknown`) is config-VARIANT (8 → 10 under the same perturbation above). Do not
  read q6 coverage as a single authoring-completeness figure when thresholds change.
- **The two census surfaces have orthogonal config-sensitivity.** `snare_epsilon_floor` /
  `snare_chi_floor` move q6 (snare ↔ tangled redistribution) but leave extraction INERT — snare and
  tangled are *both* `extractive_type`, so the extractive domain is invariant to that boundary. q6
  is sensitive to within-extractive boundaries; extraction only to the extractive ↔ non-extractive
  boundary. On the live corpus the extractive domain's binding edge is the **χ-floor, not the
  ε-floor** (`tangled_rope_extraction_floor` 0.16 → 0.50 was inert).
- **`config_validation` bounds the reachable single-param surface.** A threshold cannot be
  perturbed past its neighbor — `config_validation` halts on load (e.g. "rope_epsilon_ceiling must
  be < snare_epsilon_floor"; "tangled_rope_extraction_floor must be < tangled_rope_extraction_ceil").
  `census_sweep.py` catches the halt, records the perturbation as `config_rejected`, and continues
  (a fact about config geometry, not a sweep error). To move a threshold past a neighbor you must
  co-perturb the neighbor in the same overlay.

---

## When to read this

Before sweeping config against the commentary census, or before comparing any census `coverage` /
`prevalence` across a config, schema-refit, or corpus change. Full run + witnesses:
`audits/2026-06-16_census_sweep/`. The census itself: `prolog/commentary_census.pl`
(header documents the three bucket kinds + the coverage/prevalence/domain split).
