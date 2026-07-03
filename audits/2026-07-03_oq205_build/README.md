# OQ-205 build-phase graduation audit (2026-07-03)

The ε declaration discipline build (spec: `docs/design/epsilon_declaration_discipline.md`,
rulings R2–R4 ratified 2026-07-03). Eleven commit units U1–U11; per-unit witnesses are in
each commit message (commits `e9041905`…, this dir holds the transcripts and the all-legs
artifacts). Graduation criteria = spec §9.

## Commit units

| Unit | Commit | What |
|---|---|---|
| U1 | `e9041905` | `get_true_metric` ε absence → `unknown`, never fabricated 0.0 (spec §3 fix 1) |
| U2 | `60a69b53` | `classify_at_context_impl` fails closed — no BaseEps=0.5 / Supp=0 (spec §3 fix 2) |
| U3 | `ac14bdde` | `epsilon_provenance/5` schema + compiler emission (generator-forward) |
| U4 | `c55afb5d` | three-site equality + loud-null census checkers in data_validation |
| U5 | `0e55ff01` | fail-fast gate suite `tests/test_epsilon_declaration.pl` + pipeline gate |
| U6 | `18e6c437` | provenance emission into pipeline_output.json (explicit loud-null token) |
| U7 | `f143072f` | data-side ε-stability sweep + Control S (r=0.02, R3) |
| U8 | `3a69a8d8` | sweep wired into pipeline + enrich + report/sidecar consumers |
| U9 | `acd46a02` | Control P fixtures through the real load path (gate second swipl) |
| U10 | `006f1d04` | OQ-78 standing authorship readout (spec §8) |
| U11 | (this) | graduation audits + docs + close |

## Controls (spec §6) — green THROUGH the recurring gate

- **Control P** (`control_p_green.txt` / `control_p_break.txt`): the four fixture stories
  run through the real `corpus_loader` path inside `_prolog_epsilon_declaration_gate`
  (second swipl, fresh process, `corpus_path` overlay before load). Green side: violations
  equal EXACTLY the planted set — drift AT `eps_ctl_drifted`, loud-null AT
  `eps_ctl_missing`, `eps_ctl_clean` flag-free (two-sided), `eps_ctl_no_epsilon` reads
  ε = unknown (the U1 fallback is DEAD, not rerouted). Break side: retracting the planted
  drift makes the planted-set equality THROW (exit 2) — the pass catches a checker that
  stopped firing.
- **Control S** (`control_s_selftest_live_leg.txt` + the per-leg transcripts below): the
  sweep's selftest runs FIRST on every invocation, fail-closed — near-threshold plant,
  band-interior two-sided arm, override-locked plant, and the riskiest-shape shadow plant
  (perturbing `carbon_tax_2026`'s `constraint_metric` must be CAUGHT by the took-effect
  guard: its direct `drl_core:base_extractiveness/2` fact is clause 1 and wins the
  first-solution read). `once/1` in the guard is load-bearing — an unpinned read
  backtracks past the shadow and lies (witnessed during the build; fixed same day).
- **Gate deliberate-break** (U5 commit): a scratch overlay planting a drifted
  `epsilon_provenance` on a real story turned the suite RED at
  `no_epsilon_provenance_drift`, naming the planted story — through the same `run_tests`
  path the pipeline gate calls.

## All-four-legs stability sweep (criterion 3: per-corpus)

`epsilon_stability_testsets_haiku.json`, `epsilon_stability_testsets_flash.json`,
`epsilon_stability_kernel_v1.json` (this dir; live leg artifact =
`outputs/epsilon_stability_results.json`, regenerated every pipeline run). Selftest 4/4
PASS and tripwires PASS on every leg. `on_threshold_grid` matches the 2026-07-03 census
exactly-at column on EVERY leg — the flag classes read the same facts the census read:

| Leg | swept | on_grid (census exactly-at) | near | locked | off_grid |
|---|---|---|---|---|---|
| testsets (live) | 110 | 1 (1) | 3 | 1 | 43 |
| testsets_haiku | 960 | 9 (9) | 47 | 6 | 236 |
| testsets_flash | 960 | **218 (218)** | 0 | 4 | 126 |
| kernel_v1 | 1106 | 4 (4) | 36 | 6 | 452 |

The flash 218 rides the `on_threshold_grid` class exactly as the R3 amendment intended:
`near_threshold` (the R3-tripwire-counted class) is 0 there — the convention does not swamp
the signal, and the T2 tripwire stayed advisory-eligible but never fired on any leg.

**Finding surfaced by the sweep (not in the plan's three named classes):**
`unstable_off_grid` — FT flips under ε±0.02 while ε is >r from every ε-threshold (a χ-gate
crossing: χ = ε × f(d) × σ(S) crosses a χ-threshold even where ε is band-interior) — is the
LARGEST flag class on every leg (43/110 live = 39%; 236/960; 126/960; 452/1106). Emitted as
its own class rather than silently dropped (Pattern 6). Reading: ε-sensitivity of the final
type is mostly NOT an ε-threshold-proximity phenomenon on these corpora; the χ product is
the dominant crossing surface. This is a corpus-level analytical finding for OQ-78/OQ-48
consumers, not a defect.

## §4 interior-site fail-closed audit (criterion 2: ALL rows)

Post-U1/U2, each named interior read site verified fail-closed on missing ε (code read at
the U11 commit's tree; no local fallbacks):

- `logical_fingerprint.pl` `extraction_zone/2` — takes ε as an argument; its only caller
  `fingerprint_zone/2` guards the read: `(drl_core:base_extractiveness(C,E) -> ... ;
  ExtrZone = unknown)` — typed absence token, no fabrication.
- `arakelov_height.pl:~100` `arakelov_height_pair/3` — first body goal is
  `constraint_data:base_extractiveness(C, Eps)`; no fallback clause; absence fails the
  predicate (consumers already handle absence).
- `constraint_indexing.pl:511–533` `resolve_coalition_power/3` — the ε read is
  `(constraint_metric(...) -> floor-check ; false)`: absence = explicit `false` = no
  coalition upgrade, falls to the identity clause. Fail-closed.
- `boltzmann_compliance.pl:507–515` `excess_extraction/2` — reads
  `constraint_metric/3` (or the classify_at_time nb_getval); absence fails the predicate;
  no fallback.

The two §3 fabrication sites (`get_true_metric` 0.0; `classify_at_context_impl` 0.5/0) are
fixed at U1/U2 with all-four-corpora byte-identical witnesses and positive-control probes
(see those commit messages).

## Criterion 1: the named provenance-poor strata (never papered over)

`epsilon_provenance/5` is corpus-complete **or loud-null**; the loud-null arm is the ruled
second criterion-1 arm (operator, 2026-07-03: NO backfill of any `testsets*` leg — the
discipline lands generator-forward; corpus-complete arrives at the future rebuild). The
declared provenance-poor strata:

1. **The entire pre-build corpus** — every current story on all three live legs + the
   archives: `missing_epsilon_provenance` census = 110 on the live leg (+1 hand-authored
   `constraint_instances.pl` literal, carbon_tax_2026, via the checker's own domain), all
   960+960+1106 on the twins/kernel_v1. Surfaced per-story as the emission's
   `"none_authored"` token and counted at the gate every run.
2. **The OQ-89 0.5-neutral stratum** — the historical `BaseEps = 0.5` fabrication class
   (kernel_v1-era coupling classifications where the extractiveness metric was missing);
   the U2 fix retires the mechanism; archived outputs that carry it remain regime-bound
   (never re-cite without the U2 boundary).
3. **The ~94/116 legacy under-vectored `json/` class** (OQ-78's original census) — json
   specs whose ε was authored before the vectoring discipline; their provenance is
   reconstructible only to `derived(story_provenance)` granularity, never to Route.
4. **`unknown_author` degradation** — kernel_v1 carries zero `story_provenance/8` on disk;
   the readout counts it as its own stratum rather than erroring (witnessed in the U10
   run: the 9 live-leg no-story_provenance contradictions meta-files land there too).

## Remaining spec obligations

- v8 §9.5 leg 2 annotated built (one line, cites this dir) — U11.
- Spec §3/§9 amendment recording the no-backfill ruling — U11.
- ISSUES.md OQ-205 status → the build criteria met; OQ-78 gains the standing readout — U11.
