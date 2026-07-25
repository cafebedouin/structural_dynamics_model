# OQ-254 Step 0 — Re-witnessed recon claims (2026-07-25)

Agent recon treated as hypothesis; every load-bearing claim re-witnessed below with
pasted output and a positive control. Census script: `manifest_census.py` (this dir);
raw output: `manifest_census_output.json`.

## W1 — Manifests are gitignored and untracked; the uke twin is tracked (control)

```
$ sed -n '1,10p' .gitignore
# Generated pipeline outputs — ANCHORED to top level (2026-06-11, OQ-33 close session):
...
/outputs/
...
$ git ls-files outputs/kernel_manifests | wc -l
0
$ git ls-files agent/narrative_transform/uke | head -5     # control: tracked twin, non-empty
agent/narrative_transform/uke/112_ergodocity_kids_1783916200/112_ergodocity_kids_rev1.md
agent/narrative_transform/uke/112_ergodocity_kids_1783916200/break_contract_output.md
... (non-empty)
```

CONFIRMED — with one precision the recon missed: 22 phase-0 manifests ARE git-tracked at
`audits/2026-06-06_kernel_first_phase0/manifests/` (an audit archive copy, not a read
surface; the live populations under `outputs/` are 0-tracked).

## W2 — Manifest census (my script, not the agent's numbers)

Population: `outputs/kernel_manifests/**` (293) + `outputs/decompose/manifests/` (200)
+ `outputs/kernel_first_phase0/` (22) = **515 scope manifests**. Two root-level
`outputs/*.manifest.json` files (`giant_component_analysis`, `orbit_data`) are
pipeline-output sidecars (single key `manifest`), NOT scope manifests — excluded; their
initial inclusion was caught because they lacked `deferred_axes` entirely.

```
n_manifests            515
deferred_present       515   (100%)
deferred_nonempty      460
deferred_empty          55
axes_total            2598
selection_reason      2590 present / 8 missing
centrality_present    2590
kernel_verdict         486 present (439 true / 47 false)  [nested at
                       commitment_system_recognition.is_contested_kernel — recon's
                       "recorded" CONFIRMED after fixing my own top-level key probe]
deferred_entries      1022, deferral_reason authored on 1022/1022
```

Recon's 460/515 non-empty and 990-entry figures: non-empty count CONFIRMED exactly;
entry total re-measured at 1022 (my count supersedes; recon was hypothesis).
Recon's selection_reason 2594/2602 re-measured at 2590/2598 (same 8 missing).

## W3 — Exhaustive shortfall classification (operator flag: census, not sample)

**55 empty-`deferred_axes` manifests — all 55 classify `legit_nothing_deferred`** by the
mechanical discriminator (empty `deferred_axes` + non-empty `fracture_scan.notes` narrating
the scan result). Validation:
- Hand-read of 5 (bible_genesis_creation, bretton_woods_system, capital_punishment_kernel,
  christian_bible_scripture_authority, constitutional_supremacy): every `notes` field
  narrates a completed clean scan ("No fractures detected...", "Clean scan...") — the
  positive side validates.
- Two-sided planted control: a synthetic fixture with BOTH fields empty classifies
  `unauthored` — the discriminator can emit the other token (`planted both-empty fixture
  classifies as: unauthored / control PASS`). Zero `unauthored` in the real population is
  therefore a measured zero, not a dead branch.

**8 missing-`selection_reason` axes — all 8 examined by field values** (2 manifests):
- `outputs/decompose/manifests/n26usc469_real_estate_exemption.manifest.json` axes 3,4:
  `selected=False` + authored `deferral_reason` — deferred axes carried inline in `axes[]`
  (schema variant); reason authored, in the deferral field. NOT unauthored.
- Same manifest axes 0,1: `selected=True`, no `selection_reason`, no `deferral_reason` —
  **genuinely unauthored (the only 2 in the corpus)**.
- `outputs/kernel_manifests/flat/everything_becomes_taste_20260716_155231.manifest.json`
  axes 0–3: `kernel_reading=True`, no `selected` flag — schema variant where selection
  rationale is the kernel-level verdict (`commitment_system_recognition` block present).
  Authored at the kernel level, not per-axis.

Net: authored-rationale coverage is 2596/2598 axes with exactly 2 unauthored; "no rationale
authored" is a real but near-empty stratum.

## W4 — epsilon_provenance arg-4 (`generation_run_id`) is 'none' in all live emissions

```
$ grep -rh "epsilon_provenance(" prolog/testsets/*.pl | wc -l
71
$ grep -rh "epsilon_provenance(" prolog/testsets/*.pl | grep -c ", 'none',"
71
$ ... | grep -v ", 'none'," | wc -l
0
$ echo "...epsilon_provenance(x_reading, 0.5, 'claude-sonnet-5', 'fam_20260725_000000', direct)." \
    | grep -v ", 'none'," | wc -l
1        # planted control: a non-none line WOULD survive the filter
```

CONFIRMED: 71/71 'none'. (First grep attempt used unquoted `, none,` and returned 0 —
caught by pasting a sample line; the corrected quoted pattern is what's controlled above.)

## W5 — `_last_manifest_path` is write-only; `deferred_axes` has no live consumer

```
$ grep -rn "_last_manifest_path" agent/ python/ prolog/
agent/c-orchestrator.py:264:                self._last_manifest_path = str(self.manifest_file)
agent/c-orchestrator.py:512:            self._last_manifest_path = str(path)
```
Two writes, zero reads. CONFIRMED.

`deferred_axes` consumers in code (excluding manifests themselves and this audit):
`python/audits/scope_count_distribution_probe.py` + its archived copy in
`audits/2026-06-05_scope_count_distribution/` — two audit probes, nothing live. CONFIRMED.
(The uke narrative pipeline's tracked `scope_manifest_output.md` files also contain the
token — that is the OTHER pipeline, and is the tracked-twin contrast, not a consumer.)

## Verdict on the OQ-254 headline

The Q-choice IS declared, richly (selection_reason 2596/2598 counting schema variants,
deferral_reason 1022/1022, kernel verdict 486/515) — but on a gitignored surface with no
self-provenance and no story-side join (`generation_run_id` = 'none' in 71/71 live
emissions; `_last_manifest_path` write-only). The defect is Pattern 6 (a read site cannot
distinguish "no rationale recorded" from "rationale on someone's laptop"), not concealment.
OQ-254's headline as written ("the discipline audits the answer, not the question") is
FALSE — the question is audited, just not joinably. To be marked on the entry at close.
