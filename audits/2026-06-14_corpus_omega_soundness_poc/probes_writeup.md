# Blind-Executor writeup — corpus omega soundness POC (2026-06-14)

Executor role: BLIND. Did NOT read `adjudicator_held_scores.json`. Read-only over
`prolog/testsets_haiku/*.pl`. No `run_pipeline`, no shared-`outputs/` writes. Engine probing
wrote only to `/tmp` and this audit dir.

Corpus: `testsets_haiku/` — 960 stories. Overlay `asserta(config:param(corpus_path, testsets_haiku))`
took effect: `corpus_constraint/1` count == 960 (NOT the default-44 trap). Confirmed below.

## Recon reproduction (witness)
`python3 /tmp/recon_omega.py`:
```
stories=960  total /3 omegas=4430  distinct names=3755
per-story /3 count: min=0 max=8 median=5.0 mean=4.61
  2740 empirical / 1588 conceptual / 102 preference
distinct names occurring once: 3598/3755
TOP near-dup /5 RESOLUTION text prefixes: max-dup = 2
```
All recon numbers reproduced exactly.

---

## PROBE 1 — three-axis alignment (headline)

Reference partition = `cs_kernel_id` (authored). Parsed from all 960 files:
331 distinct kernels, mean size 2.90, max 5, only 3 singletons. JOIN CHECK: engine
constraint id == kernel key for **960/960** (clean join; no fabricated mapping).

PRE-REGISTRATION (written before running):
- KIND surfaces (1a/1b) expected to CROSS-CUT kernel (low ARI). If so -> engine structural
  organs are structure-semantic, kernel is topic-semantic, witnessed-orthogonal -> existing
  engine organs CANNOT be the frontier-identity organ.
- SURPRISE (high value): a KIND surface ALIGNS with kernel -> structural kind predicts topic.
- 1c aligning with kernel tells us little (both topic-ish). 1c MISALIGNING is POSITIVE evidence
  of a third (frontier) axis. Lexical proxy = LOWER BOUND on frontier-merging.

POSITIVE CONTROL (engine fired): `logical_fingerprint` returned
`shift(tangled_rope,snare,rope,snare)` on `abrahamic_covenant__isaac_covenant_reading`.
Coverage: 0/960 OPEN on all three engine surfaces (shift, dr_type, gauge_orbit).

Predicate signatures verified before use (per the CAUTION):
- `logical_fingerprint:logical_fingerprint(C, fingerprint(Shift,...))` — Shift is arg-1 of FP.
- `drl_core:dr_type(C, Type)` /2 used (the /3 form is `dr_type(C, Context, Type)` — arg2 is
  Context, arg3 Type, as the caution warned; the /2 derives context internally).
- `dirac_classification:gauge_orbit(C, OrbitPoints)` /2 — grepped to the real module.

### Results
| surface | clusters | ARI vs kernel | homogeneity | same-kernel→same-cluster | verdict |
|---|---|---|---|---|---|
| 1a signature+fingerprint (shift) | 61 | **-0.0004** | 0.35 | **7.65%** (73/954) | CROSS-CUT (predicted) |
| 1b orbit+classification (drtype‖orbit) | 61 | **-0.0004** | 0.35 | **7.65%** (73/954) | CROSS-CUT (predicted) — IDENTICAL to 1a |
| 1c lexical frontier (4430 omegas) | 2901 | **-0.0010** | 0.69* | 2.05% (593/28957) | MISALIGNED → positive 3rd-axis evidence |

\* 1c homogeneity is INFLATED by 2901 near-singleton clusters (sklearn warned
clusters>50% of samples); for 1c only ARI + family-span are load-bearing. 1a/1b homogeneity
(0.35 over 61 clusters) IS meaningful.

VERDICTS:
- **1a/1b = CROSS-CUT, strongly.** ARI ≈ 0 (chance / slightly negative). Of 954 same-kernel
  sibling pairs only 73 (7.65%) share a structural cluster — the engine's structural organs
  SPLIT same-kernel siblings and MERGE cross-kernel same-structure constraints. Pre-registered
  prediction confirmed: the engine's existing organs CANNOT be the frontier-identity organ.
  **No SURPRISE** (no KIND surface aligned with kernel).
- **1b is IDENTICAL to 1a** — gauge_orbit and fingerprint-shift encode the same KIND structure
  (61 distinct each, byte-identical top-count profile); the drtype prefix added zero resolution.
  Computed cleanly; NOT OPEN.
- **frontier(1c)↔kernel = MISALIGNED (positive 3rd-axis evidence).** ARI ≈ 0; suppression
  family alone spans **264 distinct kernels** (brief estimated ~219 — actual higher);
  kernel_reading spans 258. Reported as positive evidence of the frontier axis, not as a 1c
  failure. Lexical proxy is a lower bound (misses synonym families).

---

## PROBE 2 — §8-replication / content-templating (cheapest falsifier)

PRE-REG: low near-dup → authored omegas NOT §8-style content artifacts (soundness ✓ on content
axis; artifact would be identity-overstatement, not fabrication). Unexpectedly high → flips to a
§8-class finding, say so loudly.

Metric: normalized prefix bucketing (40/60/100 chars) on question and approach text, plus
combined q+approach, plus exact word-8-gram overlap across the 4430 /5 records.

Witness:
```
question prefix=60: max-dup=5   (prefix=100: max-dup=1)
approach prefix=60: max-dup=2   (prefix=100: max-dup=1)   <- reproduces recon max-dup=2
combined q+approach[60]: max-dup=1, dups>1 = 0  (every omega has a UNIQUE q+approach pair)
8-grams in >1 record: 226 of 257,887 distinct (0.09%) — all the kernel-contest boilerplate
  opener "this constraint is one reading of a contested kernel"
```
The 40-char prefix dups (max 9–18) are shared FRAMING openers
("comparative institutional analysis:", "is the measured suppression (0.71)"), not full-content
duplication — they vanish by 60–100 chars.

**VERDICT: LOW.** Authored omegas are NOT §8-style content artifacts. Soundness holds on the
content axis. The only shared text is a thin template opener, not duplicated reasoning.

---

## PROBE 3 — soundness spot-check (BLIND, 30-sample)

Scored each of `/tmp/sample_30.json` (verified 30 records, 15 E / 12 C / 3 P) independently
against `docs/omega_variables.md` four properties (Clarity / Boundedness / Irreducibility /
Unlockability) + Ω-type resolution-mode match. SOUND = all four PASS AND type_correct AND routes
OUT. Full ledger in `probe3_executor_scores.json`.

Shares:
- **Overall: 24/30 SOUND = 80.0%** (3 TYPE_INCORRECT, 3 UNSOUND).
- empirical 13/15 = 86.7% (2 type-incorrect: #1 hybrid leaning conceptual, #24 empirical
  mislabelled preference).
- conceptual 9/12 = 75.0% (3 unsound: #14, #18, #25 — all RESTATE authored deltas / kernel
  contest with no exiting resolution → fail Irreducibility/Unlockability).
- preference 2/3 = 66.7% (1 type-incorrect: #24).

The 3 UNSOUND are the `kernel_reading_contest`/contestation-space family: their "resolution" is
"generate all the sibling readings" or they re-state the already-authored ε/victim deltas —
documentation, not an external blocker. (n=3 in conceptual; sample-based, BOUNDS not proves.)

This is sample-based: it BOUNDS corpus soundness, does not prove it.

---

## PROBE 4 — agenda shape (descriptive, ASPIRATIONAL)

Witness:
```
total omegas=4430  name-keyed unique=3755
type dist: empirical 2740 / conceptual 1588 / preference 102
semantic-dedup LOWER BOUND on distinct frontiers = 2901
  6 family buckets absorb 1485 omegas (from ~1539 distinct names down to 6):
    suppression 456, kernel_reading 409, reading_contest_misc 256,
    identity_lock 210, founding_problem 88, natural_law 66
  remaining singleton frontiers: 2895
```
Collapse rules (substring match on normalized name) in `probe4_agenda.json`.

**Framing (ASPIRATIONAL — corpus is generated specs, not a tracked research log):**
"If this were an agenda, it is ~**2,901 distinct frontiers**, ~**61.9% Ω_E / 35.8% Ω_C /
2.3% Ω_P**."

---

## OPEN / failed surfaces
- NONE failed. All three engine surfaces computed 0/960 OPEN. 1b is not OPEN — it computed and
  came out identical to 1a.
- Caveat (not a failure): 1c homogeneity is not meaningful (singleton-cluster inflation; sklearn
  warned). Load-bearing 1c numbers are ARI and family-span.
- Tooling note: `/tmp/recon_omega.py` runs top-level code on import with a cwd-relative glob, so
  it crashes if imported from repo root; all Python probes here run from `prolog/` (or inline the
  parse functions). The engine CSV header used `~t` (column-fill, not a literal tab) so its
  header line is unsplit — data rows ARE real-tab-delimited and were parsed by skipping the
  header. No data affected.
```
```
