# WRITEUP — OQ-151 dual-gauge crosstab + empty-chair refinement census

**Executed:** 2026-08-09
**OQ:** OQ-151 (role-projected gauge + empty-chair detector)
**One-line verdict (scoped to the five live legs at the manifests below):** the
typed empty-chair detector partitions the ~613 mcc candidates into ~1.5% genuine
typed-dissent (9 stories), ~36% old-false-positive class (untyped chair,
`excluded_untyped` 219), ~62% typed-chair-concurs — with the Σ identity exact and
every structurally-impossible cell zero on all five legs; the dual-gauge crosstab
realizes both off-diagonal cells (observer-glues/parties-fracture and
observer-fracture/seated-consensus) as small, per-item-verified strata.
**Manifests:** all five legs regenerated this session at shared
`code_commit=3607086f` (`code_dirty:true` expected-and-explained — adjudication
in `audit_log.md`): testsets n=240 @2026-08-10T04:09:52Z; haiku n=960
@04:04:54Z; flash n=960 @04:05:57Z; kimi n=1005 @04:07:08Z; sonnet n=1001
@04:08:13Z. Every leg md5-fingerprint-bracketed FROZEN around its run
(`audit_log.md`).
**Detector under audit:** `stakeholder_seats:empty_chair_state/2` @ `e07fba7b`.

## Evidence map

- `PREREGISTRATION.md` — cell semantics, expected-zero declarations, Σ identity,
  per-item plan; md5 `ad6b73e4a2afadd1933af325110ddbfb` recorded in
  `audit_log.md` BEFORE the first crosstab line (amendment 5).
- `audit_log.md` — prereg md5, adjudicated engine-path dirty status, per-leg
  fingerprint brackets, crosstab run record.
- `oq151_freshness_regen.py` — the serialized four-leg `classify_corpus` driver
  (session tool; canonical testsets leg via `run_pipeline.py`).
- `crosstab_<leg>.json` ×5 + `crosstab_joined.md` — the 3×3 h1_band ×
  h1_stakeholder tables, cells carrying ids, null strata with reasons
  (produced by `python/audits/oq151_dual_gauge_crosstab.py` @ `3607086f`).
- `refinement_census_<leg>.log` ×5 — full `empty_chair_state` histogram, mcc
  partition, `SIGMA_IDENTITY ... ok`, `EXPECTED_ZERO ... ok` ×3, and
  `DISSENT_ITEM` per-item re-derivations (harness: `refinement_census.sh`).
- `per_item_verification.log` — 47 off-diagonal members re-derived in swipl
  (harness: `per_item_offdiag.sh`); checked-consistent count below.
- `consumer_sweep.txt` — the amendment-1 sweep of
  `manufactured_consensus_candidate` consumers.
- `probe_mc_cases.pl`, `probe_seat_sweep.pl` — the retired untracked prototypes,
  archived with defect headers; originals deleted from `prolog/`.
- `breadth_leg.sh` — the Commit-1 six-leg breadth harness (for provenance).

## 1. Refinement census (the OQ-151 headline)

Exhaustive 8-token partition of the mcc candidate set (Σ == |mcc| exact on
every leg; all three structurally-impossible cells zero on every leg — no
Pattern-2 fork between the detector and `consensus_provenance/2`):

| leg | mcc | dissent | dissent_untyp. | excluded_untyped | concurs | concurs_untyp. |
|---|---|---|---|---|---|---|
| testsets | 48 | 0 | 0 | 20 | 28 | 0 |
| haiku | 101 | 0 | 3 | 49 | 44 | 5 |
| flash | 105 | 2 | 0 | 36 | 59 | 8 |
| kimi | 85 | 2 | 0 | 19 | 62 | 2 |
| sonnet | 274 | 1 | 1 | 95 | 167 | 10 |
| **Σ** | **613** | **5** | **4** | **219** | **360** | **25** |

- **Genuine typed dissent is rare: 9/613 (~1.5%).** Every one re-derived
  per-item (`DISSENT_ITEM` lines): dominant shapes are
  `scaffold`-room/`mountain`-chair (3×) and `naturalized`-room/
  `tangled_rope`-chair (4×), plus `rope`/`tangled_rope` (magna_carta) and the
  OQ-151-original `employment_boundary__substantive_employment_reading`
  (naturalized/[tangled_rope] — the ONE case the original 4/5 hand-check ruled
  genuine, independently recovered by the shipped detector on the sonnet leg).
- **The old false-positive class is large: 219/613 (~36%)** — had the
  prototype's unfiltered condition shipped, more than a third of all
  "manufactured consensus" firings would have been untyped-chair absences
  (the 4/5 trap at corpus scale).
- Typed chairs mostly CONCUR (385/613 incl. `_untypeable`): an authored
  excluded seat usually reads the constraint the way the room does; the
  detector makes dissent the marked, checkable case.
- `testsets/` carries zero dissent — the live singleton leg's mcc candidates
  are all concurs/untyped; the dissent stratum lives on the bigger legs.

## 2. Dual-gauge crosstab (h1_band × h1_stakeholder)

Full tables: `crosstab_joined.md`. Both pre-registered off-diagonal cells are
REALIZED but small:

- **(0, >0) observer glues, parties fracture** — testsets 1, haiku 14, flash 0,
  kimi 0, sonnet 3. This is the realizable form of OQ-151's "role-H¹>0 ∧
  power-H¹=0": the canonical observer orbit reads one type while the authored
  seats disagree. Concentrated on constitutional/interpretive kernels (haiku:
  commerce clause, takings, war powers...).
- **(>0, 0) observer fracture over seated consensus** — testsets 3, haiku 6,
  flash 18, kimi 5, sonnet 9. Per prereg, intersected with
  `empty_chair_state`: on testsets, 2× `no_excluded_seat` + 1×
  `excluded_concurs` (`radiative_levitation_stratification` — the OQ-136
  hand-read false positive, now machine-readable as a CONCURRING typed chair,
  converging with the 2026-07-02 hand-read).
- Null strata carried with reasons (never coerced): h1_band null =
  `undetermined/insufficient_seats` throughout; h1_stakeholder null =
  `n_real∈{0,1}`. The haiku (pos, null) cell is large (341) — a fracture
  visible to the observer orbit over stories whose seats are mostly untypeable;
  a reader coercing null→0 would have misread all of it (the OQ-51/OQ-207
  trap the script's raise-on-missing guards against).
- **Per-item verification: 47/47 sampled off-diagonal members re-derived in
  swipl match their crosstab cell (0 mismatches)** — testsets ALL 4; haiku
  10/14 ZP + all 6 PZ; flash all 0 ZP + 10/18 PZ; kimi all 5 PZ; sonnet all
  3 ZP + all 9 PZ (`per_item_verification.log`). Silent cap declared: the
  unsampled remainder (4 haiku ZP, 8 flash PZ) is bounded by the same
  generating predicates the samples verified.

## 3. Consumer sweep of the unfiltered mcc set (amendment 1)

`consumer_sweep.txt` (70 hits; live code + trackers + historical audit dirs).
**No live recorded claim rests on the unfiltered candidate set read as typed
dissent:**

- Live code consumers (`commentary_census.pl` bucket map,
  `json_report.pl` — which deliberately does NOT serialize the verdict,
  OQ-204 D6) treat the token as a candidate flag only.
- ISSUES.md OQ-204 ("heavily live: 12/50/39") cites CANDIDATE counts,
  correctly labeled; the OQ-204 surface build is pre-warned and now gains
  `empty_chair_state/2` as its refinement source (Commit-3 instrument note).
- ISSUES.md OQ-136 resolution ("manufactured_consensus_candidate (9) =
  genuine, 8/9") is anchored to its own hand-read with in-file witnesses at
  n=119 — not to the unfiltered set's dissent implication; its one text-ruled
  false positive (radiative_levitation) is exactly the story the detector now
  reads `excluded_concurs` (§2), a convergent, not contradicting, result.
- Historical audit dirs (OQ-207/OQ-217/OQ-136/OQ-261) are point-in-time.

=> Zero correction lines owed from this sweep ("finding the number and not
checking who spent it is how the 0.245 got where it is" — here the check ran
and found no spender).

## 4. Probe disposal

`probe_mc_cases.pl` (power-gauge H¹ gate + no is_real_type filter — both
defects named in its header) and `probe_seat_sweep.pl` (corpus-pooled
histograms, inherits story typing) archived here; untracked originals deleted
from `prolog/`.

## Scope and residue

- All numbers are leg-relative at the cited manifests; five independent
  tables, never merged (GAP-31). Nothing here is a dominance/prevalence claim
  about "the corpus."
- `empty_chair_dissent*` is a structural-footprint CANDIDATE stratum (OQ-203:
  the vocabulary cannot distinguish structural from evidential exclusion);
  per-item dissent lines name chairs and types, not verdicts.
- The role-gauge H¹ itself was DECLINED (operator ruling; grounds in the
  OQ-151 close entry — this crosstab is the dual-gauge deliverable that
  replaces it).
