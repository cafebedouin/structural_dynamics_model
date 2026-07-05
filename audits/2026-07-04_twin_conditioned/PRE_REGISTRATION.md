# Pre-registration — Twin conditioned re-analyses (OQ-125 Track A, OQ-123 Track B)

**Date frozen:** 2026-07-04, committed BEFORE any conditioned analysis runs.
**Plan:** `~/.claude/plans/goofy-soaring-lemon.md` (operator-approved). Parent audit:
`audits/2026-06-13_twin_comparison/` (pre-registration + FINDINGS there).
**Harness:** `python/audits/twin_comparison.py`, extended additively (no fork; Build
Discipline Pattern 2). Conditioned analyses are new flags; the unconditioned path is
untouched and regression-witnessed (below).

## Substrate pins (verified this session)

| input | pin |
|---|---|
| `outputs/pipeline_output.haiku.json` | sha256 `c3150612801eeed1…`, commit `bbf5c92`, n=960, schema 2, corpus_path `testsets_haiku`, run 2026-06-27 |
| `outputs/pipeline_output.flash.json` | sha256 `e6c01619c4e3bada…`, commit `bbf5c92`, n=960, schema 2, corpus_path `testsets_flash`, run 2026-06-27 |
| `prolog/testsets_haiku/` / `prolog/testsets_flash/` | 960 / 960 `.pl` files |
| `prolog/testsets/` (sonnet control) | 128 `.pl` files, commit-skew `23b7faa` vs twins `bbf5c92` — source-`.pl` quantities only (commit-immaterial); no engine-computed cross-corpus compare |
| sentinel absence in sources | `grep -rl 'inferred_subject\|inferred_institutional\|coordinated_group'` over all three corpora = **0 files** (sources are clean ground truth; sentinels minted only at load) |

Permutations N=1000; harness seed **20260613** (parent default, kept). Every conditioned
analysis draws from its **own** `random.Random(f"{seed}:{tag}")` instance — the global RNG
consumed by the unconditioned path is never touched by conditioned code (RNG isolation).

## Disclosed pre-freeze observations (recon; what was and was NOT computed)

Computed during recon / parser positive-controls, before this freeze:

- Per-corpus **marginal** victim-status counts (parser controls required them):
  haiku 841 authored / 5 imputed-eligible / 114 absent; flash 877 / **0** / 83;
  sonnet (`testsets/`) 100 / 4 / 24. (The plan's "~19 imputation-eligible" sonnet estimate
  shrank to 4 under the full `supp>0.40` conjunct, as the plan anticipated.)
- Named spot-check ids (first-alphabetical, minimal-knowledge picks):
  authored = `usul_al_fiqh_method__hanafi_reading` (haiku, victim facts at source :146–147);
  imputed(haiku) = `catastrophe_proxy_sufficiency__simulation_fidelity_threshold`
  (E=0.62, S=0.48, no victim fact; JSON powerless d=0.95, type `unknown`);
  imputed(sonnet) = `animal_status_kernel__property_reading`. Flash has **no** imputed id
  (marginal = 0) — the flash-side imputed spot-check is VACUOUS by construction.
- **NOT computed pre-freeze:** any pair-cell count, any per-cell or conditioned agreement
  rate, any Δχ statistic, any same-side subset, any bootstrap. The load-bearing decision
  statistics are blind at freeze time. Exception disclosed: the §B(ii) authoring-LEVEL
  comparison inputs are the marginals above, so (ii) is **non-blind**; its rule is frozen
  anyway and its verdict will carry a "non-blind (disclosed)" label.

## Deviations from the plan document (declared at freeze, with cause)

1. **Regression baseline re-scoped.** The plan says diff the unconditioned path against
   `audits/2026-06-13_twin_comparison/twin_comparison.json` byte-for-byte; that file was
   produced at commit `8126231` on pre-OQ-138 twin outputs, and the current twin JSONs are
   the `bbf5c92` re-classification — a cross-substrate diff would fail for reasons
   unrelated to the harness change. Witness used instead (same intent): **pristine harness
   (git HEAD copy) vs extended harness, run on the SAME current inputs + seed, unconditioned
   outputs diffed byte-for-byte.**
2. **HIGH⇒(a) rule sharpened (confound found at freeze).** "Authored-both clears its chance
   band" cannot by itself license (a) imputation-drag: the parent's corpus-wide 0.397
   ALREADY cleared its band, and recon shows the imputed cells hold ≤5 pairs — arithmetically
   incapable of dragging a 960-pair rate. (a) therefore requires the authored-both rate to
   also EXCEED the same-run unconditioned rate (comparative clause, §B.2 below). Same spirit
   as the two confound fixes the plan itself made pre-freeze.
3. **Per-twin status is a TRICHOTOMY on real data:** {authored, imputed, absent} — `absent`
   = no victim fact AND imputation gate unfired (canonical fallback at load, no sentinel).
   The plan's three pair-cells are kept as the named decision/control cells; the residual
   cells (authored×absent, imputed×absent, absent-both) are REPORTED, carry no decision
   rules, and are never pooled into named cells.
4. **Secondary omission-asymmetry probe added** (frozen here): the plan-literal imputed-one
   asymmetry will be n≤5 by the marginals; the broader under-authoring signature
   (exactly-one-authored pairs, other status ∈ {imputed, absent}) is pre-registered as a
   secondary (c1) probe with the same decision rule.
5. **Imputed spot-check drops the hard `d==0.90` gate.** Recon falsified the universal
   d-tell (named imputed id has d=0.95; corpus d distribution is rich — authored
   `positional_displacement` and coalition resolution also move d). Frozen spot-check =
   `authored_victim/1` FALSE **AND** sentinel fact present at load; `d` recorded as
   consistency data, not gated.

## Track A — OQ-125 (conditioned |Δχ| colocation test)

**Operationalization (frozen):** "same side of the decision threshold" = same categorical
seat type: `perspectives.<seat>` equal and both populated. REJECTED alternative: raw-χ cut
(ill-defined under dual-threshold + priority cascade; the decision surface's output is the
type). Fields: the four typed χ fields `chi:{powerless,moderate,institutional,analytical}`.
`theater_ratio` has no seat type ⇒ reported unconditioned, labeled OPEN/exploratory, outside
the headline.

Per seat s:
- Conditioned set `C_s` = matched ids with equal populated types AND both χ numeric.
- Observed = mean|Δχ| over `C_s`. Null = 1000 shuffles of flash-χ **within `C_s`**
  (re-pair same-side against same-side only) → band5/band95.
  RNG tag `condA:<seat>`.
- **Method witness (make-or-break):** observed and band are both stamped with
  `sha256(sorted C_s ids)` + n, shown side by side. Conditioned-observed vs full-set-band
  is a FAIL of the method, not a result.
- Min-n: |C_s| ≥ 30, else that seat ships OPEN (statistic not stood up).

**Decision rule per seat (literal):**
- observed **< band5** ⇒ real continuous value-invariance beyond H1.
- **band5 ≤ observed ≤ band95** ⇒ tail was threshold-colocation, H1-entailed; not citable
  as independent invariance.
- observed **> band95** ⇒ ANOMALOUS-ABOVE (no pre-committed interpretation; report only).

**Headline scope:** OQ-125 resolves over the 4 typed χ fields only. Wording: all 4 (with
n≥30) below conditioned band5 ⇒ "value-invariance confirmed (4 typed seats;
theater_ratio exploratory)"; all 4 in-band ⇒ "tail is an H1 artifact"; mixed ⇒ SPLIT,
per-seat listing, no aggregate claim.

**Positive control (reach-demonstrating, frozen):** disagreeing subset `D_s` (both
populated, types differ, both numeric): report n, band5/band95, band width, observed.
- REACH criterion: the control is informative iff the **conditioned same-side observed**
  (the effect actually claimed — NOT the full-set observed) < `D_s` band5, i.e. the claimed
  effect would register as below-band inside the disagreeing band.
- Reach fails ⇒ control INERT, reported as saying nothing (never as a pass).

## Track B — OQ-123 (authored/imputed/absent partition)

**Per-twin per-id status (source join, frozen):**
- `authored` iff the twin's source `.pl` carries any `narrative_ontology:constraint_victim/2`
  fact (sources witnessed sentinel-free above; reuses the `stakeholder_seats.pl:312`
  `authored_victim/1` criterion — at source level, fact-presence ⇔ authored).
- else `imputed` iff source `constraint_metric(_, extractiveness, E)` with E > 0.46 AND
  `constraint_metric(_, suppression_requirement, S)` with S > 0.40 (replicates the
  `data_repair.pl:132–158` bridge gate ⇒ `inferred_subject` sentinel minted at load).
- else `absent` (no victim at load; canonical fallback; no sentinel).

**Pair cells:** decision cell **authored-both**; control cells **imputed-both**,
**imputed-one** (authored×imputed); residual cells (authored×absent, imputed×absent,
absent-both) reported only. Statistic per cell: `persp:powerless` agreement rate over
both-populated pairs; Wilson 95%; permutation null **within cell** (1000 shuffles, RNG tag
`condB:<cell>`) → band5/band95.

**Power floor (authored-both, frozen constant):** the cell is POWERED iff a benchmark
"agrees-where-authored" effect would register: `wilson_lo(round(0.672·n), n) > band95`,
where 0.672 = the parent audit's institutional-seat agreement rate (frozen benchmark).
Not powered ⇒ **OPEN (underpowered)**, n stated, no HIGH/LOW forced, **B4 NOT armed**.

**Decision rule (authored-both, powered), with r_all = same-run unconditioned
`persp:powerless` agreement rate over all matched pairs:**
- **(a) FLOAT/DRAG:** wilson_lo > band95 **AND** r_ab − r_all ≥ 0.10 ⇒ the seat agrees
  where signal is authored; corpus-wide depression attributable to non-authored cells.
  Expected closure (a)/(c1). (0.10 ≈ one third of the parent powerless→institutional
  gradient gap 0.397→0.672; frozen.)
- **PERSISTENT DIVERGENCE:** wilson_lo > band95 AND r_ab − r_all < 0.10 ⇒ imputation-drag
  (a) REFUTED as the explanation; the seat's model-sensitivity lives in authored content ⇒
  **(b)-or-(c2), LIVE**. Does NOT auto-arm B4 (plan arms B4 only on powered LOW); B4
  spend-go remains an operator call, noted as warranted-if-they-want-the-split.
- **CHANCE-LEVEL:** band5 ≤ rate ≤ band95 (Wilson overlapping band) ⇒ agreement
  indistinguishable from random re-pairing ⇒ reads against (a); (b)-or-(c2)-leaning;
  does NOT arm B4.
- **LOW:** wilson_hi < band5 ⇒ **(b)-or-(c2), LIVE** — the only outcome that ARMS B4
  (B4's role = disambiguate (b) from (c2) via a paired third-model twin; never replicate).
- (a) is NEVER concluded from imputed-cell agreement.

**Imputed-cell control readings (frozen):** imputed-both — recon marginals imply n=0 ⇒
VACUOUS, reported as such (if nonzero: HIGH corroborates forcing; moderate uninformative;
LOW is an ε-divergence finding, never "pooling was fine"). imputed-one — LOW confirms
forced-disagreement; n < 10 ⇒ labeled underpowered/report-only.

**Asymmetry probes ((c1)):**
- Primary (plan-literal): imputed-one direction counts (haiku-imputed×flash-authored vs
  reverse). Directional iff n ≥ 10 AND Wilson 95% CI of the larger direction's share
  excludes 0.5; else REPORT-ONLY.
- Secondary (frozen at deviation 4): same rule over exactly-one-authored pairs (other
  status imputed OR absent). Directional ⇒ (c1)-signature (haiku-omits ≫ flash-omits or
  reverse); symmetric ⇒ (a)-like.

**Seat-gradient robustness:** 1000 bootstrap resamples (RNG tag `condB:bootstrap`) of the
matched id list; per-seat `persp:*` agreement rates; percentile 95% CIs. Gradient ROBUST
iff powerless CI-hi < min(other three seats' CI-lo); else stated per overlap.

**Sonnet `testsets/` controls (OQ-123 only):**
- **(i) per-corpus classifier positive control:** the sonnet corpus populates the authored
  slice (recon 100) and the imputed slice (recon 4 — THIN: labeled one-sided/thin, not a
  full pass, since <10). Load-time confirmation via swipl (default corpus): on
  `animal_status_kernel__property_reading`, `authored_victim/1` FALSE and
  `constraint_victim(_, inferred_subject)` present at load; on the haiku leg (overlaid
  `corpus_path=testsets_haiku`, retractall+assertz per overlay recipe, loaded-count 960
  confirmed): same check on `catastrophe_proxy_sufficiency__simulation_fidelity_threshold`,
  and `authored_victim/1` TRUE on `usul_al_fiqh_method__hanafi_reading`. Scope honesty:
  (i) does NOT positive-control the pair-crossing (three-cell) logic — that is pair-defined
  and has no independent control.
- **(ii) authoring-LEVEL comparison (NON-BLIND, disclosed above; rule frozen anyway):**
  sonnet powerless authored-victim rate (authored/128) vs each twin's (authored/960).
  **(c1)** iff sonnet_rate − max(twin rates) ≥ 0.20 AND 95% Wilson CIs disjoint;
  **(a)-supporting** iff |sonnet_rate − pooled twin rate| < 0.10; else INDETERMINATE.
  (ii) says NOTHING about (c2). Level comparison, never a within-model rank.

**B4 (third-model paired twin): DEFERRED.** Armed ONLY by a POWERED LOW authored-both.
OPEN/underpowered does not arm; PERSISTENT/CHANCE-LEVEL do not auto-arm (operator
discretion). Role if armed: disambiguate (b) from (c2) — (c2) recovers authored-cell
agreement under a sonnet paired twin, (b) diverges.

## Verification commitments (all witnessed in the audit dir)

1. Regression: pristine-vs-extended unconditioned outputs byte-identical (same inputs,
   same seed), diff pasted.
2. Track A: id-set hash stamped on observed AND band per seat; disagreeing-control reach
   shown.
3. Track B: partition pair-cell counts reconcile against the per-corpus marginals
   (row/col sums equal 841/5/114 and 877/0/83).
4. Spot-checks per (i) above (three ids, load-time, positive AND negative).
5. Decision rules read literally off THIS document; no post-hoc narration.

## Under-claim discipline (both tracks)

One twin pair earns "model-sensitive/invariant HERE" (haiku-vs-flash, this corpus, commit
`bbf5c92`), never "in general." Headlines carry the scope.
