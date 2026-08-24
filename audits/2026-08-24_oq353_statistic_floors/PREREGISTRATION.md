# OQ-353 — PRE-REGISTRATION (frozen)

**Executed:** 2026-08-24 · **OQ:** OQ-353 (corpus-level statistic floors: the size × content ×
edge-semantics control) · **Phase:** 1 of 2 — *pre-registration and instrument only.*
**NOTHING IS MEASURED HERE AND NOTHING IS CONCLUDED.** Every bit (B1–B5) and both cross-checks
(X1, X2) require report runs and are Phase 2 in every branch.

## 0. Freeze-time provenance

Recorded so a later reader can **check** that this list predates the driver's outputs rather
than take it on trust.

| field | value |
|---|---|
| HEAD at freeze | `e01951de21d96a4ac5d777bd64a9adea8704b2a5` |
| `git status --short` | empty (clean tree) |
| sha256 `python/run_pipeline.py` | `c23d3208a995dd0fc42f092a7551acdadab913056af9cef1b27d5cdecd16ae78` |
| timestamp (UTC) | 2026-08-24T16:35:36Z |
| session-start HEAD | `67b73236dd1099c2c63088eab6851ba478c5a152` |

This file is md5'd at commit and the md5 logged in `audit_log.md` **before the first counted row**.

## Two words that are taken — defined once, here

- **provenance sidecar** — OQ-352's `*.manifest.json`, top-level `corpus_hash`, written by
  `_write_sidecar`, required to be accepted by `assert_corpus_current`.
- **stat sidecar** — this plan's §(j) artifact: a statistic-extraction JSON carrying raw count
  **and** denominator **and** named share. Unrelated to the above. Referred to as *stat sidecar*
  throughout; never as a bare "sidecar".

---

## 1. The blocking record — Phase 2 cannot start

| gate | state | what it blocks |
|---|---|---|
| **OQ-356** | `blocked_on_human` (a ruling) | `giant_comp` dies on 17 of 20 corpora → all `giant_comp` statistics, and cross-checks **X1 and X2** |
| **OQ-363** | open (`splits_from` OQ-352) | `report_corpus` forwards only `giant_comp_timeout` → the v6 `abductive` artifact and any large-archive stage ceiling |

`omega_resolver.py frontier` buckets **OQ-353 as `blocked`** (verified 2026-08-24), OQ-356 as
`blocked_on_human`, OQ-363 as `blocked`. The exposure counts in §6 were the Phase-1 GO/NO-GO on
Phase 2's compute; they now sit alongside this harder gate.

**The pair floor covers 10 stages, not 11** — `giant_comp` is blocked program-wide, not only for
v6. Verified on `outputs/legs/testsets_sonnet2/`: exactly 10 stage `.md` artifacts, no
`giant_component_analysis` product.

---

## 2. CONFOUND RECORDED BEFORE THE FREEZE — the chimera seal is VACUOUS on v6

This belongs inside the frozen text, not appended to it: a confound appended after a freeze is
indistinguishable from one discovered to fit the results.

**C1 asked whether the chimera seal ADMITS v6. The question is malformed: the seal never
evaluates v6.**

> **PRE-FREEZE NOTE ON C1'S STATUS — read this before reading C1's `Fired: live`.**
> The plan spent three passes refining a three-outcome table — **clean / warns / refuses** — and
> the true answer is a **FOURTH outcome the table does not contain: *not evaluated*.** C1 is
> recorded `Fired: live` in `audits/INVESTIGATIONS.md`, and a later reader who sees that bit
> beside a clean load will otherwise infer that the seal ran and passed v6. **It did not run.**
> The three-outcome table presupposed that the seal evaluates whatever corpus is put in front of
> it; that presupposition is false, and it is false for reasons that have nothing to do with v6.
> Wherever C1's outcome is cited, the token is *not evaluated* — never *clean*.

**This is not a v6 property — it is a defect in the seal, and it has its own OQ.** The seal's
coverage is proportional to `cs_story_uid` authorship and it reports nothing about that coverage,
so **14 corpora (7,162 stories) are exempt outright and 9 more are partially checked while
producing output identical to a clean pass** — including the LIVE leg `testsets` at 64.2%, and
7,829 readings unchecked in total. Tracked as **OQ-365**, with the three-arm control below cited
as its evidence. What is recorded *here* is the local consequence for arms (b)/(c); the instrument
defect is OQ-365's.

- v6 **loads clean** at HEAD — 3380/3380 files, `corpus_constraint` 3380, census 3380 stories /
  0 non-story / 0 other, exit 0, `config_violations.log` untouched. 0 `ERROR:` lines, 0
  `[corpus] SKIPPED`, 0 failed loads, 0 `CONFIG ERROR`, 0 `CS ERROR`. 41,864 warning records
  reduce to **11 classes**: 3 engine-side records matching `prolog/load_warning_allowlist.txt`
  verbatim, 10,463 consult-time `discontiguous` notices from generated story files, and the
  deprecated relative-source-search notice ×3.
- **But** the chimera clause (`config_validation.pl:199–210`) is guarded by a
  `current_predicate/1` test plus a non-empty requirement on the collected `cs_story_uid` keys,
  and **v6 carries `cs_story_uid` on 0/3380 files** (live legs carry it: haiku2 996/996). The
  guard fails; not one v6 reading is checked.
- **Three-arm positive control** (`c1_control/`), so this is a tested absence and not a fact
  about my search: a planted 2-story corpus with conflicting ε (0.30 / 0.70) **and**
  `cs_story_uid` **FIRES** the seal (`CS ERROR (OQ-25)`, halt, log written); the same corpus with
  a single ε **declines**; the same conflicting-ε corpus with `cs_story_uid` **removed** — v6's
  exact shape — is **SILENT**, exit 0, no log.

**Consequence, carried at the verdict's altitude wherever arms (b)/(c) are read:** C1 licenses
*"v6 loads without error at HEAD"*. It does **not** license *"v6's ε-coherence has been vetted"*.
v6 is chimera-era by declaration with ID reuse across runs (OQ-25, v7 §5.11) — precisely what
this seal exists to catch — and that check has not run on it. **Arms (b) and (c) inherit an
UNVETTED ε-coherence assumption.** Bookkeeping this clean as a pass would be Pattern 5.

---

## 2a. DECLARED DEVIATION from handoff item 5 — the stamp test's disposition

Handoff item 5 (and plan §4.3 change 6) says to adopt `report_corpus`'s softened stamp test.
**The VERDICT is adopted in full** — same three-way outcome, computed through the same
`_is_code_path` imported from `run_pipeline.py` (never redefined: a second definition of "engine
path" would be Pattern 2), fail-closed on an undecidable stamp. **The DISPOSITION deviates, and
the reason is recorded here rather than left to be discovered.**

`report_corpus` **joins** report artifacts to a same-commit classify output, so a cross-commit
pair is a real defect there and a hard refusal is right. This instrument **tabulates historical
classify outputs ACROSS legs**, and cross-commit spread is the very thing its `commit` column
exists to surface — step 0's F1–F8 are exactly such a comparison. Adopting the hard refusal
verbatim refuses **all 19 legs** (measured: every leg's output stamps a commit with real
engine-file deltas since, because the legs were classified over several days as they landed), the
instrument emits nothing, and **Verification 0's own before/after diff becomes unproducible —
which would make the refactor unprovable.**

So the default disposition here is **RECORD, never silent**: every leg's token is printed to
stderr and lands in `outputs/leg_diagnostic_table.json` → `classify_stamp`. `--strict-stamp`
restores `report_corpus`'s hard behaviour, and both sides are fixture-covered in the gate row.
**Accepted by the operator, 2026-08-24.** The divergence is therefore declared, not accidental —
if the two tools' stamp semantics are ever unified, this is the paragraph that says why they were
not.

## 3. C2 — three things recorded verbatim, not as footnotes

### 3.1 The density rule had no content; feasibility did the whole selection

Flat ranking, re-derived 2026-08-24 (`c2_density.py`, anchor `^narrative_ontology:cs_kernel_id(`):

| first leg of pure pair | stories | kernels_total | kernels_ge2 | stories/kernel | giant_comp |
|---|---|---|---|---|---|
| `testsets_stealth2` | 1005 | 331 | 331 | 3.036 | THROWS (both legs) |
| `testsets_sonnet2` | 1003 | 331 | 331 | 3.030 | THROWS (both legs) |
| `testsets_haiku2` | 996 | 331 | **331** | 3.009 | **rc=0 both legs** |
| `testsets_flash_think` | 988 | 331 | 328 | 2.985 | THROWS (both legs) |
| `testsets_flash2` | 944 | 331 | 326 | 2.852 | THROWS (both legs) |

Read this precisely. `kernels_ge2 == kernels_total == 331` on haiku2, sonnet2 **and** stealth2 —
the metric is **saturated at its ceiling**, not merely tied — while flash2 (326) and flash_think
(328) sit below it, which shows the ceiling is a property of the *larger* legs, not a structural
fact about the seed set. Sibling density is 3.009 (haiku2) vs 3.036 (stealth2): **no pure pair
differs meaningfully in strippable structure.**

C2 existed so a near-zero delta would read as *"kernel structure isn't load-bearing"* rather than
*"there was nothing to strip"*. At ~3 readings/kernel it cannot do that job **on any leg in this
seed set** (GAP-35: all 19 legs are one seed set), not merely on a wrongly-chosen one.
**A later reader must not think density chose haiku2, and must not think a better choice existed.**

### 3.2 Why the amendment is defensible despite burned blindness

The ranking was computed before the amendment, so the amendment cannot be blind. What makes it
not a post-hoc rule change is that **feasibility was derivable without the ranking**:
`giant_comp`'s throw census is OQ-356's artifact
(`audits/2026-08-23_oq352_report_driver/giant_comp_leg_census.txt`), not this plan's, and it
selects haiku2/haiku3 regardless of what the density numbers say. The frozen density rule would
have selected `stealth2/stealth3` — the one pure pair on which the arm's own cross-checks cannot
be produced, since X1 and X2 both read `n_sibling_edges_stripped` from
`giant_component_analysis.raw.json`. Under the feasibility filter the set is a **singleton**, so
no tie-break fires and the base plan's tie stop-and-ask does not apply.

### 3.3 Degeneracy is a PRE-REGISTERED outcome with its disposition fixed NOW

haiku2 passes `giant_comp` with **0 unknown-purity GC members** — it passes by degeneracy, not by
health (OQ-356). The mechanism is verified, not guessed: `effective_purity` yields `unknown` iff
`purity_scoring:purity_score` yields a non-number (the `\+ number(Intrinsic)` branch,
`drl_purity_network.pl`), which the strip **cannot** change; but the strip removes the same-kernel
exclusion and so **adds** edges, enlarging the giant component, which **can** pull unknown-purity
stories into GC membership. So the twin may throw where the production leg did not.

**Disposition, fixed before the run — not dispositioned after observation:**
twin throws ⇒ **X1 still readable on the production leg**; **X2 → `PENDING OQ-356`**;
**B5 readable on FPN and the `network_n_drifting` / `network_n_severe` diagnostic members only.**

*Readable* there means the B5 **existence bit** (does the guard move this statistic at all,
against the measured null floor) — at full strength, in both directions. It does **not** license a
magnitude reading (§9).

---

## 4. The statistic list — three partitions, never reconciled

Full machine-readable registry: `outputs/leg_diagnostic_verdicts.tsv` (61 rows) and the
`registry` / `registered_pending` blocks of `outputs/leg_diagnostic_table.json`. Each row carries
name · source artifact · key path · kind · denominator · both exposure columns.

### 4.1 Classify-side — 56 statistics, DECLARED

`json_report.pl` → `pipeline_output.<leg>.json` `diagnostic`. Frozen as an ordered declaration in
`python/audits/leg_diagnostic_table.py` `STATISTICS`, **not** derived from whichever leg sorts
first. Families: `type.*` (7), `purity.*` (8), `coupling.*` (5), `boltzmann.*` (3),
`drift_events_per_story.*` (3), `network.*` (4), `wasserstein.*` (2), `arakelov.threshold`,
`contextuality.*` (8), `monotonicity.*` (8), `severe_share_within_type.*` (7).

### 4.2 Report-stage side — 10 stages, of which 8 are Markdown-only

Re-derived against `outputs/legs/testsets_sonnet2/` (superseding the base plan's S10, which
predates the driver):

| stage | corpus-level machine-readable JSON? |
|---|---|
| `abductive_report` | **yes** — `abductive_data.json` carries a `summary` block |
| `commentary_census` | **yes** — `commentary_census.json` (`manifest` + `sources`); the stat-sidecar template |
| `orbit_report` | **no** — `orbit_data.json` is **per-story keyed**; corpus-level orbit statistics must be derived |
| `context_profile_report` | no |
| `coupling_protocol` | no |
| `covering_analysis` | no |
| `fingerprint_report` | no |
| `fpn_report` | no |
| `maxent_diagnostic_report` | no |
| `maxent_report` | no |

**THE NUMBER, published so a later reader can check it: 8 of the 10 stages are Markdown-only for
corpus-level statistics, and 2 are not.** Stat-sidecar minting in Phase 2 therefore covers **8**
stages (the 7 with no JSON at all, plus `orbit_report`, whose JSON exists but is per-story).

**Ruled (operator, base plan §1.2(j)): mint a stat sidecar. That is the default, not a per-row
judgment.** Sidecars follow the `commentary_census` contract — raw count **and** denominator
**and** named share, absence as `null` rather than a defaulted value. **Freezing a `format/2`
string is the EXCEPTION and is a stop-and-ask**, permitted only with a stated reason recorded here.

**The stat-sidecar key names ARE the contract.** They are frozen here for artifacts that do not
exist yet, so any divergence discovered at build time is reported as a **miss of the
pre-registration** — the list is amended openly with the miss recorded, never back-fitted silently.

### 4.3 Registered but not populatable — `PENDING OQ-356`

Registered with the reason, never dropped (same footing as phantom-node share):
`giant_comp.n_sibling_edges_stripped`, `giant_comp.pooled.n_edges`, `giant_comp.stratum.n_edges`,
`giant_comp.giant_size`, `giant_comp.pooled.n_nodes`.

The resolution path is mechanically supported: `stages` is a real parameter on `report_corpus`
(`run_pipeline.py:952`), so when OQ-356 lands this is an incremental `--stages giant_comp` run
over the chosen pair, joinable to the existing 10-stage set through the provenance sidecars.

---

## 5. Kind typing — four kinds, not two

A raw COUNT is size-bound *trivially*, so its (b)-vs-(c) verdict carries no information.

| kind | rule |
|---|---|
| **SHARE / RATE** | scale-free; verdict readable directly |
| **COUNT** (n-scaling) | must carry a declared normalized twin, or is marked *size-bound by construction, verdict vacuous* |
| **BOUNDED COUNT** | capped by something other than n (config caps, type-lattice caps). **Reads "construction-bound" for the wrong reason unless typed** — normalize to the known denominator |
| **PERCENTILE / MEAN** | `arakelov.threshold` (corpus p75), the EP means — scale-free but fit-dependent |
| *(CATEGORICAL)* | not a number at all; never enters a numeric floor. `network.stability` only |

**Starting partition, folded in from OQ-352's handoff item 3 (already recorded there, not
re-decided here):** the pure pair is n=1003/1003 exactly and needs no normalization; the v6 arm at
3380 does. Statistics stay flagged n-sensitive (counts, family counts, coverage denominators) vs
scale-free (shares, fractions, rates). The four kinds **refine** that partition rather than
replace it.

---

## 6. Exposure columns (f) and (f′) — and their coverage

Both are computable without a single report run, which is why they were the Phase-1 GO/NO-GO.

- **VINTAGE-EXPOSED** — the chain reads an authored field whose v6 coverage differs materially
  from a leg's. v6 (re-verified exact, 2026-08-24): `cs_kernel_id` **0**/3380, `story_provenance`
  **0**/3380, `constraint_stakeholder` **1**/3380, `coordination_type` **3329**/3380 (98.5%).
  **`coordination_type` is NOT vintage-blocked** — the base plan's correction, confirmed here.
  The genuinely absent fields are the stakeholder ones, `story_provenance` and `cs_kernel_id`.
- **MIXTURE-EXPOSED** — the chain pools / clusters / fits / cuts over the whole corpus rather than
  computing per story and averaging. Pre-registered candidates, named before any run: FPN network
  purity, HAC family count at a fixed cut, covering class counts, and anything downstream of the
  corpus-fitted `maxent_distribution/3` (`corpus_wasserstein_fracture`, `arakelov_threshold`).

**Counts as of the freeze, over the 56 classify-side statistics — and they carry their coverage,
which is the point:**

| | exposed | UNTRACED | traced-and-not-exposed |
|---|---|---|---|
| vintage | **12** | 44 | 0 |
| mixture | **15** | 41 | 0 |

**`UNTRACED` is not `False`.** Asserting *not exposed* is a claim, and an untraced row has not
earned it. 44 of 56 vintage cells and 41 of 56 mixture cells are untraced, so **these counts are a
floor on exposure, never a ceiling**, and the honest reading is: at least 12/56 and 15/56 are
exposed, the rest is unknown. Completing the tracing is §1.2(g) work that Phase 2 inherits.

The 12/15 that *are* traced are the purity/network family (the corpus-wide neighbour graph built
through `constraint_neighbors/3`, whose `:115` conjunct is the `cs_kernel_id` guard — absent on
0/3380 of v6, which is exactly what arm (a′) isolates) and the corpus-fitted-MaxEnt family.

**The program-level consequence, named in advance so it is not misread as failure:** if the two
columns end up covering most of the report-stage list, OQ-353 returns **no content verdicts at
all** and its whole answer is *"saturated or unreadable"*. **That is a legitimate result** — it
says the report tools' corpus-level headlines cannot be shown to measure content on any evidence
this project currently holds. It is not a failed investigation and must not be written up as one.

---

## 7. The verdict is a vector of independent bits, not a 5-way category

OQ-353's `**Resolution:**` line names `{draw-noise, model-disposition, content, size-bound,
construction-bound}` as if they partition. **They do not** — a statistic can carry content *and*
be size-bound. Replaced by independent bits plus a *derived* construction-bound reading. The OQ
text is amended in the same pass so the OQ and its own pre-registration do not disagree in the
substrate.

| bit | reading |
|---|---|
| **B1 draw-bound** | `within_pure_max ≥ between_model_spread` |
| **B2 model-disposition** | ratio ≥ R_hi |
| **B3 content-OR-MIXTURE-bearing** | (a) vs (c) separate beyond both draw floors, at equal n. **The name is the finding** — arms (a) and (c) differ in *three* ways (situations, schema vintage, within-corpus model heterogeneity), and mixture is irreducible, so B3 can never be narrowed to "content" on this evidence. Reported under its compound name, always |
| **B4 size-bound** | (b) vs (c) separate beyond the within-(c) floor, at fixed content |
| **B5 guard-sensitive** | production leg vs kernel-stripped twin, read **only against a measured null floor** |
| *construction-bound* | **DERIVED**: ¬B3 ∧ ¬B1. Never asserted |

**B3 is THREE-VALUED: `true` / `false` / `unreadable`. Never bookkeep `unreadable` as `false`** —
that mints a spurious *saturated* verdict, and saturated is the verdict that **demotes a
headline**, so the error runs in the costly direction.

**`unreadable` is ASYMMETRIC — it applies to `B3 = true` only:**

| observation on an EXPOSED row | B3 | why |
|---|---|---|
| (a) and (c) **separate** | **`unreadable`** | separation cannot be attributed to content — vintage or mixture could have produced it alone |
| (a) and (c) **do not separate** | **`false`** — readable | the confounds push *toward* separation and it did not happen |

So the exposed class is **`{false, unreadable}` — never `true`**: such a statistic can be shown
saturated, never content-bearing. A construction-bound verdict reached this way is a
weaker-but-usable licence, tagged **`saturated (confound-assisted)`**, never presented as clean.
Mixture-exposed rows additionally carry **`QUALIFIED`**. **Precedence: §8's resolution limit BEATS
the confound-assisted licence** — an exposed statistic that fails to separate but lands inside the
declared band is **INDETERMINATE**, not `saturated (confound-assisted)`.

**B5's null control runs BEFORE any twin delta is read:** the production leg twice, unmodified,
through the same driver, requiring delta ≡ 0 across every frozen statistic. Where it is 0, B5's
floor is 0 and any nonzero twin delta is real. **Where it is not 0, that statistic gets a measured
B5 floor from this control** — learned for the price of one extra run rather than assumed and wrong.

---

## 8. Cut-points and resolution limits

### 8.1 CUT-POINTS ARE OPEN — an operator ruling, and the classifier REFUSES rather than defaulting

The base plan requires cut-points pinned **as numbers**, with one named escape: *"any cut-point in
1.2(d) that cannot be set from step-0 evidence without a judgment that is the operator's"* is a
stop-and-ask. **That escape fires, and here is the evidence.**

Step 0 used ratio **≥ 8** (model-disposition) and **< 3** (draw-dominated) implicitly, and its
ledger line describes exactly those two ends. Measured this turn over step 0's own 52-statistic
pair table, **the middle is not empty — 10 of 52 statistics land inside [3, 8)**:

```
3.635 contextuality.by_type.tangled_rope   4.822 monotonicity.ascending
4.143 type.scaffold                        5.035 monotonicity.descending
4.259 contextuality.by_type.snare          6.867 severe_share_within_type.mountain
4.639 type.piton                           6.930 type.rope
6.952 severe_share_within_type.snare       6.997 monotonicity.constant
```

The full distribution runs 1.715 → ∞ with **no discontinuity**: 8 statistics below 3, 10 inside
[3,8), 34 at or above 8. So 3 and 8 are not a gap — they are the ends of a continuum, and
choosing where to cut it is a judgment about how much evidence licenses a model-disposition call.
**That is the operator's seat.** The amended plan adds: *do not pick a round number to keep moving.*

**Mechanically enforced, not merely noted:** `classify_bits()` in
`python/audits/leg_diagnostic_table.py` holds `CUTPOINTS = None` and **raises `CutPointsNotRuled`**
rather than defaulting; a gate fixture asserts the refusal. A default threshold here would be a
fabricated value wearing a verdict's clothes. The selftest exercises the classifier's **routing**
with values explicitly labelled `SYNTHETIC — NOT the frozen cut-points`, which does not pre-empt
the ruling.

### 8.1a RULED (operator, 2026-08-24): the cut is REFUSED, not deferred — OQ-366

The ruling is taken, not pending. **Do not set R_hi/R_lo from step-0 evidence.** A continuous
1.715→∞ distribution with no gap, and 10 of 52 statistics inside the proposed band, means any cut
named here is arbitrary — and **an arbitrary cut frozen into a pre-registration is worse than a
declared abstention, because it manufactures the appearance of a pre-committed threshold.**

1. **B1 and B2 are reported as CONTINUOUS RATIOS with the band declared `BAND_UNSET`** — not as
   bits — unless and until a principled cut exists.
2. This section records the refusal with the distribution pasted above.
3. Where Phase 2's downstream logic needs a bit, it takes **the ratio plus an explicit
   `BAND_UNSET` token — never a default.** Mechanically: `classify_bits()` returns `B1_ratio` /
   `B2_ratio` and stamps the bits `BAND_UNSET`; `require_bits=True` **raises**
   `CutPointsNotRuled` rather than defaulting. Both paths are gate-fixtured in `oq353 floors`,
   and `BAND_UNSET` is asserted textually distinct from `NOT_MEASURED` and `PENDING OQ-356`.

**What would license a cut — the concrete unblocking condition.** A corpus family with **more than
two same-model draws**, which gives B1 a **distribution** rather than a difference. The pair floor
today is a k=2 point estimate (§8.3); a third same-model draw at the same prompt and sampling turns
that floor into something with a spread, and a cut can then be sited against the spread rather than
asserted. **This is a generation spend, not a re-read** — no existing leg supplies it, since every
same-model family in the roster is a pair.

**Consequence, stated plainly at the verdict's altitude.** B1 and B2 join B3's exposed class as
bits that report a **number** rather than a **verdict**. That is a smaller answer than OQ-353 hoped
for, and it is the honest one. **A close reporting B1/B2 as continuous ratios with `BAND_UNSET` is
a legitimate close, not a failed investigation** — the same standing §6 gives the "saturated or
unreadable" outcome.

**Tracked as OQ-366** (`blocked_on_human oq366-principled-cut-requires-k-gt-2`), so the unblocking
condition surfaces in the `[NEXT]` queue rather than living only in this file.

### 8.2 The (c) floor's resolution limit — k=3 is the STRUCTURAL MAXIMUM

k=3 disjoint samples at n=1000 is the maximum available from 3380 — a ceiling, not a chosen k.
The within-(c) floor is therefore an extremum over **three** differences and is the denominator of
**two** bits (B3 and B4). A floor from k=3 is a coarse bound and cannot separate a genuinely small
effect from an unlucky draw. **B3/B4 verdicts landing within the declared band of the (c) floor
are reported INDETERMINATE rather than resolved.** Increasing k requires either smaller n
(breaking size-match with the legs) or overlapping samples (understating the floor), so this limit
is structural and **is not renegotiated at analysis time**.

### 8.3 B1 gets a resolution limit too — and its k is SMALLER

The pair floor is a **k=2 point estimate**: two draws give a difference, not a confidence
statement, and R1–R6 differing licenses *"these six move between same-model redraws"*, not a floor
magnitude. B1's denominator is therefore **smaller-k than the (c) floor's k=3**, and it gets the
same treatment §8.2 gives B3/B4: a declared indeterminate band inside which a B1 verdict is
**INDETERMINATE** rather than resolved. Without this the plan would apply a resolution limit to
its better-supported floor and none to its worse-supported one.

**The band's SIZE is part of the §8.1 ruling and is likewise OPEN.** It could not be set from
step-0 evidence for the same reason the ratio cut could not: the k=2 differences form a continuum
with no gap. `classify_bits()` takes it as `indeterminate_factor` and refuses without it. The
routing is fixture-covered (`B1 inside its OWN k=2 band -> INDETERMINATE`).

---

## 9. B5's magnitude bound — the BIT and the MAGNITUDE come apart

Every pure pair sits at ~3 readings/kernel (2.985–3.036 among the saturated legs, 2.852 at the
lowest), and `kernels_ge2` saturates at `kernels_total` on all three larger legs. Strippable
structure is thin and near-uniform and **no better leg exists to choose**. Split the bit from the
magnitude, because only one of them is bounded:

- **B5 as an EXISTENCE BIT — READABLE in both directions.** A delta exceeding the measured null
  floor means the guard touches that statistic; a delta indistinguishable from the null floor
  means it does not, **at this sibling density**. That is a scope qualifier on the
  generalization, not uninterpretability — and it is what §3.3's twin-throws disposition salvages
  on FPN and the `network_n_drifting` / `network_n_severe` members. A real bit, not a hollow one.
- **B5 as a MAGNITUDE CLAIM — the NEGATIVE direction is BOUNDED.** A small-but-nonzero delta
  **cannot distinguish "kernel structure isn't load-bearing" from "there was almost nothing to
  strip."** The positive direction is unaffected: a large delta means the guard is load-bearing.

So the WRITEUP reports B5's bit at full strength and **refuses to convert a small delta into an
importance verdict**, saying so at the verdict's altitude.

> **B5 SURVIVES THE §8.1a ABSTENTION, and the Phase-2 WRITEUP states this plainly rather than
> leaving it to be noticed.** The cut-point ruling makes B1 and B2 report numbers instead of
> verdicts, and B3's exposed class already reports `{false, unreadable}` rather than `true`. **B5
> is the exception, and the reason is structural: it is read against a MEASURED null floor — the
> production leg run twice, unmodified — not against a chosen threshold, so there is nothing in it
> for a cut-point ruling to withhold.** The consequence for the program: **the edge-semantics
> factor is the one bit OQ-353 can still answer AS A BIT.** That is a smaller program than the OQ
> set out with and a coherent one — the question "does the kernel guard move this statistic at
> all" remains fully answerable, in both directions, while the questions that needed a threshold
> return quantities instead. State it that way at the verdict's altitude; do not present the
> reduced scope as a failure. Removing this bound needs a corpus with
materially higher sibling density — a generation spend, not a re-read.

---

## 10. The arms

| arm | corpus | n | state at freeze |
|---|---|---|---|
| (a) | 12 legs — the 5 pure pairs + `kimi2` + `nemotron_think` | ~1000 ea | roster declared; **sampling is unconditional** |
| (a′) | kernel-stripped twin of **haiku2** | 996 | **BUILT** — `prolog/oq353_arm_astrip_haiku2/`, manifest `c3_strip_manifest.json` |
| (b) | `archives/datasets/original_v6` | 3380 | `expected_model=None` mandatory; `PROMPT_HASH_ABSENT` recorded, **not** refused |
| (c) | 3 **disjoint** v6 subsamples | 1000 ea | **BUILT** — `prolog/oq353_arm_c{1,2,3}/`, seed **353**, manifests committed |
| (d) | situation-fixed core | — | **DEFERRED** — OQ-347 step 4 does not exist. Declared out, not silently dropped |

Expected refusal profile per arm consumes OQ-352's tokens verbatim: `MISSING_CLASSIFY_OUTPUT`,
`SCOPE_TRACKED_GENERATOR`, `PROMPT_HASH_ABSENT`.

**Arm (c) is DISJOINT, not overlapping** — 3000 of 3380 used, 380 discarded. Three overlapping
1000-samples would share ~296 stories pairwise and **understate the within-(c) draw floor**, which
is the denominator of the content bit. Witnessed: pairwise intersections 0 / 0 / 0, union 3000
with no duplicates, all members verified v6 members, each loading with
`corpus_constraint` = unique ids = `corpus_story` = glob = 1000 and ids diff-clean against the
committed manifests.

**Real directories holding symlinked FILES, never symlinked directories.** Executed control: a
symlinked directory run through `classify_corpus`'s own expression `(PROLOG_DIR /
corpus_path).resolve()` (`run_pipeline.py:460`) collapses to v6 and sees **3380**, not 1000 — so
an arm built the forbidden way would have silently measured all of v6 while reporting as a
1000-story subsample.

---

## 11. The strip arm's expected-identical / expected-to-move partition

Established by tracing inputs into each report stage, **not by grep** — grep witnesses only
*direct* kernel-fact reads and cannot see transitive exposure through the shared neighbour graph.

Starting point (OQ-95's resolution note, five `constraint_neighbors/3` consumers): **giant_comp,
drl_fpn, network_dynamics, json_report, and `drl_purity_network`'s own `bfs_path`/cascade walks.**

- **EXPECTED TO MOVE:** FPN, and the `network_n_drifting` / `network_n_severe` diagnostic members.
- **DECLARED:** the **second** kernel read in the same file, `drl_purity_network.pl:296–297`
  (`compute_edge_contamination`), moves contamination *values* only, not topology. A fact-table
  strip moves **both** sites, which is correct for reproducing v6 and is declared here rather than
  discovered after a number moves.

Written before the run, or a moved FPN number is unfalsifiable after it.

**The strip, as built and witnessed (C3):** 1992 lines mention `cs_kernel_id` in
`testsets_haiku2`; 996 match the anchor `^narrative_ontology:cs_kernel_id(` and are removed; 996
are declaration lines, preserved and **enumerated individually** in `c3_declaration_lines.txt`;
**0 unaccounted**; 996 + 996 + 0 = 1992 reconciles exactly. Completeness is witnessed **in the
DB** — the twin loads with `cs_kernel_id` facts = **0** — because `corpus_constraint == glob`
alone does *not* witness a complete strip: a partial strip passes it and yields a silent third
edge-semantics regime. Field-level diff over all 996 file pairs: exactly one diff shape (996
deletions of `narrative_ontology:cs_kernel_id(`, zero additions, zero other predicates), every
directive count identical (multifile 996, module 996, use_module 2988).

**CORRECTION to the plan's F5, recorded because the plan's verification text inherits it:** the
declaration block is `:- multifile`, **not** `:- discontiguous` — on haiku2, haiku3, stealth2,
flash2 and on sonnet2, F5's own witness leg. F5's *mechanism* is real and confirmed; only the
directive keyword is misstated, and the real one is the more consequential, since `multifile`
governs cross-file predicate assembly. The plan's "`:- discontiguous`/`:- dynamic` declarations
are intact" is therefore **a check that cannot fail** (both are 0 on these legs); the check that
can fail is on `:- multifile`, and it passes 996 == 996.

---

## 12. X1's derivation convention — frozen BEFORE the strip is read

X1 demands an *exact* match, so a convention mismatch and a genuinely bad strip both present as
"failed exact match" and the falsifier stops discriminating. Frozen now:

```
for each kernel K:
    M := the SET of distinct story ids carrying cs_kernel_id(story, K)   # dedup authored dups
    ordered_pairs(K)   := |M| * (|M| - 1)      # DIRECTED; self-pairs EXCLUDED
    unordered_pairs(K) := ordered_pairs(K) / 2
X1_target := sum over K of ordered_pairs(K)    # A->B and B->A BOTH count
```

- **Directed**, and `A→B` / `B→A` both count. Self-pairs excluded. Duplicate authored facts
  deduplicated (`M` is a set).
- **The target is the RETRACTED-FACT COUNT** (`n_sibling_edges_stripped`, directed
  `affects_constraint` facts), **not** the edge-set delta `pooled.n_edges − stratum.n_edges`,
  which is a *deduplicated undirected* quantity. **They are different numbers and do not agree**;
  comparing against the wrong one reads a correct strip as a failure.
- `stratum` publishes no `n_nodes`, so a stratum *fraction* must take `pooled.n_nodes` as its
  denominator.

**As computed on the built twin:** 331 kernels, **2066 ordered** / 1033 unordered same-kernel
pairs (`c3_strip_manifest.json`).

**VALIDATION OF THE RULE IS OWED AND IS EXPLICITLY NOT DONE HERE.** The base plan requires the
rule be validated against a *published* `n_sibling_edges_stripped` **read in the same turn as the
fact table**, before the strip exists. That figure is a `giant_comp` product and **`giant_comp` is
blocked by OQ-356 on every corpus except `testsets`, `haiku2` and `haiku3`** — and no current
`raw.json` for the chosen pair exists. So the validation moves to Phase 2 as the **first** step of
the (a′) arm, before any delta is read, and is recorded here as an **open obligation**, not as
done. Both figures are S20 moving substrate: **re-read from the arm under test**, never carried
from either plan file or from this one.

---

## 13. Dispositions for statistics that cannot be placed

Named now so they are not discovered mid-analysis:

- **phantom-node share (OQ-95) — DOES NOT EXIST.** Two silent filters
  (`drl_purity_network.pl:119` `exclude/3`, `giant_component_analysis.pl:115–125` `ord_memberchk`);
  neither counts what it dropped. Instrumenting it is *prerequisite* work, not measurement work.
  **Declared unavailable, with the reason and the instrumentation cost — not quietly omitted.**
- **`ep_band` / `action_band` histograms — DO NOT EXIST.** Both banders are per-row only;
  `ep_band` yields one corpus number (a migration *count*, `fpn_report.pl:161`). Register the
  count; **declare the histogram absent.**
- **giant-component FRACTION is not a JSON key.** `raw.json` ships `giant_size` and
  `pooled.n_nodes` separately; the fraction exists only in Markdown. Registered as a **derived**
  quantity with its two inputs named (both in §4.3, both `PENDING OQ-356`).
- **`boltzmann_summary`** — register `coupling_summary`; mark boltzmann **derived** (exact
  coarsening, OQ-355 F4).
- **The 4 statistics the instrument used to drop SILENTLY** are now registered with their reason:
  `network.stability` (CATEGORICAL, `cascading` on 19/19), `severe_share_within_type.unknown`
  (`None` on 19/19), and — the costly two — `contextuality.by_type.piton` and
  `severe_share_within_type.scaffold`, each of which had a **real number on 18 of 19 legs** and was
  deleted from the pair table by a single leg's `None`.

---

## 14. Declines — what would fire each bit the other way

Per bit, the result that would fire it the other way, and the positive control showing the
instrument **can** decline. All are planted fixtures in
`python/audits/leg_diagnostic_table.py --selftest`, wired as gate row `oq353 floors`:

| bit | fires when | declines when | fixture |
|---|---|---|---|
| B1 | `within_pure_max ≥ between_model_spread` | strictly below | both planted |
| B2 | ratio ≥ R_hi | ratio < R_lo | both planted |
| B3 | (a)/(c) separate beyond the floor, **not exposed** | fail to separate | both planted, + 4 exposure rows |
| B4 | (b)/(c) separate beyond the within-(c) floor | fail to separate | both planted |
| B5 | \|guard delta\| > measured null floor | within the floor | both planted |

**The NOT-exposed/SEPARATES fixture is not optional.** The four exposed B3 fixtures all carry
exposure, so an instrument that stamped `unreadable` on **every** separating row would pass all
four. That bug is the mirror of the spurious-saturated one: instead of manufacturing demotions it
**deletes B3's positive findings wholesale**, and it would surface in Phase 2 as *"no statistic
carries content"* — which reads like a result.

Also fixture-covered: that `PENDING OQ-356` and `NOT_MEASURED` both reach the **written** table
and stay textually distinguishable there (a tag that dies before the read site is not a tag), the
`CLASSIFY_STAMP_LAGS` pair, and the cut-point refusal.

---

## 15. Phase-1 handoff — the conditional-consequence table for OQ-356

**NO RECOMMENDATION IS MADE ON THE OQ-356 RULING, and the reason is stated rather than implied.**

- **The strip work does not generate the evidence the ruling turns on.** `effective_purity`
  returns `unknown` iff `purity_scoring:purity_score` returns a non-number. The strip changes the
  edge set and hence **GC membership**; it cannot change what `unknown` means or which stories
  have it. So the strip yields evidence about the defect's **blast radius** under two edge
  semantics — not about **what `count_by_action_band/8`'s guard should do with an unknown EP**,
  which is the ruling.
- **The interest is structural and directional, and is declared here so a reader knows this
  evidence was produced by a party with a stake:** Phase 2 cannot begin until OQ-356 lands in
  *any* form, so the producing party benefits from a fast fix in whichever shape unblocks
  `giant_comp`. That is precisely the seat the `blocked_on_human` tag reserves.

| if OQ-356 rules… | Phase 2 GAINS | Phase 2 LOSES |
|---|---|---|
| **guard returns `unknown`/abstains on non-numeric EP** (fail-soft) | `giant_comp` runs on all 20 corpora; the 5 `PENDING` statistics acquire values; **X1 and X2 both readable**; the (a′) twin runs regardless of whether the strip pulls unknown-purity stories into the GC | the GC-membership statistics acquire an abstention stratum whose denominator must be carried (Pattern 6) — `n_scored`/`n_total` on every GC aggregate |
| **guard excludes non-numeric EP from the band count** (fail-closed on absence) | same coverage as above; band counts stay numeric and directly comparable across arms | the excluded stratum is invisible unless counted; a growing exclusion would rewrite a time series while reading stable (the OQ-306 shape) |
| **guard raises — i.e. the throw is CORRECT and the data is wrong** | a real finding about corpus authoring; the 3 completing corpora stay usable | 17 of 20 corpora stay dark. **X2 unobtainable**; X1 readable only on haiku2/haiku3; B5 salvaged only on FPN + the two network members per §3.3 |
| **no ruling** | — | **Phase 2 does not start at all.** This is the current state |

---

## 16. Residues declared at the freeze

1. **Arm (d) is DEFERRED**, not dropped — OQ-347 step 4's situation-fixed core does not exist.
2. **There is no modern-schema, different-situations arm anywhere in the project.** All 19 legs are
   one seed set; `kernel_v1` shares ~800 ids with each twin and is pre-reset regime. The (a)-vs-(c)
   contrast therefore carries an irreducible vintage component, bounded per statistic by §6 rather
   than eliminated.
3. **Model mixture is the second irreducible confound on B3.** v6 is mixed-model with
   `story_provenance` on 0/3380, so arm (c) is heterogeneous **by construction and cannot be
   stratified back** — the label does not exist, and a mixed arm assembled from legs is the
   pooled-network non-arm the OQ forbids. Removing it needs a size-matched, modern-schema,
   single-model corpus of different situations: a generation spend, not a re-read.
4. **B3 is three-valued and `unreadable` is asymmetric** (§7). The count of statistics in each
   class is itself a result.
5. **Phantom-node share is unavailable**, with its instrumentation cost named (§13).
6. **The (c) floor rests on k=3, the structural maximum**; **the pair floor rests on k=2** — a
   difference, not a distribution. Neither floor magnitude is claimed (§8.2, §8.3).
7. **The (a′) arm's cross-checks are conditional TWICE OVER** — on OQ-356 landing, and on the
   stripped twin not throwing. Both dispositions are pre-written (§3.3); neither is decided after
   observation.
8. **B5's negative magnitude reading is bounded by the seat set, not by the leg choice** (§9).
9. **v6's ε-coherence is UNVETTED** — the seal is vacuous on it (§2), and this is a defect in the
   seal rather than a property of v6: 14 corpora exempt outright, 9 partially checked, 7,829
   readings unchecked, the live leg at 64.2%. Tracked as **OQ-365**. C1's outcome token is
   *not evaluated* — a fourth outcome the plan's three-outcome table does not contain.
   New at this freeze.
10. **Cut-points are RULED UNSET, not open** (§8.1a, operator 2026-08-24; **OQ-366**). B1/B2
    report continuous ratios stamped `BAND_UNSET`; the classifier raises rather than defaulting
    when a caller demands a bit. Unblocking condition: a third same-model draw — a generation
    spend, not a re-read. New at this freeze.
11. **The exposure columns are 44/56 and 41/56 UNTRACED** (§6), so the published exposure counts
    are a **floor**, not a census. New at this freeze.
12. **The stamp test's DISPOSITION deviates from handoff item 5** — verdict adopted in full,
    disposition is record-not-refuse, reason declared at §2a (operator-accepted 2026-08-24).
    New at this freeze.
13. **F5's directive keyword correction is owed to the BASE PLAN's text** — `:- multifile`, not
    `:- discontiguous` (§11). Carried into the Phase-2 `ISSUES.md` batch per the operator, so the
    plan's stated integrity check stops being one that cannot fail.
14. **The vacuous-seal finding is owed to `KNOWN_STATE.md`** — deferred to the Phase-2 batch by
    operator ruling, not forgotten.
15. **X1's counting rule is frozen but NOT YET VALIDATED** against a published
    `n_sibling_edges_stripped`, because that figure is a `giant_comp` product and `giant_comp` is
    blocked (§12). The validation is Phase 2's first (a′) step, before any delta is read.
