# OQ-306 — POST-IMPLEMENTATION EVALUATION (independent pass)

**Executed:** 2026-08-21
**Evaluator:** fresh general-purpose subagent (R-D). Independent **of the executor's session**, not
context-free — the harness injects CLAUDE.md, MEMORY.md and gitStatus (OQ-334). Everything below
that is called a finding was run against the substrate in this session; injected rules are cited
only as the standard a claim is measured against, never as evidence.
**Target:** commits `d7b4d4f8 6e1e9fd6 1a0f87e8 72ec21fe dbde6fe5 cf568697`; the executor's claims
are `WRITEUP.md`, `CONSUMERS.md` and the ISSUES OQ-306 CLOSE.
**Serialization:** `pgrep -x swipl` checked and empty before every swipl-spawning command; no two
swipl/pipeline processes were ever live. Corpus md5 `61697262…` verified identical to the audit
log's pin at the start and end of this pass, so every number below is over the same corpus the
executor measured.

**Verdict:** the engineering is sound and almost every witness reproduces exactly. The defect is in
the **historiography**: the executor overturned a correct, live-measured figure (`9`) on the strength
of a `git ls-tree` reconstruction that the *same commit* declares systematically unreliable in
exactly that direction, and wrote the inverted conclusion into CLAUDE.md, `build_discipline.md` and
the OQ heading. 12 gaps, 2 of them material.

---

## 1. What I re-ran, and what it returned

### 1.1 Kinding census, live leg (swipl, from `prolog/`)

```
[corpus] census: 258 stories, 27 non-story, 0 other.
[corpus] Loaded 285 testsets successfully.
kinds [axiom_contradiction-27,story-258]  total=285  corpus_constraint=285  corpus_story=258
BOUND story=258 axiom=27 dual=0 unknown=0
```
Byte-for-byte the WRITEUP's witness, including the bound-call equality (Pattern 7 safety holds).

### 1.2 Artifact read (`outputs/pipeline_output.json`)

```
manifest: n_constraints 285, n_stories 258, n_nonstory_members 27, n_unclassified 0,
          nonstory_kinds {"axiom_contradiction": 27}, schema_version 3,
          code_commit 1a0f87e82e2513… code_commit_short 1a0f87e  code_dirty TRUE
member_census: {'story': 258, 'axiom_contradiction': 27, 'dual_family': 0, 'unknown': 0}
per_constraint n: 285   Counter({'story': 258, 'axiom_contradiction': 27})
unique ids 285   ids-files 0   files-ids 0
contradictions files: 27   contr kinded axiom_contradiction: 27   mis: []
non-story ids NOT matching filename convention: []
```
The fact-family partition agrees with the filename convention on **all 285** members — no member is
mis-kinded on the live leg.

### 1.3 My own two-sided control on `corpus_member_kind/2` (scratch leg, not the executor's)

Built a 4-file scratch leg: one real story, one real `*_contradictions.pl`, one planted file
carrying **neither** fact family, one planted file carrying **both**.

```
[corpus] census: 1 stories, 1 non-story, 2 other.
  ability_ceiling_reading                          -> story
  actinide_replenishment_mechanism_contradictions  -> axiom_contradiction
  planted_both                                     -> dual_family
  planted_neither                                  -> unknown
NONMEMBER: fails (good)
```
The predicate is total, disjoint, fires on all four kinds, and **declines on a non-member**.

### 1.4 Refusal — all branches, driven directly through `add_member_census_keys`

```
  'testsets'                                    refusal_scope=True
  'testsets/'                                   refusal_scope=True
  '../prolog/testsets'                          refusal_scope=True
  '<abs>/prolog/testsets'                       refusal_scope=True
  'testsets_haiku'                              refusal_scope=True
  'archives/datasets/original_v5'               refusal_scope=False
  '/tmp/scratchleg'                             refusal_scope=False

A clean live leg      : ACCEPTED -> n_stories 3, nonstory {axiom_contradiction:1}, n_unclassified 0
B live leg w/ unknown : REFUSED  -> OQ-306 REFUSAL (testsets is a live leg): 1 corpus member(s)…
C live + override     : ACCEPTED -> …n_unclassified 1, unclassified_refusal_overridden{authorized_by…}
D archive w/ unknown  : ACCEPTED -> loud stderr warning, ids named, no flag
E dual_family live    : REFUSED  -> OQ-306 REFUSAL …
  identity(i) fires: member_census.story=99 but python counted 3 story entries
  sum invariant fires: n_stories(4)+n_nonstory_members(0)+n_unclassified(0)=4 != n_constraints(99)
  absent census fires: pipeline output carries no `member_census`…
  missing member_kind fires: 1 per_constraint entr(ies) carry no `member_kind`…
```
**The DECLINE is real** (branch A accepts a clean document). No bypass found — see §5b.

### 1.5 Independent re-derivation of R-B's skew (pure `/usr/bin/grep`, no swipl, no reuse of the
executor's instrument)

```
archives/datasets/original_v5             members=702    no-self-keyed-metric=91    13.0%
archives/datasets/original_json/testsets  members=1151   no-self-keyed-metric=133   11.6%
archives/datasets/original_v6             members=3380   no-self-keyed-metric=0      0.0%
archives/datasets/kernel_v1               members=1106   no-self-keyed-metric=0      0.0%
testsets                                  members=285    no-self-keyed-metric=27     9.5%
```

### 1.6 Per-leg census + timings (`corpus_census_check.kind_census`)

```
testsets           total=  285 axiom_contradiction=27 story=258   [1.26s]
testsets_haiku     total=  960 story=960                          [2.91s]
testsets_flash     total=  960 story=960                          [2.23s]
testsets_kimi      total= 1005 story=1005                         [2.53s]
testsets_sonnet    total= 1001 story=1001                         [2.68s]
```

### 1.7 Load-time delta, re-measured by a different method

Instead of a clean/edited pair I timed the census computation directly inside a loaded corpus:
```
LOAD_WITH_CENSUS_ms=772.31  CENSUS_ONLY_ms=143.09  IMPLIED_BASELINE_ms=629.22
```
143.09 ms against the claimed **+143.80 ms** — agreement to 0.5 %. The claim reproduces.

### 1.8 Stratum series, re-derived from git

```
f724379d 2026-08-07 stratum=5  members=203
543e2f9a 2026-08-08 stratum=22 members=227
8c34157f 2026-08-17 stratum=26 members=279
2f73ce34 2026-08-21 stratum=27 members=285
```
Identical to `stratum_series.txt`. **As a git figure it reproduces exactly.** What it means is
Gap 1.

### 1.9 The gate

`./scripts/gate.sh` → exit 0, **27 rows, 27 ✓, 0 ✗, `GATE: GREEN`**.
```
  ✓ corpus census    corpus_census_check: GREEN — 5 leg(s), totality holds, stratum pinned
                     {'testsets': 27, 'testsets_haiku': 0, 'testsets_flash': 0,
                      'testsets_kimi': 0, 'testsets_sonnet': 0}, selftest (6 controls)
```
`grep -c '^run ' scripts/gate.sh` = 27. Standalone row wall time: **13.29 s**.

### 1.10 Golden baseline and corpus pin

```
238b6603aa00bce0625f13d181bfb7a9  outputs/golden_classifications.json   ids: 285
cat prolog/testsets/*.pl | md5sum → 616972623c8f8d85df19cd5ddb9b98c4   (== audit_log pin)
```

---

## 2. Claimed vs verified

| # | Claim (WRITEUP / ISSUES close) | How I checked | Result |
|---|---|---|---|
| 1 | live leg 285 = 258 story + 27 axiom_contradiction | swipl re-run §1.1 | **✓ exact** |
| 2 | `member_census {"story":258,"axiom_contradiction":27,"dual_family":0,"unknown":0}` | artifact §1.2 | **✓ exact** |
| 3 | manifest `n_stories 258 / n_nonstory_members 27 / n_unclassified 0 / schema_version 3` | artifact §1.2 | **✓ exact** |
| 4 | manifest cite `code_commit d7b4d4f8` | artifact §1.2 | **✗ artifact says `1a0f87e8`, `code_dirty: true`** (Gap 9) |
| 5 | sum `258+27+0 == 285`; `corpus_constraint=285`, `corpus_story=258` | §1.1 | **✓** |
| 6 | bound-call safety measured, 258 / 27 | §1.1 | **✓** |
| 7 | fail-closed kinds reachable (`dual_family`, `unknown`) | my own plants §1.3 | **✓ independently reproduced** |
| 8 | census zero-guard reachable, declines on intact registry | code read + §1.1 | ✓ reachable; **independence over-claimed** (Gap 11) |
| 9 | stratum series 5→22→26→27 at those four commits | `git ls-tree` §1.8 | ✓ as a *git* series; **✗ as the corpus series** (Gap 1) |
| 10 | "the 9 does not appear anywhere in the series" | `audits/2026-07-02_oq136…/membership.tsv` | **✗ REFUTED — 9 distinct ids, live-measured 2026-07-02** (Gap 1) |
| 11 | "5 → 22 inside a single day is sharper evidence of growth" | `git show -s 543e2f9a` | **✗ INVERTED — that commit is a tracking event** (Gap 2) |
| 12 | `original_v5` 91/702 (13.0 %) | independent grep §1.5 | **✓ exact** |
| 13 | `original_json/testsets` 133/1151 (11.6 %) | independent grep §1.5 | **✓ exact** |
| 14 | `original_v6` 0/3380, `kernel_v1` 0/1106 | independent grep §1.5 | **✓ exact** |
| 15 | load 590.71 → 734.51 ms, +143.80 ms | independent method §1.7 → 143.09 ms | **✓ within 0.5 %** |
| 16 | "≈2.1 s of kinding across five legs" (the number that decided R-G) | §1.6 / §1.9 | ✓ for marginal kinding; **✗ as the per-gate cost, which is 13.3 s** (Gap 10) |
| 17 | twin haiku `960 / 960 / 0 / 0` | `kind_census` §1.6 | **✓ exact** (flash 960, kimi 1005, sonnet 1001 also confirmed) |
| 18 | golden anchor md5 `238b6603…`, 285 ids, PASS | md5sum + json §1.10 | **✓ exact** |
| 19 | corpus md5 `61697262…` pinned across both halves | §1.10 | **✓ exact, and still current** |
| 20 | refusal: 3 branches + DECLINE control | driven directly §1.4 | **✓ all four reproduce** |
| 21 | scope canonicalization relative / absolute / dotdot → refusal scope | §1.4 | **✓** |
| 22 | `twin_comparison.py` fix two-sided (accepts 2,3; refuses 1,4,None) | code read | **✓** — `JOINABLE_SCHEMA_VERSIONS = (2,3)` with `not in` |
| 23 | gate GREEN, **28 rows** | §1.9 | ✓ GREEN; **✗ 27 rows** (Gap 6) |
| 24 | `corpus census` row GREEN, stratum `{testsets:27, twins:0}`, selftest 6 controls | §1.9 | **✓ verbatim** (but see Gap 5 on the 6th) |
| 25 | consumers swept, per-consumer dispositions | my own 5-tree grep §5d | **✗ ≥7 reader sites undispositioned** (Gap 7) |
| 26 | CONSUMERS.md control (e) `schema_version` → "`prolog/` … hits ✓" | `grep -rn schema_version prolog/` | **✗ 3 hits, all under `prolog/archives/`, which the sweep EXCLUDES** (Gap 8) |
| 27 | keys "cannot be present on one path and absent on the other" | manifests of sibling artifacts §5c | **✗ two artifacts stamp schema 3 without the keys** (Gap 4) |
| 28 | C1 `per_constraint` md5 `000358d6…` identical clean-vs-edited | artifacts overwritten, `outputs/` gitignored | **UNVERIFIED — not reconstructible** |
| 29 | C2 "0 per_constraint entries differ after removing added fields" | same | **UNVERIFIED — not reconstructible** |
| 30 | C4 manual `RED 1 / DECLINE / RED 2` demonstrations | not recorded as re-runnable artifacts | **UNVERIFIED as recorded**; I reproduced arm-1 discrimination independently (§1.3) and found the *selftest's* arm-2 control vacuous (Gap 5) |
| 31 | mid-session move 279 → 285, stratum 26 → 27 | `git show 2f73ce34` / `f32fe86b` | **✓ commit subjects confirm both** |
| 32 | `f32fe86b` left the emitted contradictions file untracked | `git show --stat` both commits | **✓ exact** — `2f73ce34` is literally titled "land the contradictions file … `f32fe86b` left behind" |

---

## 3. Zero classification

Every zero in the record, graded MEASURED (an instrument that demonstrably could have returned
non-zero, on the same path) vs DIDN'T-LOOK / UNDISCRIMINATED.

| Zero | Grade | Basis |
|---|---|---|
| `member_census.dual_family = 0` | **MEASURED** | I planted a both-families file in a scratch leg and the *same predicate on the same path* returned `dual_family` (§1.3). Not merely the executor's claim. |
| `member_census.unknown = 0` | **MEASURED** | Same run returned `unknown` for a neither-family file (§1.3). |
| `manifest.n_unclassified = 0` | **MEASURED** | Derived from the per-entry kinds; my branch tests B/C/D/E show every non-zero path refuses or flags (§1.4). |
| `n_nonstory_members = 0` on the four twin legs | **MEASURED** | `kind_census` enumerated 960/960/1005/1001 real members per leg through the identical code path that returns 27 on the live leg (§1.6). Two-sided within one instrument. |
| `nonstory_kinds {}` on twins | **MEASURED** | Same. Emitted as an authored empty object, not an absent key. |
| `original_v6 0/3380`, `kernel_v1 0/1106` (R-B) | **MEASURED** | The same grep returns 91 and 133 on the other two legs in the same loop (§1.5) — the instrument's decline and fire are in one run. |
| `[totality] … 0` (gate arm 1, all five legs) | **MEASURED** | The checker's planted `unknown`-shape fixture makes arm 1 fire; I additionally reproduced `unknown` kinding independently. |
| `0 declared collisions` / `0 problems` on unrelated gate rows | **MEASURED** | Each carries its own selftest count in the row text (`selftest 7/7`, `11/11`, `50/50`, …). |
| **`audit cites   ERRORS: 0`** | **UNDISCRIMINATED** | `python/audit_citation_status.py` has **no selftest and no positive control**; the gate row prints a bare `ERRORS: 0`. This is a pre-existing row, *not* introduced by OQ-306, but the task asked me to classify every zero and this one has no discrimination record. |
| **selftest arm-2 "off-by-one" control** | **VACUOUS** | `real == bumped` compares two dicts built to differ. Dead branch — cannot fail, witnesses nothing. Gap 5. |
| `0 per_constraint entries differ` (C2) | **UNVERIFIED** | Artifacts gone; `outputs/` gitignored. |
| `n_stories 0 / n_nonstory 0 / n_unclassified 0` on an empty document | **BENIGN EDGE** | I drove an empty document through `add_member_census_keys`: it is ACCEPTED (0 == 0 everywhere). Only reachable under `allow_empty_corpus`; `corpus_loader.pl` documents exactly this regime at the site. Not a gap. |

---

## 4. Control assessment

**Real and reproduced.**
- **`corpus_member_kind/2` two-sided, naturally-arising.** Fires on a real contradictions file,
  declines to `story` on a real story file, *fails* on a non-member. I reproduced all three plus
  both fail-closed kinds on my own plants. This one is exactly as advertised.
- **Refusal DECLINE control.** Branch A (clean live-leg document) is ACCEPTED with
  `n_unclassified 0` and no flag, while B and E refuse. The refusal genuinely discriminates; it does
  not always fire. Reproduced (§1.4).
- **Planted unknown-shape selftest (arm 1).** Real, and its two-sidedness (the copied real files must
  still kind `story`) is a genuine anti-fire-on-everything check.
- **`twin_comparison.py` fix.** Two-sided by construction: `not in (2,3)` accepts 2 and 3, refuses
  1, 4 and `None`. Verified by code read.

**Not real.**
- **The arm-2 pin control (Gap 5).** `real == bumped` cannot be true. It exercises none of
  `check()`'s `got != want_counts`. It nonetheless contributes to the advertised "6 controls" — the
  precise shape CLAUDE.md names as *control count rises while coverage falls*.

**Over-claimed.**
- **The census zero-guard's "two INDEPENDENT derivations" (Gap 11).** `Loaded` is incremented and
  `register_corpus_constraint/1` is called **in the same conditional branch of the same loop**
  (`corpus_loader.pl` `load_testset_list/3`). `retractall(corpus_constraint(_))` precedes the loop
  and nothing else writes between the loop and the census, so the demonstrated firing condition — a
  hand-retracted registry fact — cannot arise inside `load_all_testsets/0`. It is more than the
  by-construction total the executor correctly refused to use, but it is not two independent
  derivations, and its only witness is a plant of an unreachable state.
- **"member_census is an INDEPENDENT Prolog enumeration."** Both sides enumerate the *same*
  `corpus_constraint/1` registry through the *same* `corpus_member_kind/2`
  (`json_report.pl:64` builds `Constraints` from that registry). The identity therefore tests the
  Prolog→JSON→Python serialization boundary, not the kinding. The **in-code comment states this
  correctly** ("one DEFINITION read twice, not one read"); the WRITEUP's prose does not carry the
  qualifier down. Low severity, listed as Gap 12 for completeness.

**The plant-only altitude claim: honest in verdict, under-diagnosed in reason — and inconsistent
with the same commit's other use of the same evidence.**
Downgrading the growth guard's discrimination record to plant-only is *correct*, and it is an
under-claim rather than an over-claim, which is the right direction. But the stated reason ("a git
reconstruction is a different corpus") understates what actually happened: `543e2f9a` — the pair's
**N** — is titled *"corpus: track the 20 remaining `*_contradictions.pl` testsets (already
glob-loaded)"*. The pair does not observe a stratum that grew; it observes a *tracking event* over
files that had been on disk for weeks. The deeper problem is the asymmetry: the executor treats the
git reconstruction as **too unreliable to support a control** and, three paragraphs later, as
**reliable enough to overturn a live measurement and rewrite CLAUDE.md**. Those cannot both hold.

---

## 5. Directed checks

**(a) Is `corpus_member_kind/2` total and disjoint, or does it mis-kind?**
Total (fresh-variable catch-all, `member_kind_/2` clause 4) and disjoint (dual-family test runs
first, every clause cut, every kind atom bound *after* the cut so a bound `Kind` is safe). Verified
empirically on 285 + 3926 twin members and on my 4-file plant leg. **No mis-kinded member found.**
Two notes, neither a defect: a story file that also carried a `cs_axiom_contradiction` clause would
kind `dual_family` and redden arm 1 (declared design); a file that fails `consult` never enters the
registry at all, but the `n_stories + n_nonstory + n_unclassified == n_constraints` invariant catches
that divergence because `n_constraints` is the python-side glob — a real strength nobody claimed.

**(b) Does the refusal have a bypass the executor missed?**
**No — I looked and did not find one.** `_resolve_corpus_dir` canonicalizes both sides before
comparing; relative, trailing-slash, dotdot and absolute forms of the live leg all resolve to
refusal scope. A symlinked leg resolves *toward* refusal scope (the conservative direction). The
`prolog/../prolog/testsets` form falls out of refusal scope, but Prolog's own
`resolve_corpus_dir/2` would glob a non-existent directory and throw `corpus_empty`, so there is no
divergence to exploit. The `SDM_ALLOW_UNCLASSIFIED_MEMBERS` hatch accepts any non-empty string
(including `"n/a"`), but it is documented, it selects the pre-existing continue branch, and it stamps
`unclassified_refusal_overridden` into the manifest — verified in branch C. The raw artifact
(`pipeline_output.raw.json`) carries `member_census`/`member_kind` before any refusal runs, but it
is an intermediate with no manifest and no consumer claim attached to it.

**(c) Is `member_census` independent of the per-entry emission?**
Not in the strong sense the WRITEUP's word "INDEPENDENT" implies — see §4. It *is* independent
across the Prolog/Python boundary, which is what the identity checks actually test, and the code
comment says so accurately.

**(d) Did the sweep miss a consumer of `n_constraints` / `schema_version`?**
**Yes.** See Gaps 4, 7 and 8.

**(e) Stale or now-wrong statements left in edited files?**
**Yes.** See Gap 3. Also confirmed *fixed*: `drl_core.pl`'s "two non-story files" comment is gone
repo-wide (`grep "two non-story"` → 0 hits in `.pl`/`.py`). The `[corpus] census:` line is placed
before `Loaded N`, and the only `tail -1` in the repo is `scripts/gate.sh:31,33` over checker output
— the executor's last-line-parser claim checks out.

---

## 6. GAPS

**Material (2).**

1. **The heading correction is itself wrong: `9` was a live measurement, not an error — and the
   executor's own Finding 2 predicts exactly why the replacement series misses it.**
   `audits/2026-07-02_oq136_census_bucket_provenance/membership.tsv` (dated, pre-registered,
   contemporaneous) names **9 distinct `*_contradictions` cids** at n=119:
   ```
   actinide_replenishment_mechanism  digital_money_legitimacy  generality_standard
   knowledge_legitimacy_biomedicine  learning_difficulty_substrate  moral_causation_locus
   performance_legitimacy  polaris_document_status  visual_evidentiary_authority
   ```
   Git at the nearest commit (`d9bffdac`, 2026-07-03) tracks **4** of those 9, and **114** members
   against the live artifact's 119 — short by exactly 5 on *both* axes. The five missing files
   (`generality_standard`, `knowledge_legitimacy_biomedicine`, `learning_difficulty_substrate`,
   `moral_causation_locus`, `polaris_document_status`) were each first tracked at **`543e2f9a`
   (2026-08-08)**. So the on-disk stratum on 2026-07-02 *was* 9; the git series says 4 because the
   files were untracked — the exact mechanism Finding 2 discovered. The correction inverts the
   evidential hierarchy: a reconstruction the same commit calls "a corpus state that never existed
   on disk" was used to overturn a dated live measurement, and the OQ heading, CLAUDE.md and
   `build_discipline.md` were permanently rewritten on it. **Fix:** restore `9` as a live figure,
   state the git series as a *git* series, and keep the (genuinely valuable) Finding 2 as the reason
   the two disagree.

2. **"A 5 → 22 jump inside a single day is a sharper instance of the growth" is inverted — that
   commit is a tracking event, not growth.** `git show -s --format=%s 543e2f9a` →
   *"corpus: track the 20 remaining `*_contradictions.pl` testsets (**already glob-loaded**)"*
   (17 files, +229 lines). The executor **quotes this exact subject in Finding 2** as proof that git
   understates the stratum, then presents the same commit's delta as evidence of a one-day stratum
   growth. The real on-disk growth was gradual. This inverted sentence now sits in **CLAUDE.md**
   (always-loaded) and `build_discipline.md` as doctrine, one paragraph above a general rule that
   says *"before using git history as the population for a retrospective census, check whether the
   thing you are counting is something the commit convention actually tracks."*

**Substantive (4).**

3. **The correction was not propagated; three surfaces now contradict it, one of them the same
   file 38 lines away.**
   ```
   ISSUES.md:4965                       "the stratum has since grown **9 → 26**"
   KNOWN_STATE.md:1585                  "the stratum **grew 9 → 26** while reading stable"
   docs/technical/build_discipline.md:1850  "the stratum GREW, 9 → 26, across …"
   docs/technical/build_discipline.md:1888  "**5 → 22 → 26 → 27**, not 9 → 26; \"9\" appears nowhere"
   ```
   (`ISSUES.md:7819` is a deliberate preserved-with-annotation quotation and is fine.) Whichever
   figure is right — and per Gap 1 the `9` is — a document that asserts both is a Pattern-2 fork of
   the very kind this OQ closed. Left as-is, the next reader gets whichever line they hit first.

4. **Two artifacts stamp `schema_version: 3` while carrying none of the keys the bump defines —
   the exact "present on one path, absent on the other" the WRITEUP says is impossible.**
   `build_manifest` has **four** call sites; only two route through `inject_manifest`
   (`run_pipeline.py:446`, `:2007`). The other two — `:1081` (`commentary_census.json`) and `:1538`
   (`reading_reference_census.json`) — write a manifest directly.
   ```
   outputs/commentary_census.json        schema_version=3  n_stories=ABSENT  n_nonstory=ABSENT  n_unclassified=ABSENT
   outputs/reading_reference_census.json schema_version=3  n_stories=ABSENT  n_nonstory=ABSENT  n_unclassified=ABSENT
   outputs/orbit_data.manifest.json      schema_version=3  n_stories=258     n_nonstory=27      n_unclassified=0
   ```
   `schema_version: 3` is documented at the emitter as *meaning* those keys are present. A reader
   branching on `schema_version >= 3` gets a `KeyError` or a silent `None` on two of the three.
   `commentary_census.pl` is, additionally, one of the consumers CONSUMERS.md explicitly routes to
   OQ-136/OQ-202 — its manifest emission was not noticed.

5. **The gate selftest's arm-2 control is vacuous, and it inflates the advertised control count.**
   `corpus_census_check.py`, in `selftest()`:
   ```python
   checks += 1
   real   = {"testsets": {"axiom_contradiction": N}}
   bumped = {"testsets": {"axiom_contradiction": N + 1}}
   if real == bumped:
       fails.append("selftest: pin comparison cannot distinguish an off-by-one")
   ```
   Two dicts constructed to differ. The branch is dead; no value of `N` makes it fire; it exercises
   nothing in `check()`'s `got != want_counts`. The row nevertheless advertises **"selftest
   (6 controls)"**. A real version would call `check()` against a temporarily-bumped baseline dict
   and assert a `[pin]` problem is returned. (The WRITEUP's manual "RED 2" demonstration may well
   have been genuine; the control that **rides every gate run** is not.)

6. **The gate has 27 rows, not 28.** `grep -c '^run ' scripts/gate.sh` → 27; the run printed 27 ✓
   and 0 ✗. `GATE: GREEN` is correct; the count is not.

**Minor (6).**

7. **`CONSUMERS.md`'s roster is not the union of the five greps it says it is.** At least **nine**
   sites read `manifest["n_constraints"]` and carry no disposition anywhere in the document:
   `python/audits/schema_sieve.py:118` (compares its own row count against it),
   `python/audits/g_beneficiary_channel_audit.py:195` (prints it as *"Corpus: N constraints"* — now
   a mislabel), `python/container_typology_analysis.py:495`, `python/run_drift_mismatch.py:96`,
   `python/epsilon_authorship_readout.py:148`, `python/tensions_ledger.py:385`,
   `python/audits/audit3_synthesis.py:447`, `python/audits/oq151_dual_gauge_crosstab.py:85`,
   `python/audits/oq88_false_mountain_detector.py:260`. All nine are provenance echoes or identity
   comparisons rather than rate denominators, so **nothing is broken** — but "consumers swept with
   per-consumer dispositions" over-claims the sweep's completeness, and `grep -c` on CONSUMERS.md
   returns **0** for every one of those names.

8. **`CONSUMERS.md` control (e) is unsubstantiated.** The table claims sweep (e) `schema_version` was
   controlled at "`prolog/` (outside `python/`)" with result "hits ✓". `grep -rn schema_version
   prolog/` returns **3 hits, all under `prolog/archives/`** — a subtree the same document explicitly
   **excludes** from the sweep. There are **zero** `schema_version` occurrences in any `prolog/*.pl`.
   It is also the only row in that table with no hit count, where (a)–(d) give 1, 8, 16, 5. Either
   the control fired inside an excluded subtree (so it demonstrates nothing about the sweep as run)
   or it was never run and "hits ✓" is prose.

9. **The manifest cite does not match the artifact.** The WRITEUP cites `code_commit d7b4d4f8`;
   `outputs/pipeline_output.json` carries `code_commit 1a0f87e82e2513…` with **`code_dirty: true`**.
   Every other cited value matches. A dirty-tree artifact is not attributable to a commit at all, so
   the cite as written is not reconstructible.

10. **R-G's decisive number understates, by ~6×, the quantity R-G was actually about.** The
    one-definition-vs-textual-ratchet choice was made against "+143.8 ms … ≈ 2.1 s of kinding across
    five legs" — but the ratchet was proposed specifically *"to dodge per-gate swipl latency"*, and
    the `corpus census` row costs **13.29 s** wall standalone, because `--check` performs **seven**
    corpus loads (five legs + the live leg again inside `selftest()` + the planted tempdir leg), and
    a load dominates the kinding. The ruling is very likely still right at 13 s; the number recorded
    as supporting it is not the number that matters, and the growth trigger is stamped to file counts
    rather than to that cost.

11. **The census zero-guard's "two INDEPENDENT derivations" is over-claimed** — write-coupled in one
    loop branch; the demonstrated firing state is unreachable inside `load_all_testsets/0`. See §4.

12. **"An INDEPENDENT Prolog enumeration" (member_census) is loose in the WRITEUP** — both sides
    enumerate the same registry through the same predicate; the identity is a serialization check.
    The code comment states this correctly; the prose does not carry the qualifier. See §4.

**Categories with NO gaps found — stated explicitly rather than omitted:**
- **Mis-kinding.** None. `corpus_member_kind/2` is total, disjoint, and correct on all 285 live +
  3,926 twin members; it declines on non-members; both fail-closed kinds fire on plants I built.
- **Refusal bypass.** None found. Scope canonicalization, the hatch, and all four branches behave as
  documented, and the hatch is manifest-stamped.
- **Numeric accuracy of the measurements.** Every quantitative claim I could re-derive — the live
  census, the twin legs, the R-B skew on four corpora, the load delta, the golden md5, the corpus
  md5, the stratum figures *as git figures* — reproduced exactly or within 0.5 %. There is no sign
  of a fabricated number anywhere in this record.
- **Behaviour preservation.** Not checkable (Gaps: none — see UNVERIFIED below).

---

## 7. UNVERIFIED (with reasons)

| Item | Reason |
|---|---|
| C1 clean-vs-edited `per_constraint` md5 `000358d6…` identical | `outputs/` is gitignored and both halves were overwritten by later runs. Not reconstructible without re-running C1 against a reverted `corpus_loader.pl`, which would be a write to the tree during an evaluation pass. |
| C2 "0 per_constraint entries differ after removing the added fields" | Same. |
| Clean-side run stamps `T14:23:47Z` / `T14:30:09Z`, `code_dirty: false` | Same. |
| The C4 manual demonstrations as recorded (`RED 1` / `DECLINE` / `RED 2`) | No re-runnable artifact was committed for them. I reproduced arm-1 discrimination by an independent route (§1.3) and found the *selftest's* standing arm-2 control vacuous (Gap 5). |
| `loadtime_edited_ms.txt` showing `734.51` twice to two decimals | Statistically unusual against a ~2 ms spread, but my independent re-measurement of the same delta (143.09 vs 143.80 ms) corroborates the figure. **No evidence of a problem; recorded only because I noticed it and would rather say so.** |
| Whether the `[corpus] census:` line breaks any consumer outside the repo | Repo-internal sweep only; the executor's four-consumer enumeration and the single `tail -1` (gate.sh, over checker output) both check out inside the tree. |

---

## 8. Note on my own limits

This pass is **context-sighted**: CLAUDE.md, MEMORY.md and gitStatus were injected before I read
anything. Where I invoke a project rule (paste-or-untag, measured-vs-didn't-look, plant-only
altitude, Pattern 2/5/6) I am applying a handed-down standard, not discovering it — those are
RECALLS. The findings are the substrate reads: the 2026-07-02 `membership.tsv` naming nine cids, the
`543e2f9a` commit subject, the two `schema_version: 3` artifacts missing their keys, the dead
`real == bumped` branch, the 27-row gate, and the nine undispositioned reader sites. Each is a
command in §1 or §6 that anyone can re-run.
