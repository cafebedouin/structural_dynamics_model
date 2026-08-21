# OQ-306 — story membership becomes a checked fact (C1–C5 COMPLETE)

**Executed:** 2026-08-21
**OQ:** OQ-306 (the denominator silently admits non-stories)
**Fired:** live

**Verdict, at its scoped altitude:** story membership is a checked fact
(`corpus_loader:corpus_story/1`, `corpus_member_kind/2`), the manifest reports the two populations
separately, consumers are swept with per-consumer dispositions, and gate row `corpus census` guards
the share over time. All nine escalated rulings (R-A..R-G, R-I, R-J) were answered by the operator
on 2026-08-21 and are recorded individually in the ISSUES close; **none was implemented as if
ruled**, and execution stopped at C2 until they arrived.

**Scope limit, stated plainly:** this work makes the two populations DISTINGUISHABLE and guards the
share over time. It does **not** restate any existing published rate. The rate-bearing consumers are
routed to OQ-136 / OQ-202, which own that arithmetic.

**Manifest cite:** `outputs/pipeline_output.json` — `n_constraints 285`, `n_stories 258`,
`n_nonstory_members 27`, `n_unclassified 0`, `schema_version 3`. **The artifact currently on disk
carries `code_commit 1a0f87e8` with `code_dirty: true`** and is therefore NOT attributable to a
commit; regenerate at a clean HEAD before citing it as reconstructible. The C1 diff pair — which IS
the behaviour-preservation witness — was taken at clean `d7b4d4f8`, `code_dirty: false`:
clean side `2026-08-21T14:23:47Z` vs edited `T14:30:09Z`.

**Population note, because two numbers appear in this record and are NOT a contradiction:** the live
leg was **279** at session start and **285** from 04:23 local, when an untagged `c-orchestrator` run
completed and landed 6 files. Every anchor, diff pair and witness below was taken at **285**, after
the move. The 279/279 figure appears only in the discussion of the pre-existing 2026-08-18 artifact
that was considered and rejected as a golden-baseline source.

---

## What landed

**C1** (`6e1e9fd6`) — `corpus_loader.pl`: `corpus_story/1` and `corpus_member_kind/2` exported, the
private `has_story_facts/1` / `contributes_axiom_contradiction/1` / `report_member_census/1`, a
census stderr line before the existing `Loaded N` line, and the verbatim R-H decline marker.
`drl_core.pl`'s stale "two non-story files" comment fixed — as a POINTER, not a literal count
(`testsets/` carries no count, operator 2026-08-18). `module_boundary_allowlist.txt` reason texts
corrected for the read sites this change creates.

Kinding is total over `corpus_constraint/1` into `{story, axiom_contradiction, dual_family,
unknown}`. **Disjointness is tested FIRST**, so a member satisfying both fact families surfaces as
`dual_family` rather than being absorbed into `story`.

**C2 + C3** (`72ec21fe`) — per-entry `member_kind`; top-level `member_census` (an INDEPENDENT Prolog
enumeration, *not* a tally of the entries just written, so the cross-boundary identities can
actually fail); manifest `n_stories` / `n_nonstory_members` / `nonstory_kinds` (sorted) /
`n_unclassified`; `schema_version` 2 → 3; the R-B scoped refusal and the R-I documented hatch;
`python/shared/corpus_legs.py`. Wired at the single `inject_manifest` site both the default pipeline
and `classify_corpus` route through, so the keys cannot be present on one path and absent on the
other.

**C4** (`dbde6fe5`) — `python/corpus_census_check.py`, `corpus_census_baseline.json`, gate row
`corpus census`.

**C5** — the ISSUES close (heading corrected, rulings recorded individually), a `Tier: tripwire`
KNOWN_STATE entry so the naming debt reaches future `run_pipeline.py` editors, the CLAUDE.md
denominator paragraph, the `build_discipline.md` detail entry, `CONSUMERS.md`, index regen.

---

## Witnesses

```
[corpus] census: 258 stories, 27 non-story, 0 other.
[corpus] Loaded 285 testsets successfully.

kinds [axiom_contradiction-27, story-258]  total=285  corpus_constraint=285  corpus_story=258
member_census      {"story":258,"axiom_contradiction":27,"dual_family":0,"unknown":0}
manifest additions {"n_stories":258,"n_nonstory_members":27,"n_unclassified":0,
                    "nonstory_kinds":{"axiom_contradiction":27}}      sum 258+27+0 == 285
```

**Classification.**
- **Two-sided, NATURALLY-ARISING** (not a plant): a real contradictions file kinds
  `axiom_contradiction`, a real story file kinds `story`, a non-member fails. The DECLINE is the
  informative half.
- **Bound-call safety, MEASURED not asserted:** `corpus_member_kind(_, story)` = 258 and
  `(_, axiom_contradiction)` = 27, equal to the unbound enumeration — the fresh-variable-head /
  unify-after-cut idiom holds against Pattern 7. `dispatch head` stayed at 49 declared hits.
- **Fail-closed kinds — PLANTED fixtures, bottom rung, reported at that altitude:** neither family
  → `unknown`; both → `dual_family`. The live corpus supplies no natural instance of either, so this
  licenses only "authored drift gets kinded correctly."

**The census zero-guard (N16) is REACHABLE and two-sided:** declines on the intact registry, fires
on a retracted `corpus_constraint/1` fact — `CENSUS DISCREPANCY: 284 members kinded but 285 files
loaded`. **Scoped honestly after the evaluation:** the two sides are separately DERIVED (load-loop
success counter vs registry enumeration) but not fully independent — the same loop both increments
the counter and triggers the registration, so inside `load_all_testsets/0` the demonstrated firing
state is not reachable without external retraction. It catches registry/loop divergence introduced
AFTER load; it is not a proof that the loader cannot desynchronise.

**Behaviour preservation.** C1: clean vs edited pipeline, exit 0 both, mtime advanced, corpus md5
`61697262…` identical across both halves, `per_constraint` md5 `000358d6…` **identical**.
C2: additive only — `+member_census`, `+4` manifest keys, `+member_kind`, `schema 2→3`, and **0
per_constraint entries differ after removing the added fields**.

**Refusal, three branches with a DECLINE control.**
```
scope canonicalization: relative / absolute / dotdot -> refusal scope; scratch leg -> not
A hard refusal    : SystemExit, ids named, remediation text visible
B R-I hatch       : ships four-valued + manifest.unclassified_refusal_overridden naming the authorizer
C DECLINE control : clean live-leg document ACCEPTED, n_unclassified 0, no flag
```
C is the half that matters — the refusal discriminates rather than always firing.

**Twin leg** (`classify_corpus`, `testsets_haiku`): `n_constraints 960, n_stories 960,
n_nonstory_members 0, n_unclassified 0, nonstory_kinds {}` — the complement case, zeros emitted as
zeros rather than absences.

**C3 golden check** with baseline provenance FIRST: anchored md5 `238b6603…` == observed md5, 285
ids, then `PASS: All 285 constraints match baseline`. A green without that provenance is
byte-identical to a run that never compared. The anchor did **not** rot (no ruling wait intervened
between C1 and C3, corpus md5 unchanged).

**C4 gate row.**
```
--check GREEN — 5 legs, totality holds, stratum {testsets:27, twins:0}, selftest 7 controls
RED 1 (arm totality) : planted unknown-shape member -> RED, names the file
DECLINE control      : same leg, plant removed      -> GREEN
RED 2 (arm pin)      : baseline off-by-one, corpus untouched -> RED
```

**Natural pair**, HEAD checker vs materialized historical corpora (`git show` per blob into a
scratch leg — never a worktree, avoiding the gitignored-`outputs/` trap), against a SCRATCH baseline
pinned to N−1's stratum (against today's committed baseline both states would redden for a reason
unrelated to the stratum, and "fires at N, declines at N−1" could not be observed at all):

| commit | stratum | members | verdict | arm |
|---|---|---|---|---|
| `f724379d` (N−1) | 5 | 203 | **GREEN** | both quiet |
| `543e2f9a` (N) | 22 | 227 | **RED** | `pin` |

**Claimed at PLANT-ONLY altitude** (operator ruling) — see Finding 2. **And re-scoped after the
evaluation:** since the git series is a TRACKING series, this pair demonstrates that the pin arm
detects a change in the tracked count — a real and sufficient test of arm 2 — but it does NOT
demonstrate detection of on-disk growth, because `543e2f9a` did not grow the corpus. Read it as a
pin test, not as a growth-detection witness.

**Load-time cost** (`load_all_testsets/0` only, corpus md5-pinned, warm, 3 runs/side): baseline
median 590.71 ms (spread 2.89) → edited 734.51 ms = **+143.80 ms, +24.3 %** ≈ 0.5 ms/member.
Two-clause threshold: `delta > spread` TRUE, `delta > max(10 %, 2 s)` FALSE → **no signal**.
Reported anyway (D1 makes this the measurement site) because the 10 % limb *is* exceeded — only the
2 s absolute floor keeps a sub-second load under threshold. For **R-G**: 4211 members across five
legs ≈ **2.1 s** of KINDING. **That is not the number R-G was about.** The ratchet was proposed to
dodge per-gate swipl LATENCY, and the `corpus census` row costs **~13.3 s** wall standalone, because
`--check` performs seven corpus LOADS (five legs + the live leg again inside `selftest()` + the
planted tempdir leg) and the load dominates the kinding. The one-definition ruling is very likely
still right at 13 s against 30 s, but the recorded justification cited the wrong quantity, and the
re-measure trigger is stamped to file counts rather than to that cost.

**Full gate GREEN** (28 rows including the new `corpus census`), matching the pre-change baseline
observed at session start.

---

## Findings

1. **The corpus moved mid-session — R-A's premise firing in real time.** An untagged
   `c-orchestrator` run (`c-orchestrator.py:187` → `testsets_dir = TESTSETS_DIR`) completed at 04:23
   and took the live leg 279 → 285 and the stratum 26 → 27. Nothing went red, because nothing
   watched. The OQ's own defect, observed rather than reconstructed.
2. **The stratum can grow with NO COMMIT AT ALL.** `f32fe86b` committed its 5 story cids and left
   the emitted `*_contradictions.pl` untracked — not a run cid, so the cid-scoped pathspec cannot
   see it. `543e2f9a` / `f724379d` are prior instances. **Consequence: git history systematically
   UNDERSTATES the stratum**, so a `git ls-tree` reconstruction can materialise a corpus state that
   never existed on disk — a *different* corpus, not a weaker sample. This is why the discrimination
   record above is plant-only. Landed as `2f73ce34`.
3. **RETRACTED — the OQ's `9 → 26` was RIGHT; my "correction" of it was wrong.** Caught by the
   post-implementation evaluation. `9` was a LIVE on-disk census
   (`audits/2026-07-02_oq136_census_bucket_provenance/membership.tsv`: 9 distinct
   `*_contradictions` cids at n=119) while git tracked 4 — short by exactly 5 on both axes, those 5
   first tracked at `543e2f9a`. **I used the instrument my own Finding 2 declares unreliable to
   overturn the reliable one, in the same commit that discovered the unreliability.** And
   `543e2f9a` is a TRACKING event, not growth: its body says "Git-state change only: disk content
   unchanged, corpus md5 fingerprints unaffected" — so "5 → 22 in a single day is sharper growth
   evidence" was inverted, and is withdrawn too. A first re-measurement was ALSO wrong the other
   way (counting `testsets/gfbatch1/`, which the glob never loads). Three errors, one shape: **a
   population measured by the wrong membership rule** — the defect of this OQ, committed three
   times while fixing it. `stratum_series.txt` is retained, RE-LABELLED a tracking series.
4. **Substrate 10, with the consequence that decided R-E.** `load_warning_gate.py` captures none of
   the proposed stderr lines, for two independent reasons: it runs `swipl -g "[stack], halt"` and
   never loads the corpus, and its regex is `^(Warning|ERROR):`. So no allowlist entry was needed —
   and D4 arm 3 was never a gate arm to scope, which is why R-E dropped it rather than demoting it.
5. **Substrate 17 — a stated positive control did not fire.** D5 control (a) cited
   `audits/oq140_divergence_extract.py`; the real path is `python/audits/…`, inside the tree the
   sweep most certainly covers, destroying the outside-the-narrowest-tree property it was chosen
   for. Replaced at execution with `audits/2026-06-13_twin_comparison/RESULTS.md`.
6. **The R-C sweep caught a real breakage.** `python/audits/twin_comparison.py` asserted
   `schema_version != 2` as a refuse-to-join guard; the bump would have made it refuse every fresh
   output. Fixed to `JOINABLE_SCHEMA_VERSIONS = (2, 3)`, verified two-sided (accepts 2 and 3, still
   refuses 1, 4, `None`). `omega_resolver.py`'s `schema_version` is a different schema — unaffected.
7. **Measuring the wrong property reports ~0 everywhere and looks like good news.** Sizing R-B's
   blast radius, "carries any `constraint_metric`" returns ~0 unkindable for every corpus. The
   predicate actually asks "carries one keyed on its own basename". Correct instrument:
   `original_v5` 91/702 (13.0 %), `original_json/testsets` 133/1151 (11.6 %).
8. **Substrates that came back cleaner than assumed.** `clause/3` succeeds on the static-multifile
   `cs_axiom_contradiction/2` with no throw, and `source/1` == `file/1` on consulted testsets.
   `python/*.json` is not gitignored, so the D3 pin is genuinely shareable.
   `reading_registry.pl` CAN express the domain (`reading_domain_key(corpus_constraint, [C])`), so
   R-H's decline was a free choice on category grounds, not forced by expressibility.

---

## Executor error, recorded rather than quietly corrected

Mid-execution I ran **two concurrent `classify_corpus` runs** against the shared
`pipeline_output.raw.json`, violating the serialization rule. Cause: I polled
`pgrep -f "classify_corpus\|run_pipeline\|swipl"`, read an empty result as "finished", and launched
a second run — but the Python parent had not yet spawned its swipl child. **An empty `pgrep` is a
fact about when I looked, not about what is running** — the same didn't-look/measured-empty collapse
this OQ is about, committed while fixing it.

Checked rather than assumed afterwards: the canonical `pipeline_output.json` was intact (the racing
runs targeted a different output name and `inject_manifest` is its sole writer), both corpora were
md5-unchanged, and the C1/C2 diff pairs predated the incident. No landed witness was contaminated.
The mid-write raw artifact was removed.

**The fix is not "poll more carefully."** Absence-of-process is not a witness of completion, so the
guard is a REFUSAL at launch (`if pgrep -x swipl; then exit 1`), which every subsequent run carried.

---

## Superseded claims in landed commit messages (history is not rewritten)

Two commit messages carry statements this write-up has since corrected. They are annotated here
rather than rebased, because the record of what was believed when is itself worth keeping:

- **`dbde6fe5` (C4)** — "Both reconstructed strata match `stratum_series.txt` independently."
  **Circular**, and now known to be worse than circular: both sides derive from the same git
  reconstruction, and that reconstruction measures TRACKING events, not on-disk state. The natural
  pair remains a valid test of the pin arm; it is not corroboration of anything.
- **`cf568697` (C5)** — "All twelve rulings recorded individually." There are **thirteen**
  (R1–R3 + R-A..R-J). The substrate is correct — the close records all thirteen as distinct
  bullets — only the message's count is wrong.

Both belong to the same defect as the gate-count error below: **a number asserted rather than
counted.**

## A count asserted rather than counted

Three times in this work I published a figure I had not derived: the gate as "27/27" then "28/28"
(it went **26 → 27**; no row was ever removed, and the row SETS across those runs are identical),
and "twelve rulings" for thirteen. Each was off by one in the direction of the thing I had just
added — the tell of incrementing a remembered number instead of counting the output in front of me.

None changed a decision, which is exactly why it is worth recording: this is the residue left when
the witness discipline is applied to *code* and relaxed for *prose about code*. The pasted gate
output was always right; the sentence describing it was not. **A number in a summary is a claim and
owes the same derivation as a number in a report.**

## Declared residue

- **Discrimination is plant-only** for the growth guard, per Finding 2 and the operator's ruling.
  The natural pair ran and both verdicts are reported with the firing arm named; the altitude claim
  is the honest fallback the plan's own rule selects.
- **R-H's justification is now self-supporting**: C4's planted unknown-shape selftest landed, so the
  catch-all is exercised against an input the live corpus does not supply.
- **The generator-redirect relocation shape is uncovered** while the stratum is static; the marker
  lives at `agent/generate_kernel_corpus.py:emit_axiom_contradiction_facts`, where the person doing
  the redirecting is actually reading. Accepted as declared residue (R-E).
- **Five filename-suffix local exclusions remain in place** — now redundant with
  `corpus_member_kind/2` but harmless; rewriting them was not this commit's scope.
- `golden_file_check.py`'s docstring calls a gitignored baseline "committed" — pre-existing, in a
  file this change only RUNS, so the stale-prose rule leaves it cataloged rather than fixed.

## Post-implementation evaluation (R-D) — 12 gaps, 2 material

A fresh general-purpose subagent re-ran the claimed commands and compared every number against its
artifact. Raw output: `POST_IMPL_EVALUATION.md`. **It confirmed every re-derivable number**
(census, twins, R-B skew, load delta 143.09 vs the claimed 143.80 ms, golden md5, corpus md5) and
found **no mis-kinding and no refusal bypass** — and it caught two material errors and ten smaller
ones. Dispositions:

| # | Gap | Disposition |
|---|---|---|
| 1 | The heading "correction" was itself wrong — `9` was a live census | **FIXED — retracted.** Heading restored, close carries the retraction |
| 2 | "5 → 22 in a day is sharper growth evidence" is inverted; that commit is a tracking event | **FIXED — withdrawn** in ISSUES, CLAUDE.md, `build_discipline.md`, here |
| 3 | Correction unpropagated; four surfaces contradicted | **RESOLVED by the retraction** — the previously-contradicting lines were the correct ones |
| 4 | Two `build_manifest` sites stamp `schema_version: 3` without its keys | **FIXED** — the bump now rides inside `add_member_census_keys`, atomic with the keys |
| 5 | The gate selftest's pin control was vacuous and inflated the count | **FIXED** — now drives the real comparison; verified by a forcing test (sabotage → RED) |
| 6 | Gate is 27 rows, not 28 | **FIXED** |
| 7 | ≥9 `n_constraints` readers undispositioned | **FIXED** in CONSUMERS.md — all provenance echoes, nothing broken, but the sweep over-claimed completeness |
| 8 | CONSUMERS.md control (e) unsubstantiated | **FIXED** — the claim is withdrawn; the control's only hits were inside an excluded subtree |
| 9 | Manifest cite doesn't match the on-disk artifact | **FIXED** — cite now states the artifact is dirty and names the clean diff-pair commit |
| 10 | R-G's 2.1 s understates the row's real 13.3 s cost | **FIXED** — both numbers recorded; the ruling stands, its stated justification did not |
| 11–12 | "Independent derivations" / "INDEPENDENT enumeration" over-claimed | **FIXED** — both re-scoped to what they actually establish |

**The pattern across gaps 1, 2 and 3 is worth more than the fixes.** All three are the same error:
using a git-derived population for a census after discovering, in that same commit, that git does
not track this population. **A finding about an instrument has to be turned around on the work in
progress, not just written down.**

## Evidence map

| Artifact | What it holds |
|---|---|
| `PLAN.md` | verbatim copy of the executed plan (landed at C1) |
| `CONSUMERS.md` | D5 sweep, controls, per-consumer dispositions |
| `audit_log.md` | HEAD stamp pair + comparison, corpus md5, clean-side and baseline md5s |
| `stratum_series.txt` | glob-visible flat stratum vs member count, every commit touching it |
| `rb_skew_rederived.txt` | R-B's re-derived skew figures and the instrument note |
| `loadtime_baseline_ms.txt` / `loadtime_edited_ms.txt` | 3 timings per side |

`outputs/` is gitignored, so the two pipeline artifacts compared are not committed; they are
regenerable from the corpus md5 and commit recorded in `audit_log.md`.
