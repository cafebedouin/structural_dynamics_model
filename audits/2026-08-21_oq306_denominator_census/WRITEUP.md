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
`n_nonstory_members 27`, `n_unclassified 0`, `schema_version 3`, `code_commit d7b4d4f8`.
C1 diff pair: clean side `2026-08-21T14:23:47Z` (`code_dirty: false`) vs edited `T14:30:09Z`.

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
loaded`. The two sides are genuinely independent derivations (load-loop success counter vs registry
enumeration), so it is not a total recomputed from its own parts.

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
--check GREEN — 5 legs, totality holds, stratum {testsets:27, twins:0}, selftest 6 controls
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

Both reconstructed strata match `stratum_series.txt` independently. **Claimed at PLANT-ONLY
altitude** (operator ruling) — see Finding 2.

**Load-time cost** (`load_all_testsets/0` only, corpus md5-pinned, warm, 3 runs/side): baseline
median 590.71 ms (spread 2.89) → edited 734.51 ms = **+143.80 ms, +24.3 %** ≈ 0.5 ms/member.
Two-clause threshold: `delta > spread` TRUE, `delta > max(10 %, 2 s)` FALSE → **no signal**.
Reported anyway (D1 makes this the measurement site) because the 10 % limb *is* exceeded — only the
2 s absolute floor keeps a sub-second load under threshold. For **R-G**: 4211 members across five
legs ≈ **2.1 s** of kinding, against the unsourced 30 s figure.

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
3. **The OQ's "9 → 26" framing figure is wrong — and my first correction to it was wrong too.**
   The glob-visible flat series is **5 → 22 → 26 → 27**. A first re-measurement over-counted by
   including `testsets/gfbatch1/` run-tagged files the non-recursive glob never loads. **Both errors
   are the same class as the defect**: a population measured by the wrong membership rule. The
   corrected series is sharper evidence — 5 → 22 inside a single day.
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
