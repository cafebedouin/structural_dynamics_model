# OQ-306 — story membership becomes a checked fact (C1 landed; C2–C5 BLOCKED)

**Executed:** 2026-08-21
**OQ:** OQ-306 (denominator silently admits non-stories)
**Fired:** live

**Verdict, at its scoped altitude:** the membership predicates exist, are total and disjoint on
the live leg, and are behaviour-preserving — witnessed by a byte-identical `per_constraint` over
an md5-pinned corpus. This is a verdict about **C1 only**. Nothing consumes the predicates yet:
the manifest keys, the `member_kind` emission, the growth guard and the close are C2/C4/C5 and
are **blocked on nine pending operator rulings** (R-A..R-G, R-I, R-J). No rate anywhere in the
repo is corrected by this commit.

**Manifest cite:** `outputs/pipeline_output.json` — `pipeline_run_at 2026-08-21T14:30:09Z`,
`n_constraints 285`, `code_commit d7b4d4f8`, `schema_version 2`. Clean-side comparator:
`2026-08-21T14:23:47Z`, same commit, `code_dirty: false`.

## What C1 landed

`corpus_loader.pl` gains `corpus_story/1` and `corpus_member_kind/2` (both exported), the private
`has_story_facts/1` / `contributes_axiom_contradiction/1` / `report_member_census/1`, and a census
stderr line printed *before* the existing final `[corpus] Loaded N` line. `drl_core.pl`'s stale
"two non-story files" comment is fixed. `module_boundary_allowlist.txt` reason texts are corrected
for the read sites this change creates.

Kinding is total over `corpus_constraint/1` into `{story, axiom_contradiction, dual_family,
unknown}`. The disjointness test runs FIRST, so a member satisfying both fact families surfaces as
`dual_family` rather than being absorbed into `story`.

## Witnesses

```
[corpus] census: 258 stories, 27 non-story, 0 other.
[corpus] Loaded 285 testsets successfully.

kinds [axiom_contradiction-27, story-258]  total=285  corpus_constraint=285  corpus_story=258
```

- **Two-sided, naturally-arising** (not a plant): a real contradictions file kinds
  `axiom_contradiction`; a real story file kinds `story`; a non-member fails. The DECLINE is the
  informative half.
- **Bound-call safety, measured not asserted:** `corpus_member_kind(_, story)` = 258 and
  `(_, axiom_contradiction)` = 27 — equal to the unbound enumeration, so the fresh-variable-head /
  unify-after-cut idiom holds against Pattern 7. Gate rows `dispatch head` (49 declared hits,
  unchanged) and `bound selector` stayed GREEN.
- **N16 discrepancy branch is REACHABLE, and two-sided:** declines on the intact registry; fires
  on a retracted `corpus_constraint/1` fact —
  `[corpus] CENSUS DISCREPANCY: 284 members kinded but 285 files loaded`. The two sides come from
  genuinely independent derivations (load-loop success counter vs registry enumeration), so this
  is not a total recomputed from its own parts.
- **Fail-closed kinds fire — PLANTED fixtures, bottom rung of the discrimination ladder, reported
  at that altitude:** a member with neither family → `unknown`; a member with both → `dual_family`.
  The live corpus supplies no natural instance of either, so this licenses only "authored drift
  gets kinded correctly."
- **Behaviour preservation:** clean vs edited pipeline runs, exit 0 both, output mtime advanced
  (09:25:00 → 09:31:22), corpus md5 `61697262…` identical across both halves,
  `per_constraint` md5 `000358d6…` **identical**, no top-level keys added or removed. Manifest
  differs only at `pipeline_run_at` and `code_dirty` (C1 uncommitted at diff time).
- **Golden check:** PASS 285/285, baseline md5 `238b6603…` matching the value anchored before any
  edit. See *Baseline provenance* below — a green here without that provenance would be
  byte-identical to a run that never compared.
- **OQ-137 reading-totality gate** ran inside `_phase_prolog` (fail-fast) and the run exited 0.
- **Full gate GREEN, 27/27**, matching the pre-change baseline observed at session start.
  `module bounds` moved 737 → 738 bypass sites (the new read site), all declared.

**Load-time cost** (`load_all_testsets/0` only, corpus md5-pinned, warm, 3 runs/side):
baseline median 590.71 ms (spread 2.89) → edited median 734.51 ms = **+143.80 ms, +24.3 %**.
Against C1's two-clause threshold: `delta > spread` TRUE, `delta > max(10 %, 2 s)` FALSE → **no
signal**, proceed. Reported anyway because D1 makes this the measurement site and the 10 % limb
*is* exceeded — only the 2 s absolute floor keeps a sub-second load under threshold. Extrapolated
for **R-G**: ~0.5 s per ~1000-member twin leg, so five legs ≈ 2.5 s of swipl kinding per gate run,
well under R-G's (unsourced) 30 s figure.

## Baseline provenance

`outputs/golden_classifications.json` was **ABSENT** at anchor time — surfaced before any edit, per
the plan's M7/P8, rather than discovered at C3. Operator ruled: fresh clean-HEAD run, then bless.
`golden_file_check.py --generate` was confirmed to read the existing `pipeline_output.json` rather
than trigger its own run, so the blessed classifications come from exactly the clean-HEAD artifact:
HEAD `d7b4d4f8`, `code_dirty: false`, pipeline md5 `2b287aab…`, baseline md5 `238b6603…`, 285 ids.
The anchor is expected to rot if a ruling wait intervenes before C3 (`outputs/` is gitignored and
regenerates under any pipeline or topic run) — re-anchor at C3 and say so.

## Findings

1. **The corpus moved mid-session — R-A's premise firing in real time.** A `c-orchestrator` topic
   run (untagged, so `testsets_dir = TESTSETS_DIR` via `c-orchestrator.py:187`) completed at 04:23
   and took the live leg 279 → 285 and the stratum 26 → 27. Nothing went red, because nothing
   watches. This is the OQ's own defect observed live, not reconstructed.
2. **The stratum can grow with NO COMMIT AT ALL.** The orchestrator's scoped auto-commit
   (`f32fe86b`) took its 5 story cids but left `blindness_decomposition_kernel_contradictions.pl`
   untracked — a `*_contradictions.pl` file is not a run cid, so the cid-scoped pathspec cannot see
   it. `543e2f9a` and `f724379d` ("track the N remaining `*_contradictions.pl` testsets (already
   glob-loaded)") show this is recurring. **Consequence for D3:** git history systematically
   UNDERSTATES the stratum, so a `git ls-tree` reconstruction can materialise a corpus state that
   never existed on disk. Landed as `2f73ce34`.
3. **The OQ's "9 → 26" framing figure is wrong.** The glob-visible flat series is
   **5 → 22 → 26 → 27** (`stratum_series.txt`). An earlier count of mine was inflated by
   `testsets/gfbatch1/` run-tagged files the non-recursive glob excludes. The correction
   strengthens the OQ: a 5 → 22 jump inside one day (2026-08-07 → 08-08) is a sharper instance of
   "the growth, not the presence" than 9 → 26. **`543e2f9a` / `f724379d` is the natural
   discrimination pair D3 wants.**
4. **Substrate 10 resolved, with a consequence for R-E.** `load_warning_gate.py` captures none of
   the three proposed stderr lines, for two independent reasons: `collect_warnings()` runs
   `swipl -g "[stack], halt"` and never loads the corpus, and its regex is `^(Warning|ERROR):`,
   which `[corpus] WARNING:` also fails. So no allowlist entry is needed and
   `load_warning_allowlist.txt` drops off C1's file list — but the same fact makes **D4 arm 3 an
   unenforced stderr line rather than a gate arm**, which is the Pattern-6 shape the plan forbids
   demoting to. R-E should be ruled with this in hand.
5. **Substrate 17 — a stated positive control does not fire.** D5 control (a) cites
   `audits/oq140_divergence_extract.py`; the real path is `python/audits/oq140_divergence_extract.py`.
   The control was chosen specifically to sit *outside the narrowest tree*; at its real path it
   sits inside `python/` and tests firing rather than coverage. A valid replacement exists
   (`audits/2026-06-13_twin_comparison/RESULTS.md` carries `n_constraints`). D5 is blocked, so this
   is recorded, not fixed.
6. **Substrate 15 is moot but worth recording against R-H:** `reading_registry.pl` CAN express the
   domain — `reading_domain_key(corpus_constraint, [C])` exists. The R-H decline was therefore a
   free choice on category grounds, not forced by expressibility.
7. **Substrate 20 came back cleaner than assumed.** `clause/3` succeeds on
   `cs_axiom_contradiction/2` despite the predicate being static-multifile (no throw), and
   `source/1` and `file/1` return identical paths — 92 clauses over 27 distinct basenames.

## Declared residue

- **R-H's justification is not yet self-supporting.** The decline rests on D3's planted-unknown
  selftest, which lands in C4 (blocked on R-A + R-G). Until C4 lands, the only thing exercising the
  catch-all against a real input is the ad-hoc plant recorded above, not a standing control.
- Nine rulings remain open: R-A, R-B, R-C, R-D, R-E, R-F, R-G, R-I, R-J. R-E cannot be ruled before
  R-A. No recommendation in the plan was implemented as if ruled.
- The `n_constraints` naming debt (it counts MEMBERS) is **not** yet recorded at the emitter — that
  is R1c, a C2/C5 deliverable.

## Evidence map

| Artifact | What it holds |
|---|---|
| `PLAN.md` | verbatim copy of the executed plan (landed at C1 per fresh-pass Finding 10) |
| `audit_log.md` | HEAD stamp pair, corpus md5, clean-side and baseline md5s |
| `stratum_series.txt` | glob-visible flat stratum vs member count, every commit touching it |
| `loadtime_baseline_ms.txt` | 3 baseline-side `load_all_testsets/0` timings |
| `loadtime_edited_ms.txt` | 3 edited-side timings |

Witness runs are pasted in the session transcript and reproduced above; the two pipeline artifacts
they compare are regenerable from the pinned corpus md5 and commit recorded in `audit_log.md`
(`outputs/` is gitignored, so the artifacts themselves are not committed).
