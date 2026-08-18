# Audit log — bound-dispatch fail-loud hardening (recon + pilot)

Plan: `~/.claude/plans/if-it-is-recon-mossy-beacon.md` (operator-reviewed, executed with two
amendments: baseline-expectation statement in the Phase 0 gate paste; census-HEAD dating line
in RECON.md). No OQ minted at open (operator ruled: run the recon live as one planned unit);
follow-on OQs minted at close only if work survives the writeup.

## OPEN stamp

```
git rev-parse HEAD
6f42b67aee3ead9e4170e6516f4b526c221c79ba
```

OPEN HEAD is `6f42b67a` — the giant_comp disposition commit itself (dir marked PARTIAL,
OQ-301 minted ACTIVE, both formerly-red gate rows dispositioned). Nothing has landed on
`main` between that disposition and this audit's open.

## Prior-art grep (same pass as the finding, per audits/README.md)

Grepped `docs/technical/build_discipline.md` for `classify_from_metrics`, `is_scaffold`,
`bound` (pinned `/usr/bin/grep`):

- **Mechanism: PRIOR ART EXISTS — Pattern 7, bound-probe-bypasses-clause-order**
  (`build_discipline.md:871`, relocated there by the OQ-278 execution from the vacated
  index 3 at `:601`; located by NAME per plan). This audit's finding is a fresh live
  *instance* of that documented pattern, filed as evidence at Pattern 7 — a re-discovery
  of the mechanism, not of the instance.
- **Instance: prior art NONE** — `classify_from_metrics`: 0 hits; `is_scaffold`: 0 hits.
  The `classify_from_metrics/6` exposure (10 atom-headed clauses, unconditional terminal
  `unknown`, six live bound-arg callers, absent from the `bound_selector_check.py`
  registry) is not previously documented.

## Phase 0 baseline gate run (at OPEN stamp, before any edit)

**Baseline expected GREEN post-`6f42b67a`; any red here is investigated before Phase 1,
not carried.** The disposition commit witnessed full-gate GREEN; a red at this baseline is
therefore a finding about the disposition commit (or the dir's state having changed since),
never a pre-existing red this audit may absorb. This statement is written BEFORE the run so
the baseline cannot become a licence for whatever it happens to show.

```
# Gate checks
  ✓ issues_status    301 parsed, 0 malformed
  ✓ omega check      0 problems
  ✓ omega selftest   selftest: all positive controls fired (10/10)
  ✓ omega index      index --check: fresh (301 rows, 119 active / 182 archive)
  ✓ spec enums       spec_enum_check: GREEN — 8 enums in sync (selftest 3/3 red-capable)
  ✓ doc patterns     doc_pattern_check: GREEN — 8 indices, 0 declared collisions ...
  ✓ bound selector   bound_selector_check: GREEN — 4448 files, 1 cut-ordered predicate(s) registered, 0 exemption(s), 7 declared path exclusion(s) (...), selftest 11/11
  ✓ displaced cites  pattern_citation_check: GREEN — ... 'bound-probe' (renumbered 3 -> 7): 17 declared citation(s) across 7 files; 'destructive-replace' (vacated): SWEPT CLEAN, 0 remaining; selftest 5/5
  ✓ claim cites      claim_cite_check: 62 live citation(s), 18 recorded ...
  ✓ claim cites st     PASS
  ✓ known_state      294 entries parsed, 0 problems
  ✓ axis boundary    [AXIS-SELFTEST] ALL PASS
  ✓ audit cites      ERRORS: 0
  ✗ audit writeup    audit writeup gate: RED (185 dirs, 17 enforced, 1 problems)
  ✗ apparatus        apparatus: catch-rate 9L/1l/0n of 10 bits ... channel 33/33; RED
  ✓ gap surfaces     gap surfaces check: 3/3
  ✓ cli selftest     cli selftest: OK (255 commands across 17 groups)
  ✓ tripwire hook    SELFTEST GREEN
  ✓ oq277 freeze     prereg freeze: GREEN — stamp verified; selftest 7/7

GATE: RED
```

### Baseline red investigated (per the pre-stated rule, before Phase 1)

Two red rows; both checkers name exactly one problem, and it is THIS audit's own directory:

```
audit_writeup_gate: PROBLEM: 2026-08-17_bound_dispatch_hardening: no WRITEUP.md entry point (required for dirs dated >= 2026-08-06)
apparatus_instrument: PROBLEM: 2026-08-17_bound_dispatch_hardening: WRITEUP.md missing its **Fired:** line (required for dirs dated > 2026-08-10)
```

**Verdict: the red is SELF-INDUCED by Phase 0's own scaffolding** (audit_log.md committed
into a dated dir before its WRITEUP.md exists — the writeup gate enforces one on every
post-2026-08-06 dir, and the Fired: check rides it). It is NOT a finding about the
`6f42b67a` disposition commit: with this dir excluded, every row is green, matching the
GREEN witnessed at that commit. The apparatus row's `NO DECLINE EVER RECORDED (OQ-276)`
readout note is reporting-only and was present at the GREEN witness too — not a red cause.

**Resolution:** an IN-PROGRESS stub `WRITEUP.md` (giant_comp PARTIAL-stub precedent,
`6f42b67a`) with the required header, a provisional `**Fired:** no` line marked for
re-issue at Phase 5, and an evidence map. Effective baseline for Phase 4 comparison:
**all rows green including this dir's stub** — re-run pasted below.

```
GATE: GREEN   (full re-run, all 19 rows ✓, 2026-08-17 post-stub)
```

## Phase 1 complete (committed a76c21dd — RECON.md has the full read-only findings)

## Phase 2 — PREREGISTRATION.md frozen

md5 at freeze (logged above the first Phase-3/4 result line): `1a9f61469525ac481acc6fae47a85aea`

## Phase 3 — pre-fix RED witness (the naturally-arising positive half)

`tests/test_dispatch_bound_call.pl` at pre-fix HEAD (a558a53b + test file):

```
% [14/17] dispatch_bound_ca.._rope_over_scaffold .. **FAILED
%   test bound_call_cannot_manufacture_rope_over_scaffold: failed
% [15/17] dispatch_bound_ca..iguous_over_unknown .. **FAILED
%   test bound_call_cannot_manufacture_ambiguous_over_unknown: failed
% (5 anchor/control tests passed: engine_assigns_scaffold_at_overlap_metrics,
%  rope_body_holds_in_isolation_control, engine_assigns_unknown_on_bare_story,
%  bound_call_with_engine_type_succeeds, bound_unknown_matches_engine_on_bare_story)
ERROR: 2 tests failed
```

Interactive pre-fix witness (same fixture, this session):
engine_first=scaffold / bound_rope=SUCCEEDS_over_accept;
sig_first=unknown / bound_ambiguous=SUCCEEDS_276_artifact.

## Phase 3/4 — conversion landed; witnesses (all same-session)

Transformation applied per frozen prereg (md5 1a9f61469525ac481acc6fae47a85aea):
classify_from_metrics/6 (9 heads + terminal), constraint_signature/2 (6 locks),
classify_by_signature/3 (5 locks + terminal, ambiguous catch-all KEPT). Bodies and
clause order untouched. Same-change consumer update: check_logic_symbolic_drift.py
end-anchor re-pinned to the new terminal clause text (verified GREEN post-edit).

- **dispatch_bound_call GREEN post-fix** (17/17 pass incl. the two tests RED pre-fix)
  — the RED/GREEN before-commit pair is complete.
- **Checker 4th discrimination run:** `dispatch_head_check --check` GREEN — 70 hits
  (was 73), all three converted predicates now silent under MUST-NOT-FIRE entries.
- **Six-leg clean-vs-edited pairs: per_constraint IDENTICAL on all six**
  (testsets 279, haiku 960, flash 960, kimi 1005, sonnet 1001, kernel_v1 1106;
  manifest normalized for pipeline_run_at/code_commit/code_dirty; leg-dir md5
  fingerprints stable around BOTH halves of every pair; exit 0 + fresh outputs
  enforced by classify_corpus's raw-freshness refusal; pair_diff_output.txt).
  Per prereg: **output-preserving on the witness set, semantics-changing by
  construction** — zero diff means no witnessed disagreement REACHED per_constraint;
  the 311 live manufactured cells (RECON §5) live on the is_X surface, which
  per_constraint does not consume (dr_type routes through unbound calls).
- **Paired timings (clean → edited s):** testsets 13.33→13.29; haiku 61.76→63.57;
  flash 70.17→71.85; kimi 64.33→65.82; sonnet 83.36→84.76; kernel_v1 61.49→61.49.
  Worst pair +2.9% — indexing loss not material at corpus scale.
- **End-to-end run_pipeline pre/post:** exit 0 both; 42.93s → 39.61s wall;
  per_constraint 0/279 rows differ; manifest (normalized) same; output mtime advanced.
- **golden_file_check: NOT RUNNABLE** — no baseline artifact exists in this working
  tree (outputs/ gitignored; never generated here). Substitute witness: the pre/post
  full-pipeline per_constraint diff above carries the dr_type vector (perspectives
  map) for every corpus story — 0 rows differ. Recorded, not silently skipped.
- **check_stack pre/post byte-compare** (pre-fix via scoped stash): identical except
  two "Redefined global predicate" lines change POSITION (load-order noise, same
  lines). 0 new findings.
- **load_warning_gate:** 3 warnings, 3 allowlisted, 0 unexpected.
- **Full gate GREEN** (one intermediate red: the legacy bound_selector_check regex
  fired on the new unit's deliberate bound calls + one block-comment prose line —
  resolved by rephrasing the prose and TWO reasoned EXEMPT entries; the CUT_ORDERED
  registry itself is UNCHANGED per prereg scope guard).
- **Kill condition: NOT triggered** — no unbound-path result changed anywhere
  (six-leg + end-to-end zero-diff, check_stack at baseline).

## Phase 3 step 3 + 3b

- Gate row `dispatch head` wired (scripts/gate.sh, after `bound selector`); full gate
  GREEN with the new row.
- **3b: the plan's premise was WRONG in an instructive way, and the probe caught it.**
  cluster_by_signature's findall runs with C UNBOUND, so post-conversion clause-1's
  cut prunes the whole generator — probe witnessed 0 of 26 engine-assigned `unknown`
  members returned (NOT "correct by construction" as the plan expected). Disposition
  executed as REPAIR (dormant predicate, no callers, zero risk): enumerate
  corpus_constraint/1 + once + post-filter, site comment records both failure regimes.
  Probe re-run: MATCH on unknown (26/26), false_ci_rope (85/85), natural_law (0/0).
