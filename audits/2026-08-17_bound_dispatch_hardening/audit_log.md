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
