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

(gate output appended below after the run)
