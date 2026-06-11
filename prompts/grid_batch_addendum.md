> **SUPERSEDED FOR LIVE GENERATION (2026-06-11, flip ruling):** the live prompt
> (`constraint_story_generation_prompt_json.md`) now carries the opt-in-by-story-focus grid
> section; this addendum remains as the DEDICATED-BATCH variant text (every-story-must-author)
> and as the audit reference for the N=10 gate batch. The one-time κ gate became the
> FIRST-CONTACT gate (`python/grid_first_contact_gate.py`, wired into run_pipeline).

# GRID BATCH ADDENDUM — leveled coercion grid authoring (OQ-93 Stage C)

This addendum rides the standard JSON generation prompt for the DEDICATED GRID BATCH only
(it is not part of the live prompt). It adds ONE requirement on top of everything above:
every story in this batch MUST author the optional top-level `coercion_grid` block, because
each story in this batch is chosen so that LEVEL-RESOLVED COERCION DYNAMICS — how the
constraint's coercive pressure shifts differently at different social levels over the
interval — is the story's central subject.

## The block

```
"coercion_grid": {
  "t0": <integer — MUST equal interval.start>,
  "tn": <integer — MUST equal interval.end>,
  "points": [ { "metric": "...", "level": "...", "time_point": <t0 or tn>, "value": <0.0-1.0> }, ... ]
}
```

Author the FULL grid: all 4 metrics x all 4 levels x both endpoints = 32 points, each
(metric, level, time_point) slot exactly once. The compiler rejects duplicate slots,
endpoints that differ from the interval, and any time_point that is not t0 or tn.

## Metric semantics (value = intensity at that level, at that time, in [0,1])

- `accessibility_collapse` — how far alternatives and exits have closed off for agents AT
  THAT LEVEL: an individual's outside options, an organization's room to defect, a class's
  collective alternatives, the structural availability of any other arrangement.
- `stakes_inflation` — how far the cost of noncompliance has risen at that level: what an
  individual loses by refusing, what an organization risks by deviating, what is staked for
  the class, how much the system as a whole has riding on conformity.
- `suppression` — the active force applied at that level to keep resistance down:
  individual discipline and retaliation, organizational policy enforcement, class-directed
  policing or delegitimation, structural/legal machinery.
- `resistance` — the resistance actually being mounted at that level at that time.

## Level semantics

`individual` (one agent's situation), `organizational` (firms, agencies, unions, churches —
organized actors), `class` (a class of similarly-positioned people as a class), `structural`
(the system-level arrangement itself).

## Authoring discipline (READ CAREFULLY — these are the failure modes this batch is audited for)

1. **Every value must come from YOUR story.** Derive each number from what the narrative
   says is happening at that level at that time. Do NOT reuse a values pattern you have seen
   elsewhere, do NOT fill the grid with one constant, and do NOT give all four levels the
   same trajectory unless the story genuinely coerces all levels identically (rare — if your
   story does not distinguish levels, it does not belong in this batch).
2. **The level axis is the point.** These stories are selected because coercion moves
   DIFFERENTLY at different levels (e.g., structural pressure rising while individual-level
   pressure is dressed down, or organizational suppression hardening while class resistance
   grows). Make the grid express that differentiation; say in `commentary.logic_rationale`
   which levels diverge and why.
3. **Direction must match the story.** If your scalar `measurements[]` series and narrative
   describe intensifying extraction/enforcement, the grid should not describe system-wide
   relaxation — and vice versa. The grid and the rest of the story are ONE account.
4. **No invented precision.** Values are level-resolved judgments, not measurements; use the
   whole [0,1] range honestly, but do not manufacture differences the story gives you no
   basis for. Where the story genuinely says nothing about a level at a time, make the
   conservative judgment and note the uncertainty in an omega.

There is deliberately NO worked 32-value example here: the grid's values are the deliverable
this batch exists to evaluate, and a worked table would teach you its constants.
