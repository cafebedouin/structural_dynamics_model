# Pre-registration — OQ-92 step-2 gain-flow prototype (eight-control battery, both fields)

**Written and committed before the run.** Predictions pinned here; the run is authoritative and
may surprise this pre-registration. Rulings under test were recorded first (ISSUES.md OQ-92
Rulings block, operator 2026-06-10; commit `4e04c2dc`): tri-valued provenance design
(authored-gain-to-named-seat / explicit-`diffuse` / absent-fails-closed), one surface two fields
(gain_flow + fixing_cost), malformed-gain absorbs to fail-closed (decided default).

## What is being tested

Whether an **authored** gain-flow/receipt surface plus a fixing-cost class, read positively,
separates the cells the broken computed proxy could not
(`audits/2026-06-09_capture_axis_cut_control/` Outcome 2 HALT):

```prolog
seat_captures(C, Name)  :- stakeholder_gain_flow(C, Name), Name \== diffuse,
                           stakeholder_seats:role_of(C, Name, _).   % named seat must EXIST (checkable witness)
uncaptured(C)           :- stakeholder_gain_flow(C, diffuse).        % positive authored negative, never NAF
piton_candidate(C)      :- uncaptured(C), fixing_cost_class(C, prohibitive).
transient_neglect(C)    :- uncaptured(C), fixing_cost_class(C, cheap).
% captured + ANY fixing_cost_class stays snare-flavored; absent gain_flow -> NOTHING fires (fail-closed)
```

Prototype-only dynamic predicates in the harness file (`stakeholder_gain_flow/2`,
`fixing_cost_class/2`); **no production file is touched**. `fixing_cost_class` is a class atom
(`cheap | prohibitive`), not a calibrated scalar — the battery tests cut-representability, not
calibration.

## Scope: what a pass can and cannot witness

With asserted facts feeding two-clause predicates, cases 1–6 are **near-tautological as logic
tests**. The battery's real risk surface is: the `role_of/3` join (cases 1, 6, 8), the
cache/reporting machinery, engine interference (per-seat `dr_type_for_stakeholder` reported for
every seat as an interference check), and whether the eight stories' facts are **coherently
authorable at all**. Under-claim rule: a clean run earns "the cuts separate on these constructed
cases," never "capture/fixing_cost is representable across the range." An 8/8 pass must not
inflate in FINDINGS prose beyond what it witnesses.

Signature-layer hold-out as in the 06-09 control: every case authors `extractiveness 0.75`,
`suppression_requirement 0.65`, `theater_ratio 0.20` and **omits** `accessibility_collapse` /
`resistance` → signature `unknown` → no FCR/FSM override confounds the reads.

## The eight cases

Cases 1–4 re-author the 06-09 seat-sets (`capturer_cut_control.pl:42-59`), now WITH the authored
surface. Seat tuple defaults: `biographical / mobile / national`.

| # | case | seats | authored surface | construction intent |
|---|------|-------|------------------|---------------------|
| 1 | `cap_a` | payer_a (payer, powerless) + capturer_a (beneficiary, institutional) + `constraint_beneficiary(cap_a, capturer_a)` | gain_flow → **capturer_a**; prohibitive | genuine capturer (06-09 case (a)) |
| 2 | `mild_b` | payer_b (payer, powerless) + bystander_b (beneficiary, institutional) | gain_flow → **diffuse**; prohibitive | the 06-09 false-positive case: mild-favorable non-capturer, now separable |
| 3 | `dmv_c` | payer_c (payer, powerless) + excluded_c (excluded, powerless) | gain_flow → **diffuse**; prohibitive | DMV easy case, no beneficiary-side seat |
| 4 | `dmv_designed` | payer_d (payer, powerless) + admin_d (agenda_setter, institutional) | gain_flow → **diffuse**; prohibitive | designed-but-uncaptured with institutional agenda_setter |
| 5 | `cheap_fix_e` | payer_e (payer, powerless) + admin_e (agenda_setter, institutional) | gain_flow → **diffuse**; **cheap** | transient neglect — structurally identical to 4 except the fixing_cost class; **the load-bearing test** (OQ-90's decisive control, folded in) |
| 6 | `captured_cheap_f` | payer_f (payer, powerless) + capturer_f (beneficiary, institutional) + `constraint_beneficiary(captured_cheap_f, capturer_f)` | gain_flow → **capturer_f**; **cheap** | the fourth cell cited by ruling (b): captured + cheap fix — fixing_cost must NOT demote |
| 7 | `absent_g` | payer_g (payer, powerless) + bystander_g (beneficiary, institutional) | **NONE** (no gain_flow, no fixing_cost fact) | structural twin of case 2 with the surface absent — fail-closed register |
| 8 | `malformed_h` | payer_h (payer, powerless) + capturer_h (beneficiary, institutional) | gain_flow → **ghost_seat_h** (names NO existing seat); prohibitive | malformed-gain absorption — the decided fourth-condition default |

Case 7 rationale: the tri-valued design claims three provenance registers; a control must
exercise every register the claim covers, else absent-fails-closed ships unwitnessed
(probe-positive-controls rule). Case 8 rationale: the malformed-gain absorption was DECIDED
(OQ-92 Rulings block), so its behavior is witnessed, not assumed.

**Pairing (the silences are witnesses only via their firing twins):**
- **2 ↔ 7**: same seat structure; authored-`diffuse` fires `uncaptured`/`piton_candidate` on 2,
  absent fires nothing on 7. Case 2 is the positive control proving the probe fires on the twin —
  that is what makes case 7's nothing-fires a witness rather than a dead grep. **Report as a
  pair, not independent rows.**
- **1 ↔ 8**: same seat structure; gain_flow→existing-seat completes the `role_of` join on 1,
  gain_flow→nonexistent-seat fails it on 8. Case 1 proves the join fires; case 8's silence is
  then the absorption witness. **Report as a pair.**

## Pre-registered verdicts (predictions — run is authoritative)

| # | case | seat_captures | uncaptured | piton_candidate | transient_neglect |
|---|------|--------------|-----------|-----------------|-------------------|
| 1 | cap_a | **[capturer_a]** | false | false | false |
| 2 | mild_b | [] | **true** | **true** | false |
| 3 | dmv_c | [] | **true** | **true** | false |
| 4 | dmv_designed | [] | **true** | **true** | false |
| 5 | cheap_fix_e | [] | **true** | **false** | **true** |
| 6 | captured_cheap_f | **[capturer_f]** | false | false | false |
| 7 | absent_g | [] | false | false | false (NOTHING fires) |
| 8 | malformed_h | [] | false | false | false (NOTHING fires) |

## Pre-registered outcome mapping (fixed — NOT revisable)

1. **All eight as predicted → PASS.** The cuts separate on these constructed cases (under-claim
   stands). Step 3 unblocks: schema field + compiler emission + prompt change (OQ-83 Phase-A
   playbook), with the generated-diffuse audit gate and malformed-gain schema rejection as
   preconditions before classification wiring.
2. **Capture-side miss** (case 1 or 6 captures wrong, or any of 2–4 nonempty) → **HALT**, capture
   read broken; record in OQ-92.
3. **Case 5 returns piton_candidate true (or transient_neglect false)** → the fixing_cost cut is
   not discharging — **HALT that half only**; capture half judged on outcomes 1–2.
4. **Case 7 fires anything** → fail-closed broken → **HALT**. Valid only if case 2 fires
   (else INVALID test — silence for the wrong reason, not a witness).
5. **Case 8 fires anything** → the decided malformed absorption does not hold → **HALT** and the
   default needs re-ruling. Valid only if case 1's join succeeds (else INVALID test).

## Run mechanics

From `prolog/` (cwd convention):
`swipl -g run_gain_flow_prototype -t halt ../audits/2026-06-10_gain_flow_prototype/gain_flow_prototype.pl`
→ stdout saved to `gain_flow_prototype.out`. Harness clones the 06-09 pattern
(`assert_common_metrics`/`seat` helpers; `cache_registry:clear_all_caches` before reporting;
`:- dynamic` declared for both prototype predicates — the 06-10 desirepath control documents the
missing-dynamic failure mode).
