# OQ-356 Step 3b — sweep-candidate adjudication

Executed 2026-08-24. Instrument: `audits/2026-08-23_oq352_report_driver/purity_guard_sweep_v3.py`
(the repaired criterion; v2 is `purity_guard_sweep.py`, kept for the record).
Raw output: `sweep_v3_output.txt`.

**Scope boundary (ruled): adjudication is not authorization to patch.** Every
candidate gets a written verdict; only `giant_component_analysis.pl:1278` is
patched under this OQ. A site adjudicating **real** gets its own OQ with its own
witness. A site adjudicating **latent** mints a follow-up OQ at LOW priority.
False positives mint nothing.

## The three verdicts — and why "latent" is not "safe"

| verdict | meaning | consequence |
|---|---|---|
| **real** | an `unknown` reaches arithmetic on a live path | patched here (`:1278` only) or its own OQ |
| **latent — unreachable today by caller discipline** | the predicate is unguarded; no current caller delivers a non-number | follow-up OQ at LOW priority — a new or changed caller makes it live |
| **false positive** | no arithmetic on the path at all | nothing; sweep-criterion noise |

Recording "safe by reachability" and "false positive" under one word would
reproduce the exact equivalence trap the `NExcluded`-vs-`NUnknown` naming note
warns about. The `giant_component_analysis.pl:598` principle cuts here too:
*a filter that is a property of ONE CALLER is not a property of the predicate.*
Structural tell — `fpn_report.pl:94` is safe today for the **same reason
`testsets` passes today**, and the OQ says plainly that `testsets` *would throw
if it ever got there*.

## What the repaired sweep EMITS (2 rows)

| site | verdict | basis (in-file witness) |
|---|---|---|
| `fixtures/count_by_action_band_prefix.pl:8` (= the frozen pre-fix `giant_component_analysis.pl:1278`) | **REAL** | the witnessed throw: `>=/2: Arithmetic: 'unknown/0' is not a function` on 17 of 20 corpora (`census_prefix_HEAD.txt`), and the plunit RED at `V4b_plunit_RED_prefix.txt`. **PATCHED in Commit 1.** This row is the positive control's fires-half, scanned frozen because the live site is now fixed |
| `giant_component_analysis.pl:596` (`in_float_range/3`) | **REAL** — ruled no-patch-in-Commit-1 on WITNESSABILITY, not on scope | `in_float_range(Lo, Hi, V) :- V >= Lo, V < Hi.` — bare comparison, no `number/1`. Reached from the pre-fix `count_by_action_band` via `count_in_zone/4` (`:593`) through `include/3`. **HELD-OUT TEST, not a row the sweep was told about** — the acceptance test withholds it and requires the criterion to surface it unaided; it did, with the full chain. The `:598` comment states the governing principle six lines below it and was not applied here. Follow-up OQ owes a direct unit call on `in_float_range/3` with `unknown` — the witness Commit 1 cannot provide through the call chain, since once `number(EP)` lands at `:1278` no input reaches `:596` with a non-number |

**V3d tripwire — CHECKED AND CLEAR (the ruling was conditional on it).** The
no-patch ruling on `:596` holds only if it has exactly the caller structure
named. Re-derived rather than inherited: `in_float_range/3` has exactly ONE
caller, `count_in_zone/4` at `:593`, and no use outside
`giant_component_analysis.pl` (every other repo hit is a dated audit artifact).
`count_in_zone/4` has exactly the two caller groups named — `:560-568`, fed from
the OQ-60-guarded `gc_node_purity` cache **and** pre-filtered `IP >= 0.0` /
`EP >= 0.0` in `report_purity_landscape/2`, and `:1282-1285`. No third caller.

## What the repaired sweep DECLINES that v2 emitted (the 9 candidates)

Each verified first-hand this session; none inherited from the plan's read.

| site | verdict | basis (in-file witness) |
|---|---|---|
| `fpn_report.pl:94` (`one_hop_ep_safe/3`) | **LATENT — unreachable today by caller discipline** (v2 called it unguarded; OQ-356 called it "likely real") | The predicate IS unguarded: `catch(effective_purity(C,Ctx,EP,_),_,fail) -> true ; EP = -1.0`, no `number/1`, and `EP` reaches `ep_band/2` (`:61`) and `Diff is OH - FP` (`:66`). It is unreachable only because the row generator gates on `fpn_intrinsic_safe(C,IP), IP >= 0.0` (`:49-50`) and `drl_fpn.pl:112` collapses a non-numeric intrinsic to `-1.0` first; with S7 (a non-numeric EP requires a non-numeric intrinsic) that makes a non-number impossible on the row path. **Witnessed, not argued** — `3b-i_fpn_completion_witness.txt`: `run_fpn_report` exits rc=0 on 3 giant_comp-THROWING legs, and on `original_v6` the artifact reads `Constraints in corpus 3380 / Constraints compared 3328`, i.e. 3328 rows reached that arithmetic without throwing while `3380 - 3328 = 52` is exactly the purity-invalid count. Not a k=0 branch. **This is CALLER discipline, not a property of `one_hop_ep_safe/3`** |
| `json_report.pl:1427` | **false positive** | `NP` reaches only `write_json_number(S, NP)` (`:1456`), which has an explicit `write_json_number(S, unknown) :- !, format(S,'null')` clause at `:2637`. The CALLEE handles the token — structurally different from caller discipline, which is why it is not filed as latent |
| `json_report.pl:2185` | **false positive** | `purity_absence_classify/2` guards with `number(PS)` at `:2194` before any comparison — outside v2's 5-line window, which is why v2 missed it |
| `drl_purity_network.pl:224` | **false positive** | guarded at `:230` by `( \+ number(Intrinsic) ; Intrinsic < 0.0 )` before any arithmetic |
| `genuine_findings_query.pl:101` | **false positive** | `Purity` reaches only `format('PURITY_SCORE: ~w~n', [Purity])`. No arithmetic on the path |
| `context_profile_mining.pl:193` | **false positive** | `Purity` reaches only the `purity(Purity)` term in the emitted profile. No arithmetic |
| `abductive_triggers.pl:467` | **false positive** | `Purity` reaches only `evidence_line(signature, purity_score, Purity)` |
| `abductive_triggers.pl:825` | **false positive** | same shape as `:467` |
| `giant_component_analysis.pl:1278` (live) | **REAL — fixed in Commit 1** | now `number(EP)`-guarded inside `partition_scorable_purity/4`; the sweep correctly declines the live site and fires on the frozen pre-fix text |

## Two false-positive CLASSES found in the repaired instrument, and fixed

Both surfaced on the first repaired run, were explainable, and were repaired
rather than documented (fixing beats documenting). Recorded because a criterion
that fires where it should not trains bypass, which is exactly why this change
ships with **no gate row**.

1. **Clause-order guards were not modelled.** `ep_base_severity/2`
   (`network_dynamics.pl:284`) opens with `\+ number(EP), !, T = undetermined`,
   so the comparisons at `:285`/`:287` are unreachable with a non-number. A
   criterion that only recognises a positive `number(V)` in the SAME clause
   reported both. Modelling reachability means modelling clause order, not just
   the conjunction. Fixed: `clause_guarded/4`.
2. **Taint did not flow forward.** `drl_fpn.pl:311`
   (`TotalContam is max(0.0, IP - EP)`) sits in the THEN-branch, where `EP` comes
   from the `fpn_ep/3` cache; `effective_purity/4` binds the same variable name
   three lines later in the ELSE-branch. Flow-insensitivity reported it. Fixed:
   taint carries the line it enters at, and arithmetic before that line is not
   reachable from it.

Two further defects were caught by the instrument's own control during
construction and are recorded in the script: module-qualified goals were being
skipped by a lookbehind (so every producer in the codebase was invisible), and
`>` was matching the `>` of `->` — **v2's own bug, reappearing in a new
surface**, which is why v3 masks arrows in one place rather than guarding each
operator.

## DECLARED BOUNDS of the repaired instrument — read before citing its zeros

The sweep now emits zero unexplained fires. That is **not** the same as "no
missed sites", and the difference must not be absorbed:

- **Taint is not propagated OUT of a callee's output argument.** `fpn_report.pl:94`
  is declined for this reason, not because the instrument modelled the
  `IP >= 0.0` gate. Its verdict rests on the manual trace plus the 3b-i
  artifact witness, NOT on the sweep.
- **Recursive list construction is not tracked.** The post-fix
  `partition_scorable_purity/4` builds its list by recursion rather than
  `findall/3`; the instrument would not follow taint through it either way. The
  live decline is correct for an independent reason (the value is guarded at
  the cons site), but the sweep is not what establishes it.
- Interprocedural depth is capped at 4 and meta-call handling covers
  `include`/`exclude`/`partition`/`maplist`/`member` plus the list-arithmetic
  builtins. Anything reached through a different meta-call shape is invisible.

**Consequence for the gate-promotion trigger (Step 5): the trigger's condition
is about the rows the sweep EMITS, and every emitted row now carries a verdict
and a named in-file witness — zero INFERRED, zero verdict-less. Whether that
earns a gate row is the operator's seat, and it is filed as an OQ, not promoted
here.**
