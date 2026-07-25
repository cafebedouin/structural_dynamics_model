# OQ-62 — call-site census for the purity-bander negative guard

**Date:** 2026-07-25 · **Code state:** `a2ef8147` (Phase 1a landed; guard NOT yet applied)
**Purpose:** the Phase 1b guard converts a *throw* into a *silent token* at two banders.
"No call site depends on the throw" is a code read, not a witness. This census enumerates
every call site and records what each does with a thrown vs. returned `unknown`, **before**
the edit lands.

Probes preserved alongside this file: `reach_probe.pl`, `blast_probe.pl`, `oneleg.pl`.

---

## 1. The four banders and their complete call sites

| Bander | Call sites | Pre-filtered? |
|---|---|---|
| `logical_fingerprint:purity_zone/2` (`:614`) | `logical_fingerprint.pl:603`; `json_report.pl:327`, `:2187`; `diagnostic_summary.pl:231` | **Yes** — every site gates on `number(P), P >= 0.0` or the OQ-60 `\=` guard |
| `fpn_report:purity_zone/2` (`:109`) | `fpn_report.pl:61` (×2), `:163-164` | **No explicit filter** on the banded value — see §3 |
| `giant_component_analysis:purity_zone/2` (`:598`) | `giant_component_analysis.pl:584-585` **only** | **Yes** — `IP >= 0.0, EP >= 0.0` immediately precede both calls |
| `abductive_helpers:fpn_zone/2` (`:136`) | `abductive_helpers.pl:146` (inside `one_hop_zone/3`); `abductive_triggers.pl:525` | **No** — see §2 |

`giant_component_analysis:count_by_purity_zone/8` (`:1259`) does **not** call the bander — it
counts via `count_in_zone/4` → `in_float_range/3` against the config floors directly. So GCA's
bander genuinely has one call site, not two.

## 2. The throw is already swallowed — the "loud failure" was never loud

`abductive_triggers.pl:525` calls `fpn_zone/2` outside any `catch/3` (the `catch` at `:524`
wraps only `fpn_ep/3`). On an `unknown` input it raises
`type_error(evaluable, unknown/0)`. But its sole caller is:

```prolog
% abductive_engine.pl:142-154
run_trigger_over_constraints(TriggerPred, Constraints, Context) :-
    forall(member(C, Constraints),
        (   catch(( call(TriggerPred, C, Context, H),
                    assertz(abd_hypothesis(C, Context, H)) ),
                _Error,
                true  % Silently skip constraints where trigger fails
            )
        ;   true )).
```

A blanket `catch(_, _, true)` over every trigger. **The throw is already discarded silently** —
it is a Pattern-6 absorption, not a loud failure. So the guard does not convert loud → silent;
it converts one silent path (exception swallowed, constraint skipped) into another (bander
returns `unknown`). The premise behind Phase 1b's caution does not hold here.

**Residual risk considered and ruled out.** Removing the throw could open a *new* firing path:
with the guard, `fpn_zone(unknown, FPNZone)` yields `FPNZone = unknown`, and the migration test
at `:527` is `FPNZone \= OneHopZone` — so an *absence* would compare unequal to a real band and
read as a zone migration. That path is unreachable today because `one_hop_zone/3` (`:144`)
depends on `drl_modal_logic:effective_purity/3`, which **succeeds 0 times** on the authored
grid (control: the 4-arity `drl_purity_network:effective_purity/4` succeeds 181/181 in the same
session and context, so this is measured-empty, not didn't-look). T6
`trigger_accelerating_pathology` therefore always fails at `:526` regardless of the guard.

> **Latent, recorded for the tracker:** if `drl_modal_logic:effective_purity/3` is ever revived,
> `:527`'s `\=` test will count `unknown` vs a real band as a migration — absence read as
> presence. The fix then is to require both bands to be real, not to remove the guard.

## 3. `fpn_report` — structurally unfiltered, empirically unexercised

`one_hop_ep_safe/3` and `fpn_ep_safe/3` (`:93`, `:99`) fall back to `-1.0`, and only the
*intrinsic* is filtered (`IP >= 0.0`, `:50`). Nothing filters the two banded values. So OQ-62's
"unfiltered" reading of the code is correct.

Empirically, however, no absence token reaches the bander on **any** available corpus. Among the
rows that survive the `IP >= 0.0` filter, the token mix at the bander input is pure `value`:

| Leg | constraints | rows | one-hop mix | fpn mix | zone counts today | migrations | verdict |
|---|---|---|---|---|---|---|---|
| `testsets` | 181 | 153 | `[value-153]` | `[value-153]` | contested 65, degraded 179, sound 62 | 6 | guard inert |
| `testsets_haiku` | 960 | 492 | `[value-492]` | `[value-492]` | contested 204, **critical 14**, degraded 665, sound 101 | 39 | guard inert |
| `testsets_flash` | 960 | 668 | `[value-668]` | `[value-668]` | contested 388, **critical 12**, degraded 711, sound 225 | 98 | guard inert |
| `testsets_kimi` | 1005 | 700 | `[value-700]` | `[value-700]` | contested 413, **critical 3**, degraded 866, sound 118 | 48 | guard inert |
| `testsets_sonnet` | 1001 | 930 | `[value-930]` | `[value-930]` | contested 586, **critical 20**, degraded 1027, sound 227 | 105 | guard inert |
| `archives/datasets/kernel_v1` | 1106 | 1102 | `[value-1102]` | `[value-1102]` | contested 288, **critical 105**, degraded 1591, sound 220 | 206 | guard inert |

`corpus_constraint` counts (199 / 960 / 960 / 1005 / 1001 / 1106) match the disk-verified file
counts, so each overlay loaded the leg it claimed.

**Every `critical` band above comes from a real, low purity value — none from a sentinel.** The
`IP >= 0.0` filter transitively excludes the absence-bearing rows: on all six corpora, a
constraint whose effective purity is absent also has an absent intrinsic. That correlation is a
property of the current data, **not enforced anywhere in the code**, which is exactly why the
guard is still worth landing.

### Correction to the plan's premise 2 — three claims with three different warrants

The plan recorded the sentinel hazard as "not latent … live-shaped," reasoning from today's
0-`critical` count on `testsets`. That reasoning does not hold, and the replacement must not
overstate in the other direction. Keeping the claims separate:

- **(a) Structurally unfiltered — CODE READ.** `one_hop_ep_safe/3`/`fpn_ep_safe/3` fall back to
  `-1.0`; only the intrinsic is gated (`:50`). Nothing filters the banded values.
- **(b) No leg exercises the path — WITNESSED on six corpora.** The table above, measured at the
  bander input, per-process.
- **(c) *Why* — DATA on one leg, NOT a traced code guarantee.** On `testsets` the set of
  constraints with an absent intrinsic equals the set with an absent one-hop EP — **set equality
  by membership**, 28 ≡ 28, not merely matching cardinality (`trigger6_control.pl` PART C). So
  the `IP >= 0.0` filter is co-extensive with EP-absence *here*. Whether it structurally
  *guarantees* exclusion would require tracing `drl_fpn:fpn_intrinsic/2` against
  `drl_purity_network:effective_purity/4`; **that trace was not done.**

Consequence for the guard's justification, which does not depend on (c): **it converts a
data-dependent property into a code-guaranteed one.** Under (c) the path is *unexercised*, not
*unreachable*. Either way Phase 1b is behavior-preserving and needs no output-changer treatment.

Also: the 0-`critical` count on `testsets` is not evidence about reachability at all — it is that
leg's purity distribution. `testsets_haiku` shows 14 `critical` with no sentinel involved.

**Provenance of the six-leg table.** It was measured **per-process, after** the leg-accumulation
defect below was discovered. Verifiable from the numbers themselves: the in-process run reported
`testsets_haiku` at 642 rows; the per-process run reports 492, which is what the table carries.

### Two methodological notes (both were near-misses)

1. **First reachability probe was blind.** It used the atom `default` as context and discovered
   constraints via `corpus_constraint/1`; `fpn_run/3` then failed and every accessor reported 0
   successes — which reads exactly like "the path is unreachable." The real path uses
   `constraint_indexing:default_context/1`. Landing off the authored grid produced a clean,
   confident, wrong answer (OQ-178 dual). The `fpn_run` success count is now the probe's
   positive control.
2. **In-process leg iteration is unsound.** Retracting `corpus_loaded/0` and `corpus_constraint/1`
   does not retract the `narrative_ontology` facts the testset files asserted, so legs accumulate
   and `sort/2` masks it behind ID dedup. The tell: kimi and sonnet returned byte-identical counts
   (1005 / 696 / identical zone histogram). Re-run one leg per **process**, they differ
   (700 vs 930 rows). Any future multi-leg sweep must fork per leg. **This invalidates any prior
   in-process multi-leg measurement in this project, not only this one** — escalated to its own
   tracker item, **OQ-246**, with the detection recipe.

## 4. The rename check that the byte-identity could not provide (2a)

Recorded here because it belongs with the census: `fpn_band/2`'s only consumer is T6, T6 fires 0,
and §2's blanket `catch(_, _, true)` swallows exceptions — so a missed call site would have left
every 2a witness (0 firings, byte-identical `abductive_report.md`, green gate) exactly as it
looks when the rename is correct. `trigger6_control.pl` closes it:

- **PART A** — T6 called directly on all 181 constraints, outside the catch wrapper: **0
  exceptions**, 0 fired.
- **PART B (reach-depth)** — because "no throw" is worthless if control never reached the renamed
  goals. Walking the body: `:522 subsystem_available → true`, `:524 fpn_ep → -1.0`,
  **`:525 fpn_band/2 → unknown`**, **`:526 one_hop_band/3 → failed cleanly`**. A missing predicate
  raises existence_error rather than failing, so a cleanly-failing goal is a resolved goal.
- The overlay route (force the full body by asserting the two blockers) is **unavailable**: both
  are static procedures — `drl_purity_network:effective_purity/3` (`:249`) and
  `metric_drift_events:drift_event/3` — so `assertz` raises `permission_error`. Hence reach-depth.
- `:534`'s `evidence_line(fpn, fpn_band, _)` key is TERM data, not a goal; a missed rename there
  cannot throw and T6 emits nothing, so it is read-verified only.

Incidental finding: `:525` returning `unknown` is the **Phase-1b guard firing live in the real
trigger path** (pre-guard: `fpn_critical`). The guard therefore does change an intermediate value
at the 28 constraints whose `fpn_ep` is `-1.0` — output-invisible only because `:526` fails
immediately after. "Guard inert" is exact about *output*, not about *evaluation*.

## 5. Verdict

Guarding all four banders is **behavior-preserving at every enumerated call site**, witnessed on
six corpora rather than argued from a code read. No call site depends on the throw: the only two
throwing sites are (a) already inside a blanket trigger-level `catch`, and (b) downstream of a
static predicate that never succeeds.

The blanket catch in §2 is itself a defect larger than OQ-62 — it makes all ten trigger firing
counts ambiguous between "didn't fire" and "errored," which is why the 0-firing count for
`accelerating_pathology` cited when OQ-62 opened was never a witness of non-firing. Escalated to
**OQ-247**.
