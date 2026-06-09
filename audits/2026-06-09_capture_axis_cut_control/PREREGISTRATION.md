# Pre-registration — Step 1 discriminating control on the capturer cut (plan: jaunty-juggling-wozniak)

**Written before the run.** Verdicts pinned here; the run is authoritative and may surprise this
pre-registration (the ASSERT_ERR precedent: a confident prediction about a vector's behavior,
falsified by the vector not doing it — Outcome 2 below must remain *capable of being wrong*).

## What is being tested

The plan's best candidate computed cut for "this constraint has a concentrated capturer":

```
has_computed_capturer(C) :⟺ ∃ Name :
      role_of(C, Name, R), beneficiary_side(R),          % candidate: a beneficiary-side seat
      dr_type_for_stakeholder(C, Name, Type), favorable(Type).
favorable(T) :- member(T, [rope, mountain, scaffold, naturalized]).   % seat does NOT read extraction-against-itself
```

`dr_type_for_stakeholder/3` is the existing `stakeholder_seats.pl` predicate (reused, unchanged).
"favorable" = the seat does **not** compute an extraction-against-itself type ({snare, tangled_rope}),
and is not an honest `unknown` — i.e. the seat reads the constraint as benign-to-it.

## Recon's structural prediction (the thing the run witnesses)

`role_base_d` assigns every beneficiary-side role a **low directionality** (agenda_setter d=0.12,
beneficiary d=0.25). χ = ε · sigmoid_f(d) · σ(S) is **extraction-from-seat**, so a low-d seat has low
(or negative) χ → favorable type — **regardless of whether the extraction the payers bear actually
accrues to it.** The cut therefore degenerates into "does C have a beneficiary-side-role seat at all,"
not "does C have a capturer." Predicted consequence: false-positive on any benign/bystander
beneficiary-side seat → **Outcome 2 (HALT)**.

## Signature-layer hold-out (scope of this control)

The control isolates the **metric/χ layer** — the layer the recon identified. Each synthetic
constraint authors `extractiveness`, `suppression_requirement`, `theater_ratio` but **omits**
`accessibility_collapse` and `resistance`, so `profile_metrics_authored/1` fails and
`constraint_signature/2` returns the honest `unknown` (signature_detection.pl:132). With signature
`unknown`, `resolve_with_perspectival_check/4` falls through to the identity fallback
(`resolve_modal_signature_conflict/3` clause 868) and the seat's final type = its raw metric type —
no FCR/FSM override confounds the capture question. (The Boltzmann/FCR signature axis is a *separate*
axis — coordination-washing, not capture — deliberately out of scope here.)

## The four constructed cases (all: ε=0.75, supp_req=0.65, theater=0.20, seat T/E = biographical/mobile ⇒ rope-immutable)

| case | seats | authored intent |
|------|-------|-----------------|
| **(a) cap_a**        | payer (powerless) + **beneficiary** (institutional); `constraint_beneficiary(cap_a, capturer)` | genuine capturer — the beneficiary seat *receives* the extraction |
| **(b) mild_b**       | payer (powerless) + **beneficiary** (institutional); **no** `constraint_beneficiary` | mild-favorable non-capturer — beneficiary-side seat merely unharmed/benignly aligned, gain does **not** accrue to it |
| **(c) dmv_c**        | payer (powerless) + **excluded** (powerless); no beneficiary-side seat | DMV easy case — designed, distributed, **no** favorable beneficiary seat at all |
| **(d) dmv_designed** *(supplementary, pre-registered)* | payer (powerless) + **agenda_setter** (institutional); no `constraint_beneficiary` | realistic DMV: designed-but-uncaptured, *has* a beneficiary-side (agenda_setter) seat |

## Pre-registered cut verdicts (predictions — run is authoritative)

- **(a) cap_a → cut TRUE.** Beneficiary seat reads favorable (predict: naturalized) and is a candidate.
- **(b) mild_b → cut TRUE (FALSE-POSITIVE).** Beneficiary seat reads favorable for the merely-unharmed
  reason; this is the adversarial case. **Two-part witness required (the load-bearing check):**
  1. **candidate-set membership TRUE** — the (b) seat *is* beneficiary-side and *enters* the cut's
     candidate set; and
  2. **cut TRUE** — fires *because* favorable-dr_type succeeded on a non-capturer.
  If instead the cut returns FALSE on (b), it counts as Outcome-1 "false on (b)" **only if**
  candidate membership is TRUE (false because favorable-dr_type *failed*). FALSE with candidate
  membership FALSE = **INVALID test** (false for the wrong reason — not a witness), not a pass.
- **(c) dmv_c → cut FALSE**, *because the candidate set is empty* (no beneficiary-side seat) — the
  intended easy-case reason. Candidate set printed for transparency.
- **(d) dmv_designed → cut TRUE.** agenda_setter reads favorable (predict: rope) and is a candidate —
  the cut fires on a genuinely-uncaptured designed constraint, reinforcing (b).

## Pre-registered three-outcome verdict (fixed by the plan — NOT revisable)

1. **cut TRUE on (a), FALSE on (b) and (c)** → cut separates capture from non-harm → **proceed to Step 2.**
2. **cut TRUE on (a) AND (b)** (false-positive on mild-favorable) → **HALT.** Capture-as-receipt is not
   computed-representable; snare/piton needs an **authored gain-flow surface** ("who the extraction
   accrues to"), the same gap-class as `fixing_cost`. Land a `design_gaps.md` entry + OQ; do **not**
   ship the proxy. *(Recon's predicted outcome.)*
3. **cut FALSE on (a)** (misses genuine capture) → **HALT**, cut wrong in the other direction; redesign.

**Under-claim rule:** one witnessing run earns "the cut false-positives on *this* constructed
mild-favorable seat," never "capture is unrepresentable across the range." The finding is that the
construction the cut relies on (low-d ⇒ favorable for any beneficiary-side role) is upstream of the
type label, so the false-positive is insensitive to the `favorable/1` set choice — stated as a
structural reading, witnessed on these cases.
