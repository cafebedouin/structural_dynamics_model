# Step 4c — Cross-framing partition (PILOT, n=6; not a Type-C/B verdict)

**Date:** 2026-06-07 · **Model (both arms, pinned):** gemini-2.5-pro · **Stakeholder arm:** the 6
4b stories; **four-tuple arm:** generated same topics, existing prompt (untouched). Evidence:
`step4c_partition.py`/`.out`/`.json`, `step4c_allpin_control.py`, `step4c_gen_fourtuple.py`,
12 `*.{stakeholder,fourtuple}.json`. Deliverable is a PARTITION read against the list, not a rate
read as a verdict (OQ-83 4c pins). No live-corpus writes; temp `.pl` removed; stderr surfaced.

## Pre-registered partition (evaluability BIN-BLIND: (a) same object + (b) (HasB,HasV) profile)

| topic | label | (a) object | (b) profile | bin (ε-pinned) |
|---|---|---|---|---|
| app_store_commission | contention | match | (T,T)=(T,T) | **flipped** |
| streaming_royalty_split | contention | match | (T,T)=(T,T) | **flipped** |
| hospital_insurer_reimbursement | contention | match | (T,T)=(T,T) | **survived** |
| payday_lending | non_contention | match | (T,T)=(T,T) | **survived** |
| time_zones | non_contention | match | (T,T)≠(T,F) | **unevaluable** (profile mismatch) |
| lightspeed_latency | mountain | match | (T,T)≠(F,F) | **unevaluable** (profile mismatch) |

Bin-blind evaluability fired correctly on real grain mismatches: time_zones (stakeholder forces a
victim seat where the four-tuple authors none) and lightspeed (stakeholder forces agents onto a
mountain the four-tuple keeps agent-free). Headline (analytic) type **survived in all 4 evaluable**
— snare both arms everywhere.

## Per-flip scrutiny → the two flips are NOT a framing effect; they are a resolution artifact

The bias pin says doubt flips. Applied:

1. **All-metric-pin control** (pin ε, supp, theater all to the four-tuple's values): the flips
   **persist** — app_store stake `[tangled_rope,…]` vs four `[snare,…]`; streaming
   `[naturalized,…]` vs `[snare,…]`, both at the powerless cell. So the flips are **not
   metric-drift.** (NB: the first run of this control returned empty-equals-empty and falsely
   read "identical/metric-drift" — a swallowed probe failure caught and re-run; the corrected
   result is the opposite. Recorded because the false pass would have shipped the wrong verdict.)
2. **Mechanism — victim COUNT × coalition threshold.** `resolve_coalition_power` upgrades the
   powerless seat → organized when `constraint_victim` count ≥ `critical_mass_threshold` (=3) under
   high ε/supp. Derived victim counts: app_store stake **3** vs four **2**; streaming stake **3** vs
   four **2**; hospital stake **3** vs four **3**. The two flips are exactly the two count
   straddles of the threshold (3 vs 2); hospital (3 vs 3, both upgrade) **survived** — fully
   consistent. The flip is the stakeholder surface enumerating one more distinct payer agent
   (role→victim per agent) than the four-tuple's bundled `victims[]`, crossing the coalition count
   at the powerless seat.

**This is the resolution contamination the operator predicted, surfacing through a driver my
boolean-(b) did not control.** Finding about the criterion (not retro-applied — bin-blind
discipline forbids rewriting this pilot's bins after seeing them): **(b) is incomplete.** The
orbit reads victim *count* via `resolve_coalition_power`, not only the `(HasB,HasV)` boolean. A
corpus-scale run must pre-register an extended (b): same `(HasB,HasV)` **and** victim-count on the
same side of `critical_mass_threshold` (coalition-status match). Under that corrected criterion
app_store + streaming are unevaluable (count straddle), not flipped — but that correction is
declared for next time, not slipped into this pilot's partition.

## The cleanest framing signal is at the CLAIM layer (robust, separate from the count issue)

All 3 evaluable contention topics: **stakeholder arm claims `rope`, four-tuple arm claims
`tangled_rope`** (3/3; signatures `false_ci_rope` vs `constructed_high_extraction`). The stakeholder
surface (describe agents neutrally) systematically elicits a coordination-flattering self-claim;
the four-tuple surface (which names victims/perspectives) elicits the hybrid claim. The engine
corrects BOTH to `snare` from metrics — so this moves the claim and the signature, **not the
computed type.** (Bonus: the stakeholder arm authors slightly *higher* ε/supp while claiming the
more coordination-flattering type.)

## Structure pass — untestable at this n

All 4 evaluable topics are high-ε extraction (computed snare); the one low-ε topic (time_zones)
was resolution-excluded. So "survivors=low-ε coordination / flippers=high-ε extraction" cannot be
tested. The flips track victim-count-threshold-crossing, not ε and not the contention label
(hospital is contention and survived). No structure detectable at n=4-all-high-ε — a power limit,
not a no-structure finding.

## What this says about Type-C/B (presented, not ruled)

The engine has **no framing-sensitive classification layer**: the constraint orbit is a function
of authored structure (metrics + victim count via coalition); authored perspectives are ignored
(A1). So "framing moves the classification" reduces entirely to "framing leads the model to author
different structure" — here: the claim (rope↔tangled_rope, robust) and the victim enumeration
count (3 vs 2, the flip driver). In mountain-and-frame terms: the **computed classification
survived** every same-grain comparison (hospital, payday; headline snare everywhere); what the
framing perturbed was the **authored claim** (the seat-dependent part) and the **victim-enumeration
granularity** (a resolution difference, excludable under corrected (b)). Whether the residual
structure-authoring drift is "the framing constituting a different object" (Type B) or
generation-granularity noise is **not settled at n=6** and is exactly what the corpus-scale run +
the 2×2 model×framing Ω (does the claim-drift reproduce under a model change) must decide.

## Scale + method

PILOT (6 topics), not the corpus-scale census. Method validated: bin-blind exclusions fired on
real grain mismatches; ε-pin + all-metric-pin + count-check per-flip scrutiny worked and
**caught a flip that looked like framing and was resolution**; and it surfaced the (b)-criterion
incompleteness (count-via-coalition) to pre-register next time. The partition is the operator's to
read; the believed reading leans survival (the two flips are resolution-attributable), the claim-
layer effect is the robust framing signal, and the corpus-scale verdict is future work.
