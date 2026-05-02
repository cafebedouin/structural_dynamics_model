# Gap Analysis: observers_not_humans v4

**Scope:** What the existing codebase and framework documents imply about the four acknowledged gaps.  
**Date:** 2026-05-01  
**Methodology:** Code inspection (Prolog engine, Python diagnostics, framework docs), cross-referenced against v4 paper claims.

---

## Executive Summary

| Gap | Status | Assessment |
|-----|--------|------------|
| Reflexivity | Mostly closed in code; paper has one framing slip | Tractable correction |
| Observer-class threshold | Closer to closed than v4 surfaces | Tractable derivation from machinery |
| Mixed-flow derivation | Framed incorrectly in v4; gap is smaller but different | Requires reframing, not new theory |
| §5.5 protocol circularity | Fix already proposed; one strong addition available | Tractable |

**What's coupled:** The observer-class threshold question and the mixed-flow derivation question converge on the same insight — the framework routes based on structural relationship to the constraint (beneficiary/victim status, exit options), not on cognitive architecture. Articulating this cleanly would address both gaps in one move.

**What contradicts the prompt's framing:** The mixed-flow "derivation extension" framing in v4 is the wrong frame. The 62.4% chi-based sign-flip in tangled_rope is not evidence that the zero-sum derivation extends with dampening. It is evidence that the sigmoid's design (negative lower bound L = -0.20 at the beneficiary pole) produces opposite-sign chi for institutional beneficiaries regardless of whether flows are zero-sum. The mechanism is different; the result is actually cleaner; but it requires reframing, not more derivation work.

**Flags at the end:** One live paper-level reflexivity slip in §2.3; an unresolved concern about whether the §5.5 operationalization can actually detect what it claims to detect; a potentially useful prototype proposal for the threshold question.

---

## 1. Reflexivity

**The question after reading:** Is the metrics-as-routing principle being applied consistently across the codebase and documentation, or are there places where threshold-setting, classification, or output reporting leaks back into authority claims?

### What the code shows

**Prolog engine — clean after April 2026 fixes.** The three gate violations documented in `metrics_as_routing.md` have been repaired. The current `coupling_invariant_rope/2` in `signature_detection.pl:897–917` explicitly comments the fix:

```prolog
% Collect excess extraction as diagnostic evidence (not a gate).
% The floor override in boltzmann_floor_for/2 is editorial data,
% not a classification input — gating here allowed overrides to
% suppress CI_Rope certification on genuinely coordinating constraints.
(   excess_extraction(C, ExcessEps)
->  true
;   ExcessEps = 0.0
).
```

The `fcr_test_failure/2` predicates (`signature_detection.pl:1135–1154`) now collect failures as diagnostic evidence rather than hard gates. `classify_from_metrics/6` in `drl_core.pl:300–385` uses only structural facts as gates — `emerges_naturally(C)`, `has_coordination_function(C)`, `has_asymmetric_extraction(C)`, `effective_immutability_for_context/2` — none of which are diagnostic outputs that can be overridden.

**Python pipeline — clean.** Key diagnostics explicitly isolate themselves from classification:
- `cc_diagnostic.py` is marked STRUCTURAL throughout, "does not alter classification, χ computation, H¹, or any engine output."
- `epsilon_sensitivity.py` works on RAW (pre-override) distributions by design, explicitly because "unconditional signature overrides produce constant post-override distributions with zero ε sensitivity."
- `f2d_sensitivity_crossing.py` discloses its approximations clearly: "Prolog-side effects (immutability gates, signature overrides, scope adjustments) are NOT modelled. Type bins are approximated by chi thresholds."
- All diagnostic scripts read from pre-computed pipeline JSON; none write back to classification predicates.

**One borderline architectural issue — FCR boost.** Axiom 6 applies a 3× prior boost to tangled_rope when the Boltzmann independence test detects cross-perspectival coupling alongside extraction. The test result (a probabilistic diagnostic) feeds into prior weights. This is structurally analogous to the gate violation pattern — a test output influencing a classification — but it's architectural design, not drift, and operates on prior weights rather than on hard gates. The April 2026 principle says blocking gates should use structural facts; prior-boost from diagnostics is different in kind. Whether this is a violation depends on whether you consider "probabilistic routing influence" to be the same as "authority claim." I read it as defensible but worth watching if the 3× weight ever becomes contested.

### What the paper shows — one real slip

The paper-level reflexivity concern is in v4 §2.3, the mixed-flow update:

> "Empirical analysis of the tangled_rope subset (N=2,245) shows that the institutional sign-flip survives in mixed-flow constraints: 62.4% exhibit chi-based institutional sign-flip... The rate is below pure-flow populations (rope 90.9%, snare 77.1%), indicating that mixed flow structure attenuates but does not eliminate the mechanism."

This uses a corpus statistic (62.4%) to update the status of a theoretical claim ("the derivation extends to tangled_rope"). That is the right *direction* of inference — using evidence to update theory — but the sentence "the structural claim covers the full corpus" is an authority claim that goes beyond what 62.4% can support. More precisely: 62.4% is evidence that the chi mechanism fires in mixed-flow cases, but whether this is evidence for *the zero-sum derivation extending* (as §2.3 frames it) or for something else (as I argue in Section 3 below) is not settled by the empirical number alone.

### What's genuinely missing

An explicit audit checklist for the paper layer, analogous to the architectural check for the Prolog layer: "if a paragraph issues a scope claim about the structural derivation, every supporting premise should be a structural consequence of the axioms, not a corpus statistic." The 62.4% number belongs in the empirical evidence section; whether it validates "the derivation extends" is a distinct theoretical question.

### Next steps

1. Run the architectural check from `metrics_as_routing.md` on paper language: for each §2.3 scope claim, identify whether it rests on axiom-derived consequences or corpus statistics, and mark the latter accordingly. This is editing work, not new theory.
2. Monitor the FCR boost if the classification architecture changes; it is the only live candidate for drift back toward a gate violation.

**Assessment:** Tractable correction on the paper layer. Code layer is clean.

---

## 2. Observer-Class Threshold

**The question after reading:** What does the framework's own machinery require of an observer to sustain its analysis? Can the threshold criterion be derived from what the framework already does?

### What the machinery requires

The chi computation chain in `constraint_indexing.pl:512–526` (`extractiveness_for_agent/3`) is the most explicit statement of what the framework presupposes:

```prolog
extractiveness_for_agent(Constraint, Context, Score) :-
    Context = context(agent_power(Power), _, _, spatial_scope(Scope)),
    resolve_coalition_power(Power, Constraint, ResolvedPower),
    ...
    narrative_ontology:constraint_metric(Constraint, ExtMetricName, BaseScore),
    derive_directionality(Constraint, ResolvedContext, D),
    resolve_displacement(ResolvedPower, Delta),
    D_eff is max(0.0, min(1.0, D + Delta)),
    sigmoid_f(D_eff, PowerMod),
    scope_modifier(Scope, ScopeMod),
    Score is BaseScore * PowerMod * ScopeMod.
```

For chi to be non-trivially computed and position-dependent:
1. `BaseScore` must be > 0: the constraint must impose some flow (base extractiveness ε)
2. `PowerMod` = sigmoid_f(D_eff) must vary with position: the entity's directionality D must differ across positions
3. D is derived from `power_role_heuristic` + `exit_modulation`: D depends on whether the entity is a beneficiary/victim AND what exit options they have

The `power_role_heuristic` table (`constraint_indexing.pl:434–444`) is the key:

```prolog
power_role_heuristic(powerless,     _, true,  0.85).  % is a victim
power_role_heuristic(institutional, true, _,  0.15).  % is a beneficiary
power_role_heuristic(analytical,    _, _,     0.72).  % meta-observer
```

The directionality is not determined by cognitive architecture. It is determined by:
- Whether the entity is a **beneficiary** of the constraint (receives its coordination benefits, gains from its extraction of others)
- Whether the entity is a **victim** of the constraint (bears its extraction costs, is constrained by its enforcement)
- What **exit options** the entity has (trapped, mobile, arbitrage)

This is the hidden threshold in the machinery: **an entity can be treated as an observer if and only if it has a position-dependent beneficiary/victim relationship to the constraint and some relationship to exit options.** Not cognitive architecture — structural role.

### Why this is richer than §3.2

The §3.2 criterion (position-distinguishability, differential state-update, outcome-correlation) is correct but underdetermines the threshold. The machinery implies something stronger: **the threshold is satisfied by entities whose state updates track their net flow position (beneficiary vs. victim), mediated by their capacity to exit the constraint.**

This is why a thermostat fails despite meeting §3.2's three components in a surface reading:
- A thermostat at position A and position B both update in ways that correlate with measurable outcomes
- But a thermostat has no *beneficiary/victim* relationship to constraints — its state update doesn't track whether it is on the giving or receiving end of a flow
- More precisely: a thermostat has no exit options; its update rule is the same regardless of whether it could leave the system

The PID controller objection in §3.2 ("A position-sensitive PID controller arguably meets all three components") is answered by the machinery's deeper criterion: the state update must track **the controller's own net flow position** relative to the constraint, not just the physical state the controller is measuring. A PID controller regulates a physical quantity but doesn't have a position-relative net flow from the regulation.

An RL agent trained in an asymmetric environment passes because:
- Its reward function IS the net-flow analog: reward = resources gained - costs borne
- Its policy updates ARE differential state updates that track position-relative net flows
- It has "exit options" in the functional sense: terminal actions correspond to arbitrage or trapped, depending on action set

The machinery therefore implies a **derivable threshold criterion**:

> An entity qualifies as an observer class for DR purposes if its state update function U(state, constraint, position) has the property that the derivative ∂U/∂position is nonzero when position tracks beneficiary/victim structural role. Equivalently: the entity's internal state must be more affected by a constraint when it is in the victim role than in the beneficiary role. This is the functional form of "tracking position-relative net flows."

This criterion is non-circular (doesn't require running DR to check) and excludes the PID controller (its ∂U/∂position = 0 with respect to beneficiary/victim role, even if it's sensitive to physical position).

### What's actually missing

Two things are missing from this derivation:

**Missing (a): The minimum complexity condition.** The criterion I've derived is binary — satisfied or not — but the framework's machinery uses a six-level power scale (powerless to analytical). The question of WHICH observer-class level an entity falls into requires mapping its structural properties to one of these six atoms. For RL agents and animal coalitions, this mapping is non-trivial and will involve judgment calls about what "institutional" means in that domain. The machinery doesn't help here because it presupposes the power-atom mapping as input.

**Missing (b): The analytical meta-observer.** The canonical U₄ (analytical) observer has `power_role_heuristic(analytical, _, _, 0.72)` — it gets a fixed directionality of 0.72 regardless of beneficiary/victim status. This is the "meta-observer stance" — analytical observation without structural investment. What makes an entity an analytical observer rather than a moderate one? The machinery provides the answer operationally (power atom = analytical → context tuple → canonical d = 0.72), but the criteria for when an entity inhabits the analytical position rather than a structural position are not derived from the machinery.

### Next steps

1. Draft an explicit threshold criterion of the form above — "state update function has nonzero derivative w.r.t. beneficiary/victim structural role" — and check whether it handles the cases §3.2 lists. This is writing work, one to two paragraphs.
2. Leave missing (a) as explicitly open: "the mapping from structural properties to power-atom level is a translation judgment, not derivable from the machinery, and must be specified before any cross-class empirical test."
3. Flag the analytical observer as a special case requiring separate treatment.

**Assessment:** Tractable derivation from existing machinery. The criterion is implicit in `power_role_heuristic` + `exit_modulation`. Writing it explicitly would close this gap for v5.

---

## 3. Mixed-Flow Derivation

**The question after reading:** What does the existing machinery imply about the structural mechanism for sign-flip in mixed-flow constraints? Is there a derivation latent in the chi-vs-type gap or the non-monotonic distribution that v4 doesn't surface?

### What v4 actually says vs. what the machinery shows

V4 §2.3 frames the mixed-flow finding as: "the §2.3 derivation extends to tangled_rope, with the caveat that mixed flows produce weaker institutional sign-flip than zero-sum cases." This framing implies the zero-sum derivation works for mixed flows but is dampened by the non-zero-sum structure.

The machinery contradicts this framing. Here's why:

The zero-sum derivation says: agent at position p experiences F_A where sign(F_A) depends on position relative to beneficiaries. If the constraint is zero-sum, institutional gets +F and powerless gets -F, producing opposite-sign experiences automatically.

For **tangled_rope** constraints, the chi formula is what produces the sign-flip:

```
χ_institutional = ε × f(d_institutional) × σ = ε × f(0.10) × σ ≈ ε × (−0.07) × σ < 0
χ_powerless = ε × f(d_powerless) × σ = ε × f(0.90) × σ ≈ ε × 1.42 × σ > 0
```

The sign-flip happens because `f(0.10) < 0` — the sigmoid has a negative lower region that maps beneficiary directionalities to negative chi values. This is true regardless of whether the constraint's flows are zero-sum. The mechanism is the **sigmoid's zero-crossing at d ≈ 0.29**, not the constraint's flow structure.

The tangled_rope classification itself presupposes:
- `has_coordination_function(C)` = true (beneficiary structure exists)
- `has_asymmetric_extraction(C)` = true (victim structure exists)
- chi in the mid-range [tangled_rope_chi_floor, tangled_rope_chi_ceil]

For the institutional observer of a tangled_rope constraint, d_institutional ≈ 0.10 (from `power_role_heuristic(institutional, true, _, 0.15)` + modest exit modulation). At d = 0.10, f(d) ≈ -0.07 to -0.12, giving χ < 0. The institutional observer sees the constraint as coordination (χ < rope_chi_ceiling = 0.35) regardless of whether the coordination benefit and extraction are zero-sum.

The 62.4% chi-based sign-flip rate in tangled_rope is therefore **mechanistically explained by the sigmoid design, not by the zero-sum derivation extending.** Any tangled_rope constraint where the institutional observer has d < 0.29 will produce χ_institutional < 0. Given the canonical directionality assignment for institutional beneficiaries (d ≈ 0.10–0.15), this is nearly universal — the 37.6% of tangled_rope constraints WITHOUT institutional sign-flip are cases where the institutional observer's effective d is above 0.29 (from directionality overrides or non-standard structural roles).

### The actual mixed-flow mechanism

The correct account is:

For **zero-sum flows**: sign-flip is derived from flow-sign conservation — the same constraint that gives +F to the institutional beneficiary gives -F to the powerless victim. The zero-sum derivation in §2.3 is correct for this case.

For **mixed flows (tangled_rope)**: sign-flip is derived from the sigmoid's architecture — the beneficiary pole of the directionality axis maps to negative chi (routing the institutional observer to "coordination/rope" responses) while the victim pole maps to positive chi (routing the powerless observer to "extraction" responses). The sign-flip is a property of the routing design, not of the constraint's flow structure. It is the framework saying: "for any constraint where the institutional observer is structurally a beneficiary (d ≈ 0.10), we route them to the coordination interpretation, regardless of whether the constraint also extracts from others."

This is **not a weakening of the derivation**. It is a different derivation. The two cases are:

| Flow type | Sign-flip mechanism | Derivation source |
|-----------|--------------------|--------------------|
| Zero-sum (rope, snare) | Flow-sign conservation | §2.3 zero-sum derivation |
| Mixed (tangled_rope) | Sigmoid beneficiary/victim asymmetry | Axiom 2 design + metrics-as-routing |

The mixed-flow mechanism is actually *stronger* in one sense: it doesn't depend on the constraint being zero-sum. It depends only on the institutional observer being a structural beneficiary (d < 0.29). This is true for 62.4% of tangled_rope constraints, consistent with the corpus finding.

### What the chi-vs-type gap shows about this

The 62.1 percentage point chi-vs-type gap (62.4% chi-based vs. 0.3% type-based) demonstrates that the cover-story mechanism operates between the chi level and the type level. But it also demonstrates that the chi-level sign-flip is structural (it persists even when the type classification collapses it). This is consistent with the sigmoid-design account: the chi calculation is a function of structural position (d values), and the cover story operates at the type classification level (FCR, naturalization) to suppress the chi-level signal.

The chi-vs-type gap is not a "second independent trace of the §2.2 cover-story machinery" in the way v4 frames it. It IS a trace of the cover-story machinery, but what it also shows is that **chi is a more direct measure of the structural mechanism than type**. Chi captures the routing decision before cover-story resolution; type captures it after. This distinction has implications for what the §5.5 RL protocol should measure.

### What's genuinely missing

One thing is actually missing from the machinery: an account of WHY the 37.6% of tangled_rope constraints that do NOT show chi-based institutional sign-flip fail to do so. If the mechanism is "institutional beneficiary has d < 0.29 → chi < 0," then the 37.6% non-sign-flip cases must have institutional d ≥ 0.29. Looking at the chi sign distribution: U₃ has 37.6% positive chi and 62.4% negative chi in tangled_rope. The positive-chi cases are either:
- Directionality overrides pushing d_institutional above 0.29
- Constraints where the institutional observer is NOT the beneficiary (unusual role structure)
- Cases where coalition power adjustment changes the effective power atom

A breakdown of the 37.6% positive-chi cases at U₃ would tell you whether the non-sign-flip population is structurally different (different role assignments) or represents a calibration boundary artifact. This is a one-script empirical question that would complete the mixed-flow account.

### Next steps

1. **Reframe §2.3** from "zero-sum derivation extends with dampening" to "zero-sum constraints and mixed-flow constraints have different sign-flip mechanisms, both derivable from the axioms." This is conceptually cleaner and actually a stronger claim. Cost: two paragraphs of rewriting.
2. **Write the sigmoid-based mixed-flow derivation explicitly**: "For constraints where the institutional observer has structural beneficiary directionality d < d_zero ≈ 0.29, the sigmoid produces χ_institutional < 0 regardless of whether the constraint is zero-sum. This follows from Axiom 2's design choices (L = -0.20, d₀ = 0.50, k = 6.0)." This derivation is already implicit in the machinery; it just needs to be written.
3. **Profile the 37.6% non-sign-flip tangled_rope cases** at U₃ to verify that they have d_institutional ≥ 0.29 and explain why. A ~30-line Python script reading from `enriched_pipeline.json` would do this.

**Difficulty:** Reframing is a writing task. The 37.6% profile script is one hour of work.

**Assessment:** This is the gap most different from how v4 frames it. The 62.4% finding does not validate the zero-sum derivation extending — it validates a different mechanism. Writing that mechanism explicitly is tractable. The gap is SMALLER than v4 suggests (no new theoretical work needed) but in a DIFFERENT place (the mechanism is wrong, not just incomplete).

---

## 4. §5.5 Protocol Circularity

**The question after reading:** Does existing machinery suggest other independent tests beyond policy-divergence that could be added to §5.5?

### The fix already proposed

Policy-divergence measurement addresses the circularity cleanly: agents should show differential policy across positions (demonstrating position-sensitivity) regardless of whether sign-flip emerges. If differential policy is present AND sign-flip is absent, that implicates DR's structural claim, not the observer-class criterion.

This is the right fix and should be included.

### What the machinery suggests additionally

Three additional tests are derivable from existing machinery:

**Test A: Reward-differential vs. behavioral-classification gap (chi-vs-type analog)**

The corpus machinery produces two levels of measurement: chi (the raw flow metric before cover-story resolution) and type (the post-classification surface). The 62.1 percentage-point gap between these is the empirical signature of the cover-story mechanism.

In the RL context, an analogous two-level measurement is available:
- *Level 1*: the **reward-differential across positions** — the actual difference in cumulative reward between an agent in the high-capacity position vs. an agent in the low-capacity position, for the same set of environmental constraints
- *Level 2*: the **agent's behavioral classification** — how the agent acts when the same constraint is presented from different positions (does it choose to comply, resist, or ignore?)

If Level 1 shows strong differential (agents get different rewards from the same constraints across positions) but Level 2 does not show differential behavior (agents act the same way regardless of position), this is the RL version of the chi-vs-type gap. It would be evidence that the cover-story mechanism is operating — the agents are experiencing different flows but their behavioral response has been equalized, which is the functional analog of naturalization.

This test is more informative than sign-flip alone because it would show whether the RL training process is itself producing a cover-story dynamic — whether the training acts as a force that suppresses position-relative response at the behavioral level while the reward-differential persists at the experiential level.

**Test B: Orbit structure prediction (H¹ analog)**

DR predicts that the ORBIT STRUCTURE differs between asymmetric and symmetric environments, not just the sign-flip rate:
- Asymmetric environment: agents' value functions should produce non-constant orbits (position A classifies constraint as positive-value; position B classifies same constraint as negative-value)
- Symmetric environment: orbits should be constant (both positions agree)

More specifically, DR predicts H¹ > 0 in the asymmetric environment and H¹ = 0 in the symmetric one. This can be operationalized without sign-flip: if you can measure whether two RL agents at different positions in the asymmetric environment produce consistent or inconsistent value assessments across a set of constraints, you can compute the orbit structure.

This test is stronger than sign-flip because it tests the framework's cohomological structure claim, not just the directional prediction. It would also generate the H¹=4 suppression prediction: if the asymmetric RL agents produce orbits at all, the (3,1) partition should dominate over (2,2), which would be a DR-distinctive finding in the RL domain.

**Test C: Coalition formation as position-sensitivity signal**

The Prolog engine contains `resolve_coalition_power` (constraint_indexing.pl:468–488): when a critical mass of victim-role agents is present for a high-extraction constraint, powerless agents upgrade to organized power level. This is a specific, testable prediction: in the asymmetric RL environment, if you have multiple low-capacity agents facing the same high-extraction constraint, you should see emergent coordination behavior — the agents begin acting as if they have collective capacity they didn't have individually. This tests the framework's coalition theory in the RL domain. In the symmetric environment, no such asymmetry should drive coalition formation.

Coalition formation is a behavioral signal that doesn't require verbal classification or chi measurement — it's detectable from action sequences — making it more accessible in a standard RL framework than Tests A or B.

### Assessment of additions

Test A (reward-differential vs. behavioral gap): Most theoretically significant. Tests whether training itself produces the cover-story mechanism. Requires two measurements per agent per constraint: its reward outcome AND its behavioral choice. Feasible in standard RL frameworks with logged trajectories.

Test B (orbit structure): Most directly tests the framework's distinctive predictions. Requires operationalizing "value assessment" as a consistent measurement across positions and constraints. Medium difficulty — requires defining the constraint-presentation protocol carefully.

Test C (coalition formation): Most practically accessible. Tests a specific consequence of the machinery rather than the sign-flip mechanism directly. Easy to implement if the RL environment allows multi-agent coordination signals.

**Rank order for v5**: Add Test A (reward-differential gap) as the primary addition alongside policy-divergence, then Test B (orbit structure), then Test C (coalition) as aspirational.

**Assessment:** The policy-divergence fix is right; add the reward-differential vs. behavioral-gap test as the second core measurement. Both are tractable within the §5.5 RL protocol as described.

---

## Coupling Between Questions 2 and 3

The observer-class threshold and the mixed-flow derivation are more closely coupled than v4 surfaces. Both turn on the same structural fact:

**The framework routes based on beneficiary/victim structural relationship to the constraint, not on cognitive architecture.**

For the threshold question: what makes something an observer is that its state updates track its net flow position (victim → positive chi, beneficiary → negative chi). The threshold criterion is derivable from `power_role_heuristic` + `exit_modulation` + `sigmoid_f`.

For the mixed-flow derivation: the reason sign-flip occurs in mixed-flow constraints is that the institutional observer is a structural beneficiary (d ≈ 0.10), not that the flows are zero-sum. The mechanism is the same routing decision — map beneficiary d to negative chi.

The common insight: **chi sign is a routing decision about structural role (beneficiary vs. victim), not a measurement of physical flow.** This is consistent with metrics-as-routing.md's foundational claim. Writing this clearly for both questions would eliminate the apparent gap between them.

If v5 adds a section making this explicit — "chi is a routing instrument that maps structural position to expected flow direction; sign-flip is the routing consequence of the beneficiary/victim distinction, not a derived physical property of the flows" — both the threshold criterion and the mixed-flow derivation fall out of it.

---

## Flags

**F1. The §2.3 "extends with dampening" language risks confusion.**

V4 says the structural claim "covers the full corpus" after seeing 62.4%. But the 38% of tangled_rope constraints without chi-based institutional sign-flip are not explained. A monotonic "extends with dampening" account predicts that MORE mixed flow → less sign-flip. But the 37.6% non-sign-flip cases are not correlated with "more mixed" — they're correlated with institutional directionality being above the zero-crossing. This is a qualitatively different explanation than dampening. If someone tests this prediction and finds that the non-sign-flip cases are mixed-flow but have high-d institutional observers (structural non-beneficiaries), that confirms the sigmoid account. If they find the non-sign-flip cases have lower ε or less obvious beneficiary structure, that might support a dampening account. This ambiguity is currently unresolved in the corpus and should be flagged in v5 rather than papered over.

**F2. The §5.5 protocol can detect sign-flip absence but may not be able to distinguish the two failure modes it's designed to separate.**

The protocol's decisive outcome is: "Neither environment shows sign-flip → DR's structural claim is undermined." But this relies on the RL agents actually meeting the observer-class criterion. The proposed fix (policy-divergence measurement) addresses this, but only if you can verify that policy-divergence is actually measuring what it claims — that the agents are genuinely in different structural positions, not just different physical locations.

The concern is subtle: RL agents in an "asymmetric environment" with different action sets are structurally different in the protocol's intended sense, but whether they are different in DR's sense (beneficiary vs. victim of environmental constraints) depends on whether the constraints' flow structures actually produce differential reward — which is the thing you're trying to test. There's a potential circularity: you're testing whether agents show sign-flip when the constraints impose asymmetric flows, but the definition of "asymmetric flows" is specified by the experimenter, not by whether the agents experience them as asymmetric. The reward-differential test (Flag F2's addition) partially addresses this — measuring whether the flows are actually differential before asking whether sign-flip emerges from them.

**F3. The H¹=6 / flow_asymmetry_test_v2.jsx error is documented but not yet fixed.**

V4 §3.3 explicitly notes: "`flow_asymmetry_test_v2.jsx` incorrectly lists {1, 2, 6} as unreachable for any classification rule — only {1, 2} are combinatorially forbidden; H¹=6 is forbidden only in ternary models, not in DR's multi-type system." This JSX file is in the untracked files list (`docs/flow_asymmetry_test_v2.jsx`). The error is documented in the paper but the file itself still contains the incorrect claim. If the JSX is deployed or cited, this creates a contradicting source.

**F4. Two propagation profiles and their different evaluation criteria need explicit treatment in v5.**

The prompt's Mode A / Mode B distinction (framework machinery vs. applied outputs, specialist vs. general audiences) is not in the v4 paper. The reflexivity question for v5 is partly: which of the paper's claims are Mode A claims (being evaluated by whether they make specialists produce better analyses) and which are Mode B claims (being evaluated by whether they produce useful clarity in applied outputs)? Some of the §6 limitation language in v4 is written as if all of DR's propagation is Mode A, which understates the paper's actual reach if the UKE pipeline essays count as Mode B outputs.

---

*Analysis produced 2026-05-01 by reading: `docs/observers_not_humans_v4.md`, `docs/metrics_as_routing.md`, `docs/results/tangled_rope_sign_flip.md`, `docs/results/fragility_cross_tab.md`, `docs/results/h1_distribution_test.md`, `docs/deferential_realism_paper_v6.11.md`, `docs/core_v4.3.md`, `prolog/constraint_indexing.pl`, `prolog/drl_core.pl`, `prolog/narrative_ontology.pl`, `prolog/signature_detection.pl`, `python/tangled_rope_sign_flip.py`, `python/fragility_cross_tabulation.py`, `python/cc_diagnostic.py`, `python/f2d_sensitivity_crossing.py`, `python/run_pipeline.py`.*
