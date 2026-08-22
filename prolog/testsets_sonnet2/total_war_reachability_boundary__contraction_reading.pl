% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Nuclear-Contracted Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   Beginning in 1945 and hardening through the thermonuclear era and the
 *   achievement of survivable second-strike arsenals by the major powers,
 *   this reading holds that nuclear weapons did not merely change the
 *   probability of total war — they removed 'winnable total war between major
 *   nuclear powers' from the feasible set entirely, as a matter of physical
 *   and strategic structure rather than policy choice. Under this reading,
 *   mutually assured destruction is not a coordination equilibrium that
 *   participants maintain through effort (the dropping_reading's framing) and
 *   not a contingent capability gap that could close with new technology (the
 *   contingent_reachability_reading's framing). It is a structural closure:
 *   given current physics (fission/fusion yields, the impossibility of
 *   comprehensive damage-limitation against a survivable retaliatory force,
 *   and the climatic/radiative consequences of large-scale exchange), no
 *   strategy converts a total-war initiation into a winning outcome for the
 *   initiator. This is the contraction_reading of the
 *   total_war_reachability_boundary kernel — one of three readings generated
 *   as separate constraint stories per the ε-invariance principle. The
 *   dropping_reading treats the same boundary as an actively-maintained rope
 *   (deterrence requires continuous signaling and crisis management, and
 *   could in principle fail or be abandoned). The
 *   contingent_reachability_reading treats it as a piton — an atrophied
 *   capability gap that specific technological developments (effective
 *   missile defense, precision counterforce, hypersonic decapitation strikes)
 *   could reopen. This story generates ONLY the contraction reading: total
 *   war as a mountain, with no beneficiary structure (no actor, including
 *   nuclear-weapon states, gains from an actual total-war outcome) and a
 *   victim set that is universal — the entire species bears extinction-level
 *   tail risk from the mere existence of the arsenals that hold the boundary
 *   closed, even though no one benefits from that risk.
 *
 * KEY AGENTS:
 *   - global_population: universal victim (analytical/trapped) — bears species-level extinction-tail risk from arsenal existence, with no exit from the risk itself
 *   - nuclear_weapon_states: agenda-setters over arsenal posture and doctrine, but not beneficiaries of total war's foreclosure in any extractive sense — they are equally subject to the mountain
 *   - non_nuclear_states: bear the same tail risk as nuclear states with no agenda-setting power over the arsenals that generate it
 *   - strategic_theorists: analytical observers who study and debate which of the three kernel readings best describes the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.03).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.05).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.03).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Nuclear-Contracted Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '1c6fd352-0204-46af-8f77-dc482ab92bc9').
narrative_ontology:cs_kernel_codification('1c6fd352-0204-46af-8f77-dc482ab92bc9', implicit).
narrative_ontology:cs_authority_grounding('1c6fd352-0204-46af-8f77-dc482ab92bc9', none).
narrative_ontology:cs_reading_relation('1c6fd352-0204-46af-8f77-dc482ab92bc9', total_war_reachability_boundary__dropping_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c6fd352-0204-46af-8f77-dc482ab92bc9', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('1c6fd352-0204-46af-8f77-dc482ab92bc9', foundational, mad_produces_physical_impossibility_not_managed_equilibrium).
narrative_ontology:cs_axiom_status(mad_produces_physical_impossibility_not_managed_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('1c6fd352-0204-46af-8f77-dc482ab92bc9', mad_produces_physical_impossibility_not_managed_equilibrium, empirically_contingent).
narrative_ontology:cs_axiom('1c6fd352-0204-46af-8f77-dc482ab92bc9', secondary, current_arsenal_physics_forecloses_damage_limitation_permanently).
narrative_ontology:cs_axiom_status(current_arsenal_physics_forecloses_damage_limitation_permanently, holdable).
narrative_ontology:cs_axiom_grounding('1c6fd352-0204-46af-8f77-dc482ab92bc9', current_arsenal_physics_forecloses_damage_limitation_permanently, empirically_contingent).
narrative_ontology:cs_reference_frame('1c6fd352-0204-46af-8f77-dc482ab92bc9', pre_nuclear_total_war_feasibility).
narrative_ontology:cs_drift_state('1c6fd352-0204-46af-8f77-dc482ab92bc9', post_second_strike_survivability_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('1c6fd352-0204-46af-8f77-dc482ab92bc9', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, global_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, non_nuclear_states).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__contraction_reading, mutually_assured_destruction_makes_total_war_unwinnable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears species-level extinction-tail risk simply by existing within the geographic and atmospheric reach of thermonuclear exchange and its climatic aftermath, with zero say over arsenal posture, doctrine, or alert status. There is no exit from the risk — it is not possible to arbitrage or emigrate away from a planetary climatic effect. No entity collects what this group pays; the risk is borne, not extracted.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, global_population, payer,
    powerless, civilizational, trapped, universal).

% Set doctrine, alert levels, arsenal composition, and declaratory policy — the visible, managed surface of the deterrence order. Under this reading, however, this agenda-setting does not translate into the ability to make total war winnable; these states are as physically foreclosed from a winnable total-war outcome as any other party, and their doctrinal choices manage the arsenal rather than reopening the feasible set.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, constrained, universal).

% Bear the same tail risk as the general population, with no agenda-setting power over the arsenals of nuclear-weapon states and no ability to independently alter the physical closure. Some participate in extended-deterrence arrangements or nonproliferation regimes, but these are downstream of the boundary, not levers on it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_nuclear_states, payer,
    moderate, generational, trapped, global).

% Study deterrence stability, crisis dynamics, and arms-control regimes, producing the competing readings (mountain vs. rope vs. piton) that this kernel decomposition formalizes. They hold no material stake in the outcome and no power to alter the physical facts, only to characterize them.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

narrative_ontology:fixing_cost_class(total_war_reachability_boundary__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None, under this reading. A mountain has no coordination problem it solves for any party — the closure of winnable total war is not a solution any actor chose or maintains, it is a physical/strategic fact that happens to also prevent a coordination failure (total war) without any party having engineered that prevention as a collective-action solution.
% TRANSFER_FUNCTION: No systematic transfer occurs between named parties. What is 'moved' is risk, not resources: the mere existence of arsenals sufficient to hold the boundary closed distributes a shared, non-differential extinction-tail exposure across the entire global population, without any corresponding flow of benefit to a beneficiary.
% ABSENT_VOICES: Future generations, who bear the tail risk most acutely (civilizational time horizon) and have no voice in current arsenal or doctrinal decisions, are structurally absent from the deliberation. They are represented here only through the global_population stakeholder's civilizational time horizon, not as a distinct seat, since they cannot be interviewed.
% DISAPPEARANCE_RATIONALE: Under this reading specifically, if the boundary 'disappeared' (i.e., if total war became winnable again through some technological reversal), the rearrangement would be severe: strategic planning, alliance structures, and crisis-management doctrine would all restructure around a reopened feasible set. But whether the boundary itself is the kind of thing that COULD disappear is exactly the disputed question among the three kernel readings — this reading holds it is a physical fact that cannot simply vanish absent a change in the underlying physics, while the sibling readings hold it could erode (dropping_reading) or already be eroding via technology (contingent_reachability_reading). The verdict is authored 'contested' to reflect that the disappearance question is itself the site of the kernel disagreement, not resolved within this file.
% FOUNDING_PROBLEM: The problem this boundary is 'built to solve' is unusual for a mountain: no one built it. It emerged as a side effect of thermonuclear weapons physics and second-strike-survivable delivery systems, closing off outcomes that great-power war planners had previously assumed feasible (limited nuclear exchange survived by a 'winning' side). There was no founding intention; the founding problem framing is retrofitted by observers trying to characterize a structural fact.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside any benefiting party because there is no benefiting party under this reading: nuclear-weapon-state military planners, arms-control scholars, and independent physicists studying nuclear-winter and fallout effects converge (from otherwise adversarial institutional positions) on the assessment that no current strategy converts a total nuclear exchange into a winning outcome for the initiator. The corroboration is cross-adversarial rather than external-to-all-parties in the usual sense, since the relevant 'benefiting party' set is empty.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, contested).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.03, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03) because under this reading no actor collects rent, advantage, or differential benefit from the boundary's existence — the closure is symmetric and inescapable for every party, including those who built the arsenals that produced it. Suppression is authored low (0.05) because the boundary is not maintained by coercing any actor into compliance; it is not a rule anyone enforces against a resistant party, it is a physical/strategic fact about payoff structures. Accessibility collapse is authored very high (0.92): once the thermonuclear and second-strike facts are understood, the alternative of a winnable total war essentially disappears from the strategic imagination for any rational actor — there is no discovered path back to a feasible winnable-war strategy under current technology. Resistance is authored low (0.10): unlike a constructed constraint that must be defended against challengers, no party contests the physical facts underlying the boundary, though parties do contest which KERNEL READING best characterizes the boundary's persistence mechanism (physical law vs. maintained equilibrium vs. capability gap) — that contest is the omega material, not resistance to the boundary itself. Theater ratio is authored low but nonzero (0.08, rising slightly) to capture the genuine but secondary performative element of deterrence signaling (parades, doctrine publications, posture reviews) that exists alongside the substantive physical closure — this is far lower than would be authored for the dropping_reading, where the performative maintenance function would be central rather than secondary.
 *
 * DIRECTIONALITY LOGIC:
 *   Because this reading declares no beneficiaries, there is no beneficiary-target asymmetry to compute — every stakeholder's directionality clusters near the same region of the space, reflecting universal, non-differential exposure. Nuclear-weapon states are agenda-setters over doctrine and posture (they decide alert levels, arsenal composition, declaratory policy) but this agenda-setting power does not translate into extraction from the boundary itself — they cannot make total war winnable by choosing differently, only manage the arsenal that instantiates the closure. Non-nuclear states and the general population bear the same tail risk with zero agenda-setting power, which is why they are named as payer/victim despite the absence of any capturing beneficiary — this is the structurally unusual case of a victim set with no corresponding beneficiary, appropriate to a genuine mountain rather than a snare or tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy analysis does not apply in the ordinary sense here because there is no mandate to outlive its function — a mountain has no founding mandate, only a physical fact. The founding_problem framing is used descriptively (what problem the nuclear order was built to manage) rather than to assess whether an institutional mandate has become hollow. The interesting mandatrophy-adjacent question is routed to the kernel_reading_disagreement_location omega: if the dropping_reading is correct that deterrence is an actively maintained rope, then something IS being maintained past or within its function, and mandatrophy analysis would apply there, not in this mountain reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_naturalness_vs_technological_contingency,
    'Is the closure of winnable-total-war a genuine physical mountain (thermonuclear yield, delivery redundancy, and radiative/climatic effects making victory structurally impossible for any actor) or is it a technologically contingent state that persists only because no actor has yet achieved the missile-defense, decapitation-strike, or hardening capability that would reopen the feasible set?',
    'Track whether any state achieves a credible first-strike/damage-limitation capability (near-total counterforce plus effective missile defense) that materially restores a winnable-outcome scenario; absence of such a capability across multiple decades despite sustained investment strengthens the mountain reading.',
    'If a state achieves such a capability, this reading is falsified retroactively and the constraint should be reclassified toward the contingent_reachability_reading''s piton framing — the boundary would reveal itself as an atrophied capability gap rather than a physical limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_naturalness_vs_technological_contingency, empirical, 'Whether the contracted strategic space is a physical mountain or a contingent technological plateau.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the three readings of the total_war_reachability_boundary kernel diverge, and does the contraction reading''s mountain classification survive contact with the dropping_reading''s claim that deterrence is an actively maintained coordination equilibrium rather than a physical closure?',
    'This is a conceptual/framing question, not an empirical one within this story. The disagreement is located at whether MAD''s stability is (a) a structural fact about physics and payoff matrices that persists independent of any party''s choice to maintain it (this reading), or (b) an equilibrium that requires continuous strategic signaling, arsenal maintenance, and crisis management to hold (dropping_reading), or (c) a capability gap that specific technologies (missile defense, counterforce precision) could close (contingent_reachability_reading).',
    'If the dropping_reading''s framing is correct, this constraint''s claimed_type should be rope or tangled_rope, not mountain, and beneficiaries (nuclear-armed states preserving strategic advantage through the deterrence equilibrium) would need to be declared. If the contingent_reachability_reading is correct, this constraint is better modeled as a piton with theater_ratio rising over time as capability erodes or is reconstituted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the structural disagreement among the three kernel readings and its classification consequences.').

omega_variable(
    universal_victim_beneficiary_asymmetry,
    'A mountain typically carries no beneficiaries — does declaring ''global_population'' as a universal victim set without any beneficiary group correctly represent a constraint from which literally no actor extracts advantage, or does it obscure narrower beneficiaries (e.g., nuclear-weapon states'' diplomatic leverage, arms industry contracts) who benefit from maintaining the appearance of contraction even if they could not survive its failure?',
    'Examine whether any actor''s welfare improves under the counterfactual removal of the boundary (i.e., would any actor be better off if total war became winnable again); if no such actor exists even hypothetically, the no-beneficiary structure holds.',
    'If narrow beneficiaries exist (e.g., states whose relative power position depends on others believing total war is foreclosed), this reading would need to add beneficiaries and likely reclassify toward tangled_rope, converging with the dropping_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_victim_beneficiary_asymmetry, empirical, 'Whether the universal-victim, no-beneficiary structure is complete or masks narrower strategic beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tota_tr_t13, total_war_reachability_boundary__contraction_reading, theater_ratio, 13, 0.06).
narrative_ontology:measurement(tota_tr_t26, total_war_reachability_boundary__contraction_reading, theater_ratio, 26, 0.07).
narrative_ontology:measurement(tota_tr_t39, total_war_reachability_boundary__contraction_reading, theater_ratio, 39, 0.07).
narrative_ontology:measurement(tota_tr_t52, total_war_reachability_boundary__contraction_reading, theater_ratio, 52, 0.08).
narrative_ontology:measurement(tota_tr_t65, total_war_reachability_boundary__contraction_reading, theater_ratio, 65, 0.08).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(tota_be_t13, total_war_reachability_boundary__contraction_reading, base_extractiveness, 13, 0.02).
narrative_ontology:measurement(tota_be_t26, total_war_reachability_boundary__contraction_reading, base_extractiveness, 26, 0.03).
narrative_ontology:measurement(tota_be_t39, total_war_reachability_boundary__contraction_reading, base_extractiveness, 39, 0.03).
narrative_ontology:measurement(tota_be_t52, total_war_reachability_boundary__contraction_reading, base_extractiveness, 52, 0.03).
narrative_ontology:measurement(tota_be_t65, total_war_reachability_boundary__contraction_reading, base_extractiveness, 65, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language 'total war reachability boundary' kernel per the ε-invariance principle. contraction_reading (this file) authors near-zero extraction and mountain classification, treating the boundary as physical closure. dropping_reading authors a rope/tangled_rope-consistent profile with active-maintenance beneficiaries (nuclear-armed states preserving strategic stability through continuous signaling). contingent_reachability_reading authors a piton-consistent profile with rising theater_ratio as capability gaps are noted to be closing. The three ε values are deliberately different because they describe structurally distinct claims about the SAME kernel, not the same claim measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
