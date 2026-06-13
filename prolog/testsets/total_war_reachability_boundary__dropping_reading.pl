% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Under Deterrence Coordination (Dropping Reading)
 *   domain: political/military/strategic
 *
 * SUMMARY:
 *   Total war — simultaneous, unlimited military and economic conflict aimed
 *   at destroying the adversary's capacity to wage war — was once a coherent
 *   strategic goal (WW1, WW2). Nuclear weapons changed the calculus: total
 *   war now carries the risk of mutual annihilation. This constraint models
 *   deterrence as a coordination mechanism that keeps total war off the
 *   feasible set not because it is physically impossible, but because the
 *   strategic payoff is negative for all participants who follow the
 *   commitment to retaliate. The probability of total war has dropped
 *   (alliance disciplines, arms control treaties, established escalation
 *   norms all make conventional war less likely to escalate to the nuclear
 *   threshold). Yet reachability persists — the option remains available, and
 *   deterrence coordination must hold to prevent it. This is the
 *   'dropping_reading' of the kernel: reachability is declining but not
 *   foreclosed. The constraint is not a natural law (mountain) but a
 *   coordination game (tangled_rope) with asymmetric costs: nuclear states
 *   and security guarantors benefit from the equilibrium; civilian
 *   populations under threat bear the cost without choice or benefit.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: Set deterrence posture, control retaliation credibility, agenda-setting power over escalation boundaries — powerful, institutionalized, trapped by their own arsenals
 *   - alliance_security_guarantors: Extend deterrent coverage to allied non-nuclear states; collect political deference in exchange for retaliation guarantees — powerful, constrained exit
 *   - civilian_populations_under_threat: Hostages to the deterrence mechanism; their vulnerability makes the threat credible; powerless, trapped, deriving no benefit
 *   - non_nuclear_states_dependent_on_deterrence: Bear military deference and subordination; benefit passively from prevented war; constrained exit, moderate power
 *   - potential_revisionist_powers: Locked out of the coordination conversation by identity (military ambition incompatible with deterrence); excluded voice that would challenge reachability assumptions
 *   - strategic_analysts: Analytical seat observing the equilibrium's stability and failure modes; neither enforcing nor victimized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.68).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.71).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Under Deterrence Coordination (Dropping Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "political/military/strategic").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, 'd814af92-1916-4991-86ec-7a77754bbf16').
narrative_ontology:cs_kernel_codification('d814af92-1916-4991-86ec-7a77754bbf16', distributed).
narrative_ontology:cs_authority_grounding('d814af92-1916-4991-86ec-7a77754bbf16', extraction).
narrative_ontology:cs_interpretation_layer_present('d814af92-1916-4991-86ec-7a77754bbf16').
narrative_ontology:cs_reading_relation('d814af92-1916-4991-86ec-7a77754bbf16', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('d814af92-1916-4991-86ec-7a77754bbf16', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('d814af92-1916-4991-86ec-7a77754bbf16', foundational, reachability_persistence_through_coordination).
narrative_ontology:cs_axiom_status(reachability_persistence_through_coordination, holdable).
narrative_ontology:cs_axiom_grounding('d814af92-1916-4991-86ec-7a77754bbf16', reachability_persistence_through_coordination, empirically_contingent).
narrative_ontology:cs_axiom('d814af92-1916-4991-86ec-7a77754bbf16', foundational, deterrence_stability_as_rational_equilibrium).
narrative_ontology:cs_axiom_status(deterrence_stability_as_rational_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('d814af92-1916-4991-86ec-7a77754bbf16', deterrence_stability_as_rational_equilibrium, deontological).
narrative_ontology:cs_reference_frame('d814af92-1916-4991-86ec-7a77754bbf16', mutual_assured_destruction_rational_commitment).
narrative_ontology:cs_drift_state('d814af92-1916-4991-86ec-7a77754bbf16', post_cold_war_deterrence_erosion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d814af92-1916-4991-86ec-7a77754bbf16', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, alliance_security_guarantors).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, civilian_populations_under_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states_dependent_on_deterrence).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the asymmetry at the heart of deterrence: nuclear states extract the geopolitical benefit of deterrence without bearing proportional risk; civilian populations bear all the existential risk without choice or compensation. The metric is not at the snare level (0.85+) because deterrence genuinely solves a coordination problem — without it, conflicts would escalate more readily. Suppression (0.71) is high because maintaining the deterrence equilibrium requires active enforcement: arms race management, alliance discipline, restrictions on information about vulnerability, prevention of alternative security arrangements, and the constant signaling of commitment to retaliation (if commitment wanes, the threat becomes non-credible and extraction collapses). Theater ratio (0.42) is moderate-low: much of the enforcement activity is genuinely functional (maintaining arsenals, running drills, sustaining alliance relationships), but a growing share is theatrical — nuclear powers stage commitment even as the underlying belief in mutual rationality and the salience of deterrence has diminished. The coercion grid shows suppression intensifying over the 80-year interval, particularly at the structural and organizational levels, while individual-level resistance remains stable (individuals do not mount organized resistance to deterrence; their powerlessness is structural). The time series show extractiveness stabilizing after 1980, suggesting the deterrence equilibrium reached a steady state once proliferation fears were managed and alliance structures solidified. Accessibility collapse is highest at the individual level (0.63 by 2024) — civilians understand they cannot exit the deterrent threat structure — while organizational level accessibility remains lower (0.64), reflecting that states have more maneuvering room.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a nuclear-armed state (institutional power, trapped by its own arsenal), deterrence appears as the unavoidable logic of security in a multipolar world — a rope of coordination. From the seat of a civilian population in a target nation (powerless, trapped by geography), deterrence appears as a perpetual sword of Damocles — a snare where the state has taken hostages of its own citizens to make its retaliation threat credible. From the seat of a non-nuclear state dependent on deterrence (moderate power, constrained), the arrangement appears as enforced deference — you gain security through alignment but lose autonomy. The engine computes these divergences from the structural data: different power atoms, exit options, and beneficiary/victim positions yield different classification per seat. The agenda-setter and beneficiary seats should compute toward rope or tangled_rope; the victim seats should compute toward snare. The claim of tangled_rope captures the hybrid: genuine coordination function (deterrence prevents escalation) AND asymmetric extraction (victimhood is concentrated on the powerless).
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear armed states: d near 0.15 (full beneficiary) — they set the rules, control the threat, collect the security benefit. Alliance security guarantors: d near 0.25 — they benefit from deterrence but at higher cost (must maintain credible commitment, risk retaliation if deterrence fails). Civilian populations: d near 0.88 (near-target) — they bear all existential risk, have no exit, no negotiating power, no benefit. Non-nuclear dependent states: d near 0.65 (toward target) — they pay military deference and accept vulnerability but do receive security benefit, so not full target. Revisionist powers (excluded): d undefined by the structure (they are not in the game; the constraint's beneficiary/victim structure does not have a seat for them). The overarching directionality is asymmetric extraction (high d for victims, low d for beneficiaries) coupled with genuine coordination (the low d beneficiaries would not gain if the coordination collapsed). This is the signature of tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   Deterrence as a mandatrophy candidate: The founding mandate was to prevent nuclear war by making it too costly to be rational. That mandate is live (nuclear war remains the primary existential risk the constraint addresses). However, the enforcement mechanism is undergoing a theater-ratio increase (suppression_requirement holds steady at 0.71 even as the belief in mutual rationality, the salience of mutual vulnerability, and the disciplining effect of alliance commitments all show signs of erosion). The constraint has not yet entered the mandatrophy zone (where theater_ratio > 0.5 and extraction would continue without the original function), but the coercion grid shows structural suppression intensifying relative to resistance, suggesting the coordination is being held together more by enforcement and less by mutual interest as the interval progresses. The disappearance_verdict is contested precisely because mandatrophy is a live question: if deterrence is becoming theater, would the world rearrange if it vanished? The founding_problem_status is contested because contemporary actors disagree on whether deterrence prevents war (proving the mandate still lives) or merely delays it (suggesting the mandate is already dead and the constraint is now extractive inertia). The rising theater_ratio and the intensifying structural suppression flag the constraint as a candidate for future mandatrophy if the trend continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_stability_permanence,
    'Is the deterrence equilibrium a stable attractor of strategic behavior, or a precarious coordination on a knife-edge that could shift with technological change or political will?',
    'Longitudinal monitoring of crisis dynamics, near-miss incidents, and the trajectory of nuclear doctrine revision; observation of whether proliferation of nuclear capability strengthens or weakens the deterrence equilibrium.',
    'If the equilibrium is stable, deterrence is a genuine rope (coordination function persists indefinitely). If it is precarious, the constraint is closer to a tangled_rope with high defection risk — suggesting that total war reachability is not a natural fact but a contingent commitment that could unravel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_permanence, empirical, 'Whether deterrence coordination is a stable strategic equilibrium or a contingent commitment vulnerable to shifts in technology or political commitment.').

omega_variable(
    reachability_vs_necessity,
    'Does the measured reachability of total war — its presence in the feasible strategy set — derive from a technological fact (weapons exist) or from strategic choice (powers retain the option)?',
    'Counterfactual analysis of treaty regimes that eliminated reachability (e.g., comprehensive disarmament) vs. regimes that keep reachability while managing escalation risk (current deterrence). Assessment of whether reachability is a property of physics or of maintained capacity.',
    'If reachability is maintained by choice, deterrence is structured as a coordination game where states could collectively shift the boundary — making the constraint an engineered tangled_rope, not a natural outcome. If reachability is forced by physics, deterrence is a response to an external constraint, making it more like a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_necessity, conceptual, 'Whether total war reachability is a technological fact or a strategic choice to maintain the option.').

omega_variable(
    kernel_contest_reading_ambiguity,
    'This constraint instantiates the ''dropping_reading'' of the total_war_reachability_boundary kernel. The sibling readings — contraction_reading and contingent_reachability_reading — offer different framings of the same underlying strategic situation. What evidence would favor one reading over another?',
    'Observation of the rate of change of reachability over time (coercion_grid shows declining resistance to deterrence at structural and organizational levels, suggesting contraction pressures). Monitoring of technological developments that could expand reachability (hypersonic weapons, advanced AI, space-based systems). Assessment of political commitment to maintained arsenals vs. disarmament trends.',
    'The dropping_reading assumes reachability persists but probability declines — a tangled_rope where deterrence coordination holds but the underlying strategic option remains. If the contraction_reading is correct, reachability is actually eliminated and the constraint becomes a mountain (total war is infeasible by nature). If contingent_reachability is correct, the constraint is a piton (reachability atrophies through non-use, not commitment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_reading_ambiguity, conceptual, 'Ambiguity among sibling readings of the total_war_reachability_boundary kernel; evidence needed to disambiguate which reading best captures the strategic reality.').

omega_variable(
    civilian_victimhood_mechanism,
    'Is the perpetual vulnerability of civilian populations under deterrence a necessary feature of the deterrence mechanism (credibility requires hostage populations) or an incidental cost that could be decoupled through alternative security arrangements?',
    'Exploration of defenses that decouple civilian vulnerability from deterrent credibility (e.g., counter-force-only postures, automated retaliation systems that spare civilians). Assessment of whether deterrence would hold if civilian populations were shielded from retaliation threat.',
    'If victimhood is necessary, deterrence intrinsically victimizes the powerless and the constraint is a snare dressed as a rope. If victimhood is incidental, the constraint''s classification as tangled_rope holds — extraction can be reduced without eliminating coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_victimhood_mechanism, preference, 'Whether civilian vulnerability is a necessary component of credible deterrence or a contingent cost that could be reduced through alternative mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.35).
narrative_ontology:measurement(tota_tr_t1980, total_war_reachability_boundary__dropping_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(tota_tr_t2000, total_war_reachability_boundary__dropping_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(tota_tr_t2012, total_war_reachability_boundary__dropping_reading, theater_ratio, 2012, 0.41).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__dropping_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.55).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.62).
narrative_ontology:measurement(tota_be_t1980, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(tota_be_t2000, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(tota_be_t2012, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.68).
narrative_ontology:measurement(tota_su_t1980, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1980, 0.74).
narrative_ontology:measurement(tota_su_t2000, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement(tota_su_t2012, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary__contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_proliferation_escalation_risk).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, alliance_credibility_asymmetry).

% DUAL FORMULATION NOTE:
% The total_war_reachability_boundary kernel is contested across three readings. The dropping_reading treats reachability as a persistent feature of strategic space that deterrence coordination (tangled_rope) manages. The contraction_reading treats reachability as foreclosed by nuclear geometry (mountain). The contingent_reachability_reading treats reachability as contingent on technology maturity (piton with revival risk). These are not competing measurements of the same constraint — they are three structurally distinct constraints, each with different ε values. Shared beneficiaries (nuclear states, security guarantors) appear in all three, but the structural relationships differ: in dropping_reading, they maintain deterrence; in contraction_reading, they inherit a foreclosed world; in contingent_reachability, they manage a degrading constraint. The three constraints are linked via affects_constraints to preserve the kernel contest in the corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, powerless, 0.88).
constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
