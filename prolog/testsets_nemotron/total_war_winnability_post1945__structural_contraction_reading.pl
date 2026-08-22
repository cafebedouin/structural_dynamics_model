% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__structural_contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: total_war_winnability_post1945__structural_contraction_reading
 *   human_readable: Nuclear-Induced Structural Contraction of Total War Reachability
 *   domain: international_relations/strategic_studies/commitment_system
 *
 * SUMMARY:
 *   This constraint story captures the structural_contraction_reading of the
 *   total_war_winnability_post1945 kernel. The reading asserts that nuclear
 *   weapons physically and structurally removed total war between great
 *   powers from the reachable space of state action — not through normative
 *   prohibition, cultural taboo, or strategic choice, but through the
 *   physical logic of mutual assured destruction. A total war (mobilization
 *   of entire societies for existential conflict with the aim of
 *   unconditional surrender of the adversary) became structurally impossible
 *   because any such war between nuclear-armed powers would terminate the
 *   combatants' societies before achieving its political aims. The constraint
 *   is Mountain-class: it is a physical/logical limit on the reachable space
 *   of strategic action, akin to the speed of light limiting causal reach. No
 *   party extracts from this constraint; no party is coordinated by it in the
 *   active sense; it simply defines the boundary of the possible. The victim
 *   set is hypothetical — populations who would suffer in a counterfactual
 *   nuclear exchange that the constraint makes unreachable.
 *
 * KEY AGENTS:
 *   - great_power_governments: Primary subjects of the constraint (institutional/analytical) — their strategic reach is bounded by nuclear physics
 *   - nuclear_armed_states: Agenda-setters of the nuclear order (institutional) — they maintain the arsenals that instantiate the constraint, but do not control its structural logic
 *   - populations_in_counterfactual_exchange: Hypothetical victims (powerless/identity_locked) — exist only in the counterfactual branch the constraint forecloses
 *   - strategic_analysts: Observers (analytical) — map the boundary of the reachable space
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__structural_contraction_reading, 0.02).
domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, 0.01).
domain_priors:theater_ratio(total_war_winnability_post1945__structural_contraction_reading, 0.03).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 0.03).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__structural_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_winnability_post1945__structural_contraction_reading, "Nuclear-Induced Structural Contraction of Total War Reachability").
narrative_ontology:topic_domain(total_war_winnability_post1945__structural_contraction_reading, "international_relations/strategic_studies/commitment_system").

domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__structural_contraction_reading, '517d0c77-99b1-4c18-9495-129d8497e45c').
narrative_ontology:cs_kernel_codification('517d0c77-99b1-4c18-9495-129d8497e45c', implicit).
narrative_ontology:cs_authority_grounding('517d0c77-99b1-4c18-9495-129d8497e45c', practice).
narrative_ontology:cs_reading_relation('517d0c77-99b1-4c18-9495-129d8497e45c', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('517d0c77-99b1-4c18-9495-129d8497e45c', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('517d0c77-99b1-4c18-9495-129d8497e45c', foundational, nuclear_physics_contracts_strategic_reachable_space).
narrative_ontology:cs_axiom_status(nuclear_physics_contracts_strategic_reachable_space, holdable).
narrative_ontology:cs_axiom_grounding('517d0c77-99b1-4c18-9495-129d8497e45c', nuclear_physics_contracts_strategic_reachable_space, empirically_contingent).
narrative_ontology:cs_axiom('517d0c77-99b1-4c18-9495-129d8497e45c', foundational, total_war_requires_winnability_to_be_rational).
narrative_ontology:cs_axiom_status(total_war_requires_winnability_to_be_rational, holdable).
narrative_ontology:cs_axiom_grounding('517d0c77-99b1-4c18-9495-129d8497e45c', total_war_requires_winnability_to_be_rational, deontological).
narrative_ontology:cs_reference_frame('517d0c77-99b1-4c18-9495-129d8497e45c', pre_nuclear_great_power_war_system).
narrative_ontology:cs_drift_state('517d0c77-99b1-4c18-9495-129d8497e45c', contemporary_nuclear_order, gap(stable, minor, true)).
narrative_ontology:cs_created_at('517d0c77-99b1-4c18-9495-129d8497e45c', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, nuclear_deterrence_stability).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, mutual_assured_destruction_logic).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__structural_contraction_reading, war_termination_impossibility_post_nuclear).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_winnability_post1945__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__structural_contraction_reading, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_winnability_post1945__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_winnability_post1945__structural_contraction_reading),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_winnability_post1945__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_winnability_post1945__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because no party collects rents from the impossibility of total war — the constraint is not a transfer mechanism. Suppression is near-zero (0.01) because no enforcement machinery prevents total war; the physics of nuclear exchange does. Theater ratio is low (0.03) — arms control theater and deterrence signaling exist but are epiphenomena of the underlying structural limit, not the constraint itself. Accessibility collapse is near-total (0.95): once the nuclear logic is understood, no alternative pathway to total war between nuclear powers exists within the reachable space. Resistance is minimal (0.03): no actor resists the impossibility of total war; they adapt strategy within the contracted space (limited war, proxy war, gray zone). The measurement series shows the early period (1945-1960) had slightly higher extractiveness/theater/suppression as the nuclear order was being built and its logic was not yet settled; by 1975 the Mountain character is fully established and metrics stabilize.
 *
 * PERSPECTIVAL GAP:
 *   All seats compute as Mountain. Great power governments experience the constraint as a hard boundary on their action space. Nuclear-armed states experience it as a structural fact they administer but cannot revise unilaterally. Strategic analysts experience it as a theoretical limit. Hypothetical counterfactual populations are not a seat that experiences the constraint — they exist only in the branch the constraint eliminates. The engine will compute Mountain from every seat because the structural data (emerges_naturally, no beneficiaries, no victims, near-zero extraction/suppression) admits no alternative.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries declared — the constraint does not transfer value to any agent. No victims declared — the populations harmed in the counterfactual are not structural victims of the constraint (the constraint prevents their harm). The vindicated propositions are the doctrines (MAD, nuclear deterrence stability) that the constraint's operation confirms. Directionality is symmetric (d ≈ 0.5) for all real agents: all are equally bounded by the physics. The hypothetical populations are not agents in the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has no mandate that could atrophy — it is not an institutional arrangement with a founding purpose. It is a structural feature of the strategic physics instantiated by nuclear arsenals. Mandatrophy is inapplicable. The founding problem (if any) would be 'how to avoid great power total war' — but this constraint was not built to solve it; the physics of nuclear weapons solved it as a side effect of their existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the post-1945 contraction of total war winnability a structural-physical Mountain or a normative/cultural shift?',
    'Counterfactual stability analysis: if all nuclear arsenals were dismantled tomorrow, would total war between great powers return to the reachable space within a strategically relevant timeframe? If yes, the constraint is contingent on the physical arsenal (still a Mountain while the arsenal exists); if no, the constraint has a normative/cultural component that persists independent of physics.',
    'If normative/cultural components are substantial, the structural_contraction_reading overstates the Mountain character and the constraint family decomposition (three readings) is analytically necessary. If purely physical, this reading captures the dominant structure and sibling readings are secondary overlays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the Mountain character is purely physical or has irreducible normative/cultural components').

omega_variable(
    hypothetical_victim_ontology,
    'Do populations in counterfactual nuclear exchange constitute genuine victims of this constraint, or are they epistemic constructs of the counterfactual itself?',
    'Apply the victim definition test: does the constraint actively extract from or suppress these populations, or are they harmed only in the counterfactual branch the constraint prevents? If the latter, victim status is analytical, not structural — the constraint prevents their victimization rather than producing it.',
    'If populations are structural victims, the constraint has an extraction profile (protecting them from counterfactual harm is a transfer from the warfighting capability of nuclear powers). If they are epistemic constructs, the Mountain has zero extraction and zero victims — a pure natural-law-type constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hypothetical_victim_ontology, conceptual, 'Ontological status of counterfactual populations in Mountain constraints').

omega_variable(
    arsenal_degradation_contingency,
    'Is the Mountain status of this constraint contingent on the continued existence and credibility of nuclear arsenals, or does it persist even under degradation scenarios?',
    'Scenario analysis of partial disarmament, credibility erosion, or technological countermeasures (missile defense, hypersonic conventional strike). Determine the threshold at which total war re-enters the reachable space.',
    'If the Mountain degrades gracefully (partial arsenal = partial contraction), the constraint has a gradient structure not captured by binary Mountain classification. If it collapses discontinuously at a threshold, the Mountain character holds until the threshold is crossed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(arsenal_degradation_contingency, empirical, 'Continuity of the structural contraction under arsenal degradation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__structural_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1975, 0.04).
narrative_ontology:measurement(tota_tr_t1990, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2005, 0.03).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__structural_contraction_reading, theater_ratio, 2025, 0.03).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1960, 0.03).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1975, 0.02).
narrative_ontology:measurement(tota_be_t1990, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 1990, 0.02).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2005, 0.02).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__structural_contraction_reading, base_extractiveness, 2025, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1975, 0.01).
narrative_ontology:measurement(tota_su_t1990, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 1990, 0.01).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2005, 0.01).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__structural_contraction_reading, suppression_requirement, 2025, 0.01).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, total_war_winnability_post1945__strategic_culture_drift).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, limited_war_substitution_post1945).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, proxy_war_proliferation_post1945).
narrative_ontology:affects_constraint(total_war_winnability_post1945__structural_contraction_reading, gray_zone_competition_emergence).

% DUAL FORMULATION NOTE:
% Kernel family: total_war_winnability_post1945. This reading (structural_contraction) claims Mountain-class physical impossibility. normative_reading_drop claims normative prohibition (Rope/Scaffold). strategic_culture_drift claims ideational shift (Piton/Tangled Rope). The three readings have different ε, different stakeholder structures, and different classifications. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
