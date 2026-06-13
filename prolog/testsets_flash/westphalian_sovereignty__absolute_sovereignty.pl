% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute State Sovereignty (Westphalian Reading)
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'absolute sovereignty' reading of the
 *   Westphalian principle, asserting that states possess unconditional
 *   authority over their domestic affairs and are immune to external
 *   interference. It is a foundational claim in traditional international
 *   law, providing a shield for states against intervention, but often at the
 *   cost of domestic populations under repressive regimes. The constraint is
 *   actively enforced by states that benefit from this interpretation,
 *   particularly those with authoritarian tendencies or those prioritizing
 *   state autonomy above all else.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.7).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute State Sovereignty (Westphalian Reading)").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '4cbab4de-8874-49a2-a483-fd25282cbc02').
narrative_ontology:cs_kernel_codification('4cbab4de-8874-49a2-a483-fd25282cbc02', formalized).
narrative_ontology:cs_authority_grounding('4cbab4de-8874-49a2-a483-fd25282cbc02', lineage).
narrative_ontology:cs_interpretation_layer_present('4cbab4de-8874-49a2-a483-fd25282cbc02').
narrative_ontology:cs_reading_relation('4cbab4de-8874-49a2-a483-fd25282cbc02', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('4cbab4de-8874-49a2-a483-fd25282cbc02', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('4cbab4de-8874-49a2-a483-fd25282cbc02', foundational, state_internal_affairs_are_exclusive).
narrative_ontology:cs_axiom_status(state_internal_affairs_are_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('4cbab4de-8874-49a2-a483-fd25282cbc02', state_internal_affairs_are_exclusive, deontological).
narrative_ontology:cs_axiom('4cbab4de-8874-49a2-a483-fd25282cbc02', foundational, non_interference_is_absolute).
narrative_ontology:cs_axiom_status(non_interference_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('4cbab4de-8874-49a2-a483-fd25282cbc02', non_interference_is_absolute, deontological).
narrative_ontology:cs_reference_frame('4cbab4de-8874-49a2-a483-fd25282cbc02', classical_westphalian_order).
narrative_ontology:cs_drift_state('4cbab4de-8874-49a2-a483-fd25282cbc02', post_cold_war_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4cbab4de-8874-49a2-a483-fd25282cbc02', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, states_seeking_autonomy).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear boundary of non-interference between states, preventing external powers from meddling in internal affairs and thereby promoting international stability and state autonomy.
% TRANSFER_FUNCTION: Transfers the right to absolute internal governance from the international community (or potential interveners) to the individual state, in exchange for non-interference from other states.
% ABSENT_VOICES: Domestic populations under repressive regimes are largely absent from the international discourse that upholds this absolute reading of sovereignty. They would argue for a right to external protection when their own state fails to protect them.
% DISAPPEARANCE_RATIONALE: If the principle of absolute state sovereignty vanished overnight, the international system would undergo a profound rearrangement. Intervention in domestic affairs would become far more common, potentially leading to increased conflict or, conversely, a more robust international human rights enforcement regime. State behavior would fundamentally shift.
% FOUNDING_PROBLEM: The founding problem was to end the perpetual religious wars and external interventions that plagued Europe, establishing a stable international order based on the recognition of sovereign states as the primary actors.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of absolute sovereignty (e.g., many non-Western states, some realist international relations scholars) argue the problem of external interference and instability is still live. Human rights organizations and liberal interventionists (e.g., some Western states, international legal scholars) argue the original problem is largely solved, and the principle now serves to shield human rights abuses, as evidenced by numerous UN reports and academic analyses.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is moderate-to-high, as it allows states to maintain control and potentially repress their populations without external accountability. Suppression (0.7) is high because it actively suppresses any legitimate grounds for external intervention, requiring states to actively defend this principle against evolving norms like R2P. Theater ratio is low (0.1) as the principle is genuinely invoked and defended, not merely performed. Accessibility collapse is moderate (0.4) as alternative interpretations and norms (like human rights law) exist but are actively resisted by proponents of absolute sovereignty. Resistance is high (0.6) from human rights organizations and liberal states.
 *
 * PERSPECTIVAL GAP:
 *   Authoritarian regimes experience this as a beneficial coordination mechanism, protecting their internal control. Domestic populations under these regimes experience it as a snare, trapping them without external recourse. Liberal states and human rights advocates view it as a barrier to justice and a legitimizer of repression.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and states prioritizing autonomy are primary beneficiaries (d near 0.0) as the constraint shields them from external accountability. Domestic populations under repression and human rights advocates are victims (d near 1.0) as it denies them external avenues for relief. Other states may fall in between, depending on their own domestic governance and foreign policy objectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, originally to prevent endless religious wars and establish state order, has arguably outlived its original function in a globalized world with evolving human rights norms. Its persistence in its 'absolute' form now serves to protect state power, often at the expense of human rights, rather than solely ensuring international stability. This suggests a potential drift towards a Snare for populations, even if it remains a Rope for states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine principle of international order, or a constructed interpretation of Westphalian sovereignty that benefits specific state actors?',
    'Analysis of historical state practice and legal scholarship on the evolution of sovereignty concepts, particularly post-WWII and post-Cold War.',
    'If a constructed interpretation, the constraint''s extractiveness and suppression are higher than if it were a universally accepted principle. This reading (absolute_sovereignty) is one interpretation of the ''westphalian_sovereignty'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of the Westphalian sovereignty kernel.').

omega_variable(
    impact_of_conditional_sovereignty,
    'How would the adoption of a ''conditional_sovereignty'' reading alter the structural position of authoritarian regimes and domestic populations?',
    'Empirical observation of state behavior and international legal enforcement in contexts where conditional sovereignty principles are invoked (e.g., R2P interventions).',
    'A conditional_sovereignty reading would shift the directionality for authoritarian regimes towards ''target'' and for domestic populations towards ''beneficiary'', potentially reclassifying this constraint as a snare for regimes and a rope for populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_conditional_sovereignty, empirical, 'Analyzes the impact of the ''conditional_sovereignty'' sibling reading.').

omega_variable(
    impact_of_graduated_sovereignty,
    'How would the adoption of a ''graduated_sovereignty'' reading alter the structural position of states with varying capacities and governance legitimacy?',
    'Analysis of international aid, capacity-building programs, and differentiated responsibilities in international law, particularly for fragile states.',
    'A graduated_sovereignty reading would introduce a spectrum of directionality for states based on their capacity and legitimacy, making the ''absolute_sovereignty'' claim less universally applicable and potentially exposing weaker states to more external influence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_graduated_sovereignty, empirical, 'Analyzes the impact of the ''graduated_sovereignty'' sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(west_tr_t10, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 10, 0.1).
narrative_ontology:measurement(west_tr_t20, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(west_be_t10, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(west_be_t20, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(west_su_t10, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(west_su_t20, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'westphalian_sovereignty' kernel. Its structural properties differ significantly from the 'conditional_sovereignty' and 'graduated_sovereignty' readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
