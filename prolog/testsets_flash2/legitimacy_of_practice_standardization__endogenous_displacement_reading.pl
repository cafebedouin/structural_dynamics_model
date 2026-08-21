% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__endogenous_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__endogenous_displacement_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Practice Standardization: Endogenous Displacement Reading
 *   domain: political_history/modernization_studies/institutional_change
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous displacement' reading of
 *   practice standardization, where legitimacy for change arises from
 *   voluntary adoption driven by perceived utility or cultural evolution.
 *   This reading posits that changes like calendar reform or dress code
 *   shifts would exhibit gradual adoption, regional variation, and
 *   elite-to-mass diffusion, with resistance manifesting as temporary
 *   friction rather than sustained conflict. The 'double life' of old and new
 *   practices is seen as a transitional phase. The constraint is claimed as a
 *   Rope because it facilitates coordination through voluntary means, with
 *   low extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Standardization: Endogenous Displacement Reading").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '30aa565e-5b88-4157-9c31-1e29e55dc0f6').
narrative_ontology:cs_kernel_codification('30aa565e-5b88-4157-9c31-1e29e55dc0f6', implicit).
narrative_ontology:cs_authority_grounding('30aa565e-5b88-4157-9c31-1e29e55dc0f6', practice).
narrative_ontology:cs_interpretation_layer_present('30aa565e-5b88-4157-9c31-1e29e55dc0f6').
narrative_ontology:cs_reading_relation('30aa565e-5b88-4157-9c31-1e29e55dc0f6', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('30aa565e-5b88-4157-9c31-1e29e55dc0f6', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('30aa565e-5b88-4157-9c31-1e29e55dc0f6', foundational, voluntary_adoption_is_legitimate).
narrative_ontology:cs_axiom_status(voluntary_adoption_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('30aa565e-5b88-4157-9c31-1e29e55dc0f6', voluntary_adoption_is_legitimate, conventional).
narrative_ontology:cs_axiom('30aa565e-5b88-4157-9c31-1e29e55dc0f6', foundational, utility_drives_cultural_evolution).
narrative_ontology:cs_axiom_status(utility_drives_cultural_evolution, holdable).
narrative_ontology:cs_axiom_grounding('30aa565e-5b88-4157-9c31-1e29e55dc0f6', utility_drives_cultural_evolution, empirically_contingent).
narrative_ontology:cs_reference_frame('30aa565e-5b88-4157-9c31-1e29e55dc0f6', gradual_cultural_evolution).
narrative_ontology:cs_drift_state('30aa565e-5b88-4157-9c31-1e29e55dc0f6', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('30aa565e-5b88-4157-9c31-1e29e55dc0f6', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily adopt new practices (e.g., calendar systems, dress codes) due to perceived utility, efficiency, or social advantage. They benefit from the new practice's advantages and face minimal friction in adoption.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations, beneficiary,
    organized, biographical, mobile, regional).

% May initially resist new practices due to cultural inertia or loss of status associated with old ways, but eventually adopt or fade as the new practice gains utility. They bear the cost of adapting or losing influence.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites, payer,
    moderate, generational, constrained, local).

% Their theories are vindicated by observing practice changes that follow endogenous displacement patterns. They benefit from the explanatory power of this reading.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, beneficiary,
    analytical, civilizational, analytical, universal).

% Observe and may encourage endogenous practice change, but do not directly enforce it. Their legitimacy is enhanced when changes appear to be organic rather than imposed.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social practices around new, more efficient, or culturally resonant norms, allowing for smoother social interaction and economic activity through shared conventions.
% TRANSFER_FUNCTION: Transfers social capital and utility from older, less efficient practices to newer, more beneficial ones, driven by individual and collective choice.
% ABSENT_VOICES: Those who would prefer to maintain traditional practices without external pressure are not actively suppressed, but their preferences are gradually displaced by the perceived utility of the new norms. Their 'voice' is expressed through temporary friction rather than organized resistance.
% DISAPPEARANCE_RATIONALE: If the principle that practice change is legitimate only through endogenous displacement vanished, it would fundamentally alter how societies understand and implement institutional change. State authorities might become more interventionist, and populations might resist even beneficial changes if they perceive them as imposed, leading to different patterns of adoption and conflict.
% FOUNDING_PROBLEM: How to achieve social and institutional change in a way that is perceived as legitimate and avoids widespread resistance or coercion.
% FOUNDING_PROBLEM_CORROBORATION: Historians and sociologists outside of state authorities or modernization theorists corroborate that societies consistently grapple with the legitimacy of change, and that voluntary adoption often leads to more stable outcomes than imposed change. Ethnographic studies of cultural evolution also support this perspective.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__endogenous_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the 'cost' of change is offset by perceived utility, making it a net benefit for adopters. Suppression is low (0.1) as the change is voluntary, not coerced. Theater ratio is minimal (0.05) as there's little need for performative enforcement when adoption is organic. Accessibility collapse is high (0.8) because once the utility of the new practice is understood, alternatives (old practices) naturally recede. Resistance is low (0.1) because the change is driven by internal factors rather than external imposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adopting populations, this is a clear Rope, facilitating beneficial change. From the perspective of traditional elites, it might feel like a constrained choice, but the underlying principle of endogenous displacement still frames it as legitimate evolution rather than imposition. The engine's classification should reflect this low-extraction, high-coordination dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopting populations are beneficiaries as they gain utility. Traditional elites may initially be payers due to the cost of adapting or losing status, but this is temporary. Modernization theorists benefit from the vindication of their models. State authorities are observers, benefiting from the perceived legitimacy of organic change without direct enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    distinguishing_endogenous_from_coerced_adoption,
    'How can ''voluntary adoption driven by perceived utility'' be empirically distinguished from ''adoption under subtle, diffuse, or internalized coercion''?',
    'Longitudinal ethnographic studies tracking post-adoption satisfaction, rates of reversion when external pressures are removed, and the presence of ''double life'' practices where old norms persist in private spheres despite public adoption of new ones.',
    'If a significant portion of ''voluntary'' adoption is found to be subtly coerced, the extractiveness and suppression metrics for this reading would need to be re-evaluated upwards, potentially shifting the classification towards a Tangled Rope or Snare, as the coordination story would be revealed as cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distinguishing_endogenous_from_coerced_adoption, empirical, 'Ambiguity in distinguishing genuine voluntary adoption from subtle coercion.').

omega_variable(
    threshold_of_utility_for_displacement,
    'At what point does the ''perceived utility'' of a new practice become sufficient to displace an entrenched traditional practice without any external push?',
    'Comparative historical analysis of multiple cases of practice change, quantifying the utility differential and the rate of adoption, controlling for external factors. Agent-based modeling of cultural diffusion under varying utility gradients.',
    'Understanding this threshold would refine predictions about the speed and inevitability of endogenous displacement. If the threshold is very high, it suggests that many ''endogenous'' changes might still require a subtle ''nudge'' that this reading currently overlooks, potentially increasing its implicit extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_of_utility_for_displacement, empirical, 'Quantifying the utility required for purely endogenous practice displacement.').

omega_variable(
    framing_of_legitimacy_source,
    'Is the ''legitimacy'' of practice change an intrinsic property of the change process (as this reading suggests), or is it a social construct applied by observers or beneficiaries?',
    'Conceptual analysis of ''legitimacy'' across different philosophical traditions and empirical studies of how different social groups articulate the justification for practice change. This is a conceptual omega, not empirically resolvable in a single stroke.',
    'If legitimacy is primarily a social construct, then the ''endogenous displacement'' reading itself becomes a framework that legitimizes certain types of change, potentially masking power dynamics. This would shift the analysis to the ''legitimizing function'' of the reading itself, rather than the legitimacy of the practice change it describes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_legitimacy_source, conceptual, 'Whether legitimacy is an intrinsic or constructed property of practice change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 1900, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1900, 0.03).
narrative_ontology:measurement(legi_tr_t1925, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1925, 0.04).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(legi_tr_t1975, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1975, 0.04).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(legi_be_t1925, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1925, 0.12).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(legi_be_t1975, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1975, 0.14).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1900, 0.08).
narrative_ontology:measurement(legi_su_t1925, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1925, 0.09).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(legi_su_t1975, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1975, 0.09).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
