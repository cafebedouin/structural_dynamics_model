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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   This constraint describes a reading of practice standardization where
 *   legitimacy is derived from the voluntary adoption of new practices by
 *   populations, driven by perceived utility or cultural evolution. It posits
 *   that genuine change emerges 'from below' through diffusion and
 *   adaptation, rather than being imposed 'from above'. Examples include the
 *   gradual adoption of new agricultural techniques, changes in fashion, or
 *   the spread of new administrative methods due to their perceived
 *   efficiency. Resistance is seen as temporary friction, and 'double life'
 *   (simultaneous adherence to old and new practices) is a transitional
 *   phase.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.2).
domain_priors:suppression_score(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.1).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__endogenous_displacement_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Standardization: Endogenous Displacement Reading").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "political_history/modernization_studies/institutional_change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'baeea72b-c0e9-4365-a3ea-f1a79ef60715').
narrative_ontology:cs_kernel_codification('baeea72b-c0e9-4365-a3ea-f1a79ef60715', implicit).
narrative_ontology:cs_authority_grounding('baeea72b-c0e9-4365-a3ea-f1a79ef60715', practice).
narrative_ontology:cs_interpretation_layer_present('baeea72b-c0e9-4365-a3ea-f1a79ef60715').
narrative_ontology:cs_reading_relation('baeea72b-c0e9-4365-a3ea-f1a79ef60715', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('baeea72b-c0e9-4365-a3ea-f1a79ef60715', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, coexists_with).
narrative_ontology:cs_axiom('baeea72b-c0e9-4365-a3ea-f1a79ef60715', foundational, utility_drives_legitimate_change).
narrative_ontology:cs_axiom_status(utility_drives_legitimate_change, holdable).
narrative_ontology:cs_axiom_grounding('baeea72b-c0e9-4365-a3ea-f1a79ef60715', utility_drives_legitimate_change, empirically_contingent).
narrative_ontology:cs_axiom('baeea72b-c0e9-4365-a3ea-f1a79ef60715', foundational, cultural_evolution_is_primary_legitimizer).
narrative_ontology:cs_axiom_status(cultural_evolution_is_primary_legitimizer, holdable).
narrative_ontology:cs_axiom_grounding('baeea72b-c0e9-4365-a3ea-f1a79ef60715', cultural_evolution_is_primary_legitimizer, empirically_contingent).
narrative_ontology:cs_reference_frame('baeea72b-c0e9-4365-a3ea-f1a79ef60715', organic_cultural_evolution).
narrative_ontology:cs_drift_state('baeea72b-c0e9-4365-a3ea-f1a79ef60715', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('baeea72b-c0e9-4365-a3ea-f1a79ef60715', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_evolution_theory).
narrative_ontology:constraint_vindicates(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffusion_of_innovation_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the groups that voluntarily adopt new practices (e.g., calendar systems, dress codes, administrative procedures) because they perceive a utility advantage or cultural fit. They benefit from the perceived efficiency or prestige of the new practice.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations, beneficiary,
    organized, generational, mobile, regional).

% Leaders or groups whose authority is tied to older practices. They bear the cost of losing influence as traditional practices are displaced by new, endogenously adopted ones. Their resistance is typically a temporary friction, not a sustained opposition.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites, payer,
    powerful, biographical, constrained, local).

% Academics and policymakers who interpret societal change through the lens of endogenous evolution and utility-driven adoption. This reading vindicates their theoretical frameworks, and they benefit from its explanatory power.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, beneficiary,
    analytical, civilizational, analytical, universal).

% While not directly enforcing this specific reading, state authorities observe and may leverage the perceived legitimacy of endogenously adopted practices to further their own policy goals. They are not the primary drivers of change in this reading.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social practices (e.g., timekeeping, social rituals, administrative norms) by establishing a shared understanding of how legitimate change occurs: through organic, utility-driven adoption rather than top-down decree.
% TRANSFER_FUNCTION: Transfers social capital and legitimacy from older, less 'useful' practices to newer, more 'efficient' or 'modern' ones, driven by the collective choices of the population. It also transfers explanatory power to theories of cultural evolution.
% ABSENT_VOICES: Those who believe that practice change is only legitimate when decreed by a central authority (exogenous_override_reading) or when practices are partitioned by domain (dual_practice_equilibrium_reading) are absent from this reading's internal logic. They would argue that endogenous change is too slow or insufficient for collective benefit.
% DISAPPEARANCE_RATIONALE: If this understanding of legitimacy vanished, the process of institutional change would be fundamentally altered. The default assumption for practice evolution would shift, potentially leading to more top-down imposition or persistent dual systems, rather than gradual, utility-driven displacement.
% FOUNDING_PROBLEM: The problem of how societies transition from traditional to modern practices without resorting to coercion, ensuring that new norms are genuinely accepted and integrated.
% FOUNDING_PROBLEM_CORROBORATION: Historians and anthropologists, from outside the direct beneficiaries, corroborate that societies have indeed undergone significant practice changes through endogenous processes, driven by perceived utility and cultural evolution, providing a non-coercive path to modernization.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__endogenous_displacement_reading, 'none', 1).

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
 *   The extractiveness (0.2) and suppression (0.1) are low because this reading emphasizes voluntary adoption and utility. Any 'extraction' is primarily the cost of transitioning from old to new practices, and 'suppression' is minimal, reflecting social pressure or the natural obsolescence of less useful practices, not coercion. The theater ratio is very low (0.05) as the constraint is genuinely about functional change, not performative maintenance. The accessibility collapse is moderate (0.7) because while old practices may eventually fade, alternatives are not forcibly removed but rather displaced by superior utility.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of adopting populations, this is a beneficial coordination mechanism. From the perspective of traditional elites, it represents a loss of status and influence, even if not overtly coercive. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopting populations are beneficiaries (d near 0.0) as they gain from the perceived utility of new practices. Traditional elites are payers (d near 1.0) as their authority tied to old practices erodes. Modernization theorists are beneficiaries (d near 0.0) as this reading validates their theories. State authorities are observers in this reading, not direct enforcers or targets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_influence,
    'To what extent is ''voluntary adoption'' truly endogenous, versus subtly influenced or incentivized by state or external actors?',
    'Detailed historical case studies analyzing the causal pathways of practice change, distinguishing between genuine grassroots adoption and state-led ''nudges'' or indirect pressures.',
    'If significant exogenous influence is found, the measured extractiveness and suppression might be higher than currently estimated, pushing the classification towards a Tangled Rope or Snare, as the ''voluntary'' aspect would be a cover for subtle coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_influence, empirical, 'Distinguishing genuinely endogenous practice change from externally influenced adoption.').

omega_variable(
    displacement_vs_coexistence,
    'Is the ''displacement'' of old practices by new ones truly complete, or do older practices persist in a ''double life'' equilibrium, challenging the idea of full endogenous displacement?',
    'Longitudinal ethnographic studies tracking the persistence of traditional practices in parallel with modern ones, especially in private or ritual domains, over multiple generations.',
    'If persistent ''double life'' is widespread, this reading''s claim of endogenous displacement is weakened, and the ''dual practice equilibrium'' reading gains explanatory power, potentially shifting the classification of specific practices towards a Rope or Tangled Rope that manages coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_vs_coexistence, empirical, 'Assessing the completeness of practice displacement versus persistent coexistence.').

omega_variable(
    legitimacy_framing_ambiguity,
    'Is the ''legitimacy'' of practice change primarily derived from its endogenous origin, or is the endogenous origin merely a convenient narrative for changes driven by other factors (e.g., economic utility, power shifts)?',
    'Analysis of historical discourse and justifications for practice change: do actors genuinely appeal to ''voluntary adoption'' as the primary legitimizing factor, or is it a post-hoc rationalization?',
    'If endogenous origin is found to be a secondary justification, the constraint''s ''claimed_type'' as a Rope (coordination) might be challenged, as its core legitimizing principle is undermined, potentially revealing a more extractive or coercive underlying dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_framing_ambiguity, conceptual, 'Conceptual ambiguity in the primary source of legitimacy for practice change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 1800, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(legi_tr_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1850, 0.05).
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(legi_be_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(legi_be_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1850, 0.18).
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1950, 0.19).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 2000, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1800, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(legi_su_t1850, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1850, 0.09).
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1950, 0.09).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimacy of practice standardization' kernel. This 'endogenous displacement' reading emphasizes voluntary adoption and cultural evolution as the source of legitimacy, contrasting with readings that prioritize state decree or domain partitioning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
