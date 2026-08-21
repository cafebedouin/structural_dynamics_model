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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: legitimacy_of_practice_standardization__endogenous_displacement_reading
 *   human_readable: Legitimacy of Practice Standardization: Endogenous Displacement Reading
 *   domain: Political History/Modernization Studies/Institutional Change
 *
 * SUMMARY:
 *   This constraint describes the 'endogenous displacement' reading of how
 *   practice change gains legitimacy. It posits that changes in social
 *   practices (e.g., adoption of new calendars, dress, or technologies) are
 *   legitimate when they emerge from voluntary adoption by populations,
 *   driven by perceived utility or cultural evolution. Resistance is viewed
 *   as temporary friction, and older practices are expected to be displaced
 *   rather than coexist indefinitely. This reading emphasizes bottom-up,
 *   utility-driven change over top-down imposition.
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
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__endogenous_displacement_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__endogenous_displacement_reading, rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Legitimacy of Practice Standardization: Endogenous Displacement Reading").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__endogenous_displacement_reading, "Political History/Modernization Studies/Institutional Change").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__endogenous_displacement_reading, '6ddf27d2-8f27-4c87-a611-7f38b129c01b').
narrative_ontology:cs_kernel_codification('6ddf27d2-8f27-4c87-a611-7f38b129c01b', implicit).
narrative_ontology:cs_authority_grounding('6ddf27d2-8f27-4c87-a611-7f38b129c01b', practice).
narrative_ontology:cs_reading_relation('6ddf27d2-8f27-4c87-a611-7f38b129c01b', legitimacy_of_practice_standardization__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('6ddf27d2-8f27-4c87-a611-7f38b129c01b', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, forecloses).
narrative_ontology:cs_axiom('6ddf27d2-8f27-4c87-a611-7f38b129c01b', foundational, practice_change_is_utility_driven).
narrative_ontology:cs_axiom_status(practice_change_is_utility_driven, holdable).
narrative_ontology:cs_axiom_grounding('6ddf27d2-8f27-4c87-a611-7f38b129c01b', practice_change_is_utility_driven, empirically_contingent).
narrative_ontology:cs_axiom('6ddf27d2-8f27-4c87-a611-7f38b129c01b', foundational, legitimate_change_displaces_old_practices).
narrative_ontology:cs_axiom_status(legitimate_change_displaces_old_practices, holdable).
narrative_ontology:cs_axiom_grounding('6ddf27d2-8f27-4c87-a611-7f38b129c01b', legitimate_change_displaces_old_practices, empirically_contingent).
narrative_ontology:cs_reference_frame('6ddf27d2-8f27-4c87-a611-7f38b129c01b', gradual_social_evolution).
narrative_ontology:cs_drift_state('6ddf27d2-8f27-4c87-a611-7f38b129c01b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6ddf27d2-8f27-4c87-a611-7f38b129c01b', '').
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

% Voluntarily adopt new practices (e.g., calendar systems, dress codes, agricultural techniques) because they perceive them as more useful, efficient, or culturally resonant. They are net beneficiaries of the improved utility.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, adopting_populations, beneficiary,
    moderate, biographical, mobile, regional).

% Their authority and social capital are tied to older, traditional practices. As populations endogenously shift to new practices, their influence diminishes, effectively 'paying' through loss of status and relevance. They face constrained exit from their traditional roles.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, traditional_elites, payer,
    organized, generational, constrained, local).

% Their theories about social evolution and the diffusion of innovations are validated by observed instances of practice change driven by utility and cultural shifts. They benefit from the explanatory power of this reading.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, modernization_theorists, beneficiary,
    analytical, civilizational, analytical, universal).

% While they may facilitate or encourage new practices, their legitimacy in this reading comes from observing and aligning with endogenous shifts, rather than imposing them. They benefit from social stability and perceived progress.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, state_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Analyze long-term patterns of cultural evolution, diffusion, and the displacement of practices. They seek to understand the mechanisms and drivers of endogenous change.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__endogenous_displacement_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__endogenous_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__endogenous_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social practices around perceived utility, efficiency, or cultural resonance, leading to widespread, voluntary adoption and the gradual displacement of older practices.
% TRANSFER_FUNCTION: Transfers social capital, legitimacy, and practical benefits from older, less efficient or relevant practices to newer, more useful ones, driven by collective choice and individual utility maximization.
% ABSENT_VOICES: Those who would resist change for purely traditional or sentimental reasons, but whose perspectives are naturally marginalized as the majority voluntarily adopts new, more beneficial practices. Their resistance is seen as temporary friction rather than a fundamental challenge to legitimacy.
% DISAPPEARANCE_RATIONALE: If this mechanism for legitimate, utility-driven practice change vanished, societies would either stagnate, unable to adapt to new conditions, or experience disruptive, illegitimate changes imposed by force, leading to social instability and conflict over cultural evolution.
% FOUNDING_PROBLEM: How societies legitimately adapt and evolve their collective practices (e.g., calendars, dress, agricultural methods) to new conditions, technologies, or perceived improvements without resorting to coercion or generating persistent dual systems.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists, anthropologists, and historians studying cultural evolution, diffusion of innovations, and institutional change corroborate that successful and stable societal transformations often follow this endogenous path, where perceived utility drives adoption and displacement. Examples include the adoption of the Gregorian calendar or modern agricultural techniques in many regions.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__endogenous_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__endogenous_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__endogenous_displacement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The metrics reflect the core premise of voluntary adoption: extractiveness is low (0.15) because change is driven by perceived benefit, not coercion. Suppression is minimal (0.1) as alternatives are not actively suppressed but naturally diminish in utility. Theater ratio is low (0.05) because the change is genuine and functional, not performative. Accessibility collapse is moderate (0.5) as older practices become less viable over time, but not through active suppression. Resistance is low (0.2) due to the voluntary nature of adoption.
 *
 * PERSPECTIVAL GAP:
 *   This reading stands in direct contrast to the 'exogenous override' reading, which legitimizes change through state decree, and the 'dual practice equilibrium' reading, which posits stable coexistence of old and new practices. The core disagreement lies in the source of legitimacy for change (voluntary utility vs. state authority) and the outcome (displacement vs. coexistence).
 *
 * DIRECTIONALITY LOGIC:
 *   Adopting populations and modernization theorists are beneficiaries, as they gain from improved practices and validated theories, respectively. Traditional elites are payers, as their authority diminishes with the displacement of old practices. State authorities are seen as facilitators, aligning with endogenous shifts rather than imposing them. Cultural historians are observers, analyzing the process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this constraint truly an ''endogenous displacement'' of practices, or is it better described by a sibling reading (exogenous override or dual practice equilibrium)?',
    'Empirical analysis of historical case studies: presence of widespread voluntary adoption, utility-driven shifts, and the actual displacement of older practices would support this reading. Evidence of state coercion or persistent coexistence would favor sibling readings.',
    'Reclassification to ''exogenous_override_reading'' would imply higher extractiveness and suppression, while reclassification to ''dual_practice_equilibrium_reading'' would imply a different coordination function and potentially lower overall impact on traditional practices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Ambiguity between endogenous displacement and sibling readings of practice standardization legitimacy.').

omega_variable(
    true_voluntariness_of_adoption,
    'To what extent is ''voluntary adoption'' truly free from subtle coercive pressures (e.g., economic incentives, social pressure, or structural disadvantages of older practices)?',
    'Detailed ethnographic and historical research into individual and community decision-making processes, examining the range of available alternatives and the costs/benefits associated with each choice.',
    'If significant subtle coercion is found, the measured extractiveness and suppression of this constraint would be higher than currently assessed, potentially shifting its classification towards a Tangled Rope or Snare, even if not directly imposed by state authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_voluntariness_of_adoption, empirical, 'Assessing the genuine voluntariness of practice adoption in endogenous displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__endogenous_displacement_reading, 1900, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1920, 0.04).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1960, 0.06).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1920, 0.14).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1960, 0.16).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, base_extractiveness, 2000, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1920, 0.09).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1940, 0.1).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1960, 0.11).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__endogenous_displacement_reading, suppression_requirement, 2000, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__endogenous_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__endogenous_displacement_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
