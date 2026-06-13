% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Endogenous Climb for Imposed Practice
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint describes the challenge faced by a modernizing state
 *   attempting to displace traditional practices (e.g., lunar calendars,
 *   traditional dress codes) with new, state-sanctioned norms. From the
 *   'endogenous climb' reading, such imposed commitments fail without
 *   bottom-up adoption pathways, leading to persistent resistance and the
 *   need for continuous, often theatrical, enforcement. The state's
 *   modernization timeline becomes a victim, while communities preserving
 *   their autonomy are beneficiaries of the constraint's inherent resistance
 *   to top-down change.
 *
 * KEY AGENTS:
 *   - modernizing_state: Agenda setter (institutional/generational) — attempts to impose new practices.
 *   - local_populations: Payer (powerless/generational) — resists imposed practices, retains traditional norms.
 *   - communities_preserving_autonomy: Beneficiary (organized/generational) — maintains cultural distinctiveness.
 *   - state_modernization_timeline: Victim (analytical/generational) — suffers delays and resource drain due to resistance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.6).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous Climb for Imposed Practice").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'af008cbc-0cb0-4b5a-b9b5-88b78bd0999c').
narrative_ontology:cs_kernel_codification('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', formalized).
narrative_ontology:cs_authority_grounding('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', lineage).
narrative_ontology:cs_interpretation_layer_present('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c').
narrative_ontology:cs_reading_relation('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', foundational, practice_legitimacy_requires_internalization).
narrative_ontology:cs_axiom_status(practice_legitimacy_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', practice_legitimacy_requires_internalization, empirically_contingent).
narrative_ontology:cs_axiom('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', secondary, state_decree_insufficient_for_cultural_change).
narrative_ontology:cs_axiom_status(state_decree_insufficient_for_cultural_change, holdable).
narrative_ontology:cs_axiom_grounding('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', state_decree_insufficient_for_cultural_change, empirically_contingent).
narrative_ontology:cs_reference_frame('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', organic_cultural_evolution).
narrative_ontology:cs_drift_state('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', post_colonial_state_formation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af008cbc-0cb0-4b5a-b9b5-88b78bd0999c', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, local_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate a national identity and standardized social practices across diverse local populations, aiming for administrative efficiency and perceived modernity.
% TRANSFER_FUNCTION: Transfers cultural norms, social capital, and administrative control from local communities to the central state, while extracting compliance and resources from local populations.
% ABSENT_VOICES: Traditional elders and local cultural leaders, whose authority is undermined by state imposition, are excluded from the policy-making process. Their voices would emphasize the value of traditional practices and the social costs of their displacement.
% DISAPPEARANCE_RATIONALE: If the state's imposed practices and their enforcement vanished, local populations would likely revert to traditional practices, and the state's administrative and cultural authority would be significantly diminished, leading to a reorganization of social and political structures.
% FOUNDING_PROBLEM: The problem of creating a unified national identity and modern administrative state out of diverse, traditional communities, often in a post-colonial context.
% FOUNDING_PROBLEM_CORROBORATION: The state's continued efforts to enforce these practices, and the ongoing resistance from local populations, corroborate that the problem of national integration and modernization is still live. Independent historians and anthropologists document the persistent tension between state goals and local realities.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost imposed on local populations to comply with new practices, even superficially. Suppression (0.7) is high because the state must actively enforce the new norms against persistent cultural inertia. The theater ratio (0.4) indicates that a significant portion of state activity is performative, maintaining the illusion of compliance while genuine internalization is low. Resistance (0.8) is high due to the deep-seated nature of traditional practices and the lack of bottom-up adoption pathways. Accessibility collapse (0.45) is moderate, as alternatives (traditional practices) are not fully eliminated but pushed into private spheres.
 *
 * PERSPECTIVAL GAP:
 *   The modernizing state perceives the constraint as a necessary 'rope' for national cohesion and progress, with resistance as an irrational impediment. Local populations experience it as a 'snare' that extracts cultural autonomy and imposes alien norms. The engine's classification will likely diverge from the state's claimed 'rope' due to high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The modernizing_state, as the agenda_setter, benefits from the perceived (if not actual) compliance with its modernization agenda. Local_populations are targets, bearing the costs of cultural disruption and forced adaptation. Communities_preserving_autonomy are beneficiaries because the constraint's inherent resistance allows them to maintain their distinctiveness. The state_modernization_timeline is a victim because the imposed practices fail to achieve rapid, genuine displacement, leading to delays and resource drain.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint highlights how a mandate (state modernization) can become 'mandatrophic' when it fails to account for the social dynamics of practice displacement. The state's mandate to modernize is not resolved, but its effectiveness is severely hampered by the lack of bottom-up adoption. The classification as a 'snare' for local populations, despite the state's 'rope' framing, prevents mislabeling a coercive imposition as genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of the ''endogenous climb'' principle, or is it merely a failed ''exogenous override'' that lacked sufficient coercive force?',
    'Comparative historical analysis of similar state-building efforts with varying levels of coercive capacity and ideological scaffolding. If high coercion consistently fails without bottom-up adoption, it supports the endogenous climb reading.',
    'If genuinely an ''endogenous climb'' reading, it implies that top-down imposition of practices is inherently limited without internal buy-in, leading to high resistance and low effective extraction. If a failed ''exogenous override'', it suggests the state simply didn''t try hard enough, and the constraint''s classification would shift towards a more extractive ''snare'' if the state had more power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading (''endogenous_climb_reading'') of the ''legitimacy_of_imposed_practice'' kernel. Sibling readings include ''exogenous_override_reading'' and ''hybrid_scaffolding_reading''. This reading emphasizes the necessity of bottom-up adoption for the success of imposed practices, contrasting with views that prioritize state decree or hybrid approaches.').

omega_variable(
    internalization_measurement_ambiguity,
    'How can ''internalization'' of a practice be reliably measured, especially when public compliance coexists with private retention of prior practices?',
    'Longitudinal ethnographic studies, analysis of private correspondence, and archaeological evidence of household practices. The persistence of prior practices in private spheres despite public compliance would indicate low internalization.',
    'If internalization is low despite outward compliance, the constraint''s effective suppression is higher than structural measures suggest, as it relies on constant monitoring and enforcement rather than genuine adoption. This would push the classification towards a ''snare'' for the local populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_measurement_ambiguity, empirical, 'Ambiguity in measuring genuine internalization versus performative compliance for imposed practices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
