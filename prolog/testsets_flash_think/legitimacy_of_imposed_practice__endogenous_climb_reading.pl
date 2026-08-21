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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Imposed Practice Legitimacy
 *   domain: Political History / State Formation / Cultural Imposition
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous climb' reading of the
 *   'legitimacy of imposed practice' kernel. It describes the historical
 *   phenomenon where state-imposed cultural practices (e.g., new calendars,
 *   dress codes) fail to achieve genuine legitimacy or displace prior
 *   practices without bottom-up adoption pathways. Despite active
 *   enforcement, communities often resist or find ways to maintain
 *   traditional customs, leading to a gap between official policy and lived
 *   reality. The constraint is framed from the perspective of the state's
 *   attempt to impose, and its ultimate failure to extract full compliance
 *   and internalization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.35).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, piton).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Endogenous Climb Reading of Imposed Practice Legitimacy").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "Political History / State Formation / Cultural Imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '37547f48-df3d-4f00-ba6c-4c465c064435').
narrative_ontology:cs_kernel_codification('37547f48-df3d-4f00-ba6c-4c465c064435', formalized).
narrative_ontology:cs_authority_grounding('37547f48-df3d-4f00-ba6c-4c465c064435', extraction).
narrative_ontology:cs_interpretation_layer_present('37547f48-df3d-4f00-ba6c-4c465c064435').
narrative_ontology:cs_reading_relation('37547f48-df3d-4f00-ba6c-4c465c064435', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('37547f48-df3d-4f00-ba6c-4c465c064435', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('37547f48-df3d-4f00-ba6c-4c465c064435', foundational, legitimacy_requires_internalization).
narrative_ontology:cs_axiom_status(legitimacy_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('37547f48-df3d-4f00-ba6c-4c465c064435', legitimacy_requires_internalization, deontological).
narrative_ontology:cs_axiom('37547f48-df3d-4f00-ba6c-4c465c064435', secondary, top_down_imposition_is_brittle).
narrative_ontology:cs_axiom_status(top_down_imposition_is_brittle, holdable).
narrative_ontology:cs_axiom_grounding('37547f48-df3d-4f00-ba6c-4c465c064435', top_down_imposition_is_brittle, empirically_contingent).
narrative_ontology:cs_reference_frame('37547f48-df3d-4f00-ba6c-4c465c064435', bottom_up_legitimacy).
narrative_ontology:cs_drift_state('37547f48-df3d-4f00-ba6c-4c465c064435', contemporary_historical_analysis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37547f48-df3d-4f00-ba6c-4c465c064435', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority attempting to impose new cultural practices (e.g., calendar, dress codes) to foster national unity and modernization. It expends resources on enforcement and propaganda, but struggles to achieve genuine internalization.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% The target of imposed practices, often with deep-rooted traditional customs. They bear the costs of resistance (fines, social pressure) but often find ways to maintain their autonomy and traditional practices, leading to incomplete internalization of state mandates.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities, payer,
    powerless, generational, identity_locked, local).

% Individuals or groups who may benefit from state patronage by promoting the new practices, or who genuinely believe in the state's modernization agenda. They often act as intermediaries, but their influence on genuine bottom-up adoption is limited without deeper community buy-in.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites, agenda_setter).

% Academics and researchers who analyze the long-term outcomes of state-imposed cultural changes, often documenting the persistence of traditional practices despite official mandates and the limited success of top-down approaches.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state attempts to coordinate a unified national identity and modern social practices across a diverse populace, replacing traditional, localized customs with standardized, state-sanctioned norms.
% TRANSFER_FUNCTION: The arrangement attempts to transfer cultural loyalty, social cohesion, and behavioral compliance from traditional, local institutions to the central state apparatus, often through legal mandates and educational reforms.
% ABSENT_VOICES: Traditional religious leaders, elders, and cultural practitioners whose authority and practices are directly undermined by the state's mandates. Their perspectives, rooted in centuries of custom, are systematically excluded from the state's policy-making process.
% DISAPPEARANCE_RATIONALE: If the state's imposed practices and their enforcement vanished overnight, local communities would largely revert to or strengthen their traditional calendars, dress codes, and social rituals, as these practices were never fully displaced or internalized. The state's efforts would be revealed as largely superficial, and the cultural landscape would reorganize around pre-existing, resilient local norms.
% FOUNDING_PROBLEM: To overcome perceived 'backwardness' and fragmentation within a newly formed or modernizing state, by replacing diverse local customs with a singular, 'modern' national culture, thereby fostering unity and administrative efficiency.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, ethnographic studies, and community narratives from outside the state apparatus corroborate that the founding problem was often a pretext for control, or that the problem itself evolved beyond the state's initial framing.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).
:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.35) because the state's efforts to displace traditional practices largely fail to extract genuine compliance or internalization from local communities. Suppression is high (0.70) as the state actively enforces its mandates through legal and coercive means. However, the high theater ratio (0.60) reflects that much of this enforcement is performative; it maintains the appearance of control while the actual function of cultural displacement atrophies due to persistent resistance and lack of genuine adoption. Accessibility collapse is low (0.30) because communities find numerous ways (private observance, symbolic compliance, passive resistance) to maintain their traditional practices, demonstrating that alternatives are not fully collapsed. Resistance is high (0.80) due to the active and passive efforts of communities to preserve their cultural autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, the imposed practices are legitimate and necessary for modernization, and any resistance is a failure of compliance. From the perspective of local communities and historical observers, the constraint is an extractive imposition that fails to achieve its stated goals due to a lack of genuine legitimacy and bottom-up adoption. The engine's classification as a Piton reflects this gap: the constraint persists through theatrical maintenance, but its core function has atrophied.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus acts as the agenda-setter, attempting to impose practices and extract compliance. Local communities are the primary payers, bearing the costs of resistance and non-compliance, but also benefiting from preserving their autonomy. Cultural elites may act as beneficiaries by aligning with the state, but their role in genuine internalization is limited. Historical observers analyze the long-term outcomes, often highlighting the resilience of local cultures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_measurement_ambiguity,
    'How can ''internalization'' of a practice be reliably measured, distinguishing genuine adoption from mere outward compliance under duress?',
    'Longitudinal ethnographic studies, analysis of private vs. public practice, and linguistic shifts over generations. If practices persist in private or informal settings despite public compliance, internalization is low.',
    'If internalization is consistently low, it strengthens the ''piton'' classification and the argument that the constraint''s function has atrophied. If high, it would challenge this reading''s core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_measurement_ambiguity, empirical, 'Distinguishing genuine internalization from superficial compliance.').

omega_variable(
    exogenous_override_vs_endogenous_climb,
    'Is state decree authority sufficient to displace prior practice, or does displacement require bottom-up adoption?',
    'Comparative historical analysis of similar imposition attempts across different states and cultural contexts, evaluating long-term outcomes of purely top-down vs. participatory approaches.',
    'If state decree is found sufficient (supporting the ''exogenous_override_reading''), this constraint''s extractiveness would be re-evaluated as higher, and its classification might shift towards a Snare or Tangled Rope. If bottom-up adoption is consistently necessary, it reinforces the Piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exogenous_override_vs_endogenous_climb, conceptual, 'The fundamental disagreement between this reading and the ''exogenous_override_reading''.').

omega_variable(
    hybrid_scaffolding_effectiveness,
    'To what extent can ideological messaging and ''scaffolding'' generate quasi-endogenous pull, making top-down mandates succeed where pure decree fails?',
    'Analysis of cases where hybrid approaches were implemented, assessing whether they achieved deeper internalization than pure decree, or merely masked continued resistance.',
    'If hybrid scaffolding is found to genuinely foster internalization, it would suggest a more complex pathway to legitimacy, potentially shifting the classification towards a Scaffold (if temporary and effective) or a Tangled Rope (if it still involves asymmetric extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_scaffolding_effectiveness, empirical, 'The effectiveness of hybrid approaches in generating endogenous pull.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 1900, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1900, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(legi_tr_t1910, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1910, 0.45).
narrative_ontology:measurement(legi_tr_t1920, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1920, 0.5).
narrative_ontology:measurement(legi_tr_t1930, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1930, 0.55).
narrative_ontology:measurement(legi_tr_t1940, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1940, 0.58).
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 1950, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t1900, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(legi_be_t1910, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1910, 0.42).
narrative_ontology:measurement(legi_be_t1920, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1920, 0.39).
narrative_ontology:measurement(legi_be_t1930, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1930, 0.37).
narrative_ontology:measurement(legi_be_t1940, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1940, 0.36).
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 1950, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1900, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(legi_su_t1910, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1910, 0.63).
narrative_ontology:measurement(legi_su_t1920, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1920, 0.66).
narrative_ontology:measurement(legi_su_t1930, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1930, 0.68).
narrative_ontology:measurement(legi_su_t1940, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1940, 0.69).
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 1950, 0.7).


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
