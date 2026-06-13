% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Speech Protection Conditional on Dignity Maintenance
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of speech protection,
 *   where the scope of protected speech is conditional on it not functioning
 *   as structural subordination of target groups. It recognizes group harm as
 *   distinct from individual harm and typically leaves hate speech or group
 *   libel unprotected. The constraint is framed as a tangled rope because it
 *   genuinely coordinates a more inclusive public sphere (beneficiaries:
 *   marginalized groups, social cohesion) but does so by actively extracting
 *   certain speech rights from others (victims: speakers of subordinating
 *   speech), requiring active enforcement by courts and regulators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.45).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.3).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Speech Protection Conditional on Dignity Maintenance").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, '74935446-4c6a-4eda-be77-440f48d7317b').
narrative_ontology:cs_kernel_codification('74935446-4c6a-4eda-be77-440f48d7317b', formalized).
narrative_ontology:cs_authority_grounding('74935446-4c6a-4eda-be77-440f48d7317b', lineage).
narrative_ontology:cs_interpretation_layer_present('74935446-4c6a-4eda-be77-440f48d7317b').
narrative_ontology:cs_reading_relation('74935446-4c6a-4eda-be77-440f48d7317b', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('74935446-4c6a-4eda-be77-440f48d7317b', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('74935446-4c6a-4eda-be77-440f48d7317b', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('74935446-4c6a-4eda-be77-440f48d7317b', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('74935446-4c6a-4eda-be77-440f48d7317b', foundational, equal_dignity_as_precondition_for_speech).
narrative_ontology:cs_axiom_status(equal_dignity_as_precondition_for_speech, holdable).
narrative_ontology:cs_axiom_grounding('74935446-4c6a-4eda-be77-440f48d7317b', equal_dignity_as_precondition_for_speech, deontological).
narrative_ontology:cs_axiom('74935446-4c6a-4eda-be77-440f48d7317b', foundational, group_subordination_as_distinct_harm).
narrative_ontology:cs_axiom_status(group_subordination_as_distinct_harm, holdable).
narrative_ontology:cs_axiom_grounding('74935446-4c6a-4eda-be77-440f48d7317b', group_subordination_as_distinct_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('74935446-4c6a-4eda-be77-440f48d7317b', post_civil_rights_dignity_jurisprudence).
narrative_ontology:cs_drift_state('74935446-4c6a-4eda-be77-440f48d7317b', contemporary_digital_discourse_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('74935446-4c6a-4eda-be77-440f48d7317b', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, marginalized_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, social_cohesion).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the protection against speech that structurally subordinates them, allowing for more equitable participation in public life. Their ability to exit a subordinated status is constrained by the persistence of such speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, marginalized_groups, beneficiary,
    organized, generational, constrained, national).

% Bear the cost of having their speech restricted when it is deemed to function as structural subordination. Their exit options are to modify their speech or face legal/social consequences.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, speakers_of_subordinating_speech, payer,
    moderate, immediate, constrained, local).

% Interpret and enforce the boundaries of protected speech, balancing individual expression with group dignity. They actively define what constitutes structural subordination and apply remedies.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue against any content-based restrictions on speech, viewing this reading as an infringement on fundamental liberties. Their arguments are often considered but not adopted by the courts in this framework.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% Benefits from a more inclusive public discourse and reduced social friction, but may experience a chilling effect on certain forms of expression or perceive restrictions as overreach.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, general_public, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public discourse to ensure that all groups can participate on terms of equal dignity, preventing speech from becoming a tool of structural oppression.
% TRANSFER_FUNCTION: Transfers the right to engage in certain forms of speech from those whose expression would subordinate others, to marginalized groups who gain protection from such subordination.
% ABSENT_VOICES: Advocates for an 'absolutist' interpretation of speech rights, who believe that any content-based restriction is illegitimate, are often excluded from the foundational framing of this constraint, as their premise directly contradicts the dignity principle.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, speech that structurally subordinates marginalized groups would proliferate, leading to increased social fragmentation, reduced participation by vulnerable populations, and a breakdown of social cohesion, necessitating a re-evaluation of fundamental rights.
% FOUNDING_PROBLEM: The historical use of speech to establish and maintain systems of structural subordination, leading to unequal participation and dignity for certain groups within society.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and international human rights bodies consistently attest to the ongoing problem of speech-based structural subordination, providing extensive documentation and analysis from outside the immediate beneficiaries of this constraint.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).
:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the cost borne by speakers whose speech is restricted, balanced against the coordination benefits of a more equitable public discourse. Suppression (0.30) is moderate, as enforcement requires active judicial and regulatory interpretation, but is not absolute due to ongoing contestation. The theater ratio (0.10) is low, indicating that the constraint's stated function (protecting dignity) largely aligns with its actual operation, though some performative aspects may exist in the application of vague standards. The metrics show a gradual increase in extractiveness and suppression over time, reflecting an expanding understanding and enforcement of dignity-based speech limits.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups, this constraint is a necessary rope, enabling their participation. From the perspective of speakers whose speech is restricted, it is a snare, unjustly limiting their freedom. The courts, as agenda-setters, navigate this tension, aiming for a balanced tangled rope outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and social cohesion are the primary beneficiaries (low directionality), as the constraint directly addresses their vulnerability to speech-based harm. Speakers of subordinating speech are the targets (high directionality), as their expressive freedom is curtailed. Courts and regulators act as agenda-setters, defining and enforcing the boundaries. The general public experiences a mixed effect, benefiting from a more inclusive discourse but potentially facing a chilling effect on some forms of expression.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_subordination_definition,
    'What constitutes ''structural subordination'' in speech, and how is it reliably distinguished from mere offense or disagreement?',
    'Development of clearer legal tests and sociological evidence for systemic power imbalances and speech''s role in perpetuating them, moving beyond subjective harm claims.',
    'If the definition remains vague, the constraint risks over-extraction from speakers and under-protection for dignity; clearer definitions would stabilize its application and classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_subordination_definition, conceptual, 'Ambiguity in defining structural subordination.').

omega_variable(
    enforcement_chilling_effect,
    'Does the enforcement of this constraint create an undue ''chilling effect'' on legitimate, non-subordinating speech, particularly for speakers with less power?',
    'Empirical studies on speech patterns and self-censorship among various speaker groups following enforcement actions, compared to jurisdictions with different speech regimes.',
    'If a significant chilling effect is demonstrated, the constraint''s suppression metric may be effectively higher than measured, pushing it closer to a snare for some speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_chilling_effect, empirical, 'Potential for chilling effect on legitimate speech.').

omega_variable(
    dignity_vs_absolutism_framing,
    'Is the conflict between dignity-based speech limits and absolutist speech protection a genuine structural tension, or a preference-based dispute over fundamental values?',
    'Analysis of whether a coherent legal framework can logically reconcile both principles without one foreclosing the other, or if they represent irreconcilable normative commitments.',
    'If irreconcilable, the ''dignity_reading'' fundamentally forecloses the ''absolutist_reading'' within a single legal system; if reconcilable, they merely coexist as competing policy preferences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_vs_absolutism_framing, conceptual, 'Nature of the conflict between dignity and absolutist speech principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__dignity_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(spee_tr_t1980, speech_protection_kernel__dignity_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(spee_tr_t2000, speech_protection_kernel__dignity_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(spee_tr_t2024, speech_protection_kernel__dignity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__dignity_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(spee_be_t1980, speech_protection_kernel__dignity_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(spee_be_t2000, speech_protection_kernel__dignity_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(spee_be_t2024, speech_protection_kernel__dignity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1960, speech_protection_kernel__dignity_reading, suppression_requirement, 1960, 0.2).
narrative_ontology:measurement(spee_su_t1980, speech_protection_kernel__dignity_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(spee_su_t2000, speech_protection_kernel__dignity_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement(spee_su_t2024, speech_protection_kernel__dignity_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'speech_protection_kernel'. Its structural delta is the recognition of group harm and conditional protection based on dignity maintenance, distinguishing it from other readings that prioritize different values or mechanisms for speech regulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
