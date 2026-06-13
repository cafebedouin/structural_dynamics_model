% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Bhagavad Gita's Orthodox Literal Reading of Kurukshetra Discourse
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the orthodox literal reading of the Bhagavad
 *   Gita's Kurukshetra discourse, which mandates caste-based duty
 *   (varnashrama dharma) and legitimates righteous violence, particularly for
 *   the Kshatriya warrior class. It is a reading that has historically
 *   underpinned social hierarchy and justified conflict. This is one reading
 *   of the 'gita_kurukshetra_discourse' kernel, distinct from allegorical or
 *   universalist interpretations.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Agenda setter (institutional/arbitrage) — interprets and propagates the reading, benefits from interpretive monopoly.
 *   - kshatriya_warrior_class: Beneficiary (powerful/constrained) — receives divine sanction for violence and social status.
 *   - lower_castes: Payer (powerless/identity_locked) — bears the costs of rigid social hierarchy and limited mobility.
 *   - those_killed_in_dharmic_war: Victim (powerless/trapped) — direct casualties of conflicts legitimized by the doctrine.
 *   - gandhian_scholars_and_activists: Excluded (organized/constrained) — advocate for non-violent, allegorical readings, marginalized by orthodox institutions.
 *   - universalist_devotees: Excluded (moderate/mobile) — focus on devotional aspects, challenging hierarchy, often operating outside traditional structures.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.7).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Bhagavad Gita's Orthodox Literal Reading of Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, 'eb759915-d61d-4dac-835f-7567bf6d59c2').
narrative_ontology:cs_kernel_codification('eb759915-d61d-4dac-835f-7567bf6d59c2', fixed_text).
narrative_ontology:cs_authority_grounding('eb759915-d61d-4dac-835f-7567bf6d59c2', lineage).
narrative_ontology:cs_interpretation_layer_present('eb759915-d61d-4dac-835f-7567bf6d59c2').
narrative_ontology:cs_reading_relation('eb759915-d61d-4dac-835f-7567bf6d59c2', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('eb759915-d61d-4dac-835f-7567bf6d59c2', gita_kurukshetra_discourse__universalist_devotional_reading, forecloses).
narrative_ontology:cs_axiom('eb759915-d61d-4dac-835f-7567bf6d59c2', foundational, varnashrama_dharma_divinely_ordained).
narrative_ontology:cs_axiom_status(varnashrama_dharma_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('eb759915-d61d-4dac-835f-7567bf6d59c2', varnashrama_dharma_divinely_ordained, theological).
narrative_ontology:cs_axiom('eb759915-d61d-4dac-835f-7567bf6d59c2', foundational, kshatriya_duty_includes_righteous_violence).
narrative_ontology:cs_axiom_status(kshatriya_duty_includes_righteous_violence, holdable).
narrative_ontology:cs_axiom_grounding('eb759915-d61d-4dac-835f-7567bf6d59c2', kshatriya_duty_includes_righteous_violence, deontological).
narrative_ontology:cs_reference_frame('eb759915-d61d-4dac-835f-7567bf6d59c2', ancient_dharmic_social_order).
narrative_ontology:cs_drift_state('eb759915-d61d-4dac-835f-7567bf6d59c2', contemporary_secular_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb759915-d61d-4dac-835f-7567bf6d59c2', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high due to the significant social and material benefits accrued by upper castes and the justification of violence. Suppression (0.85) is very high because the constraint is deeply embedded in religious authority and social structures, making dissent or exit extremely difficult, especially for lower castes who are identity-locked into their roles. Theater ratio (0.2) is low, indicating that the constraint's functions (maintaining hierarchy, justifying violence) are largely genuine from the perspective of its beneficiaries, not merely performative. Accessibility collapse is high (0.75) as alternatives to the prescribed social order are severely limited by religious and social enforcement. Resistance is moderate (0.4) but often suppressed or marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin and Kshatriya classes experience this as a legitimate, divinely ordained system of coordination and duty, with clear benefits. Lower castes and victims of 'dharmic war' experience it as a highly extractive and suppressive system that denies their agency and imposes severe costs. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class and Kshatriya warrior class are clear beneficiaries, with the former acting as the primary agenda-setter. Lower castes and those killed in 'dharmic war' are direct victims. The constraint subsidizes the social and spiritual authority of the upper castes while extracting from the lower castes and those caught in conflicts. The 'identity_locked' exit option for lower castes further amplifies their directionality towards being targets.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it presents a coordination function (social order, duty) but fundamentally relies on asymmetric extraction and active enforcement to maintain a rigid hierarchy. It prevents mislabeling by highlighting that the 'coordination' comes at a severe cost to identifiable victims, and its persistence is not purely voluntary but requires active suppression of alternatives and dissenting interpretations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_truth,
    'Is the Kurukshetra discourse primarily a literal historical/ethical mandate or an allegorical spiritual teaching?',
    'Textual analysis of internal consistency, historical context of composition, and cross-referencing with other dharmic texts. However, ultimate resolution often depends on philosophical or theological commitments.',
    'If allegorical, the justification for caste-based violence collapses, reclassifying the constraint as a Snare or Piton, as its coordination function would be revealed as cover. If literal, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_vs_allegorical_truth, conceptual, 'Ambiguity in the primary mode of interpretation (literal vs. allegorical).').

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the caste hierarchy (varnashrama dharma) a divinely ordained, immutable social order, or a human-constructed system that benefits specific groups?',
    'Sociological and historical analysis of caste evolution, comparison with other social stratification systems, and theological debate on the nature of divine will versus human agency. Empirical evidence of social mobility or its suppression is key.',
    'If a social construct, the ''naturalness'' claim of the constraint is undermined, revealing its extractive nature more clearly and potentially reclassifying it as a Snare. If divinely ordained, the beneficiaries'' claims of legitimacy are strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, empirical, 'Ambiguity regarding the divine vs. human origin of caste hierarchy.').

omega_variable(
    suppression_mechanism_internalized,
    'To what extent is the suppression experienced by lower castes structural (external barriers) versus internalized (self-concept, belief in karma)?',
    'Longitudinal studies of post-emancipation social mobility, psychological impact assessments, and analysis of cultural narratives. If suppression persists after external barriers are removed, it indicates internalized components.',
    'If internalized suppression is a significant factor, the effective suppression is higher than structural measures suggest, making exit even more difficult and amplifying the constraint''s extractive power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression mechanism for lower castes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 500, 0.15).
narrative_ontology:measurement(gita_tr_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(gita_tr_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(gita_tr_t2024, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 500, 0.65).
narrative_ontology:measurement(gita_be_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement(gita_be_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1500, 0.7).
narrative_ontology:measurement(gita_be_t2024, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(gita_su_t500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 500, 0.8).
narrative_ontology:measurement(gita_su_t1000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1000, 0.85).
narrative_ontology:measurement(gita_su_t1500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1500, 0.85).
narrative_ontology:measurement(gita_su_t2024, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse__universalist_devotional_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_hindu_legal_codes).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gita_kurukshetra_discourse' kernel. This orthodox literal reading directly influences and is in tension with the allegorical and universalist readings by asserting a foundational, literal interpretation of duty and violence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
