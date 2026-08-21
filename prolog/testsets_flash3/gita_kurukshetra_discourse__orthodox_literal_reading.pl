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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Bhagavad Gita's Kurukshetra Discourse: Orthodox Literal Reading
 *   domain: religious_studies/hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint represents the orthodox, literal reading of the Bhagavad
 *   Gita's Kurukshetra discourse, which interprets the text as mandating
 *   caste-based duties (dharma) and legitimizing righteous violence in war.
 *   This reading reinforces a hierarchical social order and the authority of
 *   the priestly class. It is one of several contested readings of the same
 *   core text. The metrics reflect a highly extractive and suppressive
 *   constraint, actively enforced to maintain a specific social and moral
 *   order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.9).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Bhagavad Gita's Kurukshetra Discourse: Orthodox Literal Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '0f9221e0-a1a5-445d-bb1b-cad287d0abc9').
narrative_ontology:cs_kernel_codification('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', fixed_text).
narrative_ontology:cs_authority_grounding('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', lineage).
narrative_ontology:cs_interpretation_layer_present('0f9221e0-a1a5-445d-bb1b-cad287d0abc9').
narrative_ontology:cs_reading_relation('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', foundational, caste_dharma_is_divine_mandate).
narrative_ontology:cs_axiom_status(caste_dharma_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', caste_dharma_is_divine_mandate, theological).
narrative_ontology:cs_axiom('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', foundational, righteous_violence_is_sacred_duty).
narrative_ontology:cs_axiom_status(righteous_violence_is_sacred_duty, holdable).
narrative_ontology:cs_axiom_grounding('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', righteous_violence_is_sacred_duty, deontological).
narrative_ontology:cs_reference_frame('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', vedic_social_order_and_dharmic_warfare).
narrative_ontology:cs_drift_state('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', contemporary_global_ethics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0f9221e0-a1a5-445d-bb1b-cad287d0abc9', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds interpretive authority over sacred texts, including the Gita. Benefits from the literal reading by reinforcing its social and ritual preeminence, and by legitimizing the caste system it oversees. Its identity is fused with this interpretive tradition.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, identity_locked, regional).

% Receives divine sanction for its role in warfare and governance, provided it adheres to caste-based duty (dharma). This reading justifies violence in 'righteous' war as a sacred obligation, reducing moral ambiguity for its members.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, biographical, constrained, regional).

% Are assigned fixed, often subservient, duties within the social hierarchy, with limited avenues for social mobility or self-determination. They bear the costs of a rigid social order justified by this reading, with their suffering framed as karmic destiny.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes, payer,
    powerless, generational, trapped, local).

% Are the direct victims of violence legitimized by the concept of 'dharmic war.' Their lives are sacrificed for a divinely sanctioned duty that they may not have chosen, with their deaths framed as necessary for cosmic order.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, those_killed_in_dharmic_war, payer,
    powerless, immediate, trapped, local).

% Are marginalized or actively suppressed by the dominant orthodox reading. Alternative interpretations (e.g., allegorical, universalist) are deemed heterodox or less authoritative, limiting their reach and influence within traditional institutions.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, dissenting_interpretations, excluded,
    moderate, biographical, constrained, global).

% The abstract concept of a divinely ordained, hierarchical social structure that benefits from this reading's reinforcement. Its persistence is tied to the continued acceptance of caste-based duties and roles.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order, beneficiary,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_non_agent(gita_kurukshetra_discourse__orthodox_literal_reading, traditional_social_order).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social roles and duties within a hierarchical caste system, providing a framework for individual action and societal stability, and legitimizing the use of force for maintaining cosmic order (dharma).
% TRANSFER_FUNCTION: Transfers moral authority and social status to higher castes (Brahmins, Kshatriyas) and transfers the burden of fixed duties and potential violence to lower castes and those designated as enemies in 'dharmic' conflict.
% ABSENT_VOICES: Lower castes and those who advocate for social equality or non-violence are structurally excluded from the interpretive process. They would challenge the divine sanction of hierarchy and violence, but their voices are suppressed by the interpretive monopoly of the priestly class.
% DISAPPEARANCE_RATIONALE: If this orthodox literal reading vanished, the traditional caste system would lose a major source of its religious legitimacy, leading to significant social upheaval, challenges to existing power structures, and a re-evaluation of violence in religious contexts. The social order would be forced to reorganize.
% FOUNDING_PROBLEM: To provide moral guidance and justification for action (dharma) in a time of profound social and ethical crisis (the Kurukshetra war), particularly for the warrior class facing the dilemma of fighting kin.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious scholars and traditional institutions attest that the problem of maintaining dharma and social order in a complex world remains live, requiring adherence to the text's literal commands. Critics, however, argue that the 'problem' has shifted from a genuine ethical dilemma to a justification for maintaining an extractive social order.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) due to the significant costs imposed on lower castes and the direct victims of 'dharmic' violence, who are compelled to accept their roles and fates. Suppression is also very high (0.90) because this reading is enforced through religious authority, social norms, and the marginalization of dissenting interpretations, making exit or resistance extremely difficult, especially for those identity-locked into the system. Theater ratio is low (0.20) as the constraint's function is genuinely to maintain a specific social order, not merely to perform it, though some performative elements exist in ritual. The increasing extractiveness and suppression over time reflect the hardening of this interpretation and the increasing resistance it faces from modern ethical frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin and Kshatriya classes, this reading provides essential guidance for cosmic order and righteous living. From the perspective of lower castes and modern ethical observers, it is a highly extractive and suppressive mechanism for maintaining an unjust hierarchy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class and Kshatriya warrior class are clear beneficiaries, gaining authority, status, and moral justification for their roles. Lower castes and those killed in 'dharmic' war are the primary victims, bearing the direct costs of social hierarchy and violence. Dissenting interpretations are excluded, their very existence challenging the constraint's legitimacy. The traditional social order itself is an abstract beneficiary, its persistence tied to this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_interpretation,
    'Is the Kurukshetra war described in the Bhagavad Gita a literal historical event justifying physical violence, or an allegorical representation of an internal spiritual struggle?',
    'Textual analysis of other dharmic texts, archaeological evidence for the historicity of the war, and philosophical arguments regarding the nature of religious narrative. However, ultimate resolution often depends on a reader''s pre-existing hermeneutic commitments.',
    'If allegorical, the justification for physical violence collapses, significantly reducing the constraint''s extractiveness and suppression related to warfare. If literal, the justification for violence remains, reinforcing the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literal_vs_allegorical_interpretation, conceptual, 'Ambiguity regarding the literal vs. allegorical nature of the Gita''s central narrative.').

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the caste system, as described and reinforced by this reading, a divinely ordained and immutable social order, or a human-made social construct that benefits specific groups?',
    'Sociological and historical analysis of the evolution of caste, comparative religious studies, and ethical arguments from universal human rights. Resolution is often contested across different epistemic frameworks.',
    'If a social construct, the ''naturalness'' claim of the constraint collapses, exposing its extractive nature more clearly and potentially reclassifying it as a Snare. If divinely ordained, its perceived legitimacy and resistance to change remain high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, preference, 'Ambiguity regarding the divine vs. human origin of the caste system.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of lower castes and dissenting interpretations structural (external barriers) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., individuals leaving traditional communities still carry internalized caste-based self-concepts), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making liberation more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of caste and religious authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(gita_tr_t30, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(gita_tr_t50, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(gita_be_t30, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(gita_be_t50, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(gita_su_t30, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(gita_su_t50, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, hindu_caste_system_legitimacy).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, dharmic_war_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gita_kurukshetra_discourse' kernel. The other readings are 'gandhian_allegorical_reading' and 'universalist_devotional_reading', which offer alternative interpretations of the text's meaning and implications for social order and violence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
