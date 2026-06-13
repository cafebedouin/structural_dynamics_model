% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Hierarchy (Vedic Corpus Reading)
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the orthodox reading of Vedic texts, which
 *   interprets them as literally prescribing the Varna (caste) hierarchy as a
 *   divinely mandated cosmic order. This reading is a foundational element
 *   for social stratification in many traditional Hindu communities, leading
 *   to significant extraction of labor, status, and ritual authority from
 *   lower castes and women, primarily benefiting the Brahmin caste. It is
 *   actively enforced through social norms, religious injunctions, and
 *   historical legal systems. This is one reading of the
 *   'vedic_corpus_social_prescription' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.85).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Hierarchy (Vedic Corpus Reading)").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'c95cc947-3220-46e7-b736-dc172ac0fe2b').
narrative_ontology:cs_kernel_codification('c95cc947-3220-46e7-b736-dc172ac0fe2b', fixed_text).
narrative_ontology:cs_authority_grounding('c95cc947-3220-46e7-b736-dc172ac0fe2b', lineage).
narrative_ontology:cs_interpretation_layer_present('c95cc947-3220-46e7-b736-dc172ac0fe2b').
narrative_ontology:cs_reading_relation('c95cc947-3220-46e7-b736-dc172ac0fe2b', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('c95cc947-3220-46e7-b736-dc172ac0fe2b', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('c95cc947-3220-46e7-b736-dc172ac0fe2b', foundational, varna_is_divinely_mandated_social_order).
narrative_ontology:cs_axiom_status(varna_is_divinely_mandated_social_order, holdable).
narrative_ontology:cs_axiom_grounding('c95cc947-3220-46e7-b736-dc172ac0fe2b', varna_is_divinely_mandated_social_order, theological).
narrative_ontology:cs_axiom('c95cc947-3220-46e7-b736-dc172ac0fe2b', foundational, vedic_texts_are_literal_social_prescription).
narrative_ontology:cs_axiom_status(vedic_texts_are_literal_social_prescription, holdable).
narrative_ontology:cs_axiom_grounding('c95cc947-3220-46e7-b736-dc172ac0fe2b', vedic_texts_are_literal_social_prescription, conventional).
narrative_ontology:cs_reference_frame('c95cc947-3220-46e7-b736-dc172ac0fe2b', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('c95cc947-3220-46e7-b736-dc172ac0fe2b', contemporary_india_legal_framework, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c95cc947-3220-46e7-b736-dc172ac0fe2b', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, priestly_lineages).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, women_across_castes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from ritual authority, social prestige, and economic advantages derived from the Varna system. Interprets and enforces the texts, maintaining their own position at the top of the hierarchy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary,
    institutional, generational, arbitrage, regional).

% Subject to occupational restrictions, social exclusion, and ritual impurity based on their Varna. Their labor and service are extracted, and their social mobility is severely limited by the system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, generational, identity_locked, local).

% Considered outside the Varna system, facing extreme discrimination, untouchability, and economic exploitation. Their social position is enforced through violence and systemic exclusion, with virtually no exit options within the traditional framework.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, generational, trapped, local).

% Subject to patriarchal norms and restrictions on education, property rights, and autonomy, often justified by interpretations of Vedic and Dharmashastra texts. Their status is often tied to their male relatives, limiting individual agency.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, women_across_castes, payer,
    powerless, generational, identity_locked, local).

% Actively interpret, transmit, and enforce the orthodox reading of Vedic texts, ensuring the perpetuation of the Varna hierarchy. They derive significant social and economic power from this role.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, priestly_lineages, agenda_setter,
    institutional, generational, arbitrage, regional).

% Challenge the literal, prescriptive interpretation of Varna, arguing for a spiritual or metaphorical reading. They face social ostracism and institutional resistance from orthodox groups, limiting their influence within traditional religious structures.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_scholars, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social order that assigns roles and duties, theoretically ensuring cosmic harmony and social stability by aligning human society with divine will.
% TRANSFER_FUNCTION: Transfers ritual authority, social status, and economic advantage from lower castes and women to upper castes, particularly Brahmins, through inherited social roles and restrictions.
% ABSENT_VOICES: The voices of Shudra, Dalit, and women's communities, who are structurally marginalized and denied interpretive authority within the orthodox framework, would object to the prescriptive nature and extractive consequences of the Varna system.
% DISAPPEARANCE_RATIONALE: If the orthodox reading and its enforcement vanished, the social, economic, and ritual structures of many communities would undergo profound reorganization. Power dynamics would shift, and previously suppressed groups would assert new claims to status and resources, leading to significant social upheaval and redefinition of identity.
% FOUNDING_PROBLEM: To establish a divinely ordained social and cosmic order, ensuring dharma (righteous conduct) and preventing chaos by assigning specific roles and duties to different segments of society.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox priestly lineages and traditional institutions attest that the founding problem of maintaining cosmic order and dharma through Varna is still live. However, reformist scholars, human rights organizations, and Dalit activists, from outside the benefiting parties, corroborate that the 'problem' is now a justification for ongoing social injustice and extraction, not a genuine coordination function.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high due to the systemic transfer of resources and status from lower to upper castes. Suppression (0.92) is extremely high, as the system relies on severe social, economic, and ritual penalties for non-compliance, with identity-locked and trapped exit options for victims. The theater ratio is low (0.1) because the system is actively maintained and enforced, with little performative decay; its function, however extractive, is very real. Accessibility collapse is high (0.9) because alternatives to the Varna system are almost entirely foreclosed within the orthodox framework, and resistance (0.7) is significant from marginalized groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin caste and priestly lineages, this constraint is a divinely ordained, stable social order that ensures dharma. From the perspective of Shudra, Dalit, and women's communities, it is a deeply extractive and oppressive system maintained through coercion and the suppression of alternatives. The engine's classification will reflect this divergence, likely classifying it as a Snare for the victims and a Rope or even Mountain for the beneficiaries, despite the claimed type being Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin caste and priestly lineages are clear beneficiaries (d near 0.0), gaining ritual authority, social prestige, and economic advantage. Shudra and Dalit communities, along with women across castes, are primary targets (d near 1.0), bearing the brunt of occupational, social, and ritual restrictions. Reformist scholars are excluded, as their interpretations challenge the very basis of the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the Varna hierarchy a divinely mandated cosmic order, or a human-constructed social system justified by religious texts?',
    'Comparative theological and historical analysis of textual evolution and social practice, alongside the impact of modern legal frameworks on traditional interpretations.',
    'If divinely mandated, the constraint''s ''naturalness'' would be higher, potentially shifting its classification towards a Mountain for some adherents. If a social construct, its extractive nature would be more clearly exposed as a Snare, with implications for legal and social reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity between divine mandate and social construction of Varna.').

omega_variable(
    textual_interpretation_authority,
    'Who holds legitimate authority to interpret Vedic texts regarding social prescription: traditional priestly lineages, or a broader community of scholars and practitioners?',
    'Shifts in institutional power, academic consensus, and social acceptance of alternative interpretive communities over time.',
    'If interpretive authority broadens, the orthodox reading''s suppression of alternative interpretations would weaken, potentially reducing its overall suppression metric and opening pathways for reformist readings to gain traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_interpretation_authority, empirical, 'Contested authority over Vedic textual interpretation.').

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of the measured suppression is structural (external barriers, violence) versus internalized (belief in one''s ''place'', identity fusion with caste role)?',
    'Sociological studies tracking post-migration or post-legal-reform outcomes: if social and psychological barriers persist after external enforcement is removed, internalized suppression is significant.',
    'If internalized suppression is high, the effective suppression is higher than structural measures suggest, making exit more difficult even with legal changes. This would amplify the Snare classification''s severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in Varna system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement(vedi_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(vedi_tr_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1500, 0.08).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(vedi_be_t500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 500, 0.75).
narrative_ontology:measurement(vedi_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(vedi_be_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1500, 0.83).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(vedi_su_t500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 500, 0.8).
narrative_ontology:measurement(vedi_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.85).
narrative_ontology:measurement(vedi_su_t1500, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1500, 0.9).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vedic_corpus_social_prescription__orthodox_varna_reading, 0.08).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, traditional_marriage_norms).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, occupational_heredity_norms).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'vedic_corpus_social_prescription' kernel. Its high extractiveness and suppression contrast sharply with the reformist reading, which denies prescriptive social content, and the colonial reading, which sought to codify it for administrative control.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
