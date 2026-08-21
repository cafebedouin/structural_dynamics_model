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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Hierarchy as Divinely Mandated Cosmic Order
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the orthodox reading of Vedic texts that
 *   literally prescribe the Varna (caste) hierarchy as a divinely mandated
 *   cosmic order. This reading asserts that social stratification,
 *   occupational roles, and ritual purity are fixed by birth and are
 *   essential for cosmic harmony. It functions as a snare by extracting labor
 *   and deference from lower castes while concentrating power and privilege
 *   in upper castes, enforced through religious doctrine, social ostracism,
 *   and historical violence. The claim of 'divine mandate' serves as a
 *   powerful legitimizing narrative for a highly extractive social structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.85).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Hierarchy as Divinely Mandated Cosmic Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, '0ba72971-763d-4f89-a7fe-e0f9cd9858cd').
narrative_ontology:cs_kernel_codification('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', fixed_text).
narrative_ontology:cs_authority_grounding('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', lineage).
narrative_ontology:cs_interpretation_layer_present('0ba72971-763d-4f89-a7fe-e0f9cd9858cd').
narrative_ontology:cs_reading_relation('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', foundational, varna_hierarchy_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_hierarchy_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', varna_hierarchy_divinely_ordained, theological).
narrative_ontology:cs_axiom('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', foundational, ritual_purity_social_order_intertwined).
narrative_ontology:cs_axiom_status(ritual_purity_social_order_intertwined, holdable).
narrative_ontology:cs_axiom_grounding('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', ritual_purity_social_order_intertwined, conventional).
narrative_ontology:cs_reference_frame('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', ancient_vedic_social_order).
narrative_ontology:cs_drift_state('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', contemporary_india, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0ba72971-763d-4f89-a7fe-e0f9cd9858cd', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds primary ritual authority, interprets sacred texts, and benefits from social deference and control over religious practices. Their identity is deeply intertwined with maintaining the Varna system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from political and military power, justified by their Varna status. They enforce the social order through state power and tradition, maintaining their privileged position.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste, beneficiary,
    powerful, generational, constrained, national).

% Benefits from economic opportunities and social standing within the Varna system, often controlling trade and agriculture. Their status is secured by the hierarchy, though they are subordinate to Brahmins and Kshatriyas.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste, beneficiary,
    moderate, biographical, constrained, local).

% Bears the burden of manual labor and service, with limited social mobility, educational access, or ritual participation. Their labor is extracted, and their social status is fixed by birth.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, immediate, trapped, local).

% Suffer extreme social exclusion, untouchability, and forced labor, performing tasks deemed ritually impure. They are outside the Varna system, facing severe discrimination and violence for any attempt to exit or resist.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, excluded).

% The primary interpreters and enforcers of the Varna system, deriving their authority from their lineage and textual knowledge. They actively resist any reinterpretation that challenges the literal social prescription.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, orthodox_scholars_priests, agenda_setter,
    institutional, generational, identity_locked, national).

% Actively challenge the Varna hierarchy, advocating for social equality and alternative interpretations of Vedic texts. They face significant resistance and suppression from orthodox institutions but represent a persistent counter-narrative.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_movements, excluded,
    organized, generational, constrained, national).

% Modern legal frameworks in India formally outlaw caste discrimination, creating a tension between traditional religious authority and state law. They observe and adjudicate cases of discrimination, but enforcement is often incomplete.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, secular_legal_systems, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social order and division of labor, theoretically ensuring cosmic harmony and ritual purity through prescribed roles and duties for each Varna.
% TRANSFER_FUNCTION: Transfers labor, deference, and ritual services from lower castes (Shudra, Dalit) to upper castes (Brahmin, Kshatriya, Vaishya), while concentrating social, economic, and ritual power in the upper echelons.
% ABSENT_VOICES: Dalit communities and other marginalized groups, whose historical and ongoing experiences of oppression are systematically excluded from the dominant narrative of divine order. Reformist and anti-caste movements are actively suppressed or dismissed as illegitimate interpretations.
% DISAPPEARANCE_RATIONALE: If the belief in divinely mandated Varna hierarchy and its enforcement vanished overnight, the social, economic, and political structures of many communities would undergo profound reorganization. Labor relations, access to resources, and social status would be fundamentally challenged, leading to a reordering of power dynamics and a potential collapse of traditional authority structures.
% FOUNDING_PROBLEM: To establish a stable, ritually pure, and cosmically aligned social order, ensuring the proper functioning of society through a hierarchical division of labor and spiritual roles.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox adherents and traditional institutions attest that the founding problem of maintaining cosmic order and ritual purity is still live. However, reformist movements, secular legal systems, and historical analysis from outside the benefiting parties (e.g., Ambedkarite scholarship, sociological studies) corroborate that the 'problem' has largely shifted to maintaining social control and extraction, rather than genuine cosmic harmony.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.85) due to the systematic transfer of labor, resources, and social capital from lower to upper castes, enforced by birthright. Suppression is extremely high (0.92) because exit options are virtually non-existent for those at the bottom of the hierarchy, with severe social, economic, and physical penalties for non-compliance or attempts at upward mobility. Accessibility collapse is high (0.88) as alternatives to the prescribed social order are systematically denied or made impossible. Resistance is also high (0.75), reflecting centuries of anti-caste movements and individual acts of defiance, despite severe suppression. Theater ratio is moderate (0.60) as ritual performances and religious justifications are crucial for maintaining the legitimacy of the system, even as its functional aspects become increasingly challenged by modern legal and social norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the upper castes and orthodox interpreters, the Varna system is a natural, divinely ordained, and beneficial social order that ensures stability and cosmic harmony. From the perspective of the Shudra and Dalit communities, it is a system of profound oppression, exploitation, and dehumanization. The engine's classification as a snare reflects the latter, structurally accurate, perspective, despite the internal 'rope' framing of its beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin, Kshatriya, and Vaishya castes are clear beneficiaries, deriving social status, economic advantage, and ritual authority from the system. The Shudra caste and Dalit communities are the primary victims, subjected to labor extraction, ritual exclusion, and severe discrimination. Orthodox scholars and priests act as agenda-setters, interpreting and enforcing the texts to maintain the hierarchy. Reformist movements and secular legal systems are observers or excluded parties, challenging the constraint's legitimacy and seeking its dismantling.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, as claimed by orthodox adherents, is to maintain cosmic order and ritual purity. However, its persistence is increasingly driven by the benefits of social control and economic extraction for the upper castes, rather than a genuine, universally acknowledged coordination function. The high extractiveness and suppression, coupled with significant resistance, indicate that the coordination story is largely a cover for a coercive system. The 'divine mandate' acts as a powerful ideological lock-in mechanism, making it difficult for beneficiaries to acknowledge the system's extractive nature without undermining their own identity and authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the Varna hierarchy a divinely mandated cosmic order, or a human-constructed social system legitimized by religious texts?',
    'Theological and philosophical debate, historical-critical textual analysis, and the observed persistence of the system in the absence of universal belief in its divine origin.',
    'If a social construct, the ''divine mandate'' claim functions purely as a legitimizing cover for extraction, increasing the effective extractiveness and suppression. If genuinely divine, the constraint''s ''naturalness'' would be higher, though its extractive effects would remain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity of the Varna system''s origin and legitimacy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (economic dependency, social ostracism, violence) or internalized (belief in karma, dharma, or one''s ''place'')?',
    'Post-exit suppression trajectory: if individuals or communities continue to self-regulate according to Varna norms after structural barriers are removed (e.g., through migration to urban areas or legal protections), it indicates a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them after apparent ''exit'' from direct enforcement. This makes the snare more resilient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the Varna system.').

omega_variable(
    historical_enforcement_vs_textual_ideal,
    'To what extent has the historical practice of the Varna system deviated from or exaggerated the textual prescriptions, and how much of the extraction is due to this deviation?',
    'Comparative historical sociology and textual scholarship examining the evolution of caste practices versus scriptural ideals across different periods and regions.',
    'If historical practice significantly amplified extraction beyond textual ideals, it suggests a greater degree of human agency and opportunistic layering of extraction onto a foundational religious framework, further solidifying its snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_enforcement_vs_textual_ideal, empirical, 'Gap between textual ideal and historical practice in Varna enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 1000, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(vedi_tr_t1300, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1300, 0.45).
narrative_ontology:measurement(vedi_tr_t1600, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1600, 0.5).
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1800, 0.55).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 1947, 0.6).
narrative_ontology:measurement(vedi_tr_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1000, 0.75).
narrative_ontology:measurement(vedi_be_t1300, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1300, 0.8).
narrative_ontology:measurement(vedi_be_t1600, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1600, 0.85).
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1800, 0.88).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 1947, 0.82).
narrative_ontology:measurement(vedi_be_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1000, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1000, 0.8).
narrative_ontology:measurement(vedi_su_t1300, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1300, 0.85).
narrative_ontology:measurement(vedi_su_t1600, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1600, 0.9).
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1800, 0.95).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 1947, 0.88).
narrative_ontology:measurement(vedi_su_t2024, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
