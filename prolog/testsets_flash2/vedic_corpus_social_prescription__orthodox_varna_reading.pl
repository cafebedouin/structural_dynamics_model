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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint represents the orthodox reading of Vedic texts that
 *   literally prescribes the Varna hierarchy as a divinely mandated cosmic
 *   order. It is a snare because it enforces a rigid social stratification
 *   system that extracts labor, deference, and ritual purity from lower
 *   castes (Shudra, Dalit) for the benefit of upper castes (Brahmin,
 *   Kshatriya, Vaishya). The system is maintained through active enforcement,
 *   social sanctions, and the suppression of alternatives, with victims
 *   having severely constrained or identity-locked exit options. This is one
 *   reading of the 'vedic_corpus_social_prescription' kernel, distinct from
 *   reformist or colonial interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.95).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.95).
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
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'd2b80ef8-b8d2-403b-96e5-22de3e084390').
narrative_ontology:cs_kernel_codification('d2b80ef8-b8d2-403b-96e5-22de3e084390', fixed_text).
narrative_ontology:cs_authority_grounding('d2b80ef8-b8d2-403b-96e5-22de3e084390', lineage).
narrative_ontology:cs_interpretation_layer_present('d2b80ef8-b8d2-403b-96e5-22de3e084390').
narrative_ontology:cs_reading_relation('d2b80ef8-b8d2-403b-96e5-22de3e084390', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('d2b80ef8-b8d2-403b-96e5-22de3e084390', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('d2b80ef8-b8d2-403b-96e5-22de3e084390', foundational, varna_is_divinely_mandated_birth_based_hierarchy).
narrative_ontology:cs_axiom_status(varna_is_divinely_mandated_birth_based_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('d2b80ef8-b8d2-403b-96e5-22de3e084390', varna_is_divinely_mandated_birth_based_hierarchy, theological).
narrative_ontology:cs_axiom('d2b80ef8-b8d2-403b-96e5-22de3e084390', secondary, social_order_requires_fixed_occupational_roles).
narrative_ontology:cs_axiom_status(social_order_requires_fixed_occupational_roles, holdable).
narrative_ontology:cs_axiom_grounding('d2b80ef8-b8d2-403b-96e5-22de3e084390', social_order_requires_fixed_occupational_roles, conventional).
narrative_ontology:cs_reference_frame('d2b80ef8-b8d2-403b-96e5-22de3e084390', ancient_vedic_dharma_shastra).
narrative_ontology:cs_drift_state('d2b80ef8-b8d2-403b-96e5-22de3e084390', contemporary_india, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d2b80ef8-b8d2-403b-96e5-22de3e084390', '').
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

% Interprets and transmits Vedic texts, performing rituals and maintaining social order. Benefits from ritual authority, social deference, and exemption from manual labor. Actively enforces the Varna system through social and religious sanctions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, generational, arbitrage, regional).

% Rules and protects society, deriving legitimacy from the divinely mandated order. Benefits from political power, land ownership, and social status. Supports the Brahminical interpretation to maintain its own position.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, kshatriya_caste, beneficiary,
    powerful, generational, constrained, regional).

% Engages in trade, agriculture, and commerce. Benefits from economic stability and social recognition within the Varna system, though subordinate to Brahmins and Kshatriyas. Pays taxes and supports the system that protects its economic activities.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, vaishya_caste, beneficiary,
    moderate, biographical, constrained, local).

% Performs manual labor and service for the upper castes. Bears the burden of social and economic restrictions, limited access to education and ritual participation, and is denied upward mobility. Identity is locked into their ascribed role.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, generational, trapped, local).

% Considered outside the Varna system, subjected to extreme social exclusion, untouchability, and economic exploitation. Bears the most severe forms of extraction and suppression, with virtually no exit options due to deeply internalized and structurally enforced discrimination.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, generational, identity_locked, local).

% Challenge the literal, prescriptive reading of Varna, advocating for social equality and a spiritual interpretation of Vedic texts. Their voices are often marginalized or actively suppressed by orthodox institutions, but they persist in organizing and advocating for change.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_movements, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social hierarchy that assigns roles and duties, theoretically ensuring social stability and cosmic harmony by aligning human society with a divinely ordained order.
% TRANSFER_FUNCTION: Transfers labor, deference, and ritual purity from lower castes (Shudra, Dalit) to upper castes (Brahmin, Kshatriya, Vaishya), consolidating social, economic, and ritual power at the top of the hierarchy.
% ABSENT_VOICES: Dalit and Shudra communities, historically and presently, would object to the divine mandate and inherent justice of the Varna system, advocating for equality and dignity. Their voices are suppressed through social ostracization, economic dependency, and denial of access to interpretive authority.
% DISAPPEARANCE_RATIONALE: If the belief in divinely mandated Varna hierarchy vanished overnight, the entire social, economic, and ritual structure of traditional Hindu society would collapse. Labor relations, marriage patterns, political power, and religious practices would undergo radical transformation, leading to a complete reorganization of society.
% FOUNDING_PROBLEM: To establish a stable, divinely sanctioned social order that assigns specific roles and duties to different groups, ensuring cosmic harmony and preventing social chaos.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious authorities and traditionalists attest that the problem of maintaining cosmic order and social dharma is still live, requiring the Varna system. Reformist scholars and social justice advocates, from outside the benefiting parties, contest this, arguing the 'problem' is a justification for exploitation, not a genuine societal need.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.92) due to the systemic and intergenerational transfer of resources and status from lower to upper castes. Suppression is also very high (0.95) because the system is deeply embedded in social, religious, and economic structures, actively punishing dissent or attempts at upward mobility. Theater ratio is low (0.1) as the system is genuinely functional in its extractive and suppressive capacity, with little performative maintenance for a non-existent function. Accessibility collapse is high (0.9) as alternatives are almost entirely foreclosed by the pervasive social and religious enforcement. Resistance is moderate (0.7) reflecting ongoing, though often suppressed, challenges from marginalized communities and reformist movements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahmin caste, this constraint is a divinely ordained, stable social order that ensures cosmic harmony and their own rightful place at the top. From the perspective of Shudra and Dalit communities, it is a brutal system of exploitation and oppression, enforced through religious dogma and social coercion. The engine's classification will reflect this divergence, computing a snare for the victims and a highly beneficial, stable structure for the agenda-setters.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin, Kshatriya, and Vaishya castes are beneficiaries, with Brahmins acting as the primary agenda-setters and interpreters of the system, deriving maximum benefit. The Shudra and Dalit communities are the primary victims, bearing the brunt of extraction and suppression, with their directionality firmly at the target end due to trapped and identity-locked exit options. Reformist movements are excluded, as their interpretations challenge the very foundation of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the Varna system as a 'rope' or 'mountain' by highlighting its active enforcement, clear beneficiaries, and identifiable victims. The high extractiveness and suppression, coupled with the contested founding problem status, indicate that while it claims divine mandate, its persistence relies on coercion and the suppression of alternatives, not on universal benefit or natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_mandate_vs_social_construct,
    'Is the Varna hierarchy a divinely mandated cosmic order, or a human-made social construct that benefits specific groups?',
    'Comparative religious studies, historical sociology, and critical textual analysis to trace the evolution of Varna concepts and their implementation, alongside theological arguments for or against divine origin.',
    'If a social construct, the constraint''s ''emerges_naturally'' claim is false, and its classification as a snare is reinforced. If genuinely divinely mandated, its ''mountain'' claim would gain credence, though its extractive nature would still be measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_mandate_vs_social_construct, conceptual, 'Ambiguity between divine mandate and social construction of Varna.').

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of the measured suppression is due to external structural barriers (economic, legal, social) versus internalized beliefs (self-concept, fatalism, identity fusion with ascribed role) within victim communities?',
    'Sociological studies on post-reform mobility, psychological impact assessments, and ethnographic research on identity formation within marginalized communities. If suppression persists after external barriers are removed, it indicates internalized components.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, as victims carry the constraint within them, making exit even harder. This would deepen the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for Varna victims.').

omega_variable(
    textual_interpretation_ambiguity,
    'To what extent do the Vedic texts unambiguously prescribe a rigid, birth-based Varna hierarchy, versus offering more fluid or metaphorical interpretations?',
    'Philological analysis of original Sanskrit texts, comparative study of ancient commentaries, and engagement with diverse interpretive traditions (e.g., Mimamsa, Vedanta, Bhakti movements) to assess textual ambiguity and interpretive latitude.',
    'If texts are ambiguous, the ''orthodox_varna_reading'' relies more heavily on interpretive authority and social power than textual fidelity, weakening its claim to ''fixed_text'' kernel codification and reinforcing its ''extraction'' authority grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_interpretation_ambiguity, conceptual, 'Ambiguity in Vedic textual prescription of Varna hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t20, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(vedi_tr_t60, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(vedi_tr_t100, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(vedi_be_t20, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 40, 0.9).
narrative_ontology:measurement(vedi_be_t60, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 60, 0.91).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 80, 0.92).
narrative_ontology:measurement(vedi_be_t100, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 100, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(vedi_su_t20, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 20, 0.92).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 40, 0.93).
narrative_ontology:measurement(vedi_su_t60, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 60, 0.94).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 80, 0.95).
narrative_ontology:measurement(vedi_su_t100, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription__colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_corpus_social_prescription' kernel. This 'orthodox_varna_reading' asserts a literal, prescriptive interpretation of Varna hierarchy, distinct from reformist or colonial interpretations. Each reading is modeled as a separate constraint due to differing epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
