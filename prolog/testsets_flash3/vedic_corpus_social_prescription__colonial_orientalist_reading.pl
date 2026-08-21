% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__colonial_orientalist_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__colonial_orientalist_reading
 *   human_readable: Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Texts
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint describes the colonial-orientalist reading of
 *   Vedic/Dharmashastra texts, which posited a unified, timeless 'Hindu law'
 *   system for administrative codification. This reading served the colonial
 *   administration's need for legible legal subjects and simplified
 *   governance, transforming fluid social practices into rigid, legally
 *   enforced categories. It is a scaffold because it was a transitional
 *   measure for colonial rule, intended to provide a stable legal foundation,
 *   but it also had a sunset (the end of colonial rule). The metrics reflect
 *   its moderate extractiveness (crystallizing social fluidity into fixed
 *   categories for administrative control) and high suppression (actively
 *   replacing indigenous legal traditions).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.45).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.7).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Texts").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2').
narrative_ontology:cs_kernel_codification('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', fixed_text).
narrative_ontology:cs_authority_grounding('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', extraction).
narrative_ontology:cs_interpretation_layer_present('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2').
narrative_ontology:cs_reading_relation('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', foundational, vedic_texts_as_unified_legal_code).
narrative_ontology:cs_axiom_status(vedic_texts_as_unified_legal_code, holdable).
narrative_ontology:cs_axiom_grounding('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', vedic_texts_as_unified_legal_code, conventional).
narrative_ontology:cs_axiom('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', foundational, codification_for_administrative_legibility).
narrative_ontology:cs_axiom_status(codification_for_administrative_legibility, holdable).
narrative_ontology:cs_axiom_grounding('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', codification_for_administrative_legibility, instrumental).
narrative_ontology:cs_reference_frame('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', post_independence_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('225d6cb2-2f0a-424d-bcc9-29a05ed0c9f2', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_brahmin_pundits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seeks to establish a uniform, legible legal system for administrative control, taxation, and adjudication across diverse indigenous populations. Benefits from the simplification and categorization of complex, fluid social structures into fixed 'Hindu law'.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, regional).

% Provide the intellectual framework and textual interpretations that justify the codification. Gain academic prestige, funding, and influence within the colonial project by presenting a 'unified' and 'timeless' Hindu legal system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, beneficiary,
    powerful, biographical, mobile, global).

% Are subjected to a new, rigid legal system that often misrepresents or ignores their actual customary laws and social practices. Experience loss of autonomy, social mobility, and traditional dispute resolution mechanisms. Their identities are fixed into administrative categories like 'caste'.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, local).

% Represent the diverse, fluid, and often localized customary laws and social norms that existed prior to colonial intervention. These traditions are marginalized, suppressed, and often replaced by the codified 'Hindu law', leading to their atrophy or disappearance.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions, excluded,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_non_agent(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions).

% Are elevated by the colonial administration as authoritative interpreters of the 'Hindu law', often at the expense of other local authorities. Their interpretations, often drawn from specific Dharmashastra texts, gain state sanction and become legally binding, reinforcing their social and religious authority.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_brahmin_pundits, beneficiary,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to create a uniform, predictable legal framework for administrative governance, taxation, and dispute resolution across a vast and diverse colonial territory, replacing a multitude of fluid, localized customary laws.
% TRANSFER_FUNCTION: Transfers legal authority and interpretive power from diverse indigenous communities and local traditions to the colonial administration and its appointed interpreters (often orthodox Brahmin pundits), while fixing social identities for administrative convenience.
% ABSENT_VOICES: The diverse, localized customary legal traditions and their practitioners are largely excluded from the codification process; their nuanced, context-dependent systems are replaced by a rigid, textualist interpretation. Many colonized subjects whose social realities did not fit the codified categories also had no voice.
% DISAPPEARANCE_RATIONALE: If the colonial codification vanished, the administrative and legal structures of the colonial state would collapse, requiring a complete re-establishment of legal authority and social ordering. Indigenous legal traditions, though suppressed, might re-emerge or evolve, and social identities would regain fluidity.
% FOUNDING_PROBLEM: The colonial administration faced challenges in governing a vast, diverse territory with a multitude of uncodified, often fluid, and locally specific legal and social practices, leading to administrative inefficiency and perceived lack of control.
% FOUNDING_PROBLEM_CORROBORATION: The colonial administration initially attested the problem was live. However, post-colonial scholarship and indigenous historians widely corroborate that the 'problem' was largely a construct of colonial administrative needs, and the resulting 'solution' created more social rigidity and injustice than it resolved. The original administrative problem is dead, but the codified structures persist in post-colonial legal systems.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).
:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the codification imposed a new, often alien, legal framework that extracted autonomy and flexibility from colonized subjects, but it also provided a degree of administrative order. Suppression is high (0.7) due to the active replacement and marginalization of diverse indigenous legal traditions by a single, state-sanctioned 'Hindu law'. The theater ratio is low (0.2) because the codification was genuinely functional for colonial administration, even if its 'authenticity' claims were performative. The scaffold classification reflects its transitional nature for colonial governance, with an implicit sunset at the end of colonial rule.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administration's perspective, this was a necessary and rational act of governance, a 'scaffold' to bring order. From the perspective of colonized subjects and indigenous legal traditions, it was an act of imposition and extraction, a 'snare' that fixed their identities and suppressed their autonomy. The scaffold classification here reflects the colonial intent and its eventual sunset, while the metrics capture the extractive and suppressive reality for the victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration and orientalist scholars are beneficiaries, gaining administrative control and academic prestige, respectively. Orthodox Brahmin pundits also benefit from their elevated status as interpreters. Colonized legal subjects are the primary victims, losing autonomy and being subjected to rigid, often misrepresentative, legal categories. Indigenous legal traditions are excluded and suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_of_codified_law,
    'To what extent did the codified ''Hindu law'' genuinely reflect the diverse, pre-colonial legal and social practices, versus being a colonial construct for administrative convenience?',
    'Extensive historical and anthropological research comparing colonial legal records with pre-colonial customary law, local judicial practices, and social histories.',
    'If largely a colonial construct, the extractiveness and suppression metrics would be higher, emphasizing the imposition over any genuine coordination. If it genuinely reflected pre-existing systems, the scaffold function would be more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authenticity_of_codified_law, empirical, 'Assesses the historical fidelity of colonial ''Hindu law'' to indigenous practices.').

omega_variable(
    post_colonial_persistence_mandate,
    'Does the persistence of elements of this codified ''Hindu law'' in post-colonial legal systems represent a continued mandate or an inertial legacy of colonial power?',
    'Analysis of post-independence legal reforms, judicial interpretations, and public discourse regarding the legitimacy and relevance of these laws in contemporary society.',
    'If it''s an inertial legacy, the constraint''s post-colonial classification would drift towards a Piton or Snare, as its original (colonial) mandate is dead. If a new, legitimate mandate has emerged, it might retain a Scaffold or Rope classification under new terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_persistence_mandate, conceptual, 'Examines the mandate for the continued existence of codified ''Hindu law'' post-independence.').

omega_variable(
    interpretive_authority_shift,
    'How did the colonial codification permanently alter the locus of interpretive authority for ''Hindu law'' – from diverse local traditions to state-sanctioned textual interpretations?',
    'Sociological and legal studies tracing the evolution of legal education, judicial appointments, and the decline of traditional legal scholars and community-based dispute resolution mechanisms.',
    'A significant shift towards centralized, state-sanctioned authority would underscore the suppressive nature of the constraint and its long-term impact on indigenous legal pluralism, potentially increasing its effective extractiveness for colonized subjects even after the colonial period.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_authority_shift, empirical, 'Analyzes the long-term impact of codification on interpretive authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1772, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1772, 0.1).
narrative_ontology:measurement(vedi_tr_t1820, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(vedi_tr_t1870, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1870, 0.2).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1772, 0.3).
narrative_ontology:measurement(vedi_be_t1820, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1820, 0.4).
narrative_ontology:measurement(vedi_be_t1870, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1870, 0.45).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1772, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1772, 0.5).
narrative_ontology:measurement(vedi_su_t1820, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1820, 0.6).
narrative_ontology:measurement(vedi_su_t1870, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1870, 0.7).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
