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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   This constraint describes the colonial-orientalist project of
 *   constructing a unified, timeless 'Hindu law' system from diverse Vedic
 *   and Dharmashastra texts for administrative governance in British India.
 *   This reading served the colonial need for legal legibility and control,
 *   crystallizing fluid social practices into rigid categories for census,
 *   taxation, and adjudication. It is classified as a scaffold because its
 *   administrative function was temporary, tied to the duration of colonial
 *   rule, even if the codified law was intended to be permanent by its
 *   creators. The 'has_sunset_clause: true' reflects this analytical
 *   temporality, as the administrative framework it supported ceased with the
 *   end of colonial rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.65).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.78).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Codification of 'Hindu Law' from Vedic/Dharmashastra Texts").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, 'b70bb20e-775c-4b1a-b132-0d2d29518384').
narrative_ontology:cs_kernel_codification('b70bb20e-775c-4b1a-b132-0d2d29518384', fixed_text).
narrative_ontology:cs_authority_grounding('b70bb20e-775c-4b1a-b132-0d2d29518384', extraction).
narrative_ontology:cs_interpretation_layer_present('b70bb20e-775c-4b1a-b132-0d2d29518384').
narrative_ontology:cs_reading_relation('b70bb20e-775c-4b1a-b132-0d2d29518384', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('b70bb20e-775c-4b1a-b132-0d2d29518384', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('b70bb20e-775c-4b1a-b132-0d2d29518384', foundational, vedic_texts_are_unified_law).
narrative_ontology:cs_axiom_status(vedic_texts_are_unified_law, holdable).
narrative_ontology:cs_axiom_grounding('b70bb20e-775c-4b1a-b132-0d2d29518384', vedic_texts_are_unified_law, conventional).
narrative_ontology:cs_axiom('b70bb20e-775c-4b1a-b132-0d2d29518384', foundational, social_order_is_codifiable).
narrative_ontology:cs_axiom_status(social_order_is_codifiable, holdable).
narrative_ontology:cs_axiom_grounding('b70bb20e-775c-4b1a-b132-0d2d29518384', social_order_is_codifiable, conventional).
narrative_ontology:cs_reference_frame('b70bb20e-775c-4b1a-b132-0d2d29518384', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('b70bb20e-775c-4b1a-b132-0d2d29518384', post_independence_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b70bb20e-775c-4b1a-b132-0d2d29518384', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The British colonial government, which sought to create a uniform, legible legal system for administrative control, taxation, and adjudication across diverse Indian populations. This constraint provided the 'legal' basis for their governance.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% European scholars who interpreted, translated, and codified Sanskrit texts, establishing their academic authority and careers by presenting a 'unified Hindu law' to the colonial administration. Their interpretations often shaped colonial policy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_scholars, beneficiary,
    powerful, biographical, mobile, global).

% The diverse populations of British India, whose fluid, localized social and legal practices were forcibly replaced or rigidly codified into a 'Hindu law' system, often resulting in loss of autonomy, social mobility, and traditional rights, particularly for marginalized groups.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, regional).

% The pre-colonial, diverse, and often fluid legal and social customs, traditions, and interpretive communities that were either ignored, suppressed, or distorted by the colonial codification project. Their internal coherence and adaptability were undermined.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_legal_traditions, excluded,
    organized, generational, identity_locked, local).

% Indian thinkers and activists who, while often seeking social reform, critically engaged with and challenged the colonial-orientalist construction of 'Hindu law,' advocating for indigenous agency and a more nuanced understanding of their own traditions.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_indian_intellectuals, observer,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a seemingly unified and authoritative legal framework for the colonial administration to govern diverse Indian populations, enabling consistent application of law, taxation, and census-taking across vast territories.
% TRANSFER_FUNCTION: Transferred legal and social authority from diverse indigenous interpretive communities and fluid local practices to the centralized colonial state and its appointed legal interpreters, extracting administrative control and legitimacy for colonial rule.
% ABSENT_VOICES: The diverse, localized, and often oral indigenous legal traditions and their practitioners were largely excluded from the codification process, their nuanced understandings replaced by rigid, textual interpretations. They would have argued for the fluidity and context-dependence of dharma.
% DISAPPEARANCE_RATIONALE: If this colonial construction of 'Hindu law' had vanished overnight, the entire administrative and legal apparatus of British India, as it pertained to personal law, would have collapsed, forcing a return to or reinvention of diverse local legal systems, or a different, non-colonial form of codification.
% FOUNDING_PROBLEM: The colonial administration faced the problem of governing a vast, diverse population with no single, universally recognized legal code, leading to administrative inefficiency and perceived lack of control. They sought a 'timeless' and 'unified' system to impose order.
% FOUNDING_PROBLEM_CORROBORATION: Historians of colonial India, post-colonial legal scholars, and critical theorists attest that the 'founding problem' was primarily one of colonial administrative convenience and control, not an inherent lack of legal order in pre-colonial India. This perspective is widely accepted outside of colonial-era administrative records.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__colonial_orientalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the system imposed significant costs on colonized subjects by disrupting existing social structures and legal autonomy. Suppression is high (0.78) due to the coercive power of the colonial state in enforcing this codified law and suppressing alternative legal traditions. Theater ratio is low (0.10) because the administrative function, while based on a flawed premise, was genuinely operational and central to colonial governance, not merely performative. Accessibility collapse is moderate (0.60) as it significantly limited existing alternatives, but some local practices persisted informally. Resistance is high (0.70) reflecting ongoing challenges to colonial rule and its legal impositions.
 *
 * PERSPECTIVAL GAP:
 *   From the colonial administration's perspective, this was a necessary and benevolent act of bringing order and justice. From the perspective of colonized subjects and indigenous traditions, it was an act of cultural imposition and extraction. The analytical classification as a scaffold highlights its temporary administrative function, which would sunset with the end of colonial rule, a perspective not shared by the colonizers themselves.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration and orientalist scholars are clear beneficiaries, gaining administrative control, academic authority, and career advancement. Colonized legal subjects are the primary victims, bearing the costs of imposed rigidity and loss of autonomy. Indigenous legal traditions are excluded, their fluidity suppressed. Reformist Indian intellectuals act as observers, critically analyzing the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy_of_unification,
    'To what extent did pre-colonial Indian legal and social practices genuinely constitute a unified, timeless ''Hindu law'' system, as opposed to diverse, fluid, and localized traditions?',
    'Further historical and anthropological research into pre-colonial legal pluralism, local customs, and the actual application of Dharmashastra texts in practice, independent of colonial interpretations.',
    'If pre-colonial practices were highly diverse and fluid, it would further undermine the ''naturalness'' claim of the colonial reading, increasing its perceived extractiveness and suppression, and strengthening the ''constructed'' aspect of the scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_accuracy_of_unification, empirical, 'Ambiguity regarding the historical unity and timelessness of ''Hindu law'' prior to colonial codification.').

omega_variable(
    legitimacy_of_administrative_codification,
    'Was the administrative codification of ''Hindu law'' a legitimate act of governance, or primarily a tool for colonial control and resource extraction?',
    'Analysis of the outcomes for different social groups, the motivations of colonial administrators, and the long-term impact on social mobility and justice, from a post-colonial ethical framework.',
    'If primarily a tool for extraction, the ''scaffold'' classification''s extractiveness would be confirmed as high, and the coordination function would be seen as a cover for rent-seeking. If some genuine coordination benefits are identified, the extractiveness might be slightly lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_administrative_codification, conceptual, 'Ambiguity regarding the primary purpose and ethical legitimacy of the colonial codification project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 1800, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1800, 0.15).
narrative_ontology:measurement(vedi_tr_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1830, 0.12).
narrative_ontology:measurement(vedi_tr_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(vedi_tr_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1890, 0.08).
narrative_ontology:measurement(vedi_tr_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1920, 0.09).
narrative_ontology:measurement(vedi_tr_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 1947, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1800, 0.5).
narrative_ontology:measurement(vedi_be_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1830, 0.58).
narrative_ontology:measurement(vedi_be_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1860, 0.62).
narrative_ontology:measurement(vedi_be_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1890, 0.65).
narrative_ontology:measurement(vedi_be_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1920, 0.67).
narrative_ontology:measurement(vedi_be_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 1947, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1800, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1800, 0.65).
narrative_ontology:measurement(vedi_su_t1830, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1830, 0.7).
narrative_ontology:measurement(vedi_su_t1860, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1860, 0.75).
narrative_ontology:measurement(vedi_su_t1890, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1890, 0.78).
narrative_ontology:measurement(vedi_su_t1920, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1920, 0.8).
narrative_ontology:measurement(vedi_su_t1947, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 1947, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, post_colonial_caste_legislation).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_spiritual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vedic_corpus_social_prescription' kernel. Its structural influence on post-colonial legislation and other readings is significant, as it established a legal precedent and interpretive framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
