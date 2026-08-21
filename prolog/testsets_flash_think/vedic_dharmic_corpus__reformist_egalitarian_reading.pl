% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Vedic/Dharmic Corpus: Reformist Egalitarian Reading
 *   domain: religious/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'reformist egalitarian' reading of the
 *   Vedic/Dharmic corpus, which asserts that textual meaning must conform to
 *   constitutional equality principles, caste hierarchy is a historical
 *   accretion rather than scriptural essence, and rational critique
 *   supersedes traditional authority. This reading is in active contest with
 *   traditional interpretations and is often supported by secular state
 *   apparatuses and marginalized communities. The claimed type is
 *   'tangled_rope' because it genuinely attempts to coordinate the tradition
 *   with modern values, but in doing so, it extracts authority and privilege
 *   from traditional institutions and requires active enforcement against
 *   entrenched social practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.65).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Vedic/Dharmic Corpus: Reformist Egalitarian Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '86970cc6-1c25-40d5-82ad-c925cecad0b4').
narrative_ontology:cs_kernel_codification('86970cc6-1c25-40d5-82ad-c925cecad0b4', fixed_text).
narrative_ontology:cs_authority_grounding('86970cc6-1c25-40d5-82ad-c925cecad0b4', expertise).
narrative_ontology:cs_interpretation_layer_present('86970cc6-1c25-40d5-82ad-c925cecad0b4').
narrative_ontology:cs_reading_relation('86970cc6-1c25-40d5-82ad-c925cecad0b4', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('86970cc6-1c25-40d5-82ad-c925cecad0b4', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('86970cc6-1c25-40d5-82ad-c925cecad0b4', foundational, textual_meaning_subordinate_to_equality).
narrative_ontology:cs_axiom_status(textual_meaning_subordinate_to_equality, holdable).
narrative_ontology:cs_axiom_grounding('86970cc6-1c25-40d5-82ad-c925cecad0b4', textual_meaning_subordinate_to_equality, conventional).
narrative_ontology:cs_axiom('86970cc6-1c25-40d5-82ad-c925cecad0b4', foundational, caste_is_social_construct).
narrative_ontology:cs_axiom_status(caste_is_social_construct, holdable).
narrative_ontology:cs_axiom_grounding('86970cc6-1c25-40d5-82ad-c925cecad0b4', caste_is_social_construct, empirically_contingent).
narrative_ontology:cs_reference_frame('86970cc6-1c25-40d5-82ad-c925cecad0b4', constitutional_egalitarianism).
narrative_ontology:cs_drift_state('86970cc6-1c25-40d5-82ad-c925cecad0b4', contemporary_struggle, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86970cc6-1c25-40d5-82ad-c925cecad0b4', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_intellectuals_and_activists).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_indian_state).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_religious_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_brahminical_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, general_devotees).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, general_devotees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively reinterpret the Vedic/Dharmic corpus to align with modern egalitarian and constitutional principles. They face social backlash and academic resistance but gain legitimacy from secular institutions and marginalized communities. Their influence is intellectual and social, often seeking legal backing.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, reformist_intellectuals_and_activists, agenda_setter,
    organized, generational, constrained, national).

% Benefit directly from interpretations that dismantle caste hierarchy and affirm their equality. Their struggle for dignity and rights is deeply intertwined with the success of this reformist reading, making exit from the interpretive contest difficult without abandoning their core identity and goals.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, identity_locked, national).

% Supports interpretations that conform to its constitutional mandate of equality and non-discrimination. It provides legal and institutional backing for anti-caste reforms, implicitly endorsing readings that align with its secular values. Its role is to enforce equality, which often means challenging traditional religious authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_indian_state, agenda_setter,
    institutional, civilizational, mobile, national).

% Bear the costs of this reading through diminished authority, challenged traditions, and potential loss of followers or state patronage. They actively resist these interpretations, defending hereditary privilege and traditional scriptural readings, but are constrained by legal frameworks and changing social norms.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_religious_institutions, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_religious_institutions, agenda_setter).

% Experience a direct challenge to their hereditary ritual and interpretive monopoly. Their identity and social status are deeply tied to traditional caste-based interpretations, making any concession to reformist readings a fundamental threat to their self-conception and power base.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_brahminical_authorities, payer,
    powerful, generational, identity_locked, national).

% Are caught between traditional teachings and modern egalitarian values. They may benefit from a more inclusive religious practice but also bear the social costs of challenging established norms or the cognitive dissonance of holding conflicting beliefs. Their choices are shaped by local community pressures.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, general_devotees, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__reformist_egalitarian_reading, general_devotees, beneficiary).

% Analyze the legal and social implications of religious interpretations, particularly concerning equality and human rights. They provide critical commentary and legal arguments that inform the state's position and the reformist movements, operating from an external, analytical perspective.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for interpreting the Vedic/Dharmic corpus in a manner consistent with modern constitutional equality principles, enabling the religious tradition to maintain relevance and legitimacy in a secular, democratic society while addressing historical injustices.
% TRANSFER_FUNCTION: Transfers interpretive authority from hereditary lineage and rigid tradition to rational critique and constitutional principles; transfers social legitimacy from caste-based hierarchies to egalitarian values; and transfers state support from traditional religious institutions to those aligning with secular equality.
% ABSENT_VOICES: Those who advocate for the complete abandonment of the Vedic/Dharmic tradition due to its historical association with caste, arguing that reform is insufficient or impossible. They would call for a break from the tradition rather than its reinterpretation.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, the Vedic/Dharmic corpus would either be entirely rejected by significant segments of modern society as inherently discriminatory, or it would be fully re-captured by hereditary/orthodox interpretations, leading to intensified social conflict, legal challenges, and a crisis of legitimacy for the tradition within a secular state.
% FOUNDING_PROBLEM: The profound conflict between ancient scriptural interpretations (especially those supporting caste hierarchy) and the modern constitutional principles of equality, human dignity, and non-discrimination, leading to persistent social injustice and the delegitimization of the religious tradition in a secular, democratic nation.
% FOUNDING_PROBLEM_CORROBORATION: Dalit rights organizations, secular legal scholars, human rights activists, and independent sociological studies consistently corroborate the ongoing nature of caste discrimination and the necessity of reformist interpretations to address this foundational problem. Legislative debates and court rulings also attest to its live status.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it challenges traditional power, it also seeks to preserve the tradition's legitimacy. Suppression (0.65) is high due to the active legal and social enforcement required to counter deeply entrenched caste practices and orthodox resistance. Theater ratio (0.20) is low, reflecting a genuine, ongoing struggle for reinterpretation and social change, rather than mere performance. Accessibility collapse (0.40) indicates that while this reading opens alternatives for many, it also challenges and seeks to collapse the interpretive monopoly of traditional authorities. Resistance (0.75) is high, reflecting the strong opposition from orthodox institutions and traditionalists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformist intellectuals and Dalit movements, this reading is a necessary 'rope' for social justice and the survival of the tradition. From the perspective of orthodox institutions, it is a 'snare' that undermines divine order and their legitimate authority. The engine's computation of 'tangled_rope' reflects the structural reality of both coordination and extraction, which is actively enforced.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit movements and reformist intellectuals are primary beneficiaries, gaining social and interpretive leverage. The secular Indian state also benefits by aligning religious practice with its constitutional values. Orthodox religious institutions and traditional Brahminical authorities are the primary targets/victims, as their authority and privilege are directly challenged and diminished by this reading. General devotees are in a mixed position, potentially benefiting from inclusivity but also bearing the social costs of challenging tradition.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_essence_vs_accretion,
    'Is caste hierarchy an inherent, scripturally essential component of the Vedic/Dharmic corpus, or is it a historical accretion and misinterpretation?',
    'Comprehensive philological and historical analysis of the earliest scriptural layers, combined with sociological studies of historical practice, to determine the origin and evolution of caste within the tradition.',
    'If proven essential, the reformist reading faces a more fundamental challenge to its internal consistency, potentially shifting its classification towards a ''snare'' (imposing external values). If proven accretion, the reformist reading gains stronger internal legitimacy, reinforcing its ''rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_essence_vs_accretion, empirical, 'Ambiguity regarding the scriptural basis of caste hierarchy.').

omega_variable(
    effectiveness_of_legal_enforcement,
    'How effectively can state legal frameworks enforce egalitarian interpretations and anti-discrimination principles against deeply entrenched social and religious practices, particularly in rural or traditionally conservative areas?',
    'Longitudinal sociological studies tracking the impact of anti-caste legislation and judicial rulings on actual social behavior, access to resources, and reduction of discrimination over several decades.',
    'If legal enforcement proves largely ineffective, the ''suppression'' metric for this reading might be overstated, and the constraint''s ability to genuinely coordinate social change would be weaker, potentially pushing it towards a ''piton'' (theatrical enforcement). If effective, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_legal_enforcement, empirical, 'The gap between legal mandate and social reality in enforcing egalitarian principles.').

omega_variable(
    internalized_vs_structural_suppression,
    'To what extent does the persistence of caste discrimination, even among those who intellectually reject it, stem from internalized beliefs and social conditioning versus ongoing structural barriers?',
    'Psychological and anthropological studies examining the cognitive and social mechanisms that perpetuate caste-based biases and behaviors, even in the absence of overt legal or institutional discrimination.',
    'If internalized suppression is a dominant factor, the constraint''s effective suppression is higher than structural measures suggest, as individuals carry the suppression with them. This would make the path to genuine equality more complex than legal reform alone can address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism in caste discrimination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(vedi_tr_t1960, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(vedi_tr_t1980, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(vedi_tr_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(vedi_tr_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1947, 0.3).
narrative_ontology:measurement(vedi_be_t1960, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(vedi_be_t1980, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(vedi_be_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(vedi_be_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1947, 0.5).
narrative_ontology:measurement(vedi_su_t1960, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(vedi_su_t1980, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(vedi_su_t2000, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2000, 0.63).
narrative_ontology:measurement(vedi_su_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
