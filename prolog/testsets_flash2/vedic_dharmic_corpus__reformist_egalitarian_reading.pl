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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Vedic Dharmic Corpus: Reformist Egalitarian Reading
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   This constraint represents the 'reformist egalitarian' reading of the
 *   Vedic Dharmic corpus, which asserts that textual meaning must conform to
 *   constitutional equality principles, caste hierarchy is a historical
 *   accretion rather than scriptural essence, and rational critique
 *   supersedes traditional authority. This reading is actively enforced
 *   through legal and social mechanisms, challenging orthodox
 *   interpretations. It is a 'tangled rope' because it genuinely coordinates
 *   social integration and equality (benefiting Dalit movements and the
 *   secular state) but also extracts from traditionalist communities and
 *   orthodox institutions by suppressing their historical authority and
 *   practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.6).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Vedic Dharmic Corpus: Reformist Egalitarian Reading").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, '16a0f2de-90ad-48f1-88b7-bde5d28a03a4').
narrative_ontology:cs_kernel_codification('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', fixed_text).
narrative_ontology:cs_authority_grounding('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', extraction).
narrative_ontology:cs_interpretation_layer_present('16a0f2de-90ad-48f1-88b7-bde5d28a03a4').
narrative_ontology:cs_reading_relation('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', foundational, constitutional_equality_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', constitutional_equality_supremacy, deontological).
narrative_ontology:cs_axiom('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', foundational, rational_critique_supersedes_tradition).
narrative_ontology:cs_axiom_status(rational_critique_supersedes_tradition, holdable).
narrative_ontology:cs_axiom_grounding('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', rational_critique_supersedes_tradition, empirically_contingent).
narrative_ontology:cs_reference_frame('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', modern_secular_egalitarianism).
narrative_ontology:cs_drift_state('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('16a0f2de-90ad-48f1-88b7-bde5d28a03a4', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_legal_apparatus).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for the egalitarian interpretation of religious texts, leveraging constitutional principles and secular law to challenge traditional caste hierarchies. They benefit from the legal and social legitimacy this reading provides, but face significant social resistance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, constrained, national).

% Interprets religious texts through the lens of constitutional equality, providing legal backing for anti-discrimination efforts. Its authority is derived from the state, and it actively enforces laws that align with this reading, often against traditional religious practices.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_legal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Historically held exclusive ritual and interpretive authority, which is challenged by this reading. They bear the cost of losing social and religious dominance, facing legal restrictions and public critique. Their identity is deeply tied to the hereditary monopoly reading.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions, payer,
    institutional, civilizational, identity_locked, national).

% Adhere to traditional interpretations of caste and ritual, often feeling their cultural and religious practices are under attack by the reformist reading and its legal enforcement. They bear social pressure and legal penalties for non-compliance, but their local cohesion provides some resistance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_communities, payer,
    moderate, biographical, constrained, local).

% Offer an alternative path to spiritual authority that bypasses caste, but their focus is on personal devotion rather than legal or social reform. While aligned with egalitarian outcomes, they are often excluded from the direct legal and political contest over textual interpretation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_movements, excluded,
    organized, generational, mobile, national).

% Analyze the historical development of caste, scriptural interpretation, and reform movements. They provide critical analysis of the textual basis for both traditional and reformist claims, influencing public discourse but not directly enforcing the constraint.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Attempts to coordinate religious practice and social norms with modern constitutional principles of equality and human rights, providing a framework for inclusive religious identity in a secular state.
% TRANSFER_FUNCTION: Transfers interpretive authority from hereditary Brahminical lineages to a combination of rational critique, constitutional principles, and secular legal institutions, thereby reallocating social status and access to religious roles.
% ABSENT_VOICES: While Bhakti movements offer an egalitarian path, their non-engagement with legal and political reform means their specific theological arguments for equality are often absent from the direct contest over textual interpretation within the legal apparatus.
% DISAPPEARANCE_RATIONALE: If this reading and its legal enforcement vanished, orthodox institutions would likely reassert traditional hierarchies, Dalit movements would lose a key legal tool, and the social landscape of religious practice would revert to more traditional, caste-based structures, leading to significant social and political upheaval.
% FOUNDING_PROBLEM: The historical problem of caste-based discrimination and social exclusion within Dharmic traditions, which contradicted modern ideals of equality and human dignity, leading to social fragmentation and injustice.
% FOUNDING_PROBLEM_CORROBORATION: Dalit movements and secular human rights organizations attest that the problem of caste discrimination remains live, despite legal protections. Orthodox institutions contest this, arguing that caste is a functional division, not discrimination. Independent sociological studies and human rights reports from outside the benefiting parties corroborate the persistence of discrimination.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is moderate (0.45) because while it challenges traditional power structures, it also creates new forms of social and legal obligation for traditionalists. Suppression is high (0.6) due to the active legal and social enforcement required to displace deeply entrenched traditional authority. Theater ratio is low (0.2) as the reformist agenda is genuinely pursued, though some performative aspects exist in public discourse. Accessibility collapse is moderate (0.4) as traditional alternatives are suppressed but not entirely eliminated, and resistance is high (0.7) from those whose authority is challenged.
 *
 * PERSPECTIVAL GAP:
 *   The secular legal apparatus and Dalit movements experience this as a necessary and just coordination mechanism for social equality. Orthodox institutions and traditionalist communities experience it as an imposition and an extraction of their cultural and religious autonomy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit movements and the secular legal apparatus are beneficiaries, as this reading empowers them and aligns with their goals (low d). Orthodox Brahminical institutions and traditionalist communities are victims, as their authority and practices are directly challenged and suppressed (high d). Bhakti movements are excluded, as their approach to equality is spiritual rather than legal-reformist, placing them outside the direct contest over textual interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_social_enforcement_balance,
    'What is the actual balance between legal enforcement and social acceptance in sustaining this reading?',
    'Sociological studies tracking compliance with anti-discrimination laws versus changes in deeply held social attitudes and practices over time.',
    'If legal enforcement is the primary driver without significant social acceptance, the constraint''s suppression is higher and its long-term stability is lower, indicating a more coercive ''tangled rope''. If social acceptance grows, it moves closer to a ''rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_vs_social_enforcement_balance, empirical, 'Balance of legal vs. social enforcement for the reformist reading.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the secular legal apparatus''s interpretive authority over religious texts genuinely accepted by a significant portion of the religious community, or is it merely tolerated under duress?',
    'Surveys of religious leaders and practitioners, analysis of internal religious debates, and observation of voluntary adoption of egalitarian practices versus those enforced by law.',
    'If acceptance is low, the constraint''s suppression is effectively higher, and its legitimacy as a coordination mechanism is weaker, pushing it towards a ''snare''. If acceptance is high, its ''rope'' aspects are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Legitimacy of secular interpretive authority over religious texts.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, social pressure) or internalized (traditionalists self-censoring or adapting beliefs)?',
    'Post-legal-reform trajectory: if traditionalist practices persist after legal enforcement is removed or weakened, reclassify as partially internalized suppression, indicating deeper cultural entrenchment.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — traditionalists carry the suppression with them after external pressure lessens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vedi_tr_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vedi_be_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vedi_su_t10, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'vedic_dharmic_corpus' kernel. It directly challenges the 'hereditary_monopoly_reading' and offers a distinct path from the 'bhakti_devotional_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
