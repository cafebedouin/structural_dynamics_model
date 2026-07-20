% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Colonial Orientalist Reading of Vedic/Dharmashastra as Unified Hindu Law
 *   domain: religious_studies/social_stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint story models the colonial orientalist reading of Vedic
 *   and Dharmashastra texts as a unified, timeless Hindu law suitable for
 *   codification and administrative governance. Developed under the East
 *   India Company and Crown Raj, this reading selected, translated, and
 *   systematized heterogeneous Brahminical texts into a single personal law
 *   applied by colonial courts to Hindu subjects. The reading created
 *   legible, fixed caste categories for census and taxation, displaced
 *   flexible local custom, and generated a textualist legal authority that
 *   outlasted colonialism. It is one reading of a three-way contested kernel,
 *   alongside an orthodox varna reading (divinely mandated cosmic hierarchy)
 *   and a reformist spiritual reading (metaphorical cosmology with no
 *   prescriptive social content).
 *
 * KEY AGENTS:
 *   - colonial_administration: Primary agenda-setter (institutional/arbitrage) â constructs and enforces the unified legal framework
 *   - colonized_legal_subjects: Primary payer (powerless/trapped) â subjected to fixed caste categories and colonial personal law
 *   - local_customary_practitioners: Secondary payer (powerless/trapped) â displaced by colonial courts and codified dharmic law
 *   - indian_reformists: Excluded voice (moderate/constrained) â later contested the freezing of social categories
 *   - postcolonial_scholars: Analytical observer (analytical/analytical) â deconstructs the orientalist reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.72).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Reading of Vedic/Dharmashastra as Unified Hindu Law").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious_studies/social_stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, 'fd754bd5-a69d-4230-8e94-63411bc8114c').
narrative_ontology:cs_kernel_codification('fd754bd5-a69d-4230-8e94-63411bc8114c', fixed_text).
narrative_ontology:cs_authority_grounding('fd754bd5-a69d-4230-8e94-63411bc8114c', extraction).
narrative_ontology:cs_interpretation_layer_present('fd754bd5-a69d-4230-8e94-63411bc8114c').
narrative_ontology:cs_reading_relation('fd754bd5-a69d-4230-8e94-63411bc8114c', vedic_corpus_social_prescription__orthodox_varna_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd754bd5-a69d-4230-8e94-63411bc8114c', vedic_corpus_social_prescription__reformist_spiritual_reading, influences).
narrative_ontology:cs_axiom('fd754bd5-a69d-4230-8e94-63411bc8114c', foundational, dharmashastra_as_unified_jurisprudence).
narrative_ontology:cs_axiom_status(dharmashastra_as_unified_jurisprudence, holdable).
narrative_ontology:cs_axiom_grounding('fd754bd5-a69d-4230-8e94-63411bc8114c', dharmashastra_as_unified_jurisprudence, conventional).
narrative_ontology:cs_axiom('fd754bd5-a69d-4230-8e94-63411bc8114c', foundational, caste_legibility_for_governance).
narrative_ontology:cs_axiom_status(caste_legibility_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('fd754bd5-a69d-4230-8e94-63411bc8114c', caste_legibility_for_governance, instrumental).
narrative_ontology:cs_reference_frame('fd754bd5-a69d-4230-8e94-63411bc8114c', colonial_administrative_legibility).
narrative_ontology:cs_drift_state('fd754bd5-a69d-4230-8e94-63411bc8114c', post_independence_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fd754bd5-a69d-4230-8e94-63411bc8114c', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, local_customary_practitioners).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_unified_hindu_law_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% European colonial officials and institutions in South Asia who needed to administer personal law over a diverse population. They sponsored translations of Dharmashastra texts, constructed a unified Hindu law from heterogeneous sources, and administered it through colonial courts for marriage, inheritance, and caste status. They could modify or abandon the system as administrative needs changed.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Individuals across South Asia classified under colonial personal law. Their caste status, inheritance rights, and marriage rules were determined by colonial court interpretations of selected Sanskrit texts rather than local custom or communal negotiation. They could not opt out of the colonial legal classification system.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, biographical, trapped, local).

% Village headmen, caste councils, and local jurists who previously handled disputes and status allocation through flexible customary practice. Colonial courts displaced their authority by recognizing only the codified Hindu law as valid, rendering their traditional roles legally irrelevant.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, local_customary_practitioners, payer,
    powerless, biographical, trapped, local).

% Social reformers in colonial and early post-colonial India who argued that caste was not a rigid scriptural mandate but a corrupt social practice. They sought legislative intervention to override colonially-fixed personal law categories but were initially excluded from the colonial codification process.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indian_reformists, excluded,
    moderate, generational, constrained, national).

% Academic historians and legal scholars analyzing how colonial knowledge projects constructed Hindu law as a unified system. They document the selectivity of translation, the elision of regional variation, and the enduring effects of colonial legal categories on postcolonial society.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, postcolonial_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, uniform legal framework for governing the personal status of a religiously diverse colonized population, replacing hundreds of local customary variations with one administrable code.
% TRANSFER_FUNCTION: Moves the authority to define caste status, marriage validity, and inheritance rules from local communities and customary practitioners to colonial courts and appointed translators; moves tax and census legibility to the colonial state.
% ABSENT_VOICES: Lower-caste and tribal communities whose local customs were overwritten by the Brahminical-textual focus of colonial codification; women whose inheritance and marriage rights were reinterpreted through selective textual emphasis; Indian reformists who contested the ossification of social practice but were not consulted in the construction of the legal code.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the colonial administration would lose its primary textual justification for personal law; census operations, caste-based legal classifications, and the revenue and administrative infrastructure built on Hindu law would require immediate reorganization around alternative customary, statutory, or common-law bases.
% FOUNDING_PROBLEM: Colonial power needed to govern a large, legally diverse population without extending English law to personal status matters, requiring a native law that was uniform, textually grounded, and administratively tractable.
% FOUNDING_PROBLEM_CORROBORATION: Postcolonial legal historians such as Bernard Cohn and Lata Mani corroborate that the founding problem was administratively driven, not a recovery of indigenous legal unity; post-independence legislative reforms and the eventual passage of secular and reformed personal laws confirm the colonial framing is no longer live, though its categories persist.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the constraint genuinely coordinated some governance functions (uniform personal law replaced chaotic forum shopping) but extracted heavily through the suppression of customary flexibility and the creation of fixed, taxable caste categories. Suppression is high (0.72) because colonial courts actively refused to recognize local custom when it conflicted with translated textual authority. Theater ratio is moderate-high (0.60 at interval end) because a growing share of scholarly and judicial activity was devoted to maintaining the fiction of a unified, timeless Hindu law in the face of overwhelming textual heterogeneity. Accessibility collapse is substantial (0.65) because colonial legal education and court precedent progressively delegitimized local customary alternatives. Resistance is moderate (0.55) because colonized subjects and later reformists actively contested the fixed categories, though usually within the colonial legal framework rather than outside it.
 *
 * PERSPECTIVAL GAP:
 *   The colonial administration seat experiences this constraint as a necessary administrative scaffoldâcoordinating governance across diversityâwhile the colonized subject seat experiences it as an imposed identity cage. The engine should compute divergent per-seat classifications from this structural asymmetry: low directionality for the administration (beneficiary of legibility), high directionality for subjects (targets of fixed categorization), and intermediate for local practitioners (displaced but not directly taxed).
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial administration is the structural beneficiary: it gains census legibility, tax revenue, and a manageable legal forum. Colonized legal subjects and local customary practitioners are the victims: they lose the flexibility of negotiated custom and are fixed into colonial categories. Directionality is derived from these structural positionsâadministration near the beneficiary end (low d), subjects near the target end (high d). No override is needed because the beneficiary and victim structure transparently determines the direction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâgoverning personal status without English lawâwas solved by the mid-nineteenth century, yet the codified categories persisted well beyond the colonial era, suggesting a classic mandatrophy dynamic. However, the scaffold classification is structurally appropriate because the arrangement was justified by its transitional governance function rather than a claim of timeless necessity. The post-independence reforms that replaced colonial personal law with statutory codes function as the scaffold's removal, though residue persists in contemporary caste legibility practices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_scaffold_temporality,
    'Was the colonial codification genuinely intended as transitional scaffolding for governance, or was it designed to permanently fix social categories for extraction?',
    'Archival analysis of colonial legal correspondence; comparison with other colonial personal-law codifications to determine if sunset was structurally implicit.',
    'If permanent, reclassification toward tangled_rope or snare; if genuinely transitional despite lack of formal sunset, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_scaffold_temporality, empirical, 'Whether the colonial codification was transitional or permanent in intent').

omega_variable(
    customary_law_suppression,
    'To what extent did the colonial codification actively suppress viable local customary alternatives versus merely overlaying a new forum?',
    'Ethnographic and legal-historical recovery of pre-colonial dispute-resolution records; comparison of case outcomes under colonial courts versus caste panchayats.',
    'High suppression of viable alternatives increases extractiveness and suppression scores; low suppression suggests the constraint was more purely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_suppression, empirical, 'Degree of alternative legal forum suppression').

omega_variable(
    kernel_reading_exclusivity,
    'Does the colonial orientalist reading structurally foreclose the reformist spiritual reading, or do they occupy distinct institutional domains?',
    'Analysis of whether colonial legal education and state curricula actively delegitimized reformist spiritual interpretations of Vedic texts.',
    'If foreclosing, the kernel functions as a commitment system with contradictory axioms; if coexisting, the readings are separable constraints with different victim sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Relationship between colonial and reformist readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vedi_tr_t20, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(vedi_tr_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 80, 0.6).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(vedi_be_t20, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(vedi_be_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 80, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vedi_su_t20, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(vedi_su_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 80, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
