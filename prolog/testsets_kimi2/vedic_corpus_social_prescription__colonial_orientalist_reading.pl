% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__colonial_orientalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Colonial Orientalist Codification of Hindu Law
 *   domain: religious studies/social stratification/hermeneutics
 *
 * SUMMARY:
 *   This constraint instantiates the colonial_orientalist_reading of the
 *   kernel vedic_corpus_social_prescription. Under this reading,
 *   heterogeneous Vedic and Dharmashastra texts are treated as a single,
 *   timeless 'Hindu law' suitable for codification by the colonial
 *   administration. The reading crystallizes fluid social practices into
 *   fixed legal categoriesâcaste, joint family, inheritance rulesâto
 *   create legible subjects for census, taxation, and adjudication. It is
 *   authored as a scaffold: a coordination structure erected to solve a
 *   specific colonial governance problem, carrying a structural sunset in the
 *   form of colonial rule itself. The metrics and claim are independently
 *   authored: the claimed type is scaffold, while the metrics register
 *   substantial extraction (0.55) and suppression (0.72) because the
 *   codification actively displaces local alternatives and fixes social
 *   hierarchy for administrative convenience.
 *
 * KEY AGENTS:
 *   - colonial_administration (institutional/arbitrage): agenda-setter and beneficiaryâconstructs and enforces the codified legal framework
 *   - colonized_legal_subjects (powerless/trapped): payerâbear the costs of fixed legal identities and caste categories
 *   - indigenous_jurists (moderate/constrained): excludedâdisplaced local authorities whose fluid practices were overridden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.55).
domain_priors:suppression_score(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.72).
domain_priors:theater_ratio(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__colonial_orientalist_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__colonial_orientalist_reading, scaffold).
narrative_ontology:human_readable(vedic_corpus_social_prescription__colonial_orientalist_reading, "Colonial Orientalist Codification of Hindu Law").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__colonial_orientalist_reading, "religious studies/social stratification/hermeneutics").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__colonial_orientalist_reading).
narrative_ontology:has_sunset_clause(vedic_corpus_social_prescription__colonial_orientalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__colonial_orientalist_reading, '25a53e41-3fec-4471-8766-40f610aff51a').
narrative_ontology:cs_kernel_codification('25a53e41-3fec-4471-8766-40f610aff51a', fixed_text).
narrative_ontology:cs_authority_grounding('25a53e41-3fec-4471-8766-40f610aff51a', extraction).
narrative_ontology:cs_interpretation_layer_present('25a53e41-3fec-4471-8766-40f610aff51a').
narrative_ontology:cs_reading_relation('25a53e41-3fec-4471-8766-40f610aff51a', vedic_corpus_social_prescription__orthodox_varna_reading, influences).
narrative_ontology:cs_reading_relation('25a53e41-3fec-4471-8766-40f610aff51a', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_axiom('25a53e41-3fec-4471-8766-40f610aff51a', foundational, vedic_texts_unified_timeless_law).
narrative_ontology:cs_axiom_status(vedic_texts_unified_timeless_law, holdable).
narrative_ontology:cs_axiom_grounding('25a53e41-3fec-4471-8766-40f610aff51a', vedic_texts_unified_timeless_law, empirically_contingent).
narrative_ontology:cs_axiom('25a53e41-3fec-4471-8766-40f610aff51a', foundational, codification_necessary_for_governance).
narrative_ontology:cs_axiom_status(codification_necessary_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('25a53e41-3fec-4471-8766-40f610aff51a', codification_necessary_for_governance, instrumental).
narrative_ontology:cs_reference_frame('25a53e41-3fec-4471-8766-40f610aff51a', unified_timeless_legal_code).
narrative_ontology:cs_drift_state('25a53e41-3fec-4471-8766-40f610aff51a', postcolonial_legal_reform_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('25a53e41-3fec-4471-8766-40f610aff51a', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__colonial_orientalist_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, orientalist_legal_essentialism).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__colonial_orientalist_reading, textual_unification_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constructs and enforces a unified code of Hindu law from heterogeneous Dharmashastra and Vedic texts to create administrable legal categories for census, taxation, and court adjudication. Benefits from legible, governable subjects and a centralized textual authority that displaces fluid local customs.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to fixed legal identities and caste categories derived from colonial codification of textual traditions. Their lived practices are overridden by court-enforced textual rules; they cannot opt out of the colonial legal identity assigned to them and bear the costs of codified social hierarchy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, colonized_legal_subjects, payer,
    powerless, generational, trapped, national).

% Local jurists, caste councils, and customary authorities whose fluid, practice-based legal traditions were displaced by the colonial demand for a single, timeless textual authority. They were excluded from the codification process and lost interpretive autonomy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__colonial_orientalist_reading, indigenous_jurists, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__colonial_orientalist_reading, colonial_administration).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, uniform legal framework for governing diverse Hindu populations across British India, replacing fragmented local customs with a centralized textual authority that enables consistent adjudication, census classification, and revenue collection.
% TRANSFER_FUNCTION: Transfers interpretive authority over social identity from local jurists and fluid custom to colonial courts and Orientalist scholars; transfers administrative legibility and governability from colonized populations to the colonial state.
% ABSENT_VOICES: Local jurists, subaltern castes, women, and non-Brahmanical communities whose lived practices diverged from the textual tradition were excluded from the codification process; their voices would contest the timelessness, unity, and prescriptive authority attributed to the texts.
% DISAPPEARANCE_RATIONALE: If the colonial codification vanished overnight, the administrative apparatus of personal law would lose its textual anchor; census categories, court adjudication of caste and family law, and revenue extraction would need to reorganize around local custom or new legislation, and the colonial state's grip on social legibility would weaken.
% FOUNDING_PROBLEM: The colonial administration needed to govern a large, religiously and culturally diverse population with unfamiliar local customs; ad-hoc adjudication was inefficient and threatened consistent revenue extraction and social control.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial historians and legal scholars (outside the beneficiary set) attest that the founding problem was colonial administrative convenience rather than indigenous demand; Indian nationalist historiography corroborates that the need for a unified Hindu law was constructed by the colonizer to serve extractive governance.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__colonial_orientalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__colonial_orientalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__colonial_orientalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__colonial_orientalist_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because the constraint extracts interpretive authority and social legibility without necessarily extracting direct rent; suppression is high (0.72) because the codification requires active colonial enforcement to displace customary law and fix categories. Theater ratio (0.40) reflects the growing gap between the Orientalist claim of discovering timeless law and the historical reality of textual heterogeneity. Accessibility collapse (0.68) is high because once codified, the textual rules became the default legal reality, shrinking the space for customary alternatives. Resistance (0.45) captures subaltern and nationalist pushback. The temporal series show extraction and enforcement rising through the colonial period, peaking around the late 19th century, then gradually declining after independence as post-colonial reforms eroded the colonial legal scaffold.
 *
 * PERSPECTIVAL GAP:
 *   The colonial_administration seat experiences the constraint as a necessary coordinative scaffold that brings order to legal chaos; the colonized_legal_subjects seat experiences it as an imposed identity framework that freezes social fluidity and reinforces hierarchy. The indigenous_jurists seat experiences exclusion and loss of authority. The engine will compute divergent per-seat types from this structural asymmetry: low directionality for the administration, high directionality for the colonized subjects.
 *
 * DIRECTIONALITY LOGIC:
 *   The colonial_administration is the structural beneficiary (low d): it gains legibility, revenue, and control. The colonized_legal_subjects are the structural targets (high d): they pay through subjection to fixed categories and loss of customary autonomy. Indigenous_jurists sit in between, excluded from the arrangement rather than coordinated by it. No override is needed because beneficiary/victim declarations and exit options correctly derive the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcolonial administrative need for a uniform legal frameworkâis dead. Colonial rule ended, yet the codified categories persisted into post-colonial personal law and social identity. The scaffold's mandate expired, but the structure remained. This is a classic mandatrophy risk: the classification as scaffold prevents mislabeling the residual structure as natural law or as still-functional coordination. The temporal measurements show declining suppression post-independence, indicating the constraint is eroding but has not vanished.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vedic_unity_empirical_status,
    'Do the Vedic and Dharmashastra texts constitute a unified, timeless legal system, or is this unity an artifact of Orientalist selection and translation?',
    'Philological and historical scholarship comparing regional manuscripts, stratigraphic textual analysis, and study of pre-colonial juridical practice to determine whether the texts were historically treated as a single code.',
    'If the unity is an artifact, the constraint''s foundational premise collapses and the extraction profile shifts toward pure snare (imposition of a fictional order); if genuine, the scaffold claim gains support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vedic_unity_empirical_status, empirical, 'Whether textual unity is discovered or constructed').

omega_variable(
    colonial_codification_transience,
    'Was the colonial legal codification structurally intended as a temporary scaffold toward modern governance, or as a permanent apparatus of extraction and control?',
    'Archival analysis of colonial legislative intent, correspondence between governors and Orientalists, and comparison with other colonial legal codification projects.',
    'If the sunset was genuine, the scaffold classification is structurally apt; if rhetorical, the constraint is better read as a tangled rope or snare using scaffold language as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_codification_transience, conceptual, 'Whether the scaffold sunset was real or rhetorical').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of codified caste categories after colonial rule due to structural enforcement by post-colonial institutions or internalized identity adoption by colonized subjects?',
    'Post-independence trajectory analysis: measure persistence of categories in jurisdictions that repealed colonial personal law versus those that retained it, combined with ethnographic study of identity formation.',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s extractive reach is deeper than institutional data suggest; if purely structural, reform is a matter of legal repeal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    sibling_reading_structural_delta,
    'How would the classification change if the orthodox_varna_reading or reformist_spiritual_reading were adopted as the analytical frame instead of this colonial reading?',
    'Parallel constraint story generation for each sibling reading under the same kernel, comparing epsilon values, beneficiary/victim structures, and computed seat types.',
    'The orthodox reading likely yields higher extraction for lower-caste subjects and lower theater; the reformist reading likely yields a mountain or low-extraction constraint with no victims. The variance measures the kernel''s contested nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Classification sensitivity to kernel reading choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__colonial_orientalist_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedic_colonial_tr_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vedic_colonial_tr_t30, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(vedic_colonial_tr_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(vedic_colonial_tr_t90, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement(vedic_colonial_tr_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 120, 0.5).
narrative_ontology:measurement(vedic_colonial_tr_t150, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 150, 0.45).
narrative_ontology:measurement(vedic_colonial_tr_t180, vedic_corpus_social_prescription__colonial_orientalist_reading, theater_ratio, 180, 0.4).

% Extraction over time
narrative_ontology:measurement(vedic_colonial_be_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vedic_colonial_be_t30, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(vedic_colonial_be_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(vedic_colonial_be_t90, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 90, 0.65).
narrative_ontology:measurement(vedic_colonial_be_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(vedic_colonial_be_t150, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 150, 0.55).
narrative_ontology:measurement(vedic_colonial_be_t180, vedic_corpus_social_prescription__colonial_orientalist_reading, base_extractiveness, 180, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(vedic_colonial_su_t0, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vedic_colonial_su_t30, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(vedic_colonial_su_t60, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(vedic_colonial_su_t90, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 90, 0.8).
narrative_ontology:measurement(vedic_colonial_su_t120, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 120, 0.75).
narrative_ontology:measurement(vedic_colonial_su_t150, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 150, 0.5).
narrative_ontology:measurement(vedic_colonial_su_t180, vedic_corpus_social_prescription__colonial_orientalist_reading, suppression_requirement, 180, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__colonial_orientalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, orthodox_varna_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__colonial_orientalist_reading, reformist_spiritual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel vedic_corpus_social_prescription. The colonial_orientalist_reading extracts a unified legal system for governance; the orthodox_varna_reading extracts divine varna hierarchy; the reformist_spiritual_reading denies prescriptive social content entirely. Decomposition follows the epsilon-invariance principle: each reading instantiates a structurally distinct constraint with different epsilon values and stakeholder configurations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
