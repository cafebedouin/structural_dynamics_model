% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Hebrew Linguistic Life: Marketplace Pidgin Reading
 *   domain: sociolinguistics/history/religious_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'marketplace_pidgin_reading' of the
 *   'hebrew_linguistic_life' kernel. It asserts that Hebrew was continuously
 *   alive as an inter-communal medium for practical coordination in Jerusalem
 *   markets prior to 1880, functioning as a modified Medieval Hebrew pidgin.
 *   This reading challenges narratives of Hebrew's 'death' and 'revival' by
 *   positing continuous adaptation and functional use, independent of native
 *   speaker status or sacred function. The constraint is classified as a
 *   Mountain because it describes a historical linguistic reality, presented
 *   as an unchangeable fact by its proponents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.1).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew Linguistic Life: Marketplace Pidgin Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/history/religious_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, 'bfcedf6f-82a9-4af0-97ed-338c823d1d59').
narrative_ontology:cs_kernel_codification('bfcedf6f-82a9-4af0-97ed-338c823d1d59', implicit).
narrative_ontology:cs_authority_grounding('bfcedf6f-82a9-4af0-97ed-338c823d1d59', expertise).
narrative_ontology:cs_interpretation_layer_present('bfcedf6f-82a9-4af0-97ed-338c823d1d59').
narrative_ontology:cs_reading_relation('bfcedf6f-82a9-4af0-97ed-338c823d1d59', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfcedf6f-82a9-4af0-97ed-338c823d1d59', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_axiom('bfcedf6f-82a9-4af0-97ed-338c823d1d59', foundational, language_function_over_native_form).
narrative_ontology:cs_axiom_status(language_function_over_native_form, holdable).
narrative_ontology:cs_axiom_grounding('bfcedf6f-82a9-4af0-97ed-338c823d1d59', language_function_over_native_form, empirically_contingent).
narrative_ontology:cs_axiom('bfcedf6f-82a9-4af0-97ed-338c823d1d59', secondary, pidgin_as_valid_linguistic_life).
narrative_ontology:cs_axiom_status(pidgin_as_valid_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('bfcedf6f-82a9-4af0-97ed-338c823d1d59', pidgin_as_valid_linguistic_life, empirically_contingent).
narrative_ontology:cs_reference_frame('bfcedf6f-82a9-4af0-97ed-338c823d1d59', functional_communication_paradigm).
narrative_ontology:cs_drift_state('bfcedf6f-82a9-4af0-97ed-338c823d1d59', contemporary_sociolinguistic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('bfcedf6f-82a9-4af0-97ed-338c823d1d59', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, sociolinguists_of_pidgins).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, historians_of_jerusalem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_revivalists_pre_1880).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_continuity_thesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, pidgin_as_living_language_concept).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and defend this reading of linguistic vitality, finding evidence for continuous functional use of Hebrew in historical contexts. Their theories are vindicated by this interpretation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sociolinguists_of_pidgins, agenda_setter,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__marketplace_pidgin_reading, sociolinguists_of_pidgins, beneficiary).

% Contribute historical evidence from archival records and social histories that supports the continuous use of Hebrew as a functional pidgin in Jerusalem markets, thereby validating their research.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, historians_of_jerusalem, beneficiary,
    analytical, generational, analytical, local).

% Their narrative of Hebrew as a 'dead' language requiring 'revival' is challenged by this reading, which asserts continuous, albeit non-native, vitality. They bear the cost of having their foundational premise undermined.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_revivalists_pre_1880, payer,
    moderate, biographical, constrained, local).

% Would strongly reject this definition of 'linguistic life' as it de-centers native acquisition and challenges the ideological purity of their language revival project. They are excluded from the discourse that validates this reading.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, native_speaker_supremacists, excluded,
    powerful, generational, identity_locked, national).

% Focus on the continuous use of Hebrew in sacred texts and rituals. While not directly challenged by this reading, their emphasis on liturgical function is distinct from the marketplace pidgin's practical coordination.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, sociolinguists_of_pidgins).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled practical communication and trade between diverse linguistic communities in Jerusalem markets, providing a common medium for daily interactions.
% TRANSFER_FUNCTION: Facilitated the exchange of goods, services, and information among speakers of different native languages, using a simplified, adapted form of Hebrew.
% ABSENT_VOICES: Linguistic purists and native-speaker ideologues who define 'life' strictly by mother-tongue acquisition would object, as this reading challenges their narrative of Hebrew's 'death' and 'revival'. They are structurally excluded from the academic discourse that validates this reading.
% DISAPPEARANCE_RATIONALE: The historical linguistic reality of Hebrew's functional use in Jerusalem markets pre-1880 would remain a fact, regardless of whether this specific interpretation of 'linguistic life' is adopted or discarded. The constraint describes a past state, not an active, enforced structure.
% FOUNDING_PROBLEM: To establish a robust, evidence-based criterion for linguistic vitality that accounts for diverse forms of language use beyond native-speaker acquisition or sacred function, particularly for languages with complex historical trajectories like Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Independent historical linguistic research, analysis of archival trade documents, and comparative sociolinguistic studies of pidgin and creole formation from outside the immediate circle of Hebrew revivalists or religious scholars.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__marketplace_pidgin_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect a descriptive claim about a historical linguistic phenomenon. Extractiveness, suppression, and theater ratio are all low because the constraint describes a natural, functional linguistic state, not an actively enforced or extractive human construct. Accessibility collapse is high, and resistance is low, as the claim is presented as a factual assertion about historical reality, which, if accepted, leaves little room for alternative interpretations of that specific historical period's linguistic state.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap exists between those who define linguistic life by functional inter-communal use (this reading) and those who define it by native generational transmission or purely sacred function. This constraint explicitly forecloses the native-generational reading by asserting vitality 'regardless of native speaker status'.
 *
 * DIRECTIONALITY LOGIC:
 *   Sociolinguists and historians who advance this reading are beneficiaries, as their theoretical frameworks and research are validated. Pre-1880 Hebrew revivalists are 'payers' in a conceptual sense, as their narrative of a 'dead' language is challenged. Native-speaker supremacists are excluded, as their ideological stance is fundamentally contradicted by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evidence_robustness_for_pidgin,
    'Is the historical and linguistic evidence for continuous pidgin use robust enough to definitively establish ''linguistic life'' by this definition, or does it merely suggest limited functional use?',
    'Further interdisciplinary research combining historical linguistics, sociolinguistics, and archival studies to uncover more direct evidence of daily, inter-communal communication in Hebrew pidgin.',
    'If evidence is found to be insufficient, the claim of continuous ''linguistic life'' by this reading would weaken, potentially shifting its classification from Mountain to a more contested type, or reducing its accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidence_robustness_for_pidgin, empirical, 'Assesses the empirical strength of the claim for continuous pidgin use.').

omega_variable(
    definition_of_linguistic_life,
    'Does ''linguistic life'' fundamentally require native generational transmission, or is inter-communal coordination (as in a pidgin) sufficient to qualify a language as ''alive''?',
    'Conceptual clarification and consensus within the field of sociolinguistics regarding the minimal criteria for linguistic vitality, potentially influenced by comparative studies of other pidgins and creoles.',
    'If native generational transmission is deemed a necessary condition, this reading would be conceptually foreclosed, and the ''native_generational_reading'' would gain stronger theoretical grounding. If functional coordination is accepted, this reading''s validity is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_linguistic_life, conceptual, 'Examines the conceptual boundary of ''linguistic life'' in sociolinguistics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1800, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(hebr_tr_t1840, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(hebr_be_t1840, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1840, 0.15).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1800, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(hebr_su_t1840, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1840, 0.1).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
