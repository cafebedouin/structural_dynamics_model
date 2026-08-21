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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'marketplace_pidgin_reading' of the
 *   'hebrew_linguistic_life' kernel. It asserts that Hebrew was continuously
 *   alive in Jerusalem markets prior to 1880, functioning as a modified
 *   Medieval Hebrew pidgin for inter-communal practical coordination. This
 *   reading emphasizes continuous adaptation and functional use over native
 *   speaker status or sacred function, challenging narratives of a 'dead'
 *   language awaiting 'revival' or solely preserved in liturgy. This
 *   constraint is one reading of the 'hebrew_linguistic_life' kernel,
 *   instantiating the 'marketplace_pidgin_reading'. Sibling readings include
 *   'liturgical_preservation_reading' and 'native_generational_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew Linguistic Life: Marketplace Pidgin Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '7e680100-5db8-4449-abf0-839e69a0be44').
narrative_ontology:cs_kernel_codification('7e680100-5db8-4449-abf0-839e69a0be44', implicit).
narrative_ontology:cs_authority_grounding('7e680100-5db8-4449-abf0-839e69a0be44', practice).
narrative_ontology:cs_reading_relation('7e680100-5db8-4449-abf0-839e69a0be44', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e680100-5db8-4449-abf0-839e69a0be44', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_axiom('7e680100-5db8-4449-abf0-839e69a0be44', foundational, language_function_over_form).
narrative_ontology:cs_axiom_status(language_function_over_form, holdable).
narrative_ontology:cs_axiom_grounding('7e680100-5db8-4449-abf0-839e69a0be44', language_function_over_form, empirically_contingent).
narrative_ontology:cs_axiom('7e680100-5db8-4449-abf0-839e69a0be44', secondary, inter_communal_use_is_life).
narrative_ontology:cs_axiom_status(inter_communal_use_is_life, holdable).
narrative_ontology:cs_axiom_grounding('7e680100-5db8-4449-abf0-839e69a0be44', inter_communal_use_is_life, empirically_contingent).
narrative_ontology:cs_reference_frame('7e680100-5db8-4449-abf0-839e69a0be44', functional_linguistic_continuity).
narrative_ontology:cs_drift_state('7e680100-5db8-4449-abf0-839e69a0be44', post_modern_hebrew_revival, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7e680100-5db8-4449-abf0-839e69a0be44', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, inter_communal_traders).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_adaptation_theory).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__marketplace_pidgin_reading, language_as_tool_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Analyze historical linguistic data to understand how languages function in diverse social contexts. This reading supports their theoretical frameworks on language contact and pidginization.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, sociolinguists, observer,
    analytical, generational, analytical, global).

% Benefited from the pidgin as a practical means of communication for trade in the diverse linguistic environment of pre-1880 Jerusalem. Their primary concern was effective transaction, not linguistic purity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants, beneficiary,
    moderate, biographical, mobile, local).

% Used the Hebrew pidgin as a lingua franca to facilitate commerce and interaction across different ethnic and religious communities in the broader region, prior to the modern revival.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, inter_communal_traders, beneficiary,
    moderate, biographical, mobile, regional).

% Would object to this definition of 'linguistic life' as it challenges their narrative of a 'dead' language being 'miraculously revived' into a modern native tongue. Their ideological commitment is to a specific, pure form of Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_revivalists, excluded,
    powerful, generational, identity_locked, national).

% Would argue that the 'life' of Hebrew is solely in its sacred function and continuous study of religious texts, not in its use as a secular or pidginized medium. This reading is irrelevant or even offensive to their framework.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, traditional_liturgists, excluded,
    organized, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a functional, inter-communal medium for practical coordination, primarily trade, among diverse linguistic groups in Jerusalem and surrounding areas before the modern Hebrew revival.
% TRANSFER_FUNCTION: Transferred practical information, facilitated commercial transactions, and enabled basic social interaction between speakers of different native languages.
% ABSENT_VOICES: Hebrew revivalists and traditional liturgists are excluded from this functional definition of 'life'; they would argue for stricter criteria based on native acquisition or sacred use, respectively.
% DISAPPEARANCE_RATIONALE: The historical fact that Hebrew functioned as a marketplace pidgin in a specific period would remain unchanged, even if the conceptual framework for defining 'linguistic life' were to shift or disappear. The constraint describes a historical state of affairs.
% FOUNDING_PROBLEM: The practical need for a common, accessible language for trade and basic communication among diverse linguistic communities in Jerusalem and the broader region.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguistic analyses, travelogues, and ethnographic accounts from the period, as well as comparative sociolinguistic studies of language contact and pidginization, corroborate the existence and function of such a pidgin. This evidence comes from outside the ideological frameworks of revivalists or liturgists.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is claimed as a Mountain because it describes a historical fact about language use, which, if empirically true, is an unchangeable feature of the past. Extractiveness, suppression, and theater ratio are very low (0.05) because the pidgin was an organic, functional tool for communication, not an extractive or coercive structure. Accessibility collapse is moderate (0.4) as a pidgin simplifies language for broader access, but at the cost of full linguistic complexity. Resistance is low (0.1) as this is a descriptive claim about historical practice, not an actively contested social constraint in its own time.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies in the definition of 'linguistic life' itself. While this reading focuses on functional, inter-communal use, other readings (liturgical preservation, native generational) apply different, often stricter, criteria. The engine will compute how these different definitions lead to different classifications of Hebrew's historical status.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jerusalem merchants and inter-communal traders are beneficiaries, as the pidgin directly facilitated their practical coordination and commerce. Sociolinguists are observers, benefiting from the empirical evidence this historical fact provides for their theories. Hebrew revivalists and traditional liturgists are excluded, as their ideological frameworks are challenged by this functional definition of 'life', leading them to reject its premise.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading implicitly addresses mandatrophy by offering an alternative definition of 'linguistic life'. It prevents mislabeling a functionally active language as 'dead' simply because it doesn't conform to narrow, often ideologically driven, criteria (e.g., native speakers only, or sacred use only). By broadening the definition, it highlights the continuous, adaptive nature of language, thereby challenging the notion that a language's 'mandate' for life can only be fulfilled in one specific form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_sufficiency,
    'Is the historical evidence sufficient to definitively establish Hebrew''s continuous function as an inter-communal marketplace pidgin in Jerusalem pre-1880?',
    'Discovery of new primary source documents (e.g., trade records, personal letters) or further linguistic analysis of existing texts that demonstrate pidgin features and widespread inter-communal use.',
    'If evidence is insufficient, the claim''s status as a ''Mountain'' (historical fact) would be weakened, potentially reclassifying it as a ''Rope'' (a coordination of scholarly interpretation) or ''Tangled Rope'' (if academic careers are built on asserting it without robust evidence).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_sufficiency, empirical, 'Sufficiency of historical evidence for continuous pidgin use.').

omega_variable(
    definition_of_linguistic_life,
    'Is ''functioning as an inter-communal medium for practical coordination'' a valid and sufficient criterion for defining a language as ''alive'', or is it merely one aspect of vitality?',
    'Conceptual clarification and consensus within sociolinguistics and related fields regarding the necessary and sufficient conditions for linguistic vitality. This is a definitional, not purely empirical, question.',
    'If this criterion is deemed insufficient or secondary, the ''Mountain'' classification (as an unchangeable fact of ''life'') would be challenged, potentially shifting the constraint to a ''Conceptual'' type, where its ''truth'' depends on the adopted definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_linguistic_life, conceptual, 'Conceptual validity of the ''marketplace pidgin'' criterion for linguistic life.').

omega_variable(
    false_summit_of_historical_fact,
    'Is the assertion of Hebrew''s continuous life as a marketplace pidgin a purely descriptive historical fact (Mountain), or does its prominence primarily serve to validate a particular sociolinguistic theory or challenge a nationalist/religious narrative (false summit)?',
    'Analysis of the discourse surrounding this claim: if its primary function is found to be ideological or theoretical validation rather than neutral historical description, it would indicate a false summit.',
    'If identified as a false summit, the constraint would reclassify from ''Mountain'' to ''Tangled Rope'' (if it benefits specific academic/ideological groups while extracting from others by invalidating their narratives) or ''Rope'' (if it primarily coordinates a new academic consensus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_of_historical_fact, conceptual, 'Whether the historical claim functions as a genuine natural law or a constructed claim benefiting specific agendas.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1700, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(hebr_tr_t1745, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1745, 0.05).
narrative_ontology:measurement(hebr_tr_t1790, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement(hebr_tr_t1835, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1835, 0.05).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.05).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(hebr_be_t1745, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1745, 0.05).
narrative_ontology:measurement(hebr_be_t1790, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1790, 0.05).
narrative_ontology:measurement(hebr_be_t1835, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1835, 0.05).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(hebr_su_t1745, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1745, 0.05).
narrative_ontology:measurement(hebr_su_t1790, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1790, 0.05).
narrative_ontology:measurement(hebr_su_t1835, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1835, 0.05).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_linguistic_life' kernel, focusing on functional, inter-communal use. It contrasts with the liturgical preservation and native generational readings, which offer different criteria for linguistic vitality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
