% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra: Reformist Contextual Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'reformist contextual' reading of the
 *   Dharmasastra corpus, which seeks to interpret these ancient Hindu legal
 *   and ethical texts by separating a universal ethical core (dharma as
 *   righteous conduct) from time-bound social prescriptions, particularly
 *   those related to caste (varna/jati) and gender. This reading aims to
 *   preserve the spiritual authority of the texts while adapting them to
 *   modern ethical standards and social realities. It is a 'tangled_rope'
 *   because it genuinely coordinates ethical guidance but still carries
 *   residual extraction from historical hierarchies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.4).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra: Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, '363500ba-d034-408d-b5a9-b74daa55b6df').
narrative_ontology:cs_kernel_codification('363500ba-d034-408d-b5a9-b74daa55b6df', fixed_text).
narrative_ontology:cs_authority_grounding('363500ba-d034-408d-b5a9-b74daa55b6df', lineage).
narrative_ontology:cs_interpretation_layer_present('363500ba-d034-408d-b5a9-b74daa55b6df').
narrative_ontology:cs_reading_relation('363500ba-d034-408d-b5a9-b74daa55b6df', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('363500ba-d034-408d-b5a9-b74daa55b6df', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('363500ba-d034-408d-b5a9-b74daa55b6df', foundational, dharma_is_universal_righteousness).
narrative_ontology:cs_axiom_status(dharma_is_universal_righteousness, holdable).
narrative_ontology:cs_axiom_grounding('363500ba-d034-408d-b5a9-b74daa55b6df', dharma_is_universal_righteousness, deontological).
narrative_ontology:cs_axiom('363500ba-d034-408d-b5a9-b74daa55b6df', foundational, scripture_is_contextual_revelation).
narrative_ontology:cs_axiom_status(scripture_is_contextual_revelation, holdable).
narrative_ontology:cs_axiom_grounding('363500ba-d034-408d-b5a9-b74daa55b6df', scripture_is_contextual_revelation, conventional).
narrative_ontology:cs_reference_frame('363500ba-d034-408d-b5a9-b74daa55b6df', ethical_core_supremacy).
narrative_ontology:cs_drift_state('363500ba-d034-408d-b5a9-b74daa55b6df', contemporary_social_justice_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('363500ba-d034-408d-b5a9-b74daa55b6df', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_scholars).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, hindu_community_leaders).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_individuals).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Dharmasastra texts to emphasize universal ethical principles (dharma) while recontextualizing or discarding caste-based and gender-discriminatory prescriptions. They gain authority by making the tradition relevant to modern ethics.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_scholars, agenda_setter,
    institutional, generational, analytical, global).

% Benefit from a more inclusive and ethically defensible framework for community guidance, which helps retain adherents in a modern context. They implement the reformist interpretations in local practice, facing pressure from both orthodox and abolitionist views.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, hindu_community_leaders, beneficiary,
    organized, biographical, constrained, national).

% While explicit enforcement of caste discrimination is reduced, they still bear the residual social stigma and historical disadvantages rooted in the Dharmasastra's traditional interpretations. Their identity is often deeply intertwined with the religious tradition, making exit difficult.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_individuals, payer,
    powerless, generational, identity_locked, national).

% Experience a softening of traditional gender roles and restrictions, but still face implicit biases and expectations derived from historical interpretations. They benefit from the ethical core but pay through lingering social expectations.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women, payer,
    moderate, biographical, constrained, national).

% Reject the reformist contextualization, insisting on the literal and eternal validity of all Dharmasastra prescriptions, including caste and gender hierarchies. They are excluded from the reformist discourse but continue to exert influence within their own spheres.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalists, excluded,
    powerful, generational, identity_locked, global).

% Advocate for the complete rejection of Dharmasastra, viewing it as fundamentally oppressive and beyond reform. They are excluded from the reformist project, as their goal is to dismantle the entire textual authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_activists, excluded,
    organized, biographical, mobile, national).

% Academics and scholars who study the evolution of Dharmasastra interpretation, analyzing its social impact and the dynamics of reform movements without being directly subject to its normative authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, reformist_scholars).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for righteous conduct (dharma) and social order by adapting ancient religious texts to modern ethical sensibilities, thereby maintaining the tradition's relevance and authority.
% TRANSFER_FUNCTION: Transfers moral guidance, spiritual meaning, and social cohesion to the community, while implicitly transferring interpretive authority to reformist scholars and maintaining some residual social hierarchy and traditional expectations.
% ABSENT_VOICES: Orthodox literalists would object to any reinterpretation that deviates from the literal text, arguing for the eternal and unchanging nature of all prescriptions. Abolitionist activists would reject the entire framework as inherently oppressive and beyond reform.
% DISAPPEARANCE_RATIONALE: If this reformist interpretation vanished, the Hindu community would lose a crucial mechanism for reconciling tradition with modernity. This could lead to either a resurgence of rigid literalism or a widespread abandonment of Dharmasastra, causing significant social, ethical, and religious reorganization.
% FOUNDING_PROBLEM: To reconcile ancient religious texts containing socially oppressive elements (like strict caste rules and gender restrictions) with evolving modern ethical standards and social realities, while preserving the texts' spiritual authority and cultural continuity.
% FOUNDING_PROBLEM_CORROBORATION: Numerous contemporary Hindu reform movements, interfaith dialogues, and academic theological studies corroborate the ongoing challenge of adapting religious traditions to modern ethics, supporting the live status of this problem. Legal reforms in India addressing caste discrimination also highlight the continued societal impact of these historical structures.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while explicit enforcement of oppressive caste rules is largely abandoned, residual social stigma and traditional expectations persist, particularly for lower-caste individuals and women. Suppression (0.40) is also moderate; active coercion for caste observance has decreased, but social pressure to conform to community norms and respect traditional authority remains. The theater ratio (0.25) is low to moderate, reflecting genuine efforts at ethical adaptation, but also some performative reinterpretation that doesn't fully dismantle underlying power structures. The claimed type is 'tangled_rope' because it provides a genuine coordination function (ethical guidance and community cohesion) but simultaneously maintains an asymmetric extraction, requiring active enforcement to manage the tension between tradition and modernity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of reformist scholars, this reading is a necessary and beneficial adaptation, a 'rope' that preserves tradition while promoting justice. From the perspective of lower-caste individuals, it may still feel like a 'snare' or 'tangled_rope' due to the persistence of subtle forms of discrimination and the slow pace of change, despite the reformist intent. The engine's classification as 'tangled_rope' captures this inherent tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and community leaders are beneficiaries; they gain authority and relevance by making the tradition adaptable. Lower-caste individuals and women are payers; while benefiting from ethical guidance, they still bear the costs of residual social hierarchy and traditional expectations. Orthodox literalists and abolitionist activists are excluded, as their positions fundamentally challenge the reformist project.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the reformist effort as pure extraction (snare) by acknowledging its genuine coordination function in providing ethical guidance and preserving cultural continuity. Conversely, it prevents mislabeling it as pure coordination (rope) by recognizing the persistent, albeit softened, extraction from historically marginalized groups. The 'tangled_rope' accurately reflects the ongoing struggle to balance tradition, ethics, and social justice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_reformist_contextual,
    'This constraint is the ''reformist_contextual'' reading of the ''dharmasastra_corpus'' kernel. What would be the classification impact if an alternative reading (e.g., ''orthodox_literalist'' or ''abolitionist_rejection'') were adopted?',
    'Analysis of the structural properties (extraction, suppression, beneficiaries, victims) of the alternative reading as a separate constraint story.',
    'An ''orthodox_literalist'' reading would likely result in higher extraction and suppression, potentially classifying as a ''snare'' or ''tangled_rope'' with a larger victim set. An ''abolitionist_rejection'' reading would classify the existing corpus as a ''snare'' with maximal extraction and suppression, advocating for its complete dismantling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_reformist_contextual, conceptual, 'Impact of adopting sibling readings of the Dharmasastra corpus.').

omega_variable(
    implicit_caste_persistence,
    'To what extent does the ''ethical core'' of Dharmasastra, even under reformist interpretation, remain implicitly tied to or enable the persistence of ''time-bound caste prescriptions'' in actual social practice?',
    'Sociological field studies and ethnographic research on contemporary Hindu communities, measuring the actual impact on social mobility, inter-caste relations, and access to resources for lower-caste individuals.',
    'If implicit ties are strong, the effective extraction from lower-caste individuals is higher than measured, pushing the classification closer to a ''snare'' for those seats. If the ethical core genuinely detaches, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_caste_persistence, empirical, 'Persistence of caste-based extraction despite reformist interpretations.').

omega_variable(
    gender_role_reinterpretation_efficacy,
    'How effectively do reformist interpretations dismantle or reframe traditional gender roles and restrictions prescribed in Dharmasastra, and what is the lived experience of women under these interpretations?',
    'Qualitative and quantitative studies on women''s agency, educational attainment, economic participation, and domestic roles within communities guided by reformist interpretations, compared to those under orthodox interpretations.',
    'If reinterpretation is largely symbolic, the effective extraction from women is higher, indicating a greater ''theater_ratio'' and pushing the classification closer to a ''snare'' for women. If it leads to substantive changes, extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_role_reinterpretation_efficacy, empirical, 'Efficacy of reformist interpretations in addressing gender inequality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__reformist_contextual, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(dhar_tr_t1960, dharmasastra_corpus__reformist_contextual, theater_ratio, 1960, 0.17).
narrative_ontology:measurement(dhar_tr_t1970, dharmasastra_corpus__reformist_contextual, theater_ratio, 1970, 0.19).
narrative_ontology:measurement(dhar_tr_t1980, dharmasastra_corpus__reformist_contextual, theater_ratio, 1980, 0.21).
narrative_ontology:measurement(dhar_tr_t1990, dharmasastra_corpus__reformist_contextual, theater_ratio, 1990, 0.23).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__reformist_contextual, theater_ratio, 2000, 0.24).
narrative_ontology:measurement(dhar_tr_t2010, dharmasastra_corpus__reformist_contextual, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(dhar_tr_t2020, dharmasastra_corpus__reformist_contextual, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(dhar_be_t1960, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(dhar_be_t1970, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1970, 0.52).
narrative_ontology:measurement(dhar_be_t1980, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1980, 0.49).
narrative_ontology:measurement(dhar_be_t1990, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1990, 0.47).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2000, 0.46).
narrative_ontology:measurement(dhar_be_t2010, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(dhar_be_t2020, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(dhar_su_t1960, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(dhar_su_t1970, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1970, 0.47).
narrative_ontology:measurement(dhar_su_t1980, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(dhar_su_t1990, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement(dhar_su_t2010, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(dhar_su_t2020, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
