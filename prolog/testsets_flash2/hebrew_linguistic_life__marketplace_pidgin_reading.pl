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
 *   This constraint defines 'linguistic life' for Hebrew through the lens of
 *   its continuous function as an inter-communal medium for practical
 *   coordination in Jerusalem markets, prior to the modern revival movement.
 *   It argues that Hebrew was continuously 'alive' in this pidgin form,
 *   adapting to serve a functional role, regardless of whether it was a
 *   native tongue or used for sacred texts. This reading challenges the
 *   notion that Hebrew was 'dead' before its modern revival, by focusing on
 *   its pragmatic utility. The constraint is claimed as a Mountain because it
 *   describes a structural feature of linguistic function, not a
 *   human-enforced rule.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.15).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.05).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Hebrew Linguistic Life: Marketplace Pidgin Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:emerges_naturally(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '265dc14f-6ce6-4b15-b638-109749904449').
narrative_ontology:cs_kernel_codification('265dc14f-6ce6-4b15-b638-109749904449', distributed).
narrative_ontology:cs_authority_grounding('265dc14f-6ce6-4b15-b638-109749904449', diffuse_epistemic).
narrative_ontology:cs_reading_relation('265dc14f-6ce6-4b15-b638-109749904449', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('265dc14f-6ce6-4b15-b638-109749904449', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_axiom('265dc14f-6ce6-4b15-b638-109749904449', foundational, functional_utility_equals_life).
narrative_ontology:cs_axiom_status(functional_utility_equals_life, holdable).
narrative_ontology:cs_axiom_grounding('265dc14f-6ce6-4b15-b638-109749904449', functional_utility_equals_life, conventional).
narrative_ontology:cs_axiom('265dc14f-6ce6-4b15-b638-109749904449', secondary, pidgin_is_language).
narrative_ontology:cs_axiom_status(pidgin_is_language, holdable).
narrative_ontology:cs_axiom_grounding('265dc14f-6ce6-4b15-b638-109749904449', pidgin_is_language, conventional).
narrative_ontology:cs_reference_frame('265dc14f-6ce6-4b15-b638-109749904449', continuous_functional_adaptation).
narrative_ontology:cs_drift_state('265dc14f-6ce6-4b15-b638-109749904449', contemporary_sociolinguistics, gap(stable, minor, true)).
narrative_ontology:cs_created_at('265dc14f-6ce6-4b15-b638-109749904449', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, diverse_pilgrims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used a modified Medieval Hebrew pidgin as a lingua franca for trade with diverse communities, enabling practical coordination in the absence of a shared vernacular. Benefited from its utility as a low-friction medium.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, jerusalem_merchants, beneficiary,
    moderate, biographical, mobile, local).

% Relied on the marketplace pidgin for basic transactions and communication during their stays in Jerusalem, bridging linguistic gaps with local traders and other pilgrims. Their benefit was immediate practical utility.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, diverse_pilgrims, beneficiary,
    powerless, immediate, constrained, local).

% Analyze historical texts and records to reconstruct the actual usage of Hebrew in various contexts, including its function as a pidgin in pre-modern Jerusalem markets. Their role is to document and interpret linguistic evolution.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, linguistic_historians, observer,
    analytical, generational, analytical, global).

% Would likely dismiss the marketplace pidgin as a 'degraded' form of Hebrew, not true linguistic life, as their definition centers on native, comprehensive use. Their perspective is excluded from this reading's definition of 'alive'.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_revivalists, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled inter-communal trade and basic communication in a linguistically diverse environment by providing a shared, albeit simplified, medium.
% TRANSFER_FUNCTION: Facilitated the exchange of goods, services, and basic information between speakers of different vernaculars, transferring practical understanding.
% ABSENT_VOICES: Modern Hebrew revivalists and purists would object, arguing that a pidgin does not constitute 'true' linguistic life, but their criteria for aliveness are not relevant to this reading's functional definition.
% DISAPPEARANCE_RATIONALE: This reading describes a historical linguistic phenomenon. Its 'disappearance' would mean the pidgin ceased to be used, but the underlying principle (language is alive if it functions for coordination) would remain a valid analytical framework for other languages. The world of linguistic analysis would not rearrange.
% FOUNDING_PROBLEM: The need for practical communication and trade between diverse linguistic communities in a central hub like Jerusalem.
% FOUNDING_PROBLEM_CORROBORATION: Linguistic historians attest to the historical existence and function of such pidgins. The problem of inter-communal communication in Jerusalem has since been addressed by other lingua francas and the modern revival of Hebrew, rendering the specific pidgin obsolete.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, world_unchanged).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The metrics reflect the nature of a functional linguistic medium. Extractiveness is low (0.15) because the pidgin served as a tool for mutual benefit in trade, not for extraction. Suppression is negligible (0.05) as its use was voluntary and driven by utility; no active enforcement was required. Theater ratio is very low (0.02) as its function was purely practical, not performative. Accessibility collapse is high (0.88) because once the pidgin was understood, it effectively collapsed the need for other, more complex linguistic solutions for basic trade. Resistance is minimal (0.01) because it was a useful tool, not a burden.
 *
 * PERSPECTIVAL GAP:
 *   This reading directly contrasts with those that define linguistic life by native speakers or sacred use. From the perspective of a native generational reading, the pidgin would be seen as a 'dead' or 'degraded' form, while from the liturgical preservation reading, its secular, simplified use would be irrelevant to its 'aliveness'. This constraint asserts a different, functional criterion for linguistic vitality.
 *
 * DIRECTIONALITY LOGIC:
 *   Both Jerusalem merchants and diverse pilgrims are beneficiaries (d near 0.0) as they directly gained from the pidgin's ability to facilitate trade and communication. Linguistic historians are observers (d near 0.5). Hebrew revivalists are excluded (d near 1.0) as their definitional framework for 'linguistic life' is fundamentally at odds with this reading, making them targets of its conceptual implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_for_pidgin,
    'How robust is the historical and linguistic evidence for the continuous, widespread use of a functional Hebrew pidgin in Jerusalem markets prior to 1880?',
    'Discovery of new primary source documents (e.g., merchant ledgers, travelogues, legal records) that explicitly describe or demonstrate such usage, or further comparative linguistic analysis of surviving texts.',
    'Stronger evidence would solidify this reading''s claim of continuous linguistic life, weakening arguments for a ''dead'' period. Weaker evidence would shift the classification towards a conceptual construct rather than an empirical mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_for_pidgin, empirical, 'Empirical support for the historical existence and function of the Hebrew pidgin.').

omega_variable(
    definition_of_linguistic_life,
    'Is ''functional inter-communal medium for practical coordination'' a sufficient criterion for ''linguistic life'', or are other criteria (e.g., native speakers, full grammatical complexity, sacred function) also necessary?',
    'Conceptual analysis and consensus within sociolinguistics on the minimal criteria for a language to be considered ''alive''. This is a definitional rather than empirical question.',
    'If functional coordination is deemed insufficient, this constraint''s ''mountain'' classification would be challenged, potentially reclassifying it as a ''conceptual rope'' or ''snare'' depending on who benefits from the definitional choice. If sufficient, it reinforces the mountain status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_linguistic_life, conceptual, 'The conceptual boundary of ''linguistic life'' and the sufficiency of functional criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 1600, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1600, 0.02).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1700, 0.02).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 1880, 0.02).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(hebr_be_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 1880, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1600, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(hebr_su_t1700, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1700, 0.05).
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 1880, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, information_standard).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life__native_generational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'hebrew_linguistic_life' kernel, focusing on its functional use as a marketplace pidgin. It is linked to sibling readings that define linguistic life differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
