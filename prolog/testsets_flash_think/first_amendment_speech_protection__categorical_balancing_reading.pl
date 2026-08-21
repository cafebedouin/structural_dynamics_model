% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Categorical Balancing of Speech
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint describes the dominant judicial reading of the First
 *   Amendment, which establishes categories of protected and unprotected
 *   speech through a case-by-case balancing of speech value against potential
 *   harm. This approach grants significant interpretive power to the
 *   institutional judiciary, defining the boundaries of free expression and
 *   actively enforcing those boundaries through legal precedent and rulings.
 *   The constraint is claimed as a Rope by its proponents (a necessary
 *   coordination mechanism), but its operation, as described by the metrics,
 *   is substantially extractive and suppressive, making it a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.75).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing of Speech").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '6b9f8d3f-2402-4808-8313-e43dc9432d9a').
narrative_ontology:cs_kernel_codification('6b9f8d3f-2402-4808-8313-e43dc9432d9a', fixed_text).
narrative_ontology:cs_authority_grounding('6b9f8d3f-2402-4808-8313-e43dc9432d9a', lineage).
narrative_ontology:cs_interpretation_layer_present('6b9f8d3f-2402-4808-8313-e43dc9432d9a').
narrative_ontology:cs_reading_relation('6b9f8d3f-2402-4808-8313-e43dc9432d9a', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('6b9f8d3f-2402-4808-8313-e43dc9432d9a', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('6b9f8d3f-2402-4808-8313-e43dc9432d9a', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('6b9f8d3f-2402-4808-8313-e43dc9432d9a', speech_is_not_absolute, conventional).
narrative_ontology:cs_axiom('6b9f8d3f-2402-4808-8313-e43dc9432d9a', foundational, judicial_role_in_defining_speech_boundaries).
narrative_ontology:cs_axiom_status(judicial_role_in_defining_speech_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('6b9f8d3f-2402-4808-8313-e43dc9432d9a', judicial_role_in_defining_speech_boundaries, conventional).
narrative_ontology:cs_reference_frame('6b9f8d3f-2402-4808-8313-e43dc9432d9a', judicial_balancing_tradition).
narrative_ontology:cs_drift_state('6b9f8d3f-2402-4808-8313-e43dc9432d9a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6b9f8d3f-2402-4808-8313-e43dc9432d9a', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, legal_scholars).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, general_public).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, political_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, general_public).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, harm_principle_in_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreter and enforcer of First Amendment doctrine, defining categories of protected and unprotected speech through case-by-case balancing. Benefits from maintaining interpretive control and the complexity of the doctrine.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups whose speech often falls into categories deemed 'unprotected' (e.g., obscenity, incitement) or is subject to balancing tests that limit its scope. They bear the cost of legal uncertainty and potential suppression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers, payer,
    powerless, immediate, constrained, national).

% Benefit from the intellectual complexity and ongoing debate surrounding the categorical balancing approach, which provides a rich field for academic analysis, commentary, and career advancement.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_scholars, beneficiary,
    organized, biographical, analytical, global).

% Speakers whose controversial or challenging political speech may be subject to incitement or true threat categories, leading to suppression or self-censorship due to the unpredictable nature of balancing tests.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, political_dissidents, payer,
    powerless, immediate, constrained, national).

% Benefits from the perceived order and protection from certain harms (e.g., incitement to violence, defamation) that the balancing framework aims to prevent. However, they also bear the cost of reduced overall speech and the chilling effect on controversial expression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, general_public, payer).

% Advocates for a near-absolute interpretation of the First Amendment, who find their core premise of 'no law' systematically rejected by the categorical balancing approach. Their arguments are often marginalized within mainstream jurisprudence.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_advocates, excluded,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To define the boundaries of protected speech, preventing perceived harms (e.g., incitement, obscenity) while upholding a core of free expression, thereby providing a framework for judicial review of speech regulations.
% TRANSFER_FUNCTION: Transfers interpretive authority over speech categories to the judiciary, and the burden of navigating complex, evolving legal tests to speakers, particularly those whose expression is controversial or challenges established norms.
% ABSENT_VOICES: Advocates for an absolutist reading of the First Amendment, who argue that any categorical exclusion or balancing test undermines the core principle of 'no law' and would object to the judiciary's expansive role in defining speech categories. They are excluded by the very premise of this reading.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the legal system would lack a coherent method for adjudicating speech disputes, leading to either unchecked speech with potential harms or arbitrary suppression. The entire body of modern First Amendment jurisprudence would collapse, requiring a new framework for balancing rights and harms.
% FOUNDING_PROBLEM: To reconcile the constitutional guarantee of free speech with the need to maintain public order and protect individuals from certain harms, which the framers did not explicitly define, leading to a need for judicial interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and the judiciary itself attest that the tension between free speech and public order remains a live problem, requiring ongoing judicial interpretation. Critics, however, argue that the problem has been over-solved in favor of judicial power, and that the original intent was for broader protection.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the balancing framework allows for the selective application of speech limits, often to the detriment of less powerful speakers, and benefits the judiciary by maintaining its interpretive authority. Suppression is also high (0.75) due to the active judicial enforcement of categorical exclusions (e.g., obscenity, incitement, true threats) and the chilling effect of unpredictable balancing tests. The theater ratio is moderate (0.45) as the balancing act can sometimes be performative, masking underlying policy preferences or political considerations behind legalistic reasoning. Accessibility collapse is moderate-high (0.65) because alternatives to judicial interpretation for defining speech categories are severely limited. Resistance is high (0.70) due to constant litigation, academic debate, and public discourse challenging the boundaries and application of these categories.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional judiciary, this framework is a necessary and principled method for coordinating free speech with other societal values and preventing harm. From the perspective of minority speakers and political dissidents, the same framework can appear as an arbitrary and suppressive mechanism that disproportionately targets their expression, serving to maintain existing power structures. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary is a primary beneficiary (low d) as it gains and maintains interpretive control over a fundamental constitutional right. Legal scholars also benefit (low d) from the complexity and ongoing intellectual activity generated by this framework. The general public is a mixed beneficiary/payer (symmetric d), gaining perceived order but losing some expressive freedom. Minority speakers and political dissidents are clear targets (high d), as their speech is often the subject of suppression or legal challenge under this framework. Legal predictability is a victim (high d) because the case-by-case balancing introduces inherent uncertainty.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_principled_application,
    'To what extent is the categorical balancing a principled application of legal doctrine versus an exercise of judicial policy preference or political expediency?',
    'Longitudinal empirical studies of judicial decision-making, analyzing correlations between judicial ideology/background and outcomes in speech cases, controlling for legal factors.',
    'If primarily policy-driven, the constraint''s extractiveness and suppression would be reclassified as higher, reflecting a more arbitrary and less legitimate exercise of power. If principled, the current metrics are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_principled_application, empirical, 'Ambiguity regarding the true basis of judicial balancing decisions.').

omega_variable(
    categorical_necessity_vs_slippery_slope,
    'Are the established categories of unprotected speech (e.g., incitement, obscenity) truly necessary to prevent specific harms, or do they create a ''slippery slope'' that enables broader suppression of speech?',
    'Comparative legal analysis with jurisdictions employing different speech frameworks, assessing outcomes regarding harm prevention and expressive freedom. Historical analysis of how categories have expanded or contracted over time.',
    'If categories are found to be unnecessary or to enable broader suppression, the suppression metric would be re-evaluated upward, and the constraint''s classification would lean more towards Snare. If necessary, the current metrics are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_necessity_vs_slippery_slope, conceptual, 'Debate over the structural necessity and potential overreach of speech categories.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''categorical_balancing_reading'' of the ''first_amendment_speech_protection'' kernel. What are the precise structural differences between this reading and its siblings?',
    'Detailed comparative analysis of the core premises, beneficiaries, victims, and enforcement mechanisms of the ''absolutist_reading'' and ''harm_limited_reading'' siblings.',
    'Clarifies the specific structural delta that leads to different classifications across readings of the same kernel, enabling precise cross-reading comparisons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Documents this constraint as one specific reading within a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1950, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(firs_tr_t1960, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(firs_tr_t1970, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1970, 0.4).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1980, 0.42).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1990, 0.44).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(firs_tr_t2020, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(firs_be_t1950, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(firs_be_t1960, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(firs_be_t1970, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1970, 0.61).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1980, 0.64).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2000, 0.67).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(firs_be_t2020, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1950, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1950, 0.6).
narrative_ontology:measurement(firs_su_t1960, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(firs_su_t1970, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1990, 0.74).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(firs_su_t2020, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the First Amendment's speech protection kernel. Each reading instantiates a different constraint with unique structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
