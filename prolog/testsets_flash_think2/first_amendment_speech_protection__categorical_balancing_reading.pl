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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: First Amendment Categorical Balancing Doctrine
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint describes the First Amendment doctrine where speech
 *   protection is determined by judicial categorization (e.g., obscenity,
 *   incitement, true threats are unprotected) and case-by-case balancing of
 *   speech value against potential harm. This is one reading of the broader
 *   'first_amendment_speech_protection' kernel. The doctrine aims to
 *   reconcile free speech with social order but grants significant
 *   interpretive power to the judiciary, leading to ongoing debates about
 *   predictability and fairness.
 *
 * KEY AGENTS:
 *   - institutional_judiciary: Primary agenda-setter (institutional/arbitrage) — defines and enforces categories.
 *   - minority_speakers: Primary target (powerless/constrained) — bears the costs of unpredictable suppression.
 *   - legal_predictability: Victim (non-agent/trapped) — suffers from the case-by-case nature.
 *   - absolutist_advocates: Excluded (organized/constrained) — advocate for a different framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.65).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.75).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Categorical Balancing Doctrine").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '7fe622a5-178a-445a-b1aa-3945a137764e').
narrative_ontology:cs_kernel_codification('7fe622a5-178a-445a-b1aa-3945a137764e', fixed_text).
narrative_ontology:cs_authority_grounding('7fe622a5-178a-445a-b1aa-3945a137764e', lineage).
narrative_ontology:cs_interpretation_layer_present('7fe622a5-178a-445a-b1aa-3945a137764e').
narrative_ontology:cs_reading_relation('7fe622a5-178a-445a-b1aa-3945a137764e', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7fe622a5-178a-445a-b1aa-3945a137764e', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('7fe622a5-178a-445a-b1aa-3945a137764e', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('7fe622a5-178a-445a-b1aa-3945a137764e', speech_is_not_absolute, conventional).
narrative_ontology:cs_axiom('7fe622a5-178a-445a-b1aa-3945a137764e', foundational, judicial_interpretive_supremacy).
narrative_ontology:cs_axiom_status(judicial_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('7fe622a5-178a-445a-b1aa-3945a137764e', judicial_interpretive_supremacy, conventional).
narrative_ontology:cs_reference_frame('7fe622a5-178a-445a-b1aa-3945a137764e', chaplinsky_framework).
narrative_ontology:cs_drift_state('7fe622a5-178a-445a-b1aa-3945a137764e', contemporary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7fe622a5-178a-445a-b1aa-3945a137764e', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, legal_scholars).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, controversial_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and applies categories of protected and unprotected speech, balancing societal interests against individual expression. Benefits from maintaining interpretive control over a complex and evolving area of law.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the ongoing complexity, ambiguity, and evolution of the doctrine, which provides rich material for academic research, commentary, and debate.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_scholars, beneficiary,
    analytical, generational, analytical, universal).

% Often bear the brunt of speech restrictions or have their expression devalued by categorization, facing uncertainty and chilling effects due to the unpredictable nature of balancing tests.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers, payer,
    powerless, biographical, constrained, national).

% Their speech is frequently subject to balancing tests, leading to unpredictable outcomes and potential suppression, as their expression often pushes the boundaries of established categories.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% The case-by-case nature of categorical balancing inherently reduces the ability of individuals and organizations to predict legal outcomes for their speech, creating a chilling effect.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Advocate for a much broader, less categorized protection of speech, arguing that 'no law' means virtually no law. Their view is largely outside the mainstream judicial doctrine and is structurally excluded from its interpretive framework.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_advocates, excluded,
    organized, biographical, constrained, national).

% Advocate for speech limits based purely on demonstrable, unconsented-to harm, a different balancing approach than the categorical one. Their preferred framework is not the one currently applied by the judiciary.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, harm_principle_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for distinguishing between protected and unprotected speech, allowing for some regulation of harmful expression (e.g., incitement, obscenity) while generally upholding free speech, thereby maintaining social order and public safety.
% TRANSFER_FUNCTION: Transfers interpretive authority over speech limits to the institutional judiciary, and transfers the burden of uncertainty and potential suppression to speakers whose expression falls into contested or 'unprotected' categories.
% ABSENT_VOICES: Absolutist advocates (who see any categorization as an infringement) and pure harm-principle advocates (who want a different, more consistent harm threshold) are structurally excluded from the categorical balancing framework. They would argue for clearer, more consistent, or less restrictive speech protections.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished overnight, the legal landscape for speech would be chaotic. Without a framework for distinguishing protected from unprotected speech, either all speech would be protected (leading to social disorder) or all speech could be regulated (leading to authoritarianism), completely reorganizing public discourse and legal practice.
% FOUNDING_PROBLEM: To reconcile the broad, unqualified language of the First Amendment ('Congress shall make no law...') with the practical necessity for a functioning society to regulate certain types of harmful expression (e.g., incitement, defamation, obscenity).
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the ideological spectrum, historical legal texts, and ongoing public policy debates consistently acknowledge the tension between expansive free speech and the needs of social order, corroborating the enduring nature of this problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the doctrine grants the judiciary significant power to define and redefine speech categories, which can be leveraged to suppress disfavored speech or maintain institutional control. Suppression is high due to the chilling effect created by unpredictable balancing tests and the active enforcement against speech deemed 'unprotected'. Theater ratio is moderate as the balancing act can sometimes appear ritualized or inconsistent, though genuine legal analysis occurs. Accessibility collapse is high for speech falling into unprotected categories, and resistance is high due to constant legal challenges and academic critique.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this doctrine is a necessary and evolving mechanism for maintaining social order while upholding free speech. From the perspective of speakers, particularly those in minority or controversial groups, the same structure can feel arbitrary, unpredictable, and suppressive, leading to a chilling effect on expression.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary benefits from its interpretive control and the complexity of the doctrine, which solidifies its role as the ultimate arbiter of speech. Legal scholars also benefit from the rich material for analysis. Minority and controversial speakers are targets, bearing the costs of uncertainty and potential suppression. Legal predictability, as a non-agent, is a victim of the doctrine's inherent case-by-case nature. Absolutist and harm-principle advocates are excluded, as their frameworks are not the ones being applied.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction and suppression) or a pure Snare (which would ignore the genuine, albeit imperfect, coordination function of defining speech limits). The doctrine genuinely attempts to coordinate free expression with social order, but it does so with substantial asymmetric extraction of interpretive control and imposition of costs on certain speakers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_objectivity,
    'To what extent is the categorical balancing doctrine applied objectively, versus being influenced by judicial discretion, ideological leanings, or political pressures?',
    'Empirical analysis of judicial decisions over time, correlating outcomes with judicial appointments, political climate, and consistency across similar cases. Comparative legal studies of how different courts apply the doctrine.',
    'If discretion is high, the effective extractiveness and suppression are higher than measured, as the constraint''s application is less about neutral principles and more about power. If objectivity is high, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_objectivity, empirical, 'Ambiguity regarding the objectivity of judicial application of speech categories.').

omega_variable(
    effectiveness_of_categorical_approach,
    'Is the categorical balancing approach the most effective and just method for reconciling free speech with societal needs, or does it create more problems (e.g., chilling effects, unpredictability) than it solves compared to alternative frameworks?',
    'Comparative analysis with legal systems employing different speech regulation models (e.g., pure harm principle, content-neutrality focus). Public opinion surveys on perceived fairness and predictability. Longitudinal studies of speech suppression trends.',
    'If less effective, the constraint''s coordination function is weaker, and its extraction/suppression are less justified, pushing it closer to a Snare. If more effective, its Rope-like qualities are reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_categorical_approach, conceptual, 'Whether the categorical approach is optimal for speech regulation.').

omega_variable(
    minority_speaker_impact_disparity,
    'Does the categorical balancing doctrine disproportionately impact minority or marginalized speakers, leading to a greater chilling effect or suppression of their expression compared to dominant groups?',
    'Empirical studies analyzing the demographics of individuals and groups whose speech is challenged or suppressed under this doctrine. Content analysis of speech deemed ''unprotected'' versus ''protected'' across different social groups.',
    'If disproportionate impact is confirmed, the effective extractiveness and suppression for minority speakers are significantly higher than the aggregate measures suggest, highlighting a deeper structural inequity within the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_speaker_impact_disparity, empirical, 'Disparate impact of speech categories on marginalized groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1940, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(firs_tr_t1956, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1956, 0.25).
narrative_ontology:measurement(firs_tr_t1972, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1972, 0.3).
narrative_ontology:measurement(firs_tr_t1988, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1988, 0.35).
narrative_ontology:measurement(firs_tr_t2004, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2004, 0.38).
narrative_ontology:measurement(firs_tr_t2020, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1940, 0.55).
narrative_ontology:measurement(firs_be_t1956, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1956, 0.58).
narrative_ontology:measurement(firs_be_t1972, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1972, 0.61).
narrative_ontology:measurement(firs_be_t1988, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1988, 0.63).
narrative_ontology:measurement(firs_be_t2004, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2004, 0.64).
narrative_ontology:measurement(firs_be_t2020, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1940, 0.6).
narrative_ontology:measurement(firs_su_t1956, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1956, 0.65).
narrative_ontology:measurement(firs_su_t1972, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1972, 0.7).
narrative_ontology:measurement(firs_su_t1988, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1988, 0.72).
narrative_ontology:measurement(firs_su_t2004, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2004, 0.74).
narrative_ontology:measurement(firs_su_t2020, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'first_amendment_speech_protection' kernel, alongside the 'absolutist_reading' and 'harm_limited_reading'. Each reading instantiates a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
