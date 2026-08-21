% ============================================================================
% CONSTRAINT STORY: state_execution_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__abolition_reading, []).

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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: Categorical Impermissibility of State Execution (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of state
 *   execution authority, which holds that state execution is categorically
 *   impermissible regardless of crime severity or procedural safeguards. From
 *   this perspective, the act of execution is a violation of a fundamental
 *   moral limit, and any justification (retribution, deterrence) is rejected.
 *   The constraint is claimed as a 'Mountain' because its impermissibility is
 *   asserted as a natural moral law, even though the state's actual practice
 *   operates as an extractive, enforced mechanism. This divergence between
 *   the claimed type and the operational metrics is central to the analysis.
 *
 * KEY AGENTS:
 *   - abolitionist_advocates: Primary agenda-setter (organized/constrained) — seeks to establish impermissibility
 *   - executed_persons: Primary target (powerless/trapped) — bears ultimate extraction
 *   - state_execution_authorities: Primary beneficiary (institutional/constrained) — benefits from exercising impermissible power
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) — monitors and advocates for abolition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.85).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, mountain).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "Categorical Impermissibility of State Execution (Abolitionist Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).
domain_priors:emerges_naturally(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'da910bce-5136-4f11-9961-a7f7482dcf22').
narrative_ontology:cs_kernel_codification('da910bce-5136-4f11-9961-a7f7482dcf22', formalized).
narrative_ontology:cs_authority_grounding('da910bce-5136-4f11-9961-a7f7482dcf22', lineage).
narrative_ontology:cs_interpretation_layer_present('da910bce-5136-4f11-9961-a7f7482dcf22').
narrative_ontology:cs_reading_relation('da910bce-5136-4f11-9961-a7f7482dcf22', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('da910bce-5136-4f11-9961-a7f7482dcf22', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('da910bce-5136-4f11-9961-a7f7482dcf22', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('da910bce-5136-4f11-9961-a7f7482dcf22', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('da910bce-5136-4f11-9961-a7f7482dcf22', foundational, inherent_right_to_life).
narrative_ontology:cs_axiom_status(inherent_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('da910bce-5136-4f11-9961-a7f7482dcf22', inherent_right_to_life, deontological).
narrative_ontology:cs_reference_frame('da910bce-5136-4f11-9961-a7f7482dcf22', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('da910bce-5136-4f11-9961-a7f7482dcf22', contemporary_global_abolition_movement, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('da910bce-5136-4f11-9961-a7f7482dcf22', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, state_execution_authorities).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, pro_death_penalty_advocates).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, society_as_a_whole).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively campaign against state execution, seeking to establish its categorical impermissibility in law and practice. They face institutional resistance but represent a growing global movement.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Are the direct targets of state execution, experiencing the ultimate and irreversible extraction of life. From this reading, their guilt or innocence is irrelevant to the impermissibility of the act.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% Bear the profound and lasting emotional, social, and often economic costs of state execution. They are often marginalized in public discourse and face systemic barriers to justice or recognition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed, payer,
    powerless, biographical, trapped, local).

% From this reading, society bears a moral burden and suffers a degradation of its ethical standards by permitting state execution, regardless of individual crime. This cost is diffuse but fundamental.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, society_as_a_whole, payer,
    moderate, generational, constrained, national).

% Are the governmental bodies (courts, prisons, executive branches) that authorize and carry out executions. From this reading, they benefit from maintaining a power that is categorically impermissible, even if they claim it is for justice or public safety.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_authorities, beneficiary,
    institutional, generational, constrained, national).

% Advocate for the continued use of capital punishment, often citing retribution or deterrence. From this reading, they benefit from the state's exercise of an impermissible power, aligning with a system that violates fundamental moral limits.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, pro_death_penalty_advocates, beneficiary,
    organized, biographical, mobile, national).

% Monitor and report on state execution practices, advocating for universal abolition based on human rights principles. They provide an external, analytical perspective on the constraint's operation and legitimacy.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this abolitionist reading, state execution has no legitimate coordination function; it is a categorical violation of an inherent moral limit, not a solution to a collective action problem.
% TRANSFER_FUNCTION: Transfers the right to life from the individual to the state, and transfers a moral burden onto society by engaging in an act deemed categorically impermissible.
% ABSENT_VOICES: Future generations who would inherit a more just society without state execution; those who would be wrongly executed; and the full moral weight of human dignity that is violated by the practice.
% DISAPPEARANCE_RATIONALE: If state execution were universally and permanently abolished overnight, the legal, moral, and political landscape regarding state power over life would fundamentally shift. It would necessitate a re-evaluation of punishment, justice, and human rights, leading to significant reorganization of legal systems and international relations.
% FOUNDING_PROBLEM: The problem of state-sanctioned killing, which this reading asserts is inherently and universally wrong, regardless of the crime committed or the procedural safeguards in place.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, philosophical arguments for inherent human dignity, and the historical record of wrongful executions and disproportionate application of the death penalty, all corroborate the ongoing nature of the problem of state-sanctioned killing.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_execution_authority__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_execution_authority__abolition_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, ExtMetricName, E),
    domain_priors:suppression_score(state_execution_authority__abolition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_execution_authority__abolition_reading),
    narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is set very high (0.95) because from this reading, the taking of a human life by the state is the ultimate extraction, a categorical violation. `Suppression` is also high (0.85) as states actively enforce their right to execute, suppressing the right to life of those condemned. `Theater_ratio` is low (0.10) because the act of execution is a stark, undeniable reality, not a performance. `Accessibility_collapse` is high (0.90) as for the executed, all alternatives collapse completely. `Resistance` is high (0.75) due to ongoing global abolitionist movements. The `claimed_type` is 'mountain' to reflect the reading's assertion of a fundamental, unchangeable moral limit, even as the metrics describe a highly extractive, enforced operation that violates this limit.
 *
 * PERSPECTIVAL GAP:
 *   The state and pro-death penalty advocates perceive state execution as a legitimate, necessary, or just function of the legal system (likely computing as a Tangled Rope or Snare from their seats). In contrast, this abolitionist reading views the same act as a categorical violation of a fundamental moral law, an impermissible extraction. The engine's computation will highlight this divergence between the claimed 'mountain' and the operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Executed persons and their families are clear targets, bearing the ultimate costs. Society as a whole is also a target, bearing a diffuse moral cost. State execution authorities and pro-death penalty advocates are beneficiaries, as they uphold and benefit from the exercise of this impermissible power. Abolitionist advocates act as agenda-setters, striving to enforce the moral mountain of impermissibility.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is less directly applicable to a categorical moral claim than to a functional constraint. However, if the 'mandate' is understood as the moral imperative to *not* execute, then the persistence of state execution represents a continuous failure to resolve this moral mandate. The high extractiveness and suppression indicate that the constraint (state execution) persists not due to a live, legitimate function, but due to institutional power and resistance to the moral imperative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_power,
    'Is the categorical impermissibility of state execution a genuine natural moral law (a Mountain), or is the state''s authority to execute a constructed power that benefits identifiable agents (a Snare/Tangled Rope)?',
    'Philosophical consensus on inherent human rights, or a global shift in legal practice that universally recognizes the impermissibility without active enforcement.',
    'If a genuine Mountain, the state''s practice is a violation of natural law. If a constructed power, the classification shifts to reflect its extractive and coercive nature, highlighting the beneficiaries of this power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_power, conceptual, 'Ambiguity between inherent moral limit and constructed state power.').

omega_variable(
    deontological_vs_consequentialist_grounding,
    'Is the impermissibility of state execution grounded purely in deontological principles (inherent right to life, dignity), or does it also rely on consequentialist arguments (risk of wrongful execution, disproportionate application)?',
    'Analysis of the core arguments used by abolitionist movements and legal scholars: if the argument holds even with perfect procedural safeguards and no risk of error, it is purely deontological.',
    'If purely deontological, the ''Mountain'' claim is stronger and less susceptible to empirical counter-arguments (e.g., ''perfect deterrence''). If partly consequentialist, the claim is more vulnerable to empirical challenges, potentially shifting its structural stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deontological_vs_consequentialist_grounding, conceptual, 'Grounding of the impermissibility claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1950, state_execution_authority__abolition_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(stat_tr_t1965, state_execution_authority__abolition_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(stat_tr_t1980, state_execution_authority__abolition_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(stat_tr_t1995, state_execution_authority__abolition_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(stat_tr_t2010, state_execution_authority__abolition_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(stat_tr_t2020, state_execution_authority__abolition_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1950, state_execution_authority__abolition_reading, base_extractiveness, 1950, 0.9).
narrative_ontology:measurement(stat_be_t1965, state_execution_authority__abolition_reading, base_extractiveness, 1965, 0.92).
narrative_ontology:measurement(stat_be_t1980, state_execution_authority__abolition_reading, base_extractiveness, 1980, 0.93).
narrative_ontology:measurement(stat_be_t1995, state_execution_authority__abolition_reading, base_extractiveness, 1995, 0.94).
narrative_ontology:measurement(stat_be_t2010, state_execution_authority__abolition_reading, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(stat_be_t2020, state_execution_authority__abolition_reading, base_extractiveness, 2020, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1950, state_execution_authority__abolition_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(stat_su_t1965, state_execution_authority__abolition_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(stat_su_t1980, state_execution_authority__abolition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(stat_su_t1995, state_execution_authority__abolition_reading, suppression_requirement, 1995, 0.82).
narrative_ontology:measurement(stat_su_t2010, state_execution_authority__abolition_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(stat_su_t2020, state_execution_authority__abolition_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, retributive_justice_system).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, deterrence_theory_in_law).
narrative_ontology:affects_constraint(state_execution_authority__abolition_reading, human_rights_law_development).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_execution_authority' kernel. It asserts categorical impermissibility, contrasting with retributive and deterrence readings that justify execution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
