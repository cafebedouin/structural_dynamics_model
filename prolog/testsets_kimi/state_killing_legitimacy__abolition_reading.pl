% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__abolition_reading, []).

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
 *   constraint_id: state_killing_legitimacy__abolition_reading
 *   human_readable: Abolitionist Reading: Categorical Prohibition of State Killing
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the abolition_reading of the
 *   state_killing_legitimacy kernel. It is the normative and legal
 *   prohibition that state killing categorically violates human dignity
 *   regardless of desert or utility. The constraint is authored as a
 *   tangled_rope because it coordinates a genuine protection (condemned
 *   persons' lives) while asymmetrically extracting from state punitive
 *   sovereignty. The condemned person is the beneficiary; the state execution
 *   authority is the payer and declared victim. International human rights
 *   bodies administer the norm, while crime victims and retributive advocates
 *   are structurally excluded from its dignity-based frame.
 *
 * KEY AGENTS:
 *   - condemned_persons: Primary beneficiary (powerless/trapped) â their lives are the object of unconditional protection.
 *   - state_execution_authority: Primary payer/victim (institutional/constrained) â bears the loss of sovereign killing power.
 *   - international_human_rights_bodies: Agenda setter (institutional/analytical) â administers and legitimates the norm globally.
 *   - crime_victim_families: Excluded voice (moderate/identity_locked) â retributive needs absent from dignity frame.
 *   - retributive_justice_advocates: Excluded voice (organized/constrained) â desert-based justification foreclosed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, 0.78).
domain_priors:suppression_score(state_killing_legitimacy__abolition_reading, 0.72).
domain_priors:theater_ratio(state_killing_legitimacy__abolition_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_legitimacy__abolition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__abolition_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__abolition_reading, "Abolitionist Reading: Categorical Prohibition of State Killing").
narrative_ontology:topic_domain(state_killing_legitimacy__abolition_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__abolition_reading, '25b37bf5-b6ac-4396-b523-627a74197faa').
narrative_ontology:cs_kernel_codification('25b37bf5-b6ac-4396-b523-627a74197faa', formalized).
narrative_ontology:cs_authority_grounding('25b37bf5-b6ac-4396-b523-627a74197faa', lineage).
narrative_ontology:cs_interpretation_layer_present('25b37bf5-b6ac-4396-b523-627a74197faa').
narrative_ontology:cs_reading_relation('25b37bf5-b6ac-4396-b523-627a74197faa', state_killing_legitimacy__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('25b37bf5-b6ac-4396-b523-627a74197faa', state_killing_legitimacy__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('25b37bf5-b6ac-4396-b523-627a74197faa', foundational, execution_categorically_impermissible).
narrative_ontology:cs_axiom_status(execution_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('25b37bf5-b6ac-4396-b523-627a74197faa', execution_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('25b37bf5-b6ac-4396-b523-627a74197faa', foundational, human_dignity_inviolable).
narrative_ontology:cs_axiom_status(human_dignity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('25b37bf5-b6ac-4396-b523-627a74197faa', human_dignity_inviolable, deontological).
narrative_ontology:cs_reference_frame('25b37bf5-b6ac-4396-b523-627a74197faa', unconditional_human_dignity_floor).
narrative_ontology:cs_drift_state('25b37bf5-b6ac-4396-b523-627a74197faa', contemporary_retentionist_resurgence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('25b37bf5-b6ac-4396-b523-627a74197faa', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__abolition_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__abolition_reading, condemned_persons).
narrative_ontology:constraint_victim(state_killing_legitimacy__abolition_reading, state_execution_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals under sentence of death who are categorically protected from execution by the dignity-based prohibition; their survival depends entirely on the constraint's enforcement against state power, and they cannot opt out of the protection without being executed.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, condemned_persons, beneficiary,
    powerless, immediate, trapped, national).

% The state's penal and executive institutions that historically exercised the power to kill condemned prisoners. The prohibition extracts their sovereign capacity to execute, forcing them to maintain incarceration and foreclosing a tool they previously claimed as rightful.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, state_execution_authority, payer,
    institutional, generational, constrained, national).

% Treaty bodies and monitoring organizations that legitimate, adjudicate, and enforce the abolition norm through international law, reporting mechanisms, and individual petitions against retentionist states.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Families of murder victims who would advocate for execution as retributive closure. Their voice is structurally excluded from dignity-based frameworks that treat the condemned person's life as unconditional and non-negotiable regardless of the harm suffered.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, crime_victim_families, excluded,
    moderate, biographical, identity_locked, national).

% Legal and political actors who hold that desert-based proportionality justifies execution. They are absent from the abolitionist frame because their core premise is categorically foreclosed by the dignity prohibition.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__abolition_reading, retributive_justice_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the state from extinguishing human life as a tool of punishment, establishing an unconditional floor below which state power cannot descend against any condemned person.
% TRANSFER_FUNCTION: Transfers the final say over the condemned person's existence from the state's punitive apparatus to a legal-ethical norm that unconditionally protects life regardless of desert or social utility.
% ABSENT_VOICES: Crime victims' families seeking retributive closure and retributive justice advocates who believe proportional desert justifies execution are structurally excluded because the dignity frame treats killing as unconditionally impermissible and therefore non-negotiable.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, states would resume executions, international human rights law would lose a foundational jus cogens anchor, and condemned persons would lose their unconditional protection; the global criminal justice landscape would reorganize around the re-legitimation of state killing.
% FOUNDING_PROBLEM: State killing as an instrument of criminal justice created a class of persons whose lives were contingent on sovereign will, judicial error, or political mood, with no inviolable floor against deliberate extinction by the state.
% FOUNDING_PROBLEM_CORROBORATION: International human rights treaty bodies and constitutional courts outside the direct beneficiary class attest to the problem; however, retentionist states and victim-advocacy groups deny the framing entirely, asserting that the founding problem is not the absence of a dignity floor but the absence of just desert.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__abolition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__abolition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__abolition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__abolition_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__abolition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__abolition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because a categorical prohibition imposes an absolute, non-compensable cost on the state's punitive capacity: the permanent loss of execution as a tool regardless of context. Suppression is high (0.72) because the constraint's persistence requires active legal and diplomatic enforcement against retentionist pressure. Theater ratio is low (0.25) because the dignity claim is substantively operational, though some enforcement is symbolic diplomacy. Accessibility collapse is high (0.75): once the norm is accepted, execution ceases to be an accessible policy option. Resistance is moderate (0.60) because retentionist states and victim-advocacy groups actively contest the norm. The metrics and claim are independently authored: the constraint is claimed as tangled_rope because it has both a genuine coordination function and asymmetric extraction, matching the high epsilon and suppression values without tuning either to the other.
 *
 * PERSPECTIVAL GAP:
 *   From the condemned person's seat, the constraint is protective coordination with near-zero extraction (subsidized existence). From the state execution authority's seat, the same constraint is heavy extraction of sovereign discretion (high effective extraction). From the international body's seat, it is neutral coordination with analytical distance. The engine computes this divergence from the structural data: beneficiary versus payer roles, trapped versus constrained exit, and powerless versus institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are declared beneficiaries with trapped exit and zero power, placing directionality near the full-beneficiary end; effective extraction is damped into subsidy. State execution authority is declared victim with constrained exit and institutional power, placing directionality near the full-target end; effective extraction is amplified. International bodies are agenda-setters with analytical exit and are neither beneficiaries nor victims, so they revert to canonical fallback. The victim-beneficiary asymmetry is the engine's primary input for seat divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) â the cost to state sovereignty is real and asymmetric, not a reciprocal arrangement. It also prevents mislabeling as pure extraction (snare) â the beneficiary is genuinely protected by the constraint's operation, not merely a cover story. If the protection function atrophied while the prohibition persisted (for example, if states ignored it while paying lip service), it would drift toward piton; the authored metrics show active enforcement and low theater, indicating the coordination function remains live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abolition_retributive_foreclosure,
    'Does the abolition reading''s core axiom logically foreclose the retributive and deterrence readings within a single legal framework, or do they merely coexist as incompatible ideologies across different polities?',
    'Engine computation from cs_axiom_contradiction once sibling readings are loaded; empirical test by examining whether any single legal system simultaneously maintains categorical dignity-based abolition and desert-based or deterrence-based execution.',
    'If foreclosed, the readings cannot coexist in one jurisdiction and their global distribution reflects jurisdictional partitioning, not interpretive pluralism within one system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolition_retributive_foreclosure, conceptual, 'Logical relationship between abolition and sibling readings within a single framework.').

omega_variable(
    true_cost_bearer_ambiguity,
    'Does the categorical prohibition primarily extract from state sovereignty, or does the cost diffuse to crime victims and societies denied the retributive option they prefer?',
    'Comparative analysis of abolitionist versus retentionist jurisdictions measuring victim-family satisfaction, public trust in justice, and state carceral costs.',
    'If costs fall on victims or society rather than on state power, the victim declaration should shift and directionality toward the state may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_bearer_ambiguity, empirical, 'Whether the extraction cost is borne by state power or diffuses to broader society.').

omega_variable(
    enforcement_source_ambiguity,
    'Is the constraint''s active enforcement sustained by genuine normative commitment or by diplomatic pressure and treaty conditionality?',
    'Track execution resumption rates in states after they exit international human rights frameworks or face reduced diplomatic pressure.',
    'If enforcement is externally coerced rather than internally legitimate, the coordination function may be weaker and the constraint may read as more extractive from the state''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_source_ambiguity, empirical, 'Source of enforcement legitimacy for the abolition norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__abolition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__abolition_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__abolition_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__abolition_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__abolition_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__abolition_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__abolition_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__abolition_reading, base_extractiveness, 10, 0.74).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__abolition_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__abolition_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__abolition_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__abolition_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__abolition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__abolition_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__abolition_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__abolition_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__abolition_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__abolition_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__abolition_reading, deterrence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_legitimacy kernel. The retributive and deterrence readings instantiate structurally distinct constraints from the same kernel, each with different beneficiary-victim structures and epsilon values, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
