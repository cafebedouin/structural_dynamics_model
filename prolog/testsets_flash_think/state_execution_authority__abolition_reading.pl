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
 *   constraint_id: state_execution_authority__abolition_reading
 *   human_readable: Categorical Impermissibility of State Execution (Abolitionist Reading)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'abolition_reading' of the
 *   'state_execution_authority' kernel. From this perspective, state
 *   execution is categorically impermissible, regardless of the severity of
 *   the crime or the procedural safeguards in place. It rejects the
 *   justifications of retribution and deterrence as morally illegitimate,
 *   viewing the act itself as a fundamental violation of human dignity. All
 *   executed persons, including the guilty, are considered victims, and the
 *   act is seen as degrading to society.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__abolition_reading, 0.95).
domain_priors:suppression_score(state_execution_authority__abolition_reading, 0.88).
domain_priors:theater_ratio(state_execution_authority__abolition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_execution_authority__abolition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__abolition_reading, snare).
narrative_ontology:human_readable(state_execution_authority__abolition_reading, "Categorical Impermissibility of State Execution (Abolitionist Reading)").
narrative_ontology:topic_domain(state_execution_authority__abolition_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__abolition_reading, 'b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d').
narrative_ontology:cs_kernel_codification('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', formalized).
narrative_ontology:cs_authority_grounding('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', extraction).
narrative_ontology:cs_interpretation_layer_present('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d').
narrative_ontology:cs_reading_relation('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', state_execution_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', state_execution_authority__deterrence_reading, forecloses).
narrative_ontology:cs_axiom('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', foundational, human_life_inherently_inviolable).
narrative_ontology:cs_axiom_status(human_life_inherently_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', human_life_inherently_inviolable, deontological).
narrative_ontology:cs_axiom('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', secondary, state_power_limited_by_dignity).
narrative_ontology:cs_axiom_status(state_power_limited_by_dignity, holdable).
narrative_ontology:cs_axiom_grounding('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', state_power_limited_by_dignity, deontological).
narrative_ontology:cs_reference_frame('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', universal_human_dignity).
narrative_ontology:cs_drift_state('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', contemporary_human_rights_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('b2d67b92-e1f5-4e2c-ad56-2e4d3cef021d', '').
narrative_ontology:cs_kernel_id(state_execution_authority__abolition_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, executed_persons).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, families_of_executed).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, society_at_large).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_execution_authority__abolition_reading, pro_death_penalty_public).
narrative_ontology:constraint_victim(state_execution_authority__abolition_reading, abolitionist_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state entity (e.g., judiciary, executive) that authorizes and carries out executions. From this reading, it asserts an illegitimate power over human life, rejecting any moral justification for its actions.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, state_execution_authority, agenda_setter,
    institutional, civilizational, constrained, national).

% Individuals subjected to state execution. This reading considers their lives to be inviolable, regardless of their crimes, making them victims of an impermissible act.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, executed_persons, payer,
    powerless, immediate, trapped, local).

% The relatives and loved ones of executed persons, who bear the profound and irreversible loss. They are victims of the state's action, experiencing trauma and injustice.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, families_of_executed, payer,
    powerless, biographical, trapped, local).

% Individuals and organizations actively campaigning against capital punishment. They bear the costs of resistance, advocacy, and moral struggle against state power, often facing political and social opposition.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, abolitionist_advocates, payer,
    organized, generational, constrained, global).

% Segments of the public who support capital punishment, often believing it provides justice, retribution, or deterrence. From this reading, their perceived 'benefit' is based on a morally flawed premise.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, pro_death_penalty_public, beneficiary,
    moderate, biographical, mobile, national).

% The broader societal collective, which, according to this reading, is morally degraded by the state's practice of execution. The constraint normalizes state violence and diminishes the value of human life, impacting the collective moral fabric.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, society_at_large, payer,
    moderate, civilizational, identity_locked, national).

% Organizations and legal frameworks (e.g., UN, Amnesty International) that monitor and advocate for human rights, including the right to life. They analyze state practices against universal moral and legal standards.
narrative_ontology:constraint_stakeholder(state_execution_authority__abolition_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this abolitionist reading, the constraint serves no legitimate coordination function; any claimed coordination (e.g., maintaining social order) is achieved through morally impermissible means.
% TRANSFER_FUNCTION: Transfers the life of executed persons to the state's assertion of ultimate power, and transfers moral degradation and a diminished respect for human dignity to society at large.
% ABSENT_VOICES: Future generations who would inherit a society less respectful of human life, and those whose moral sensibilities are dulled or corrupted by the state's use of violence. The voices of the executed are permanently silenced.
% DISAPPEARANCE_RATIONALE: If state execution authority vanished overnight, legal systems would need fundamental restructuring, the state would lose a significant tool of ultimate power, and the moral landscape of society would undergo a profound shift towards valuing all human life, even that of offenders.
% FOUNDING_PROBLEM: The problem of state violence and the inherent dignity and inviolability of human life, which this reading asserts is fundamentally violated by execution.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, philosophical traditions emphasizing human dignity, and global abolitionist movements consistently attest to the ongoing nature of this problem and the moral imperative to end state execution. This corroboration comes from outside the state authorities that benefit from maintaining the practice.
narrative_ontology:disappearance_verdict(state_execution_authority__abolition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_execution_authority__abolition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_execution_authority__abolition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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

:- end_tests(state_execution_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.95) because the constraint represents the ultimate, irreversible taking of a human life, which this reading deems an absolute moral wrong with no legitimate offsetting benefit. Suppression is high (0.88) because the state actively enforces its authority to execute, suppressing challenges to this power through legal and institutional means. Theater ratio is low (0.1) as the act of execution is a direct, functional exercise of state power, not primarily performative. Resistance is high (0.75) due to persistent and organized abolitionist movements globally.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state execution authority, the act is a legitimate exercise of justice or a necessary deterrent. From the abolitionist reading, it is an act of pure, illegitimate extraction and violence. The engine's classification will highlight this divergence by computing a Snare from the abolitionist seat, contrasting with a likely Rope or Tangled Rope from a retributive or deterrence seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The state execution authority is the agenda-setter, asserting and enforcing this power. Executed persons and their families are direct payers/victims, bearing the ultimate cost. Abolitionist advocates are also payers, bearing the costs of resistance. The pro-death penalty public may perceive a 'benefit' (justice, safety) but this reading rejects its legitimacy. Society at large is a victim due to moral degradation. International human rights bodies act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent moral imperative, or merely one reading of the ''state_execution_authority'' kernel?',
    'Analysis of the logical independence of its foundational axioms from the kernel''s historical and legal codification.',
    'If genuinely independent, its classification stands alone; if merely a reading, its classification is contextualized by the kernel''s overall contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading within a contested kernel.').

omega_variable(
    moral_degradation_quantification,
    'How can the ''moral degradation to society'' (as a victim cost) be empirically measured or robustly demonstrated?',
    'Longitudinal studies comparing societal values, crime rates, and public trust in justice systems in abolitionist vs. retentionist states, controlling for other variables.',
    'Empirical evidence would strengthen the claim of ''society_at_large'' as a victim; lack of evidence would weaken it, potentially reducing the overall extractiveness from this seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_degradation_quantification, empirical, 'Quantifying the societal cost of state execution.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of abolitionist alternatives structural (legal barriers, state power) or internalized (public acceptance of state authority)?',
    'Post-abolition trajectory: if public support for execution persists after legal abolition, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the public carries the suppression with them after legal changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for state execution authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__abolition_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_execution_authority__abolition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_execution_authority__abolition_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(stat_tr_t20, state_execution_authority__abolition_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(stat_tr_t30, state_execution_authority__abolition_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(stat_tr_t40, state_execution_authority__abolition_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(stat_tr_t50, state_execution_authority__abolition_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_execution_authority__abolition_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(stat_be_t10, state_execution_authority__abolition_reading, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(stat_be_t20, state_execution_authority__abolition_reading, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(stat_be_t30, state_execution_authority__abolition_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(stat_be_t40, state_execution_authority__abolition_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(stat_be_t50, state_execution_authority__abolition_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_execution_authority__abolition_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(stat_su_t10, state_execution_authority__abolition_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(stat_su_t20, state_execution_authority__abolition_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(stat_su_t30, state_execution_authority__abolition_reading, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(stat_su_t40, state_execution_authority__abolition_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(stat_su_t50, state_execution_authority__abolition_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
