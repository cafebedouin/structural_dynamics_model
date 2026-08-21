% ============================================================================
% CONSTRAINT STORY: state_killing_authority__categorical_abolition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__categorical_abolition, []).

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
 *   constraint_id: state_killing_authority__categorical_abolition
 *   human_readable: Categorical Abolition of State Killing (Reading)
 *   domain: criminal_justice/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'categorical abolition' reading of
 *   the 'state_killing_authority' kernel. From this perspective, the practice
 *   of state killing is inherently impermissible, regardless of crime or
 *   consequence, because life is inalienable. The constraint itself is the
 *   normative prohibition against state killing. The metrics reflect the
 *   operation of the *practice* of state killing, which this reading
 *   identifies as a Snare, due to its high extractiveness (taking of life)
 *   and suppression (state power over individuals).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, 0.85).
domain_priors:suppression_score(state_killing_authority__categorical_abolition, 0.9).
domain_priors:theater_ratio(state_killing_authority__categorical_abolition, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, extractiveness, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__categorical_abolition, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__categorical_abolition, snare).
narrative_ontology:human_readable(state_killing_authority__categorical_abolition, "Categorical Abolition of State Killing (Reading)").
narrative_ontology:topic_domain(state_killing_authority__categorical_abolition, "criminal_justice/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__categorical_abolition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__categorical_abolition, '4795f0ed-3220-4b38-a600-a6d5f9d56fc8').
narrative_ontology:cs_kernel_codification('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', formalized).
narrative_ontology:cs_authority_grounding('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', lineage).
narrative_ontology:cs_interpretation_layer_present('4795f0ed-3220-4b38-a600-a6d5f9d56fc8').
narrative_ontology:cs_reading_relation('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', state_killing_authority__retributive_desert, forecloses).
narrative_ontology:cs_reading_relation('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', state_killing_authority__deterrence_instrument, forecloses).
narrative_ontology:cs_axiom('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', foundational, life_is_inalienable).
narrative_ontology:cs_axiom_status(life_is_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', life_is_inalienable, deontological).
narrative_ontology:cs_axiom('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', secondary, state_power_is_limited_by_rights).
narrative_ontology:cs_axiom_status(state_power_is_limited_by_rights, holdable).
narrative_ontology:cs_axiom_grounding('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', state_power_is_limited_by_rights, deontological).
narrative_ontology:cs_reference_frame('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', universal_human_rights_framework).
narrative_ontology:cs_drift_state('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', contemporary_legal_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4795f0ed-3220-4b38-a600-a6d5f9d56fc8', '').
narrative_ontology:cs_kernel_id(state_killing_authority__categorical_abolition, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, political_authorities).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, judicial_system).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, state_executioners).
narrative_ontology:constraint_beneficiary(state_killing_authority__categorical_abolition, victims_families_pro_execution).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, abolitionist_advocates).
narrative_ontology:constraint_victim(state_killing_authority__categorical_abolition, victims_families_anti_execution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The direct targets of state killing, whose lives are taken. From this reading's perspective, their inherent right to life is violated, and they are completely trapped by the state's power.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, condemned_persons, payer,
    powerless, immediate, trapped, national).

% Actively campaign against state killing, viewing it as a fundamental violation of human rights. They bear the costs of moral compromise and political struggle against entrenched state power.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, abolitionist_advocates, payer,
    moderate, generational, constrained, global).

% Seek retribution and closure through state killing, believing it provides justice for their loss. They benefit from the state's exercise of capital punishment, which this reading views as an unjust act.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_pro_execution, beneficiary,
    moderate, biographical, constrained, local).

% Oppose state killing on moral or religious grounds, even for those who harmed their loved ones. They bear the cost of being marginalized by a system that often prioritizes retributive justice.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, victims_families_anti_execution, payer,
    moderate, biographical, constrained, local).

% Legislate and uphold laws that permit state killing. They benefit from the perceived ability to deliver 'tough on crime' policies and maintain state power over life and death, which this reading views as illegitimate.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, political_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Interprets and applies laws that lead to state killing. They benefit from maintaining the existing legal framework and their role within it, even if it means participating in what this reading deems an impermissible act.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Carry out the act of state killing. They are direct agents in the extractive process, benefiting from their institutional role and the perceived necessity of their function.
narrative_ontology:constraint_stakeholder(state_killing_authority__categorical_abolition, state_executioners, agenda_setter,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__categorical_abolition, political_authorities).
narrative_ontology:fixing_cost_class(state_killing_authority__categorical_abolition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From this reading's perspective, the constraint (impermissibility of state killing) aims to coordinate state action around the fundamental sanctity and inalienability of human life, preventing arbitrary or unjust deprivation of life.
% TRANSFER_FUNCTION: The practice of state killing, which this reading opposes, transfers the life of condemned persons to the state, asserting ultimate state power over individual existence.
% ABSENT_VOICES: Those who have been executed by the state are permanently silenced. Their perspectives on the injustice of their fate, as seen by this reading, are entirely absent from the ongoing discourse.
% DISAPPEARANCE_RATIONALE: If the normative constraint of 'state killing is inherently impermissible' vanished overnight, the state would face no fundamental moral or legal barrier to taking life, potentially leading to widespread arbitrary executions and a complete reordering of human rights protections.
% FOUNDING_PROBLEM: The historical problem of states wielding arbitrary and absolute power over the lives of their citizens, leading to tyranny and gross human rights abuses.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, philosophical traditions emphasizing inherent dignity, and historical records of state-sponsored violence (e.g., political purges, genocides) corroborate the ongoing relevance of limiting state power over life. Abolitionist organizations and legal scholars also attest to the problem's live status.
narrative_ontology:disappearance_verdict(state_killing_authority__categorical_abolition, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__categorical_abolition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__categorical_abolition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_authority__categorical_abolition, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__categorical_abolition, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__categorical_abolition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__categorical_abolition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__categorical_abolition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is high because the ultimate cost, life itself, is extracted from the condemned. Suppression (0.90) is very high, reflecting the state's near-absolute power over the condemned once a death sentence is passed, with few effective exit options. Theater ratio (0.10) is low, as the act of execution is a stark, irreversible reality, not primarily performative. Accessibility collapse (0.95) is near total for the condemned, as alternatives to execution are almost entirely foreclosed. Resistance (0.70) is substantial, reflecting ongoing global abolitionist movements and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   The 'categorical abolition' reading views the state's practice of killing as a Snare, fundamentally violating inalienable rights. In contrast, proponents of state killing (e.g., 'retributive desert' or 'deterrence instrument' readings) would frame the same practice as a legitimate exercise of justice or a necessary tool for public safety, potentially classifying it as a Rope or even a Mountain (if framed as an inherent right of the sovereign). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are the ultimate targets (high d), bearing the full cost. Abolitionist advocates and victims' families who oppose execution also bear costs (higher d) by being morally compromised or marginalized by the system. Political authorities, the judicial system, and state executioners are beneficiaries (low d) as they wield and benefit from the power to take life. Victims' families who support execution are also beneficiaries (low d) as they receive the desired retributive outcome.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''categorical_abolition'' reading of the ''state_killing_authority'' kernel?',
    'Comparison with canonical texts of abolitionist philosophy and human rights law.',
    'If misidentified, the analysis of the kernel''s internal contestation would be flawed, potentially misrepresenting the structural relationships between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ensures accurate representation of the specific kernel reading.').

omega_variable(
    sibling_delta_retributive_desert,
    'How does the ''retributive_desert'' reading structurally differ from ''categorical_abolition''?',
    'Analysis of the core axioms: ''retributive_desert'' asserts forfeiture of life, which ''categorical_abolition'' denies as impossible due to inalienability.',
    'The ''categorical_abolition'' reading logically forecloses ''retributive_desert'' within the same normative framework, as their foundational axioms are contradictory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_retributive_desert, conceptual, 'Structural difference from the retributive desert reading.').

omega_variable(
    sibling_delta_deterrence_instrument,
    'How does the ''deterrence_instrument'' reading structurally differ from ''categorical_abolition''?',
    'Analysis of the core axioms: ''deterrence_instrument'' bases justification on empirical outcomes, which ''categorical_abolition'' rejects as irrelevant to inherent impermissibility.',
    'The ''categorical_abolition'' reading logically forecloses ''deterrence_instrument'' within the same normative framework, as inherent impermissibility cannot be overridden by instrumental benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_deterrence_instrument, conceptual, 'Structural difference from the deterrence instrument reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__categorical_abolition, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__categorical_abolition, theater_ratio, 0, 0.12).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__categorical_abolition, theater_ratio, 20, 0.11).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__categorical_abolition, theater_ratio, 40, 0.1).
narrative_ontology:measurement(stat_tr_t60, state_killing_authority__categorical_abolition, theater_ratio, 60, 0.1).
narrative_ontology:measurement(stat_tr_t80, state_killing_authority__categorical_abolition, theater_ratio, 80, 0.1).
narrative_ontology:measurement(stat_tr_t100, state_killing_authority__categorical_abolition, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__categorical_abolition, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__categorical_abolition, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__categorical_abolition, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(stat_be_t60, state_killing_authority__categorical_abolition, base_extractiveness, 60, 0.84).
narrative_ontology:measurement(stat_be_t80, state_killing_authority__categorical_abolition, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(stat_be_t100, state_killing_authority__categorical_abolition, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__categorical_abolition, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__categorical_abolition, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__categorical_abolition, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(stat_su_t60, state_killing_authority__categorical_abolition, suppression_requirement, 60, 0.89).
narrative_ontology:measurement(stat_su_t80, state_killing_authority__categorical_abolition, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(stat_su_t100, state_killing_authority__categorical_abolition, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__categorical_abolition, identity_coordination).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, criminal_sentencing_guidelines).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, human_rights_treaties).
narrative_ontology:affects_constraint(state_killing_authority__categorical_abolition, state_sovereignty_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel, which also includes 'retributive_desert' and 'deterrence_instrument' readings. Each reading instantiates a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
