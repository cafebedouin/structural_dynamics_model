% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: State Authority to Execute for Retributive Desert
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'retributive desert' reading of
 *   the state's authority to kill, specifically focusing on the principle
 *   that murderers forfeit their right to life and that proportional
 *   punishment (lex talionis) requires death for death. This reading grounds
 *   the state's authority in a moral imperative for justice, distinct from
 *   utilitarian considerations like deterrence or categorical prohibitions
 *   against state killing. The metrics reflect the ultimate extraction of
 *   life, the high degree of state enforcement, and the low theatricality of
 *   the act itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.95).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.9).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Authority to Execute for Retributive Desert").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d').
narrative_ontology:cs_kernel_codification('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', formalized).
narrative_ontology:cs_authority_grounding('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', lineage).
narrative_ontology:cs_interpretation_layer_present('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d').
narrative_ontology:cs_reading_relation('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', foundational, right_to_life_is_forfeitable_by_murder).
narrative_ontology:cs_axiom_status(right_to_life_is_forfeitable_by_murder, holdable).
narrative_ontology:cs_axiom_grounding('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', right_to_life_is_forfeitable_by_murder, deontological).
narrative_ontology:cs_axiom('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', foundational, lex_talionis_is_proportional_justice).
narrative_ontology:cs_axiom_status(lex_talionis_is_proportional_justice, holdable).
narrative_ontology:cs_axiom_grounding('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', lex_talionis_is_proportional_justice, deontological).
narrative_ontology:cs_reference_frame('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', lex_talionis_proportionality).
narrative_ontology:cs_drift_state('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', contemporary_human_rights_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e7d2e7e8-3e26-47e6-808a-a2dd3aceaa8d', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, society).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victims_families).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_murderers).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportional_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies laws, including capital statutes, ordering executions based on retributive principles. Its legitimacy is tied to upholding justice and societal order through proportional punishment.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of the constraint, forfeiting their right to life as the ultimate punishment. Their options are exhausted by the legal process, leading to execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_murderers, payer,
    powerless, immediate, trapped, local).

% Often seek and receive a form of justice or closure through the execution of the murderer, seeing it as a proportional response to their loss. Their influence can shape public and political discourse.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victims_families, beneficiary,
    moderate, biographical, constrained, local).

% Benefits from the perceived upholding of moral order and the vindication of justice, reinforcing collective values regarding the sanctity of life and the consequences of its violation. Public opinion can shift over time.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, society, beneficiary,
    organized, generational, mobile, national).

% Actively campaign against capital punishment, arguing for the inherent impermissibility of state killing. Their arguments are structurally excluded from the retributive desert reading's core logic, which asserts the right to forfeit life.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the legal, philosophical, and ethical underpinnings of capital punishment, including its consistency with constitutional principles and evolving standards of decency. They do not directly benefit or pay but influence legal discourse.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ultimate justice, ensuring that the most heinous crimes are met with a punishment deemed proportionally equivalent, thereby upholding the moral order and societal values.
% TRANSFER_FUNCTION: Transfers the life of the condemned from themselves to the state, in exchange for the vindication of the victim's right to life and the societal demand for proportional justice.
% ABSENT_VOICES: Abolitionist advocates, who argue for the inherent impermissibility of state killing, are excluded from the core logic of this retributive reading; their arguments are dismissed as irrelevant to the principle of just deserts.
% DISAPPEARANCE_RATIONALE: If the state's authority to execute for retributive desert vanished, the entire criminal justice system's sentencing philosophy would need to be re-evaluated, potentially leading to a crisis of legitimacy for the state's punitive power and a fundamental shift in how society conceives of ultimate justice for murder.
% FOUNDING_PROBLEM: How to justly respond to the ultimate crime of murder, ensuring that the punishment fits the crime and that society's moral order is upheld.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, some victims' families, and certain legal scholars corroborate that the problem of ultimate justice for murder remains live. Opponents dispute this, arguing the problem is either solved by life imprisonment or that the solution itself is immoral.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near maximal (0.95) as the constraint involves the ultimate forfeiture of life. Suppression is very high (0.90) due to the state's monopoly on legitimate force and the legal system's power to enforce capital sentences. Theater ratio is low (0.10) because the act of execution, while ritualized, is a direct and irreversible application of the constraint's core function, not a performance masking atrophy. Accessibility collapse is high (0.75) as legal avenues for the condemned are severely limited, and resistance is moderate (0.55) reflecting ongoing, but often unsuccessful, abolitionist efforts and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and beneficiaries, the constraint is a necessary mechanism for justice and moral order. From the perspective of the condemned, it is absolute extraction. Abolitionist advocates view it as an immoral act, regardless of the crime. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The state judicial system acts as the agenda-setter, interpreting and enforcing the retributive principle. Society and victims' families are beneficiaries, receiving the perceived vindication of justice. Condemned murderers are the clear targets, bearing the ultimate cost. Abolitionist advocates are excluded, as their core premise (inalienable right to life) is directly contradicted by this reading's foundational axiom.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeitability_of_life,
    'Is the ''right to life'' truly forfeitable by committing murder, or is it an inalienable right?',
    'Philosophical and legal consensus shift over generations, or a definitive international legal ruling on the inalienability of life that gains universal adherence.',
    'If the right to life is deemed inalienable, this reading''s foundational axiom collapses, potentially reclassifying the constraint as a Snare or leading to its abolition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forfeitability_of_life, conceptual, 'Ambiguity regarding the moral and legal forfeitability of the right to life.').

omega_variable(
    proportionality_of_death,
    'Is ''death for death'' the only truly proportional punishment for murder, or are other severe punishments (e.g., life imprisonment without parole) also proportional?',
    'Evolving societal standards of ''cruel and unusual punishment'' as interpreted by courts, or a broad philosophical re-evaluation of proportionality in punitive justice.',
    'If death is not deemed uniquely proportional, the ''lex talionis'' axiom weakens, potentially shifting the constraint towards a less extractive form of justice or undermining its specific justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_death, preference, 'Ambiguity regarding the unique proportionality of capital punishment for murder.').

omega_variable(
    moral_status_of_state_killing,
    'Is state killing morally permissible under any circumstances, even for retributive justice, or does it inherently violate fundamental moral principles?',
    'Global shift in human rights norms and legal frameworks, or a widespread philosophical consensus on the inherent immorality of state-sanctioned execution.',
    'If state killing is deemed inherently immoral, the entire basis of this reading is undermined, leading to its rejection and the reclassification of the constraint as a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(moral_status_of_state_killing, conceptual, 'Fundamental moral permissibility of state-sanctioned killing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 1976, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__retributive_desert, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(stat_tr_t1985, state_killing_authority__retributive_desert, theater_ratio, 1985, 0.09).
narrative_ontology:measurement(stat_tr_t1995, state_killing_authority__retributive_desert, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__retributive_desert, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(stat_tr_t2015, state_killing_authority__retributive_desert, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(stat_tr_t2023, state_killing_authority__retributive_desert, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__retributive_desert, base_extractiveness, 1976, 0.9).
narrative_ontology:measurement(stat_be_t1985, state_killing_authority__retributive_desert, base_extractiveness, 1985, 0.92).
narrative_ontology:measurement(stat_be_t1995, state_killing_authority__retributive_desert, base_extractiveness, 1995, 0.95).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__retributive_desert, base_extractiveness, 2005, 0.94).
narrative_ontology:measurement(stat_be_t2015, state_killing_authority__retributive_desert, base_extractiveness, 2015, 0.93).
narrative_ontology:measurement(stat_be_t2023, state_killing_authority__retributive_desert, base_extractiveness, 2023, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__retributive_desert, suppression_requirement, 1976, 0.85).
narrative_ontology:measurement(stat_su_t1985, state_killing_authority__retributive_desert, suppression_requirement, 1985, 0.88).
narrative_ontology:measurement(stat_su_t1995, state_killing_authority__retributive_desert, suppression_requirement, 1995, 0.9).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__retributive_desert, suppression_requirement, 2005, 0.89).
narrative_ontology:measurement(stat_su_t2015, state_killing_authority__retributive_desert, suppression_requirement, 2015, 0.87).
narrative_ontology:measurement(stat_su_t2023, state_killing_authority__retributive_desert, suppression_requirement, 2023, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'state_killing_authority' kernel. Each reading offers a different justification and structural profile for capital punishment, and they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
