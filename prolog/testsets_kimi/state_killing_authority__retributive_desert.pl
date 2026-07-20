% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive Desert Justification for State Execution
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the retributive-desert reading of the state
 *   killing authority kernel. It holds that murderers forfeit their right to
 *   life and that proportional punishment under lex talionis requires the
 *   state to execute the condemned. The murdered victim is positioned as the
 *   beneficiary of posthumous vindication; the condemned person is
 *   structurally excluded from the rights-holder community by the forfeiture
 *   logic. State authority is grounded in a proportionality norm rather than
 *   empirical deterrence. The reading coexists with instrumental deterrence
 *   justifications but forecloses categorical abolition, which rejects
 *   forfeiture root and branch.
 *
 * KEY AGENTS:
 *   - state_execution_authority: agenda-setter (institutional/analytical) â administers execution and retributive justification
 *   - condemned_persons: primary target (powerless/trapped) â bears the life-cost of the constraint
 *   - murdered_victims: posthumous beneficiary (powerless/trapped/non-agent) â named recipient of vindication
 *   - victim_survivors_community: living beneficiary (moderate/constrained) â receives formal standing and symbolic grievance-resolution
 *   - abolitionist_movement: excluded voice (organized/mobile) â contests the forfeiture axiom from outside the retributive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.92).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.78).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.92).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive Desert Justification for State Execution").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, '68057409-88c3-4d8d-b560-fef0a70ff5de').
narrative_ontology:cs_kernel_codification('68057409-88c3-4d8d-b560-fef0a70ff5de', formalized).
narrative_ontology:cs_authority_grounding('68057409-88c3-4d8d-b560-fef0a70ff5de', lineage).
narrative_ontology:cs_interpretation_layer_present('68057409-88c3-4d8d-b560-fef0a70ff5de').
narrative_ontology:cs_reading_relation('68057409-88c3-4d8d-b560-fef0a70ff5de', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('68057409-88c3-4d8d-b560-fef0a70ff5de', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('68057409-88c3-4d8d-b560-fef0a70ff5de', foundational, murderers_forfeit_right_to_life).
narrative_ontology:cs_axiom_status(murderers_forfeit_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('68057409-88c3-4d8d-b560-fef0a70ff5de', murderers_forfeit_right_to_life, deontological).
narrative_ontology:cs_axiom('68057409-88c3-4d8d-b560-fef0a70ff5de', foundational, proportional_punishment_requires_death_for_death).
narrative_ontology:cs_axiom_status(proportional_punishment_requires_death_for_death, holdable).
narrative_ontology:cs_axiom_grounding('68057409-88c3-4d8d-b560-fef0a70ff5de', proportional_punishment_requires_death_for_death, deontological).
narrative_ontology:cs_reference_frame('68057409-88c3-4d8d-b560-fef0a70ff5de', lex_talionis_proportionality).
narrative_ontology:cs_drift_state('68057409-88c3-4d8d-b560-fef0a70ff5de', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('68057409-88c3-4d8d-b560-fef0a70ff5de', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murdered_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, victim_survivors_community).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the machinery of capital punishment: prosecutors seek death sentences, courts affirm proportionality, and correctional departments carry out executions. Derives its authority from criminal codes and the proportionality norm rather than empirical outcomes. Could abolish the practice by legislative or judicial repeal but maintains it as a retributive duty.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_execution_authority, agenda_setter,
    institutional, generational, analytical, national).

% Convicted of murder and sentenced to death. Under the retributive framework, their crime is treated as forfeiting their right to life. They are confined on death row while appellate and clemency processes run. Their physical exit is blocked by custody; their legal exit depends on reversing the sentence or commutation.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, local).

% The individuals whose deaths occasion the prosecution. They are not alive to participate, but the retributive framework positions their violated rights as the basis for the state's lethal response. Their memory is invoked in victim-impact statements and prosecutorial rhetoric as the party vindicated by the execution.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murdered_victims, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murdered_victims).

% Family members and communities of the murdered who are afforded standing in capital proceedings. They receive formal acknowledgment of their loss and are told that the execution balances the harm done. Some support the execution; others oppose it but are channeled into the retributive process.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victim_survivors_community, beneficiary,
    moderate, biographical, constrained, national).

% Human rights organizations, defense attorneys, and moral critics who argue that no crime voids the right to life and that state killing degrades the community. They participate in appellate litigation and clemency advocacy but their foundational objectionâthat forfeiture is impossibleâis treated as illegitimate within the retributive framework itself.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_movement, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political community's response to murder by specifying a proportionate punishment that honors the victim's worth, expresses the gravity of the wrong, and prevents private vengeance.
% TRANSFER_FUNCTION: Transfers the condemned person's life to the state's custody for destruction, and transfers symbolic vindication to the victim's memory and survivors.
% ABSENT_VOICES: The abolitionist movement and human rights frameworks that reject forfeiture are formally present in litigation but structurally excluded from the retributive justification, which treats their core claim as illegitimate. The condemned person's objectionâthat they retain rights despite their crimeâis ruled out by the forfeiture premise.
% DISAPPEARANCE_RATIONALE: If the retributive authorization for state killing vanished, capital prosecutions would convert to life sentences, victim-survivor expectations would shift toward non-lethal accountability, and the state's claimed monopoly on proportionate lethal justice would collapse into incapacitation or rehabilitation frameworks.
% FOUNDING_PROBLEM: How a political community should respond to murder in a way that honors the victim's worth, expresses the gravity of the wrong, and prevents private vengeance.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist organizations and international human rights monitors outside the beneficiary set attest that the founding problem is solvable without state killing; empirical criminology corroborates that alternative sanctions achieve social outcomes comparable to execution. Retentionist political actors assert the problem is live, but their testimony originates within the coalition that benefits from the retributive framing.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.92, 'kimi-k2.6', 'none', direct).

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
 *   Extraction is near-maximum (0.92) because the constraint authorizes the state to take a human lifeâthe most absolute extraction possible. Suppression is high (0.78) because the constraint actively suppresses the condemned's survival and excludes abolitionist alternatives through legal procedure and normative closure. Theater is moderate (0.35): the execution is functionally real, but the elaborate appellate and clemency rituals, victim-impact protocols, and solemn staging create a growing performative layer around the killing. Accessibility collapse is very high (0.85) because once the retributive norm is institutionalized, the legal process channels the condemned toward execution with vanishing exit probability. Resistance is substantial (0.68) due to persistent abolitionist challenge, but remains below dominance because retentionist political majorities continue to legitimate the practice.
 *
 * PERSPECTIVAL GAP:
 *   From the state_execution_authority seat, the constraint is the discharge of a retributive duty owed to the victim and the moral order; from the condemned_persons seat, it is the terminal extraction of life by forfeiture logic they reject; from the victim_survivors_community seat, it is a grievance-response mechanism that delivers vindication. The engine computes these divergences from the structural data: same constraint, different directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_execution_authority is the agenda-setter and enforcer with analytical exit (it could abolish the practice), placing it near the beneficiary end of directionalityâit does not pay the cost of the constraint. The condemned_persons are declared victims with trapped exit, placing them at full-target directionality. The murdered_victims and victim_survivors_community are declared beneficiaries with constrained or trapped exit, giving them low directionality despite their powerlessness. The abolitionist_movement is excluded from the retributive framework; its mobile exit and opposition role do not place it in the victim set because it does not bear the life-cost, but its exclusion is structurally significant.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists piton classification because it retains a concentrated beneficiary structure (victim-survivor communities receive tangible standing and symbolic goods) and a live agenda-setter that actively maintains the practice for retributive reasons rather than inertia. Theater is present but below the 0.5 threshold. Mandatrophy would require the founding problem (proportionate response to murder) to be dead while the arrangement persists; the status is contested, not dead, because retributive constituencies continue to assert the problem is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_axiom_naturalness,
    'Is the murderer''s forfeiture of the right to life a natural moral fact discoverable by reason, or a constructed legal fiction authored by the state?',
    'Cross-cultural anthropological and historical legal analysis: if forfeiture appears only in specific legal traditions rather than universally, it is constructed; if it recurs across disconnected moral systems as a spontaneous norm, it may be natural.',
    'If forfeiture is constructed, the constraint''s classification shifts toward snare or tangled_rope with a coordination cover story; if natural, it approaches a mountain-like norm with high accessibility_collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_axiom_naturalness, conceptual, 'Whether the forfeiture of life is natural law or constructed norm.').

omega_variable(
    proportionality_without_lethality,
    'Can the proportionality norm underlying lex talionis be satisfied by non-lethal sanctions, or does retributive logic necessarily require death?',
    'Comparative jurisprudence in abolitionist jurisdictions: if life imprisonment without parole is accepted by victim-survivor communities as proportionate, then death is not structurally required.',
    'If non-lethal satisfaction is possible, the constraint''s extractiveness is a policy choice rather than a logical entailment of the retributive framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_without_lethality, empirical, 'Whether retributive proportionality necessitates death.').

omega_variable(
    victim_vindication_verifiability,
    'Does posthumous vindication to murdered victims represent a real benefit to a recipient, or a symbolic ascription without verifiable reception?',
    'Phenomenological and sociological study of victim-survivor outcomes: compare psychological recovery and sense of justice in retentionist versus abolitionist jurisdictions.',
    'If vindication is purely symbolic, the beneficiary structure is hollow and the constraint''s coordination function rests on an empty transfer; if real, the beneficiary claim is structurally grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_vindication_verifiability, empirical, 'Whether posthumous vindication is a real or symbolic benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.25).
narrative_ontology:measurement(stat_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.3).
narrative_ontology:measurement(stat_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.35).
narrative_ontology:measurement(stat_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.4).
narrative_ontology:measurement(stat_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(stat_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.87).
narrative_ontology:measurement(stat_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.89).
narrative_ontology:measurement(stat_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.9).
narrative_ontology:measurement(stat_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.91).
narrative_ontology:measurement(stat_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(stat_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(stat_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stat_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(stat_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(stat_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the state_killing_authority kernel. The kernel decomposes into three structurally distinct constraints because the natural-language label 'state killing authority' conflates retributive (proportionality/desert), instrumental (deterrence/outcome), and abolitionist (inherent impermissibility) claims. Each reading has a different beneficiary/victim structure, grounding norm, and epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
