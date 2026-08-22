% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_legitimacy__deterrence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: Capital Punishment as Rational Deterrence Signal
 *   domain: criminal justice/political philosophy/legal theory
 *
 * SUMMARY:
 *   This story instantiates the deterrence_reading of the kernel
 *   state_killing_legitimacy: the claim that execution is justified as a
 *   rational, costly signal that prevents future murders. Per the
 *   epsilon-referent rule, extractiveness is authored for the standing
 *   arrangement under contest, retentionist capital-punishment regimes as
 *   actually practiced, assessed by this reading's own instrumental lights,
 *   never for the abolitionist alternative it argues against. The sibling
 *   readings (retributive_reading, abolition_reading) are separate constraint
 *   files linked through the network section; their structural deltas are
 *   routed to omega variables rather than folded into this story.
 *   Claim/metric independence is preserved deliberately: the reading's own
 *   self-description is 'justified coordination,' while the authored metrics
 *   describe a moderately extractive, actively enforced arrangement whose
 *   namesake function rests on contested evidence. The engine measures that
 *   divergence; this file does not reconcile it. KEY AGENTS (by structural
 *   relationship): - condemned_defendants: Primary target (powerless/trapped)
 *   — bears the terminal cost; exit runs through clemency and appeal, neither
 *   controlled by them - wrongfully_executed_innocents: Sharpest target class
 *   (powerless/trapped) — bear the full price of the signal with no desert
 *   grounding; identified posthumously - potential_murder_victims: Declared
 *   beneficiary (powerless/constrained) — the diffuse class whose lives the
 *   signal is supposed to save; benefit contingent on unverified empirics -
 *   prosecutors_offices: Agenda-setter and operational beneficiary
 *   (powerful/mobile) — convert death-row risk into plea leverage and
 *   conviction capital - law_and_order_officeholders: Agenda-setter
 *   (institutional/arbitrage) — enact, expand, and campaign on the sanction
 *   at negligible personal cost - surviving_co_victims_families:
 *   Dual-positioned beneficiary/payer (organized/constrained) — receive
 *   expressive satisfaction while enduring decades of reopened proceedings -
 *   deterrence_researchers: Analytical observer (analytical/analytical) —
 *   hold the evidentiary hinge the whole reading stands on
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.55).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.62).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "Capital Punishment as Rational Deterrence Signal").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal justice/political philosophy/legal theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'd07cf39b-0c3d-4096-a300-4e47b79f2ea1').
narrative_ontology:cs_kernel_codification('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', formalized).
narrative_ontology:cs_authority_grounding('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', lineage).
narrative_ontology:cs_interpretation_layer_present('d07cf39b-0c3d-4096-a300-4e47b79f2ea1').
narrative_ontology:cs_reading_relation('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', foundational, deterrence_suffices_for_execution_legitimacy).
narrative_ontology:cs_axiom_status(deterrence_suffices_for_execution_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', deterrence_suffices_for_execution_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', secondary, expected_lives_saved_outweigh_wrongful_execution_risk).
narrative_ontology:cs_axiom_status(expected_lives_saved_outweigh_wrongful_execution_risk, holdable).
narrative_ontology:cs_axiom_grounding('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', expected_lives_saved_outweigh_wrongful_execution_risk, instrumental).
narrative_ontology:cs_reference_frame('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', rational_deterrence_equilibrium).
narrative_ontology:cs_drift_state('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', post_nrc_2012_evidence_review, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d07cf39b-0c3d-4096-a300-4e47b79f2ea1', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_murder_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, prosecutors_offices).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, law_and_order_officeholders).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, surviving_co_victims_families).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_defendants).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, wrongfully_executed_innocents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, surviving_co_victims_families).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, deterrence_hypothesis).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, rational_actor_model_of_homicide).
narrative_ontology:constraint_vindicates(state_killing_legitimacy__deterrence_reading, costly_signaling_theory_of_punishment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convicted of capital murder in a jurisdiction that retains the death penalty. Spends years to decades on death row under a sentence that, if carried out, ends their agency permanently. Their ways out run through appellate reversal or executive clemency, neither of which they control; the process consumes their entire biographical horizon.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_defendants, payer,
    powerless, biographical, trapped, national).

% Convicted and executed for murders later shown, through DNA testing, witness recantations, or investigative journalism, to be ones they did not commit. They paid the full price of the sanction without the guilt that even this reading's own warrant presupposes, and no remedy reaches them; identification happens posthumously.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, wrongfully_executed_innocents, payer,
    powerless, biographical, trapped, national).

% The diffuse class of people who might be murdered and whose lives the signal is supposed to save. No organization represents them as such; they cannot verify the protection they are said to receive, and the benefit arrives only if the contested empirical claim holds.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_murder_victims, beneficiary,
    powerless, biographical, constrained, national).

% Decide which homicide cases to charge capitally and how hard to press for death. The availability of the death sentence lets them offer life imprisonment as a concession defendants accept to remove execution risk, producing convictions without trial. Offices build reputations and careers on capital cases; they can decline to seek death in any individual case but operate inside statutes they did not write alone.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, prosecutors_offices, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, prosecutors_offices, beneficiary).

% Enact and expand capital statutes, fund prosecution and death-row infrastructure, appoint judges, and campaign on the sanction. They bear essentially none of its costs personally and can leave the position by changing platforms, losing elections, or term-limiting out.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, law_and_order_officeholders, agenda_setter,
    institutional, generational, arbitrage, national).

% Relatives of murder victims. Some report that a death sentence and eventual execution bring closure or deserved finality; many others describe decades of appeals, retrials, and anniversary coverage that repeatedly reopen the loss. Organized co-victim groups exist on both sides of the dispute, and both sides invoke families symbolically in hearings.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, surviving_co_victims_families, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(state_killing_legitimacy__deterrence_reading, surviving_co_victims_families, payer).

% Criminologists and economists who test whether execution adds a deterrent margin beyond lengthy imprisonment. Their syntheses, most prominently the 2012 National Research Council assessment, found existing studies unable to answer the question in either direction. They hold no stake in outcomes beyond disciplinary standing, which cuts toward candor.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, deterrence_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_legitimacy__deterrence_reading, prosecutors_offices).
narrative_ontology:fixing_cost_class(state_killing_legitimacy__deterrence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the collective-action problem of protecting members from homicidal violence: under this reading, a credibly administered death penalty raises the expected price of murder above what imprisonment signals, coordinating general compliance with the homicide prohibition among rationally calculating potential offenders.
% TRANSFER_FUNCTION: Moves the condemned defendant's remaining life and liberty-years into the state's sanctioning account; converts death-row risk into plea concessions moved from defendants to prosecutors; delivers an assurance good (perceived safety) to the public and an electoral good to officeholders.
% ABSENT_VOICES: The condemned themselves, whose participation ends at execution and who testify posthumously only through investigators; abolition-minded co-victims who reject killings done in their name and sit largely outside retentionist agenda-setting; and the potential victims the signal protects, a diffuse class with no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: Prosecutorial charging patterns and plea leverage would reorganize within a budget cycle; the capital litigation machinery (specialized defenders, expedited habeas tracks, death-row facilities) would decommission; officeholders would lose a signature issue. Murder rates, on the best available evidence, would not measurably change, so the rearrangement is institutional rather than criminological.
% FOUNDING_PROBLEM: Pre-modern states lacked reliable long-term confinement: prisons were escape-prone, expensive, or nonexistent, so definitive removal of the worst killers and exemplary demonstration of sovereign resolve required death.
% FOUNDING_PROBLEM_CORROBORATION: Historians of punishment (the carceral-transition literature from Spierenburg and Foucault onward) corroborate the confinement-capacity origin, and modern corrections administrators attest that life-without-parole achieves permanent neutralization; retentionist prosecutors and officeholders, by contrast, attest that the exemplary-demonstration need remains live. Corroboration for obsolescence exists outside the benefiting parties; corroboration for continued necessity comes mostly from inside them.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_legitimacy__deterrence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_legitimacy__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (interval end): the arrangement takes the condemned's entire remaining life, a maximal per-person taking, but this reading's own lights credit the taking as potentially compensated by lives saved, and the contested evidence base leaves the compensatory claim unbanked, landing the net between justified coordination and pure extraction. Suppression (0.62) reflects the machinery required to hold the practice against resistance: expedited habeas tracks, specialized prosecution units, clemency standards tuned against innocence claims. Suppression is a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Theater (0.36) captures the growing share of activity that stages the signal (victim-impact presentation, execution-witness protocol, campaign rhetoric) relative to any demonstrated deterrent output. Accessibility collapse is low (0.35): life-without-parole is a fully functional substitute, and the many jurisdictions that abolished show no coordination breakdown. Resistance is high (0.72): exoneration-driven doubt, moratoria, and multi-decade repeal campaigns meet the practice continuously. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: a mid-interval ratchet (aggravating-factor expansion, appeals limitation) followed by partial decay (drug shortages, moratoria, shrinking death rows). All three tracked series share one time grid (points 0 through 50) so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the prosecutor seat the arrangement is a working policy instrument, an option that settles cases; from the condemned seat it is terminal cost-bearing with no exit; from the potential-victim seat it is insurance whose payout is asserted rather than evidenced; from the research seat it is an unfalsified-but-unconfirmed hypothesis wearing the costume of settled policy. The engine computes these per-seat classifications from the structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: potential_murder_victims sit near the beneficiary pole, condemned_defendants and wrongfully_executed_innocents near the full-target pole. Two overrides correct derivations the declarations alone would misplace. Surviving co-victims are declared beneficiaries, which would derive a strongly beneficiary-side d, but their situation documents heavy cost-bearing (decades of appeals reopening grief, symbolic conscription by both sides), so their override moves them toward symmetric (0.42). Prosecutors are declared beneficiaries and would derive near-full-beneficiary d, but their gain is option-value and career capital rather than receipt of the taking itself, so their override sits at 0.24. On the receipt surface, the arrangement's gains demonstrably accrue to prosecutors_offices as plea leverage and conviction capital regardless of whether the deterrent claim holds; fixing is cheap in cost-class terms because a fully functional substitute exists off the shelf and abolishing jurisdictions show no coordination breakdown.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Reading the arrangement as pure extraction would erase its genuine coordination face: incapacitation is uncontested, and the homicide-protection problem it addresses is real. Accepting the deterrence self-description as pure coordination would launder contested cost-bearing behind an unverified empirical claim. The tangled_rope claim holds both faces apart: coordination function present, asymmetric cost-bearing present, enforcement load-bearing. The temporal series shows the failure mode forming: theater rising monotonically while extractiveness plateaus and enforcement capacity decays from its mid-interval peak. If the marginal-deterrence omega resolves null while the practice persists, the mandate has outlived its function and the trajectory bends toward inertial maintenance, performance continuing after function. The founding problem (pre-modern confinement incapacity) is corroborated as solved by historians and corrections practitioners outside the benefiting parties; what persists is the dispute over whether exemplary signaling remains necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (deterrence_reading) of the kernel state_killing_legitimacy; what structural changes would instantiating a sibling reading instead produce?',
    'Generate and compare the sibling stories (retributive_reading, abolition_reading) against this one: victim sets, beneficiary sets, epsilon, and axioms should differ along the warrant axis.',
    'An abolition instantiation would push epsilon toward the maximum and recast every condemned defendant as categorically wronged rather than instrumentally spent; a retributive instantiation would relocate the beneficiary structure from potential future victims to the moral order satisfied by desert.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of three readings of the state-killing-legitimacy kernel.').

omega_variable(
    warrant_disagreement_location,
    'Where exactly do the three readings disagree: on the act of state killing, or on the warrant that legitimizes it?',
    'Structural comparison of the three stories'' axiom sets: if the disagreement localizes to the warrant (empirical utility versus proportional desert versus categorical dignity) while all three describe the same physical arrangement, the kernel is the warrant, not the practice.',
    'Locating the dispute in the warrant explains why the same execution can be simultaneously coordinated (deterrence), deserved (retributive), and impermissible (abolition) depending on seat, and predicts that empirical deterrence evidence moves only this reading, not the siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(warrant_disagreement_location, conceptual, 'The kernel contest is over the legitimizing warrant, not the physical act.').

omega_variable(
    marginal_deterrence_evidence_status,
    'Does execution add a marginal deterrent effect on homicide beyond life imprisonment, large enough to offset its costs?',
    'Quasi-experimental panel designs exploiting execution-propensity variation across jurisdictions and time, held to the standard set by the 2012 National Research Council review (which found no existing study informative in either direction); resolution awaits designs that survive replication.',
    'Confirmation lowers effective extraction, since the taking becomes the price of saved lives and strengthens the coordination face; refutation collapses the coordination story and pushes the arrangement toward pure extraction or inertial maintenance, cost-bearing persisting without the function that justified it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_deterrence_evidence_status, empirical, 'The empirical hinge on which this reading''s warrant turns.').

omega_variable(
    innocent_execution_under_pure_signal_logic,
    'Under pure signal logic, an innocent execution carries the same signaling value as a guilty one; does the deterrence reading internally tolerate wrongful execution, or does it covertly import desert?',
    'Doctrinal analysis of how retentionist systems handle innocence evidence (clemency standards, posthumous exonerations, whether near-certain innocence halts execution) compared against what strict signal optimization would recommend.',
    'If tolerated, wrongfully_executed_innocents must be counted as full-price targets and epsilon rises; if desert is imported, the reading is hybridized with the retributive sibling and its distinctness claim weakens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innocent_execution_under_pure_signal_logic, conceptual, 'Internal consistency test of the signal-only warrant.').

omega_variable(
    operative_function_drift,
    'Is the arrangement''s operative function today still deterrence, or has it drifted to expressive and electoral functions that ride on deterrence rhetoric?',
    'Compare stated justifications in legislation and litigation against revealed behavior (execution pace, victim-impact staging, campaign usage); the theater_ratio series tracks the gap.',
    'If drifted, this reading instantiates a constraint whose namesake function is vestigial, meaning the mandate is resolved in fact even while officially contested, bending classification toward inertial maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_function_drift, empirical, 'Whether deterrence remains the live function or a retained label.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_killing_legitimacy__deterrence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t10, state_killing_legitimacy__deterrence_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(stat_tr_t10, observed).
narrative_ontology:measurement(stat_tr_t20, state_killing_legitimacy__deterrence_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(stat_tr_t20, observed).
narrative_ontology:measurement(stat_tr_t30, state_killing_legitimacy__deterrence_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t40, state_killing_legitimacy__deterrence_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(stat_tr_t40, observed).
narrative_ontology:measurement(stat_tr_t50, state_killing_legitimacy__deterrence_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(stat_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_killing_legitimacy__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t10, state_killing_legitimacy__deterrence_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(stat_be_t10, observed).
narrative_ontology:measurement(stat_be_t20, state_killing_legitimacy__deterrence_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(stat_be_t20, observed).
narrative_ontology:measurement(stat_be_t30, state_killing_legitimacy__deterrence_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t40, state_killing_legitimacy__deterrence_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(stat_be_t40, observed).
narrative_ontology:measurement(stat_be_t50, state_killing_legitimacy__deterrence_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(stat_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_killing_legitimacy__deterrence_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t10, state_killing_legitimacy__deterrence_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(stat_su_t10, observed).
narrative_ontology:measurement(stat_su_t20, state_killing_legitimacy__deterrence_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(stat_su_t20, observed).
narrative_ontology:measurement(stat_su_t30, state_killing_legitimacy__deterrence_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t40, state_killing_legitimacy__deterrence_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement_basis(stat_su_t40, observed).
narrative_ontology:measurement(stat_su_t50, state_killing_legitimacy__deterrence_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(stat_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the capital punishment debate' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one kernel: this deterrence reading (warrant: empirical utility), the retributive reading (warrant: proportional desert), and the abolition reading (warrant: categorical dignity). Each carries its own epsilon, beneficiary/victim structure, and axioms; the warrant axis, not the physical act, is where they differ. The retributive warrant is historically upstream (lex talionis predates utilitarian penology); the deterrence reading cites public-protection evidence downstream of it; the abolition reading contests both warrants at once.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, organized, 0.42).
constraint_indexing:directionality_override(state_killing_legitimacy__deterrence_reading, powerful, 0.24).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
