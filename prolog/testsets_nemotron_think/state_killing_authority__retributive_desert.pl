% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Retributive Death Penalty (Lex Talionis)
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the retributive_desert reading of the
 *   state_killing_authority kernel. The reading holds that murderers forfeit
 *   their right to life by the act of murder, and that proportional
 *   punishment (lex talionis) requires the state to execute them — not to
 *   deter, not to incapacitate, but because justice demands the equivalence
 *   of penalty to crime. The murdered victim enters the beneficiary set
 *   posthumously via vindication; the condemned exits the rights-holder set
 *   via forfeiture; state authority is grounded in the proportionality norm
 *   itself, not in outcomes. This is one of three live readings of the same
 *   kernel; the other two (categorical_abolition, deterrence_instrument)
 *   instantiate different constraints with different beneficiary/victim
 *   structures and different ε values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.72).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.85).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Retributive Death Penalty (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'f760b63e-b180-4bf0-b648-5389ee197a95').
narrative_ontology:cs_kernel_codification('f760b63e-b180-4bf0-b648-5389ee197a95', formalized).
narrative_ontology:cs_authority_grounding('f760b63e-b180-4bf0-b648-5389ee197a95', lineage).
narrative_ontology:cs_interpretation_layer_present('f760b63e-b180-4bf0-b648-5389ee197a95').
narrative_ontology:cs_reading_relation('f760b63e-b180-4bf0-b648-5389ee197a95', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('f760b63e-b180-4bf0-b648-5389ee197a95', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('f760b63e-b180-4bf0-b648-5389ee197a95', foundational, murderer_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murderer_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('f760b63e-b180-4bf0-b648-5389ee197a95', murderer_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('f760b63e-b180-4bf0-b648-5389ee197a95', foundational, lex_talionis_requires_death_for_death).
narrative_ontology:cs_axiom_status(lex_talionis_requires_death_for_death, holdable).
narrative_ontology:cs_axiom_grounding('f760b63e-b180-4bf0-b648-5389ee197a95', lex_talionis_requires_death_for_death, deontological).
narrative_ontology:cs_reference_frame('f760b63e-b180-4bf0-b648-5389ee197a95', classical_retributive_justice).
narrative_ontology:cs_drift_state('f760b63e-b180-4bf0-b648-5389ee197a95', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f760b63e-b180-4bf0-b648-5389ee197a95', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumous).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, society_vindicated_justice).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_prisoners).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_families).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_proportionality).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, right_to_life_forfeitable_by_murder).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the death penalty through legislatures, courts, and corrections departments. Claims moral authority from carrying out proportional justice. Controls the entire apparatus: sentencing, appeals, execution protocols. Can abolish or retain the penalty; bears institutional legitimacy costs but no personal risk.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% The murdered persons themselves, deceased. In this reading, their moral standing is vindicated when the murderer receives death. They cannot speak, consent, or dissent. Their 'benefit' is a normative claim made on their behalf by the living retributive framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumous, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims_posthumous).

% The collective that sees the moral order restored when murder is answered with death. Experiences the execution as communicative justice — the community's declaration that murder is met with equivalent penalty. Can exit by emigrating or by shifting cultural consensus toward abolition.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, society_vindicated_justice, beneficiary,
    organized, generational, mobile, national).

% Individuals sentenced to death under this constraint. Have forfeited their right to life in the reading's logic. No exit from the constraint once sentenced — appeals are within the constraint, not escape from it. Execution is the terminal extraction.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_prisoners, payer,
    powerless, immediate, trapped, local).

% Families of the condemned who bear the visceral, generational trauma of state killing of their kin. No role in the crime, no voice in the sentencing, no exit from the grief. Their suffering is structurally necessary to the constraint's operation but unacknowledged in its justification.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_families, payer,
    powerless, biographical, constrained, local).

% Organizations and lawyers who argue life is inalienable and state killing is inherently impermissible. Systematically excluded from the retributive framework's internal logic — their premise (inalienability) contradicts the framework's premise (forfeiture). They operate outside the constraint, seeking to dismantle it.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Scholars and policymakers who justify capital punishment by its marginal deterrent effect, not by desert. They observe the retributive constraint from a consequentialist seat — they may support the same practice but for a different reason, and would abandon it if deterrence evidence failed.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, deterrence_proponents, observer,
    institutional, generational, analytical, national).

% Academic commentators who analyze the constraint's doctrinal history, philosophical coherence, and constitutional trajectory. They neither administer nor suffer the penalty; they map the conceptual architecture. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_killing_authority__retributive_desert, diffuse).
narrative_ontology:fixing_cost_class(state_killing_authority__retributive_desert, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Vindicates the moral order by imposing proportional punishment: communicates that the gravest violation of another's life is answered by forfeiture of the violator's own life, restoring the equilibrium of justice.
% TRANSFER_FUNCTION: Moves the condemned's life to the state's account of satisfied justice; moves posthumous vindication to the murdered victim's moral standing; moves expressive affirmation to the community that justice has been done.
% ABSENT_VOICES: The murdered victims themselves (dead, cannot consent to or reject the killing done in their name); future potential murderers (deterrence is explicitly not this reading's claim); international human rights bodies that declare the death penalty inherently cruel; the global majority of abolitionist nations.
% DISAPPEARANCE_RATIONALE: If the death penalty vanished overnight, the lex talionis principle — 'life for life' — would be abandoned as the governing norm for murder. Murderers would serve life without parole; the symbolic equilibrium of proportional retribution would break; the state's claim to vindicate victims through equivalent penalty would collapse; the retributive framework would lose its institutional anchor.
% FOUNDING_PROBLEM: How to proportionally punish the ultimate crime (murder) in a way that vindicates the victim's moral standing and restores the justice equilibrium that the murder destroyed — when any lesser penalty appears to treat the victim's life as worth less than the murderer's.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal tradition (Blackstone, Kant, Hegel) attests the founding problem as the anchor of retributive justice. Contemporary philosophical defenders (Moore, Pojman, Kantian revivalists) corroborate from within the tradition. No corroboration exists outside the retributive tradition itself — abolitionist and consequentialist traditions reject the premise that proportionality requires death.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is high (0.72) because the constraint takes a human life — the maximum possible extraction from the condemned. The reading claims this is not 'extraction' but 'proportional response'; the engine's referent is the standing arrangement (state killing), so the metric reflects the structural severity. Suppression is very high (0.85) because the condemned has zero exit: no appeal to a higher principle, no commutation within the framework, no geographical escape. Theater ratio is low (0.15) — the killing is real, not performative; the ritual serves the proportionality logic, not a substitute for it. Accessibility collapse is near-total (0.90) — once the death warrant issues, alternatives vanish. Resistance is substantial (0.60) — sustained abolitionist litigation, moral campaigns, and international pressure contest the constraint continuously.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (state) experiences this as coordination — it administers a justice system that solves the problem of proportional punishment. The payer seats (condemned, their families) experience it as terminal extraction with no exit. The beneficiary seats (posthumous victims, society) experience it as vindication — but the victims cannot speak, and society's vindication is expressive. The engine will compute these divergences from the structural data. The claimed_type (tangled_rope) reflects the author's judgment that the constraint has both a genuine coordination function (proportional justice communication) and asymmetric extraction (state killing of a rights-forfeited person) requiring active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   The state_execution_authority is the agenda_setter with arbitrage-grade exit (can abolish the penalty legislatively) — derived d near beneficiary end. Murder_victims_posthumous are beneficiaries by the reading's logic but are dead (powerless, trapped) — their 'benefit' is a normative claim made by others. Society_vindicated_justice benefits expressively with mobile exit. Condemned_prisoners are the pure targets: powerless, trapped, immediate horizon — derived d = 1.0. Condemned_families are collateral payers: powerless, constrained exit, biographical horizon. Abolitionist_advocates are excluded — their premise (inalienability) contradicts the framework's premise (forfeiture), so they cannot be seated within it. Deterrence_proponents and legal_scholars are observers at analytical distance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proportional punishment for murder) is contested: abolitionists argue life without parole satisfies proportionality; retributivists argue only death does. The constraint persists not because the founding problem is universally accepted as live, but because the retributive tradition holds institutional power in death-penalty jurisdictions. Mandatrophy is unresolved — the constraint's mandate (lex talionis) has not been acknowledged as obsolete by its administrators, though its empirical basis (deterrence) is rejected by this reading and its moral premise (forfeiture) is rejected by abolitionists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_mechanism_ambiguity,
    'Is rights forfeiture a metaphysical fact (the murderer actually loses the right to life by the act of murder) or a legal fiction (the state declares forfeiture to justify killing)?',
    'Philosophical analysis of the forfeiture doctrine''s coherence; legal history of whether forfeiture language performs explanatory work or merely restates the conclusion.',
    'If metaphysical fact, the constraint''s extraction is justified by the condemned''s own act — χ approaches 0 for the condemned seat. If legal fiction, the constraint is the state killing a rights-holder — χ remains high. This omega determines whether the reading''s core premise holds structural weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_mechanism_ambiguity, conceptual, 'Whether forfeiture is a real moral transformation or a justificatory label.').

omega_variable(
    proportionality_measurement,
    'How is ''life for life'' measured when lives are incommensurable — different ages, potentials, relationships, subjective experiences?',
    'Comparative analysis of proportionality doctrines across legal systems; philosophical work on the commensurability of harms.',
    'If proportionality is incommensurable, the lex talionis claim collapses into ''death for death'' as a ritual equivalence, not a measured one — the coordination function becomes purely expressive. If a metric exists, the constraint''s coordination claim gains structural specificity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_measurement, conceptual, 'Whether proportional punishment can be non-arbitrarily calibrated.').

omega_variable(
    state_moral_authority,
    'Does the state gain moral authority from executing murderers, or does the act of state killing undermine the moral authority it claims to vindicate?',
    'Longitudinal study of public legitimacy, institutional trust, and international standing in retentionist vs. abolitionist jurisdictions.',
    'If state killing erodes moral authority, the constraint''s coordination function (vindicating justice) is self-defeating — it becomes a snare. If it builds authority, the coordination function is reinforced. This omega bears on whether the constraint is tangled_rope (genuine coordination + extraction) or snare (coordination story as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_moral_authority, empirical, 'Whether the constraint''s expressive function achieves its stated aim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_killing_retributive_tr_t0, state_killing_authority__retributive_desert, theater_ratio, 0, 0.12).
narrative_ontology:measurement(state_killing_retributive_tr_t10, state_killing_authority__retributive_desert, theater_ratio, 10, 0.13).
narrative_ontology:measurement(state_killing_retributive_tr_t20, state_killing_authority__retributive_desert, theater_ratio, 20, 0.14).
narrative_ontology:measurement(state_killing_retributive_tr_t30, state_killing_authority__retributive_desert, theater_ratio, 30, 0.15).
narrative_ontology:measurement(state_killing_retributive_tr_t40, state_killing_authority__retributive_desert, theater_ratio, 40, 0.15).
narrative_ontology:measurement(state_killing_retributive_tr_t50, state_killing_authority__retributive_desert, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(state_killing_retributive_be_t0, state_killing_authority__retributive_desert, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(state_killing_retributive_be_t10, state_killing_authority__retributive_desert, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(state_killing_retributive_be_t20, state_killing_authority__retributive_desert, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(state_killing_retributive_be_t30, state_killing_authority__retributive_desert, base_extractiveness, 30, 0.72).
narrative_ontology:measurement(state_killing_retributive_be_t40, state_killing_authority__retributive_desert, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(state_killing_retributive_be_t50, state_killing_authority__retributive_desert, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(state_killing_retributive_su_t0, state_killing_authority__retributive_desert, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(state_killing_retributive_su_t10, state_killing_authority__retributive_desert, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(state_killing_retributive_su_t20, state_killing_authority__retributive_desert, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(state_killing_retributive_su_t30, state_killing_authority__retributive_desert, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(state_killing_retributive_su_t40, state_killing_authority__retributive_desert, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(state_killing_retributive_su_t50, state_killing_authority__retributive_desert, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__retributive_desert, 0.1).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'state_killing_authority' into three readings with distinct ε values and beneficiary/victim structures. Retributive_desert: ε=0.72, beneficiaries include posthumous victims. Deterrence_instrument: ε would be lower (conditional on deterrence working), beneficiaries are potential future victims. Categorical_abolition: ε≈0 for the abolitionist constraint (no state killing), but high for the existing death penalty it opposes. The readings are linked by shared kernel_id and cross-referenced in affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
