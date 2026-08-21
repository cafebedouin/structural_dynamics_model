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
 *   human_readable: State Killing Authority: Retributive Desert
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint describes the state's authority to impose capital
 *   punishment based on the principle of retributive desert, where murderers
 *   are deemed to forfeit their right to life and proportional justice (lex
 *   talionis) demands 'death for death'. It is one reading of the broader
 *   'state_killing_authority' kernel, distinct from deterrence-based or
 *   categorical abolitionist views. The constraint is highly extractive, as
 *   it removes the ultimate right (life) from the condemned, and heavily
 *   suppressed, as it requires overcoming significant legal and moral
 *   resistance to enforce.
 *
 * KEY AGENTS:
 *   - state_judicial_system: Agenda setter (institutional/constrained)
 *   - condemned_persons: Primary target (powerless/trapped)
 *   - murder_victims_posthumously: Posthumous beneficiary (powerless/analytical)
 *   - aggrieved_families: Beneficiary (moderate/constrained)
 *   - retributive_justice_advocates: Beneficiary (organized/mobile)
 *   - human_rights_organizations: Excluded (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.9).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.95).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, snare).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "State Killing Authority: Retributive Desert").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'cb553c32-17fe-40e7-b5aa-28b0d610659d').
narrative_ontology:cs_kernel_codification('cb553c32-17fe-40e7-b5aa-28b0d610659d', formalized).
narrative_ontology:cs_authority_grounding('cb553c32-17fe-40e7-b5aa-28b0d610659d', lineage).
narrative_ontology:cs_interpretation_layer_present('cb553c32-17fe-40e7-b5aa-28b0d610659d').
narrative_ontology:cs_reading_relation('cb553c32-17fe-40e7-b5aa-28b0d610659d', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_reading_relation('cb553c32-17fe-40e7-b5aa-28b0d610659d', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('cb553c32-17fe-40e7-b5aa-28b0d610659d', foundational, life_forfeiture_by_murder).
narrative_ontology:cs_axiom_status(life_forfeiture_by_murder, holdable).
narrative_ontology:cs_axiom_grounding('cb553c32-17fe-40e7-b5aa-28b0d610659d', life_forfeiture_by_murder, deontological).
narrative_ontology:cs_axiom('cb553c32-17fe-40e7-b5aa-28b0d610659d', foundational, lex_talionis_proportionality).
narrative_ontology:cs_axiom_status(lex_talionis_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('cb553c32-17fe-40e7-b5aa-28b0d610659d', lex_talionis_proportionality, deontological).
narrative_ontology:cs_reference_frame('cb553c32-17fe-40e7-b5aa-28b0d610659d', classical_retributive_justice).
narrative_ontology:cs_drift_state('cb553c32-17fe-40e7-b5aa-28b0d610659d', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb553c32-17fe-40e7-b5aa-28b0d610659d', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victims_posthumously).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, aggrieved_families).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, retributive_justice_advocates).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, death_row_inmates).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_principle).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, proportional_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces capital punishment, grounding its authority in the principle of retributive justice and the forfeiture of rights by murderers. It determines guilt and imposes sentences, acting as the arbiter of proportional punishment.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Are the direct targets of the constraint, facing the ultimate penalty. Their right to life is deemed forfeited by their crime, and they have no exit from the state's authority once convicted and sentenced.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Are posthumously vindicated by the execution of their murderer, fulfilling the 'death for death' principle. This is a symbolic benefit, as they are no longer living agents.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victims_posthumously, beneficiary,
    powerless, generational, analytical, local).
narrative_ontology:stakeholder_non_agent(state_killing_authority__retributive_desert, murder_victims_posthumously).

% Receive a form of justice or closure through the execution of the person who murdered their loved one, seeing the principle of proportional retribution upheld. Their benefit is emotional and symbolic.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, aggrieved_families, beneficiary,
    moderate, biographical, constrained, local).

% Are those who philosophically or ideologically support capital punishment based on the principle of 'just deserts' and lex talionis. They benefit from the state's actions affirming their moral and legal framework.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, retributive_justice_advocates, beneficiary,
    organized, generational, mobile, national).

% Are excluded from the direct decision-making process but actively campaign against capital punishment, arguing for the inherent right to life and against state-sanctioned killing. Their arguments are often dismissed by proponents of retributive justice.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, human_rights_organizations, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the state's response to murder by establishing a clear, proportional penalty that aligns with a specific moral philosophy of justice, ensuring consistency in severe punishment.
% TRANSFER_FUNCTION: Transfers the right to life from the condemned person to the state, which then exercises its authority to execute, symbolically 'balancing the scales' for the victim.
% ABSENT_VOICES: Human rights organizations and abolitionist movements are structurally excluded from the retributive framework's core logic; they would argue that the state's authority does not extend to taking a life, regardless of the crime.
% DISAPPEARANCE_RATIONALE: If the authority for capital punishment based on retributive desert vanished, the entire criminal justice system's sentencing philosophy for murder would need to be re-evaluated. Life sentences would become the maximum penalty, and the moral justification for state punishment would shift dramatically, reorganizing legal and philosophical discourse.
% FOUNDING_PROBLEM: The problem of how to justly respond to the ultimate crime of murder, ensuring that the punishment fits the severity of the offense and vindicates the victim's lost life.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of capital punishment, including some victims' families and legal scholars, attest that the problem of achieving proportional justice for murder remains live. Opponents, however, argue that the problem is framed in a way that necessitates state killing, which they reject on moral grounds.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is near maximal (0.9) because it involves the permanent removal of life. Suppression is also very high (0.95) due to the active legal and physical force required to carry out executions and overcome persistent challenges. The theater ratio is low (0.1) because the act of execution, while ritualized, is directly functional to the constraint's purpose of ending a life. Accessibility collapse is high (0.9) as there is no alternative for the condemned once the process is complete. Resistance is substantial (0.7) from legal challenges and advocacy groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the condemned, this is a pure snare, an inescapable extraction of life. From the perspective of retributive justice advocates and aggrieved families, it is a necessary act of justice and vindication. The state judicial system, as the agenda setter, frames it as upholding a fundamental principle of law.
 *
 * DIRECTIONALITY LOGIC:
 *   Condemned persons are full targets (d=1.0) as they bear the entire cost. Murder victims (posthumously) and aggrieved families are beneficiaries (d near 0.0) as the constraint is enacted 'for' them. Retributive justice advocates also benefit by seeing their principles affirmed. The state judicial system, while administering, also benefits from the perceived legitimacy of upholding 'justice'. Human rights organizations are excluded, their arguments suppressed by the prevailing legal framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (proportional punishment for murder) is considered 'live' by its proponents, preventing a mandatrophy classification. However, the high extractiveness and suppression, coupled with significant resistance, indicate it operates as a snare from the perspective of its targets. The classification prevents mislabeling it as a 'rope' of justice, which would ignore the profound extraction and coercion involved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retributive_proportionality_ambiguity,
    'Is ''death for death'' the only or most appropriate interpretation of proportional punishment for murder?',
    'Philosophical consensus on alternative proportional penalties (e.g., life imprisonment without parole as a proportional response) or evolving societal norms regarding ''cruel and unusual'' punishment.',
    'If ''death for death'' is not the sole proportional response, the constraint''s justification weakens, potentially reclassifying it from a snare (based on a contested principle) to a more arbitrary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retributive_proportionality_ambiguity, conceptual, 'Ambiguity in the interpretation of proportional punishment for murder.').

omega_variable(
    forfeiture_of_rights_scope,
    'Does the act of murder truly entail a complete and irrevocable forfeiture of the right to life, or only a forfeiture of certain liberties?',
    'Legal and philosophical re-evaluation of the concept of ''inalienable rights'' and the limits of state authority to extinguish them, even for heinous crimes.',
    'If the right to life is deemed inalienable, the foundational premise of this constraint collapses, reclassifying it as an illegitimate snare or even a mountain of false authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(forfeiture_of_rights_scope, conceptual, 'The scope and irrevocability of rights forfeiture by murderers.').

omega_variable(
    corroboration_bias,
    'Is the ''live'' status of the founding problem (just response to murder) genuinely corroborated by independent parties, or is it primarily asserted by those who benefit from the retributive framework?',
    'Analysis of public opinion, legal scholarship, and international human rights discourse that is not directly tied to the enforcement or advocacy of capital punishment.',
    'If corroboration is found to be biased, the perceived legitimacy of the constraint''s mandate would diminish, potentially shifting its classification towards a piton (maintained by inertia/performance) if the problem is widely seen as ''dead'' by neutral observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_bias, empirical, 'Bias in the corroboration of the founding problem''s ''live'' status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__retributive_desert, theater_ratio, 1976, 0.05).
narrative_ontology:measurement(stat_tr_t1990, state_killing_authority__retributive_desert, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__retributive_desert, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__retributive_desert, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__retributive_desert, base_extractiveness, 1976, 0.85).
narrative_ontology:measurement(stat_be_t1990, state_killing_authority__retributive_desert, base_extractiveness, 1990, 0.9).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__retributive_desert, base_extractiveness, 2005, 0.92).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__retributive_desert, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__retributive_desert, suppression_requirement, 1976, 0.9).
narrative_ontology:measurement(stat_su_t1990, state_killing_authority__retributive_desert, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__retributive_desert, suppression_requirement, 2005, 0.97).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__retributive_desert, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
