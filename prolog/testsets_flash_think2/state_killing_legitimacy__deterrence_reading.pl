% ============================================================================
% CONSTRAINT STORY: state_killing_legitimacy__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: state_killing_legitimacy__deterrence_reading
 *   human_readable: State Killing for Deterrence (Deterrence Reading)
 *   domain: criminal_justice/political_philosophy/legal_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the justification of state killing
 *   (capital punishment) through the lens of its claimed deterrent effect on
 *   future murders. It is one reading of the broader
 *   'state_killing_legitimacy' kernel, focusing on the instrumental use of
 *   punishment to achieve a social good. The constraint is claimed as a
 *   Tangled Rope, reflecting its dual function of coordinating social order
 *   through deterrence while extracting the life of the condemned, with the
 *   efficacy of the deterrence being a central point of contestation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_legitimacy__deterrence_reading, 0.65).
domain_priors:suppression_score(state_killing_legitimacy__deterrence_reading, 0.9).
domain_priors:theater_ratio(state_killing_legitimacy__deterrence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(state_killing_legitimacy__deterrence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_legitimacy__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_legitimacy__deterrence_reading, "State Killing for Deterrence (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_legitimacy__deterrence_reading, "criminal_justice/political_philosophy/legal_theory").

domain_priors:requires_active_enforcement(state_killing_legitimacy__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_legitimacy__deterrence_reading, 'd94c2416-a9b8-40a6-9a67-33cbefdc795b').
narrative_ontology:cs_kernel_codification('d94c2416-a9b8-40a6-9a67-33cbefdc795b', formalized).
narrative_ontology:cs_authority_grounding('d94c2416-a9b8-40a6-9a67-33cbefdc795b', lineage).
narrative_ontology:cs_interpretation_layer_present('d94c2416-a9b8-40a6-9a67-33cbefdc795b').
narrative_ontology:cs_reading_relation('d94c2416-a9b8-40a6-9a67-33cbefdc795b', state_killing_legitimacy__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('d94c2416-a9b8-40a6-9a67-33cbefdc795b', state_killing_legitimacy__abolition_reading, forecloses).
narrative_ontology:cs_axiom('d94c2416-a9b8-40a6-9a67-33cbefdc795b', foundational, capital_punishment_deters_crime).
narrative_ontology:cs_axiom_status(capital_punishment_deters_crime, holdable).
narrative_ontology:cs_axiom_grounding('d94c2416-a9b8-40a6-9a67-33cbefdc795b', capital_punishment_deters_crime, empirically_contingent).
narrative_ontology:cs_axiom('d94c2416-a9b8-40a6-9a67-33cbefdc795b', foundational, offender_life_subordinate_to_social_safety).
narrative_ontology:cs_axiom_status(offender_life_subordinate_to_social_safety, holdable).
narrative_ontology:cs_axiom_grounding('d94c2416-a9b8-40a6-9a67-33cbefdc795b', offender_life_subordinate_to_social_safety, instrumental).
narrative_ontology:cs_reference_frame('d94c2416-a9b8-40a6-9a67-33cbefdc795b', utilitarian_social_contract).
narrative_ontology:cs_drift_state('d94c2416-a9b8-40a6-9a67-33cbefdc795b', contemporary_criminology_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d94c2416-a9b8-40a6-9a67-33cbefdc795b', '').
narrative_ontology:cs_kernel_id(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, state_authority).
narrative_ontology:constraint_beneficiary(state_killing_legitimacy__deterrence_reading, general_public).
narrative_ontology:constraint_victim(state_killing_legitimacy__deterrence_reading, condemned_offenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The sovereign entity that authorizes and carries out executions, claiming to do so to protect its citizens by deterring future crimes. It maintains the legal and physical infrastructure for capital punishment.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, state_authority, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Individuals sentenced to death, whose lives are extracted by the state. They are the direct targets of the constraint, with no exit options from the legal process once all appeals are exhausted.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, condemned_offenders, payer,
    powerless, immediate, trapped, local).

% Abstract group of individuals whose lives are theoretically protected by the deterrent effect of capital punishment. They benefit from the perceived reduction in crime, though this benefit is indirect and empirically contested.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, potential_future_victims, beneficiary,
    powerless, generational, trapped, regional).

% Benefits from the perceived sense of security and justice that capital punishment is claimed to provide. Their support or opposition can influence policy, but individual members have limited direct impact on specific cases.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% Organizations and individuals who argue against capital punishment on moral and ethical grounds, often citing its ineffectiveness as a deterrent and the risk of executing innocent people. They are excluded from the direct decision-making process but exert public and legal pressure.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, abolitionist_advocates, excluded,
    organized, generational, mobile, global).

% Academics and researchers who analyze the legal, ethical, and empirical aspects of capital punishment, including its deterrent effect. They provide critical analysis but do not directly participate in its enforcement or suffer its consequences.
narrative_ontology:constraint_stakeholder(state_killing_legitimacy__deterrence_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order by establishing a severe deterrent against capital crimes, aiming to prevent future harm to potential victims and maintain public safety.
% TRANSFER_FUNCTION: Transfers the life of the condemned offender to the state, instrumentalizing it as a means to a social end (crime prevention).
% ABSENT_VOICES: The condemned offenders themselves, whose voices are silenced by execution. Also, human rights organizations and criminologists who dispute the deterrent effect, often excluded from policy-making debates.
% DISAPPEARANCE_RATIONALE: If state killing vanished overnight, the criminal justice system would need to fundamentally rethink its approach to severe crimes, punishment, and public safety. The state's claim to ultimate punitive power and its strategy for deterring severe crimes would be fundamentally altered, leading to a reorganization of sentencing guidelines and public discourse on justice.
% FOUNDING_PROBLEM: To prevent severe crimes, particularly murder, and maintain social order by demonstrating the state's resolve to punish the most heinous acts.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (some law enforcement officials, victims' families, and segments of the public) attest that the problem of severe crime and the need for deterrence are still live. Opponents (criminologists, human rights groups, and legal scholars) contest the empirical efficacy of capital punishment as a deterrent, citing studies that show no significant difference in crime rates between abolitionist and retentionist jurisdictions. Legislative hearing testimony and independent academic research from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(state_killing_legitimacy__deterrence_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_legitimacy__deterrence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_legitimacy__deterrence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(state_killing_legitimacy__deterrence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_legitimacy__deterrence_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` is moderate (0.65) because the ultimate cost (life) is extracted, but the justification rests on a contested empirical claim of deterrence. If deterrence were proven ineffective, extractiveness would be higher, approaching a Snare. `Suppression` is very high (0.90) as the state wields ultimate coercive power to enforce this constraint. `Theater_ratio` is moderate (0.40) because while there is a genuine claim of deterrence, the performative aspect of executions (sending a 'signal') often overshadows the empirically uncertain functional effect. `Accessibility_collapse` is moderate (0.50) as alternatives like life imprisonment exist but are not chosen by the state in this reading. `Resistance` is moderate (0.60) due to ongoing abolitionist movements and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and its supporters, this constraint is a necessary, albeit severe, tool for maintaining social order and protecting citizens. From the perspective of the condemned and abolitionist advocates, it is an unjust and often ineffective act of state violence, instrumentalizing individuals for an unproven social benefit. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The `state_authority` and `general_public` are beneficiaries, as they are theoretically protected and benefit from the perceived order. `Potential_future_victims` are also beneficiaries, as their safety is the primary stated goal. `Condemned_offenders` are the clear targets, bearing the ultimate cost. The state's institutional power and the offender's trapped exit options drive the directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents mislabeling it as a pure Rope (which would imply clear, symmetric benefits from deterrence) or a pure Snare (which would deny any genuine coordination function, even if contested). It acknowledges the state's stated coordination goal (deterrence) while highlighting the asymmetric extraction and the empirical uncertainty that underpins its legitimacy. If the deterrence claim were definitively disproven, the constraint would drift towards a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does capital punishment actually deter murder more effectively than life imprisonment?',
    'Longitudinal, cross-jurisdictional empirical studies comparing murder rates in states/countries with and without capital punishment, controlling for other variables.',
    'If deterrence is proven ineffective, the constraint''s extractiveness would be re-evaluated as higher (closer to a Snare), as the primary coordination justification would collapse. If proven effective, extractiveness would be lower (closer to a Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Empirical uncertainty regarding the core justification for capital punishment in this reading.').

omega_variable(
    moral_instrumentalization_ambiguity,
    'Is it morally permissible to instrumentalize an individual''s life (the condemned offender) as a means to a social end (deterrence)?',
    'Philosophical debate and public consensus on deontological vs. consequentialist ethics in criminal justice. This is a conceptual and preference-based question.',
    'If instrumentalization is deemed morally impermissible, the entire deterrence justification for state killing would be undermined, pushing the constraint towards a Snare regardless of empirical efficacy. If deemed permissible, the ethical foundation of this reading would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_instrumentalization_ambiguity, conceptual, 'Conceptual uncertainty regarding the ethical permissibility of using a person as a means to an end.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_legitimacy__deterrence_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1970, state_killing_legitimacy__deterrence_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(stat_tr_t1980, state_killing_legitimacy__deterrence_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(stat_tr_t1990, state_killing_legitimacy__deterrence_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(stat_tr_t2000, state_killing_legitimacy__deterrence_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(stat_tr_t2010, state_killing_legitimacy__deterrence_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(stat_tr_t2020, state_killing_legitimacy__deterrence_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1970, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(stat_be_t1980, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(stat_be_t1990, state_killing_legitimacy__deterrence_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(stat_be_t2000, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(stat_be_t2010, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(stat_be_t2020, state_killing_legitimacy__deterrence_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1970, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(stat_su_t1980, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1980, 0.87).
narrative_ontology:measurement(stat_su_t1990, state_killing_legitimacy__deterrence_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(stat_su_t2000, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(stat_su_t2010, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(stat_su_t2020, state_killing_legitimacy__deterrence_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_legitimacy__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__retributive_reading).
narrative_ontology:affects_constraint(state_killing_legitimacy__deterrence_reading, state_killing_legitimacy__abolition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_legitimacy' kernel. Its structural properties and classification are distinct from sibling readings, which justify or oppose state killing on different grounds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
