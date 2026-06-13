% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Capital Punishment as Deterrence Instrument
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint models capital punishment as justified solely by its
 *   capacity to deter future murders at an acceptable cost. It is one reading
 *   of the broader 'state killing authority' kernel. The core claim is
 *   instrumental: the state's authority to execute is contingent on its
 *   efficacy as a crime prevention tool. This reading places the lives of
 *   potential future victims as the primary beneficiaries, while the
 *   condemned individual is treated as an instrumental cost. The empirical
 *   evidence for deterrence is highly contested, leading to a significant gap
 *   between the claimed justification and its actual operation.
 *
 * KEY AGENTS:
 *   - state_legislatures: Agenda setter (institutional/constrained)
 *   - state_judiciary: Agenda setter (institutional/constrained)
 *   - potential_future_victims: Primary beneficiary (powerless/trapped)
 *   - condemned_individuals: Primary target/payer (powerless/trapped)
 *   - defense_attorneys: Payer (moderate/constrained)
 *   - families_of_condemned: Payer (powerless/trapped)
 *   - criminologists_and_statisticians: Observer (analytical/analytical)
 *   - human_rights_advocates: Excluded (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.65).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.9).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Capital Punishment as Deterrence Instrument").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, '7178e4ce-b1c5-43ba-9a62-693821e1971f').
narrative_ontology:cs_kernel_codification('7178e4ce-b1c5-43ba-9a62-693821e1971f', formalized).
narrative_ontology:cs_authority_grounding('7178e4ce-b1c5-43ba-9a62-693821e1971f', lineage).
narrative_ontology:cs_interpretation_layer_present('7178e4ce-b1c5-43ba-9a62-693821e1971f').
narrative_ontology:cs_reading_relation('7178e4ce-b1c5-43ba-9a62-693821e1971f', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('7178e4ce-b1c5-43ba-9a62-693821e1971f', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_axiom('7178e4ce-b1c5-43ba-9a62-693821e1971f', foundational, punishment_as_crime_prevention).
narrative_ontology:cs_axiom_status(punishment_as_crime_prevention, holdable).
narrative_ontology:cs_axiom_grounding('7178e4ce-b1c5-43ba-9a62-693821e1971f', punishment_as_crime_prevention, instrumental).
narrative_ontology:cs_axiom('7178e4ce-b1c5-43ba-9a62-693821e1971f', secondary, state_monopoly_on_violence_for_public_good).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_for_public_good, holdable).
narrative_ontology:cs_axiom_grounding('7178e4ce-b1c5-43ba-9a62-693821e1971f', state_monopoly_on_violence_for_public_good, conventional).
narrative_ontology:cs_reference_frame('7178e4ce-b1c5-43ba-9a62-693821e1971f', utilitarian_crime_prevention_framework).
narrative_ontology:cs_drift_state('7178e4ce-b1c5-43ba-9a62-693821e1971f', contemporary_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7178e4ce-b1c5-43ba-9a62-693821e1971f', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, potential_future_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, state_prosecutors).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, political_incumbents).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_individuals).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, defense_attorneys).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, families_of_condemned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and maintain laws authorizing capital punishment, often in response to public demand for 'tough on crime' policies. They frame capital punishment as a necessary tool for public safety and crime prevention.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Interprets and applies capital punishment statutes, overseeing trials and appeals. While bound by law, judicial decisions can shape the practical application and perceived efficacy of deterrence.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Are the theoretical beneficiaries of capital punishment if it genuinely deters future murders. Their lives are hypothetically saved, but they have no agency in the constraint's operation.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, potential_future_victims, beneficiary,
    powerless, generational, trapped, local).

% Are the direct targets of the constraint, losing their lives as an instrumental cost for the alleged deterrence benefit. Their agency is entirely suppressed by the state's power.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_individuals, payer,
    powerless, immediate, trapped, local).

% Bear the professional and emotional costs of defending individuals facing execution. They challenge the efficacy and constitutionality of capital punishment, often citing lack of deterrence evidence.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, defense_attorneys, payer,
    moderate, biographical, constrained, national).

% Experience profound loss and trauma, often becoming advocates against capital punishment. They bear the social and emotional costs without any perceived benefit.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, families_of_condemned, payer,
    powerless, generational, trapped, local).

% Conduct empirical studies on the deterrent effect of capital punishment. Their findings, largely inconclusive or negative regarding deterrence, directly challenge the foundational premise of this reading.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminologists_and_statisticians, observer,
    analytical, biographical, analytical, global).

% Argue against capital punishment on moral and ethical grounds, regardless of its deterrent effect. While they can influence public opinion and international law, their arguments are often dismissed by deterrence proponents as irrelevant to the instrumental goal.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, human_rights_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal behavior by establishing a severe consequence for murder, thereby deterring potential offenders and ensuring public safety.
% TRANSFER_FUNCTION: Transfers the life of a condemned individual from their person to the state, in exchange for the alleged benefit of preventing an unknown number of future murders for potential victims.
% ABSENT_VOICES: The condemned, whose voices are silenced by execution, and human rights advocates, whose categorical objections are deemed irrelevant by the instrumental logic of deterrence, are excluded from the core justification framework.
% DISAPPEARANCE_RATIONALE: If the authority to execute for deterrence vanished, the criminal justice system would need to fundamentally re-evaluate its sentencing structures, public safety strategies, and moral justifications for punishment. The political discourse around crime would also shift significantly.
% FOUNDING_PROBLEM: The problem of preventing heinous crimes, specifically murder, and ensuring public safety through the most severe available punishment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state legislatures, some prosecutors) attest the problem is live and capital punishment is a necessary deterrent. Criminologists, defense attorneys, and human rights groups, from outside the benefiting parties, widely contest the deterrence claim, citing empirical evidence that it does not significantly reduce murder rates beyond life imprisonment.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because it involves the ultimate cost (life) for a benefit (deterrence) that is empirically unproven. Suppression (0.90) is very high due to the state's monopoly on legitimate force and the complete lack of exit for the condemned. The theater ratio (0.40) reflects the gap between the stated deterrence goal and the actual, often symbolic, function of executions, especially as empirical evidence for deterrence weakens. Resistance (0.70) is high due to active legal and advocacy challenges. Accessibility collapse (0.80) is high because once the state asserts this authority, alternatives for the condemned are almost entirely foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state legislatures and some prosecutors, this is a legitimate, albeit severe, tool for public safety. From the perspective of the condemned, their families, and defense attorneys, it is an unjust and irreversible extraction based on a dubious premise. Criminologists observe a lack of empirical support, further widening this gap.
 *
 * DIRECTIONALITY LOGIC:
 *   The state (legislatures, judiciary) and political incumbents are beneficiaries (d near 0.0) as they gain perceived public safety and political capital. Potential future victims are also beneficiaries (d near 0.0) if deterrence is real. Condemned individuals, defense attorneys, and their families are clear targets/victims (d near 1.0). Criminologists and human rights advocates are observers or excluded, with analytical or mobile exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (deterrence) is increasingly contested. If the deterrence function is proven 'dead' (as many criminologists argue), but the practice persists, it would indicate mandatrophy, where the constraint continues due to inertia, political symbolism, or other unstated functions, rather than its original purpose. This would shift its classification closer to a Snare or Piton, as the coordination story (deterring crime) becomes a cover for pure extraction or theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does capital punishment actually deter future murders more effectively than life imprisonment?',
    'Longitudinal, controlled empirical studies comparing murder rates in jurisdictions with and without capital punishment, controlling for socioeconomic factors.',
    'If deterrence is disproven, the instrumental justification for this reading collapses, shifting its classification towards a Snare (pure extraction) or Piton (theatrical maintenance). If proven, it would strengthen the Rope or Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'Empirical evidence for the deterrence effect of capital punishment.').

omega_variable(
    cost_benefit_acceptability,
    'What constitutes an ''acceptable cost'' for the alleged deterrence benefit, particularly when that cost is a human life?',
    'Societal consensus through deliberative democracy, or judicial rulings establishing constitutional limits on ''acceptable cost'' in terms of human rights and due process.',
    'If the cost is deemed unacceptable, even with proven deterrence, the constraint''s legitimacy would collapse, pushing it towards a Snare. If a societal consensus on acceptability emerges, it would reinforce the claimed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_acceptability, preference, 'Societal definition of ''acceptable cost'' for deterrence.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine deterrence instrument, or is the deterrence claim a cover for retributive impulses or political signaling?',
    'Analysis of legislative intent, judicial opinions, and public discourse: if the primary justification consistently shifts to retribution or political expediency when deterrence evidence is challenged, it suggests a different underlying reading.',
    'If the deterrence claim is primarily a cover, the constraint is better classified as the ''retributive_desert'' reading, or a Snare if the cover is purely extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing deterrence as a genuine instrument from a cover story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__deterrence_instrument, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(stat_tr_t1990, state_killing_authority__deterrence_instrument, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(stat_tr_t2000, state_killing_authority__deterrence_instrument, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(stat_tr_t2010, state_killing_authority__deterrence_instrument, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__deterrence_instrument, base_extractiveness, 1976, 0.7).
narrative_ontology:measurement(stat_be_t1990, state_killing_authority__deterrence_instrument, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(stat_be_t2000, state_killing_authority__deterrence_instrument, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(stat_be_t2010, state_killing_authority__deterrence_instrument, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__deterrence_instrument, suppression_requirement, 1976, 0.85).
narrative_ontology:measurement(stat_su_t1990, state_killing_authority__deterrence_instrument, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(stat_su_t2000, state_killing_authority__deterrence_instrument, suppression_requirement, 2000, 0.9).
narrative_ontology:measurement(stat_su_t2010, state_killing_authority__deterrence_instrument, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'state_killing_authority' kernel, focusing on deterrence. It is linked to sibling readings (retributive_desert, categorical_abolition) which offer alternative justifications or rejections of state killing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
