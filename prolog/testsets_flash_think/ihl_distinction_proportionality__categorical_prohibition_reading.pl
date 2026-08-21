% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__categorical_prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__categorical_prohibition_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ihl_distinction_proportionality__categorical_prohibition_reading
 *   human_readable: Categorical Prohibition of Autonomous Weapons (Martens Clause Reading)
 *   domain: International Humanitarian Law / Military Ethics / Technology Governance
 *
 * SUMMARY:
 *   This constraint represents the 'categorical_prohibition_reading' of the
 *   'ihl_distinction_proportionality' kernel. It asserts that the Martens
 *   Clause principles of humanity and public conscience inherently prohibit
 *   autonomous weapons, regardless of their technical performance. The act of
 *   delegating lethal decision-making to machines is seen as a violation of
 *   human dignity per se. This reading claims a fundamental, natural-law-like
 *   status (claimed_type: mountain) for this prohibition, even though its
 *   operation is highly contested and extractive for those developing such
 *   systems. The divergence between the claimed type and the high
 *   extractiveness/suppression metrics is intentional, designed to trigger
 *   False Summit Mountain detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.9).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.85).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, mountain).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Categorical Prohibition of Autonomous Weapons (Martens Clause Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "International Humanitarian Law / Military Ethics / Technology Governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).
domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '3c002667-47a2-42b5-95ff-2c3be40d1f7e').
narrative_ontology:cs_kernel_codification('3c002667-47a2-42b5-95ff-2c3be40d1f7e', formalized).
narrative_ontology:cs_authority_grounding('3c002667-47a2-42b5-95ff-2c3be40d1f7e', lineage).
narrative_ontology:cs_interpretation_layer_present('3c002667-47a2-42b5-95ff-2c3be40d1f7e').
narrative_ontology:cs_reading_relation('3c002667-47a2-42b5-95ff-2c3be40d1f7e', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c002667-47a2-42b5-95ff-2c3be40d1f7e', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('3c002667-47a2-42b5-95ff-2c3be40d1f7e', foundational, machine_killing_violates_dignity_per_se).
narrative_ontology:cs_axiom_status(machine_killing_violates_dignity_per_se, holdable).
narrative_ontology:cs_axiom_grounding('3c002667-47a2-42b5-95ff-2c3be40d1f7e', machine_killing_violates_dignity_per_se, deontological).
narrative_ontology:cs_reference_frame('3c002667-47a2-42b5-95ff-2c3be40d1f7e', human_dignity_supremacy_in_warfare).
narrative_ontology:cs_drift_state('3c002667-47a2-42b5-95ff-2c3be40d1f7e', contemporary_laws_development, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3c002667-47a2-42b5-95ff-2c3be40d1f7e', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_advocates).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, autonomous_weapons_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively campaign for the categorical prohibition of autonomous weapons, framing it as a moral imperative derived from human dignity and the Martens Clause. They seek to establish this principle as an unassailable legal norm.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, human_rights_advocates, agenda_setter,
    organized, generational, constrained, global).

% Benefits from the establishment of a clear ethical boundary that prevents the dehumanization of warfare. Their advocacy aligns with this reading, and its success would vindicate their core principles.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, constrained, global).

% Benefit from a categorical ban as it levels the playing field, preventing technologically advanced states from gaining a decisive military advantage through autonomous weapons. They align with the prohibition to avoid a costly and destabilizing arms race.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    organized, generational, constrained, global).

% Bear the cost of foregone military advantage and strategic flexibility if autonomous weapons are categorically prohibited. They argue for the necessity of these systems for national security and military effectiveness.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_advantage_advocates, payer,
    powerful, biographical, constrained, global).

% Invest heavily in the development and deployment of autonomous weapons, viewing them as crucial for future defense strategies. A categorical prohibition would render these investments obsolete and constrain their military options.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).

% Face the direct economic and professional cost of a categorical ban, as their research, development, and potential market for autonomous weapons would be eliminated. They advocate for the responsible development and ethical use of the technology.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, autonomous_weapons_developers, payer,
    powerful, biographical, constrained, global).

% Analyze the legal and ethical implications of autonomous weapons, interpreting IHL principles like the Martens Clause. They contribute to the debate but do not directly benefit or pay from the constraint's operation, maintaining an analytical distance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_legal_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international legal and ethical consensus around a fundamental prohibition on autonomous weapons systems, establishing a clear boundary for acceptable military technology and preventing a 'race to the bottom' in human moral responsibility.
% TRANSFER_FUNCTION: Transfers the moral and legal authority for lethal decision-making from machines back to human agents, and transfers the 'right' to develop/deploy autonomous weapons from technologically advanced states to a global, principled prohibition.
% ABSENT_VOICES: Future generations who would live under the precedent of machine-decided killing, potentially normalized and expanded; non-human entities whose dignity might be implicated by such systems.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished, the development and deployment of autonomous weapons would accelerate without a fundamental ethical or legal barrier, fundamentally altering the nature of warfare, human responsibility for killing, and the global security landscape.
% FOUNDING_PROBLEM: The ethical and legal vacuum created by emerging autonomous weapons technology, specifically the concern that delegating lethal decision-making to machines violates fundamental principles of human dignity and international humanitarian law, as encapsulated by the Martens Clause.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross, the UN Secretary-General, numerous civil society organizations (e.g., Campaign to Stop Killer Robots), and a significant body of international legal and ethical scholarship corroborate the live status of this profound ethical and legal problem. This corroboration comes from outside the direct beneficiaries of the ban.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, ExtMetricName, E),
    domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ihl_distinction_proportionality__categorical_prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.90) because this reading seeks to impose a complete ban on an entire class of military technology, extracting the potential for strategic advantage and economic investment from states and developers. Suppression is also high (0.85) as the persistence of this prohibition requires active and continuous enforcement against powerful military and industrial interests. Resistance is high (0.80) due to the strong opposition from states and entities investing in autonomous weapons. Accessibility collapse is high (0.90) as the goal is to entirely close off the option of developing/deploying these systems. Theater ratio is low (0.10) because the argument is based on fundamental ethical and legal principles, not performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates and civil society, this constraint is a fundamental moral and legal truth, a 'mountain' that should guide all action. From the perspective of military powers and developers, it is an arbitrary and costly imposition, a 'snare' that stifles innovation and compromises national security. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and states lacking LAWS capability are structural beneficiaries (d near 0.0) as the prohibition aligns with their interests and values. Advocates for military technological advantage, states with advanced autonomous systems, and autonomous weapons developers are the primary targets (d near 1.0), as the constraint directly extracts from their strategic and economic interests. IHL legal scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the categorical prohibition of autonomous weapons a genuine natural law or an ethical/legal construct that benefits identifiable agents?',
    'Analysis of the historical development and contestation of the Martens Clause''s application to new technologies, and the political economy of its advocacy.',
    'If a genuine natural law, the ''mountain'' classification holds. If a constructed prohibition, the engine''s reclassification (e.g., to ''tangled_rope'' or ''snare'') would be affirmed, highlighting the underlying power dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'Ambiguity between a fundamental moral principle and a strategically beneficial legal construct.').

omega_variable(
    scope_of_human_dignity_violation,
    'Does machine-decided killing inherently violate human dignity, or is the violation contingent on specific contexts or outcomes?',
    'Further philosophical and ethical debate, potentially informed by empirical studies on the psychological and social impacts of autonomous weapons.',
    'If the violation is truly ''per se'', the categorical prohibition is strengthened. If contingent, it opens pathways for conditional acceptance of LAWS, weakening the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_human_dignity_violation, conceptual, 'The scope and nature of the human dignity violation claimed by the Martens Clause.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of autonomous weapons development structural (legal bans, resource denial) or internalized (ethical norms, public stigma)?',
    'Post-ban trajectory: if development persists covertly despite legal bans, reclassify as partially internalized suppression (e.g., ''black market'' development).',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, as the pressure persists even without overt enforcement. If purely structural, enforcement must be constant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for autonomous weapons development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2013, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2013, 0.1).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ihl__tr_t2017, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(ihl__tr_t2019, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(ihl__tr_t2021, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(ihl__tr_t2023, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2013, 0.75).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(ihl__be_t2017, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2017, 0.83).
narrative_ontology:measurement(ihl__be_t2019, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2019, 0.86).
narrative_ontology:measurement(ihl__be_t2021, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2021, 0.88).
narrative_ontology:measurement(ihl__be_t2023, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2023, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2013, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2013, 0.7).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(ihl__su_t2017, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2017, 0.78).
narrative_ontology:measurement(ihl__su_t2019, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2019, 0.81).
narrative_ontology:measurement(ihl__su_t2021, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2021, 0.83).
narrative_ontology:measurement(ihl__su_t2023, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__categorical_prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality__outcomes_based_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ihl_distinction_proportionality' kernel. This 'categorical_prohibition_reading' asserts an inherent ban, while the 'human_agency_reading' focuses on human control, and the 'outcomes_based_reading' on performance metrics. Their ε values and structural properties differ significantly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
