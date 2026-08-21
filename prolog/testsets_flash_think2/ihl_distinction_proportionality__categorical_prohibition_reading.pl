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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Martens Clause Categorical Prohibition of Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents a reading of the Martens Clause that posits a
 *   categorical prohibition on autonomous weapons systems (LAWS), arguing
 *   that machine-decided killing inherently violates human dignity and
 *   principles of public conscience, regardless of technical performance. It
 *   claims the status of a fundamental, unchangeable ethical and legal
 *   boundary. The high extractiveness reflects the complete foreclosure of a
 *   technological path, while high suppression reflects the active resistance
 *   required to prevent LAWS development and deployment. The claimed type is
 *   'mountain' because this reading asserts an irreducible, natural-law-like
 *   prohibition, even as the metrics reflect its contested and enforced
 *   nature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__categorical_prohibition_reading, 0.85).
domain_priors:suppression_score(ihl_distinction_proportionality__categorical_prohibition_reading, 0.9).
domain_priors:theater_ratio(ihl_distinction_proportionality__categorical_prohibition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__categorical_prohibition_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__categorical_prohibition_reading, mountain).
narrative_ontology:human_readable(ihl_distinction_proportionality__categorical_prohibition_reading, "Martens Clause Categorical Prohibition of Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__categorical_prohibition_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__categorical_prohibition_reading).
domain_priors:emerges_naturally(ihl_distinction_proportionality__categorical_prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__categorical_prohibition_reading, '6d8bc088-ce4e-4493-9e73-8b5ded359d7b').
narrative_ontology:cs_kernel_codification('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', fixed_text).
narrative_ontology:cs_authority_grounding('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', lineage).
narrative_ontology:cs_interpretation_layer_present('6d8bc088-ce4e-4493-9e73-8b5ded359d7b').
narrative_ontology:cs_reading_relation('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_axiom('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', foundational, human_dignity_absolute_prohibition).
narrative_ontology:cs_axiom_status(human_dignity_absolute_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', human_dignity_absolute_prohibition, deontological).
narrative_ontology:cs_axiom('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', foundational, martens_clause_categorical_application).
narrative_ontology:cs_axiom_status(martens_clause_categorical_application, holdable).
narrative_ontology:cs_axiom_grounding('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', martens_clause_categorical_application, conventional).
narrative_ontology:cs_reference_frame('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', inherent_human_dignity_framework).
narrative_ontology:cs_drift_state('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', contemporary_technological_advances, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6d8bc088-ce4e-4493-9e73-8b5ded359d7b', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the categorical ban on autonomous weapons, seeing it as a fundamental protection of human dignity and a step towards preventing dehumanized warfare. Benefits from the principle's adoption and enforcement.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, anti_militarist_civil_society, beneficiary,
    organized, generational, mobile, global).

% Benefits from a categorical prohibition that prevents technologically advanced adversaries from gaining a decisive military advantage through autonomous weapons, leveling the playing field and reducing pressure to develop costly systems.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_lacking_laws_capability, beneficiary,
    institutional, generational, constrained, global).

% Bears the cost of this prohibition by being denied the opportunity to develop and deploy a new class of weapons perceived as offering strategic advantages (e.g., speed, precision, risk reduction for human personnel). Their innovation path is foreclosed.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, military_technological_developers, payer,
    powerful, biographical, constrained, global).

% Bears the cost of this prohibition by losing a perceived military advantage and the ability to project power through advanced autonomous systems. They resist the ban, arguing for the legality and utility of such systems under existing IHL.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, states_with_advanced_autonomous_systems, payer,
    institutional, generational, constrained, global).

% Interpret and debate International Humanitarian Law, with some actively promoting this categorical prohibition reading of the Martens Clause as essential for upholding human dignity in armed conflict. They shape the discourse and legal arguments.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, ihl_legal_scholars, agenda_setter,
    analytical, generational, analytical, global).

% Serve as forums for international debate and potential codification of norms and prohibitions related to autonomous weapons. They are a key institutional site where this reading seeks to gain formal recognition and enforcement.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__categorical_prohibition_reading, un_disarmament_bodies, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__categorical_prohibition_reading, diffuse).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__categorical_prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate international norms and legal interpretations to establish a clear, categorical ethical boundary against machine-decided killing, preventing a moral and technological 'race to the bottom' in autonomous weapons development.
% TRANSFER_FUNCTION: Transfers the moral burden of lethal decision-making away from machines and back to human responsibility, and transfers potential military advantage from technologically advanced states (who would develop LAWS) to those prioritizing ethical constraints and human dignity.
% ABSENT_VOICES: Proponents of unrestricted technological development in warfare, who would argue for efficiency, strategic advantage, and reduced human risk over categorical prohibitions. They are present in the debate but their core premise is rejected by this reading.
% DISAPPEARANCE_RATIONALE: If this categorical prohibition vanished, the ethical and legal landscape for autonomous weapons would be fundamentally altered. It would likely accelerate the development and deployment of LAWS without a strong, principled moral constraint, leading to a significant shift in military doctrine and international relations.
% FOUNDING_PROBLEM: The existential threat to human dignity and the erosion of moral responsibility posed by delegating lethal decision-making to machines, particularly in the context of armed conflict where human judgment and conscience are deemed irreducible.
% FOUNDING_PROBLEM_CORROBORATION: International ethical bodies, human rights organizations, and a significant segment of IHL scholars corroborate the ongoing nature of this problem, independent of military interests. Their analyses and advocacy provide external validation for the problem's persistence.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__categorical_prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__categorical_prohibition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ihl_distinction_proportionality__categorical_prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__categorical_prohibition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) and suppression (0.90) reflect the ambition of this reading: to completely ban an entire class of technology and to actively prevent its development and use. The accessibility collapse is near total (0.95) because the principle aims to make the alternative (LAWS) morally and legally unthinkable. Resistance is high (0.90) due to strong military and technological interests that oppose such a ban. The theater ratio is low (0.10) because the claim is direct, principled, and actively advocated, not performative. The increasing extractiveness and suppression over time reflect the growing urgency and intensity of the debate as LAWS technology advances.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those advocating for the categorical prohibition, this is a fundamental, self-evident truth (a Mountain). From the perspective of military developers and states seeking technological advantage, it is an arbitrary, highly extractive restriction (a Snare or Tangled Rope) that forecloses legitimate strategic options. The engine's FSM mechanism will detect this divergence between the claimed Mountain type and the presence of identifiable beneficiaries and victims, signaling a potential false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Anti-militarist civil society and states lacking LAWS capability are beneficiaries, as the prohibition aligns with their ethical stances and strategic interests. Military technological developers and states with advanced autonomous systems are victims, as the prohibition directly restricts their capabilities and development paths. IHL legal scholars and UN disarmament bodies act as agenda-setters, interpreting and promoting this reading within international legal and ethical frameworks.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the categorical prohibition of autonomous weapons truly an emergent natural law (Mountain) derived from inherent human dignity, or a constructed ethical/legal stance (Snare/Tangled Rope) that benefits identifiable actors?',
    'Philosophical and legal consensus building on the universality and irreducibility of the ''human dignity per se'' argument, or empirical observation of whether the prohibition''s persistence depends on active enforcement by its beneficiaries rather than inherent moral force.',
    'If resolved as a genuine natural law, its classification as Mountain would be affirmed. If resolved as a constructed prohibition, it would reclassify towards a Snare or Tangled Rope, reflecting its active enforcement and beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'Ambiguity between inherent moral truth and constructed ethical/legal norm.').

omega_variable(
    martens_clause_interpretive_ambiguity,
    'Does the Martens Clause, by invoking ''principles of humanity and public conscience,'' categorically prohibit machine-decided killing, or does it allow for interpretations that permit LAWS under certain conditions (e.g., with human oversight or superior performance)?',
    'Further development of international customary law, state practice, and opinio juris, or a definitive ruling by an international court on the scope of the Martens Clause regarding LAWS.',
    'If the categorical prohibition is affirmed, this reading''s strength is reinforced. If alternative interpretations allowing LAWS gain traction, this reading''s suppressive force and extractiveness would diminish, potentially leading to a reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(martens_clause_interpretive_ambiguity, empirical, 'Ambiguity in the interpretation and application of the Martens Clause to new technologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__categorical_prohibition_reading, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2010, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2015, 0.11).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(ihl__tr_t2025, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__categorical_prohibition_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2010, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(ihl__be_t2025, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2025, 0.83).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__categorical_prohibition_reading, base_extractiveness, 2030, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2010, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2020, 0.87).
narrative_ontology:measurement(ihl__su_t2025, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2025, 0.89).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__categorical_prohibition_reading, suppression_requirement, 2030, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
