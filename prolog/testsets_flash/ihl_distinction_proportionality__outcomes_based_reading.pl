% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__outcomes_based_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Outcomes-Based Compliance for Autonomous Weapons
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'outcomes_based_reading' of International
 *   Humanitarian Law (IHL) obligations regarding autonomous weapon systems.
 *   It posits that IHL's requirements for distinction and proportionality are
 *   met if autonomous systems can demonstrably achieve performance equal to
 *   or exceeding human operators, emphasizing a technology-neutral approach
 *   where the law governs outcomes, not the means. This reading is contested
 *   by those who advocate for irreducible human agency or categorical
 *   prohibition of such systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.55).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.4).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Outcomes-Based Compliance for Autonomous Weapons").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '0a2e66c6-decc-4a86-b50b-cd64b27557b5').
narrative_ontology:cs_kernel_codification('0a2e66c6-decc-4a86-b50b-cd64b27557b5', formalized).
narrative_ontology:cs_authority_grounding('0a2e66c6-decc-4a86-b50b-cd64b27557b5', extraction).
narrative_ontology:cs_interpretation_layer_present('0a2e66c6-decc-4a86-b50b-cd64b27557b5').
narrative_ontology:cs_reading_relation('0a2e66c6-decc-4a86-b50b-cd64b27557b5', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('0a2e66c6-decc-4a86-b50b-cd64b27557b5', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('0a2e66c6-decc-4a86-b50b-cd64b27557b5', foundational, technology_neutrality_of_ihl).
narrative_ontology:cs_axiom_status(technology_neutrality_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('0a2e66c6-decc-4a86-b50b-cd64b27557b5', technology_neutrality_of_ihl, conventional).
narrative_ontology:cs_axiom('0a2e66c6-decc-4a86-b50b-cd64b27557b5', foundational, measurable_performance_equals_compliance).
narrative_ontology:cs_axiom_status(measurable_performance_equals_compliance, holdable).
narrative_ontology:cs_axiom_grounding('0a2e66c6-decc-4a86-b50b-cd64b27557b5', measurable_performance_equals_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('0a2e66c6-decc-4a86-b50b-cd64b27557b5', ihl_as_outcome_governor).
narrative_ontology:cs_drift_state('0a2e66c6-decc-4a86-b50b-cd64b27557b5', contemporary_ai_development_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0a2e66c6-decc-4a86-b50b-cd64b27557b5', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians_interpretive_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the use of autonomous systems to enhance military effectiveness, reduce human risk, and achieve precision in combat. This reading provides a legal pathway for their objectives.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates, beneficiary,
    institutional, generational, mobile, global).

% Develop and supply autonomous weapon systems. This reading creates a market for their products by providing a framework for legal compliance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% International legal bodies, academics, and NGOs responsible for interpreting and upholding IHL. This reading challenges their traditional interpretations emphasizing human judgment and risks diluting the 'spirit' of the law.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians_interpretive_authority, payer,
    institutional, civilizational, constrained, universal).

% Bear the ultimate risk of harm if autonomous systems fail to meet distinction and proportionality standards, or if the metrics themselves are insufficient to capture ethical nuances. Their safety is contingent on the performance and oversight of these systems.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations, payer,
    powerless, immediate, trapped, global).

% These states actively develop and promote this interpretation of IHL to justify their investment in autonomous weapon systems. They set the technical standards and legal arguments for compliance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_adopting_outcomes_based_approach, agenda_setter,
    institutional, generational, constrained, global).

% Advocate for stronger protections for civilians and often for a ban on autonomous weapons. They are excluded from the direct decision-making process of states adopting this reading, but exert pressure through advocacy and public opinion.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_rights_organizations, excluded,
    organized, generational, constrained, global).

% Analyze the ethical implications and technical feasibility of autonomous weapon systems. They provide critical assessments of the 'outcomes-based' approach, often highlighting risks and limitations.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ethicists_and_ai_safety_researchers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to develop and deploy autonomous weapon systems while claiming compliance with International Humanitarian Law, coordinating military innovation with legal obligations.
% TRANSFER_FUNCTION: Transfers the burden of ethical decision-making from human operators to technical systems, and potentially transfers risk from military personnel to civilian populations if systems fail. It also transfers interpretive authority over IHL from traditional custodians to technical experts and military strategists.
% ABSENT_VOICES: Advocates for a categorical ban on autonomous weapons, and those who insist on irreducible human moral judgment in lethal force decisions, are largely absent from the framing of this 'outcomes-based' approach. They would argue that the very premise of delegating such decisions to machines violates fundamental principles of humanity and public conscience.
% DISAPPEARANCE_RATIONALE: If this outcomes-based reading of IHL vanished, states developing autonomous weapons would face significant legal uncertainty and pressure to halt or severely restrict their programs. The debate would shift back towards human agency or categorical prohibition, fundamentally altering military R&D and international legal discourse.
% FOUNDING_PROBLEM: The challenge of integrating rapidly advancing autonomous weapon technologies with existing International Humanitarian Law, which was primarily designed for human-operated systems, without stifling military innovation or compromising legal compliance.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors and defense ministries corroborate that the problem of technology integration with IHL is live and pressing. Human rights organizations and ethicists, while disagreeing with the proposed solution, also corroborate the existence of the underlying problem of regulating autonomous weapons, though they frame it as a need for stronger prohibitions rather than flexible compliance pathways.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it offers a coordination function (allowing military innovation and efficiency) but also involves asymmetric extraction. Beneficiaries (military efficiency advocates, defense contractors) gain from the flexibility and potential cost savings of autonomous systems. Victims (IHL custodians' interpretive authority, civilian populations if metrics fail) bear the risk of reduced human control and the potential erosion of IHL principles. Active enforcement is required to establish and maintain the technical standards and legal interpretations that permit this approach. Extractiveness is moderate (0.55) as it enables significant military advantages, while suppression (0.4) reflects the ongoing debate and resistance from other IHL interpretations. Theater ratio is low (0.2) as there's a genuine effort to define and measure performance, though some aspects might be performative.
 *
 * PERSPECTIVAL GAP:
 *   Military efficiency advocates and defense contractors would experience this as a Rope, enabling innovation and operational effectiveness. IHL custodians and civilian populations, particularly those advocating for human agency or categorical prohibition, would experience it as a Snare, perceiving it as an erosion of fundamental protections and a dangerous delegation of moral responsibility. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Military efficiency advocates and defense contractors are beneficiaries (d near 0.0) as this reading enables their objectives and market. IHL custodians' interpretive authority and civilian populations are victims (d near 1.0) as their concerns about human control and potential harm are sidelined or made contingent on technical performance. States adopting this reading are agenda-setters, balancing military needs with legal compliance. Human rights organizations and ethicists are observers, critically evaluating the implications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not currently experiencing mandatrophy, as the debate around autonomous weapons and IHL compliance is very much 'live'. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction from IHL principles and civilian populations) or a pure Snare (ignoring the genuine coordination function for military innovation and the attempt to define compliance metrics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid interpretation of the IHL distinction/proportionality kernel, or does it fundamentally misrepresent IHL''s underlying principles?',
    'International legal consensus building, ICJ advisory opinions, or state practice evolution.',
    'If deemed a valid reading, it legitimizes the development and deployment of autonomous weapons under certain performance criteria. If deemed a misrepresentation, it would be rejected as a basis for IHL compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''outcomes_based_reading'' of the ''ihl_distinction_proportionality'' kernel. Sibling readings (''human_agency_reading'', ''categorical_prohibition_reading'') would reject this interpretation, arguing for irreducible human judgment or outright prohibition.').

omega_variable(
    metric_reliability_and_bias,
    'Can ''distinction'' and ''proportionality'' be reliably and unbiasedly quantified by technical metrics, especially in complex, dynamic combat environments?',
    'Extensive empirical testing, independent validation of AI performance metrics, and review by interdisciplinary ethics committees.',
    'If metrics are unreliable or biased, the constraint''s claim of IHL compliance is undermined, potentially leading to increased civilian harm and legal challenges. If reliable, it strengthens the case for autonomous systems in IHL compliance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_reliability_and_bias, empirical, 'The effectiveness of this reading hinges on the ability to objectively measure complex ethical concepts with technology.').

omega_variable(
    civilian_population_risk_assessment,
    'Does the ''outcomes_based_reading'' adequately protect civilian populations, or does it introduce new, unquantifiable risks by delegating lethal decision-making to machines?',
    'Long-term observational studies of autonomous weapon system deployment, independent humanitarian impact assessments, and public consultation.',
    'If new risks are identified, the reading''s legitimacy would be severely challenged, potentially leading to its rejection or significant modification. If risks are demonstrably managed, it would bolster its acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_population_risk_assessment, empirical, 'Assessing the real-world impact on civilian populations is crucial for the ethical validity of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'ihl_distinction_proportionality' kernel. Its structural delta permits autonomous weapons if technical metrics pass compliance thresholds, benefiting military efficiency and defense contractors, while potentially victimizing IHL interpretive authority and civilian populations. This contrasts with sibling readings that emphasize human agency or categorical prohibition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
