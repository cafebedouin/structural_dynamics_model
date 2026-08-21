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
 *   constraint_id: ihl_distinction_proportionality__outcomes_based_reading
 *   human_readable: IHL Distinction/Proportionality: Outcomes-Based Compliance for Autonomous Systems
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'outcomes-based' reading of International
 *   Humanitarian Law (IHL) obligations regarding autonomous weapon systems.
 *   It posits that IHL's requirements for distinction and proportionality are
 *   met if autonomous systems can demonstrably achieve performance equal to
 *   or exceeding human operators, emphasizing a technology-neutral approach
 *   where law governs outcomes, not means. This reading is one of several
 *   competing interpretations of the IHL kernel, allowing for the development
 *   and deployment of autonomous systems under specific performance criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.45).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.6).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Distinction/Proportionality: Outcomes-Based Compliance for Autonomous Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34').
narrative_ontology:cs_kernel_codification('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', formalized).
narrative_ontology:cs_authority_grounding('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', lineage).
narrative_ontology:cs_interpretation_layer_present('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34').
narrative_ontology:cs_reading_relation('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', foundational, ihl_is_technology_neutral).
narrative_ontology:cs_axiom_status(ihl_is_technology_neutral, holdable).
narrative_ontology:cs_axiom_grounding('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', ihl_is_technology_neutral, conventional).
narrative_ontology:cs_axiom('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', foundational, performance_is_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(performance_is_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', performance_is_sufficient_for_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', traditional_ihl_outcomes_focus).
narrative_ontology:cs_drift_state('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', contemporary_aws_development, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('79d7d3e3-4dd1-4ed2-a39e-b0b51ecb0a34', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ability to deploy autonomous systems that can potentially achieve faster, more precise, or more scalable military operations, reducing risk to human combatants and increasing operational tempo. This reading provides a legal pathway for such development.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_advocates, beneficiary,
    institutional, generational, mobile, global).

% Profits from the development, sale, and maintenance of autonomous weapon systems. This interpretation opens a significant market for advanced military AI and robotics, aligning legal permissibility with technological innovation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    organized, biographical, arbitrage, global).

% Bears the cost of potentially diluted or shifted interpretive power over IHL principles. The focus on measurable outcomes might reduce the emphasis on human moral judgment and the 'spirit' of the law, challenging traditional legal frameworks and the authority of humanitarian law custodians.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_authority, payer,
    institutional, civilizational, constrained, global).

% Potentially benefits from reduced civilian casualties if autonomous systems genuinely perform better than humans, but also bears the risk of new forms of harm if performance metrics are flawed, systems fail, or accountability becomes diffuse. Their safety depends on the robustness of the 'demonstrably achieve' clause.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations, payer,
    powerless, immediate, trapped, local).

% Would argue that human moral agency is an irreducible requirement for lethal force decisions, regardless of technical performance. This reading marginalizes their concerns about dehumanization and accountability by prioritizing measurable outcomes.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Are tasked with interpreting and applying IHL within military operations. This reading provides a framework for assessing the legality of autonomous systems, but also places a burden on them to define and verify performance metrics, potentially shifting their role from legal interpretation to technical validation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_legal_advisors, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to assess and potentially integrate autonomous weapon systems into their military doctrines while claiming compliance with IHL, by focusing on measurable performance outcomes rather than the means of achieving them.
% TRANSFER_FUNCTION: Transfers the burden of IHL compliance from human moral judgment to technical performance metrics, potentially shifting accountability and interpretive authority from legal bodies to technical experts and military operators. It also transfers risk to civilian populations if metrics are insufficient.
% ABSENT_VOICES: Advocates for a categorical prohibition on autonomous weapons, and those who insist on irreducible human agency in lethal force decisions, are marginalized by this outcomes-based framing. Their arguments about the inherent moral wrongness or the necessity of human judgment are not directly addressed by a performance-centric approach.
% DISAPPEARANCE_RATIONALE: If this outcomes-based reading vanished, states developing autonomous weapons would face significant legal uncertainty regarding IHL compliance. This would likely slow or halt development, forcing a re-evaluation of legal interpretations and potentially leading to a more restrictive stance on autonomous systems, thus rearranging military doctrine and defense industry investment.
% FOUNDING_PROBLEM: The problem of how to integrate rapidly advancing autonomous military technology with existing International Humanitarian Law, which was drafted with human-operated systems in mind.
% FOUNDING_PROBLEM_CORROBORATION: Military legal departments and defense industry associations attest that the problem is live, as technology continues to outpace legal frameworks. Humanitarian organizations and ethicists also corroborate the problem's existence, though they advocate for different solutions.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).
:- end_tests(ihl_distinction_proportionality__outcomes_based_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it permits military efficiency gains and opens markets for defense contractors, it also imposes a significant burden of proof for 'demonstrably achieve' performance. Suppression (0.6) is moderate because this reading actively suppresses alternative interpretations that prioritize human agency or categorical prohibition, requiring active legal and political defense against those views. Theater ratio (0.1) is low, as the core claim is genuinely about measurable performance, not merely a cover story, though the difficulty of 'demonstrably achieve' could introduce performative elements over time. The claimed type is Tangled Rope because it offers a coordination function (legal clarity for AWS development) but also involves asymmetric extraction (from IHL interpretive authority and potential risks to civilians).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (military/contractors) perceive this as a pragmatic, necessary adaptation of IHL to new technology, ensuring continued relevance. The victims (IHL authority/civilians) perceive it as a dangerous dilution of fundamental principles, potentially leading to a 'race to the bottom' in ethical standards. The engine's classification as Tangled Rope reflects this tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Military efficiency advocates and defense contractors are beneficiaries, as this reading provides a legal pathway for their interests. IHL interpretive authority and civilian populations are victims, bearing the costs of shifted interpretive power and potential risks. Military legal advisors act as agenda-setters, navigating the implementation of this reading. Human rights advocates are excluded, as their core concerns are sidelined by the outcomes-based focus.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling as a pure Rope by highlighting the active enforcement required to suppress alternative readings and the costs borne by IHL interpretive authority. It also avoids mislabeling as a Snare by acknowledging the genuine coordination function of providing a legal framework for emerging technology, even if that framework is contested and carries risks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_demonstrably_achieve,
    'What constitutes ''demonstrably achieve'' in practice, and who defines the metrics and verification standards?',
    'International consensus on technical standards, independent auditing bodies, and case law establishing precedents for performance verification.',
    'If ''demonstrably achieve'' is loosely defined or self-regulated by military powers, the extractiveness for IHL interpretive authority and civilian populations will increase significantly, pushing the constraint closer to a Snare. If robustly defined and independently verified, it could reduce extractiveness and strengthen the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_demonstrably_achieve, empirical, 'Ambiguity in the operationalization of performance-based compliance.').

omega_variable(
    accountability_gap_for_aws,
    'How is accountability assigned for IHL violations committed by autonomous systems operating under this outcomes-based reading?',
    'Development of clear legal frameworks for command responsibility, developer liability, and system design accountability, tested through international legal proceedings.',
    'If accountability remains diffuse or untraceable, the effective extractiveness for civilian populations increases dramatically, as harms may go unpunished, pushing the constraint towards a Snare. Clear accountability mechanisms would reduce this extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_gap_for_aws, conceptual, 'Uncertainty regarding legal accountability for autonomous weapon systems.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine outcomes-based reading of IHL, or is it a strategic framing to permit AWS development?',
    'Analysis of state practice, military doctrine, and legal interpretations over time. If the ''outcomes-based'' justification consistently shifts to accommodate technological capabilities rather than IHL principles, it suggests strategic framing.',
    'If it''s primarily a strategic framing, the underlying extractiveness is higher than currently assessed, as the coordination story is more theatrical. This would push the classification closer to a Snare or a more extractive Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing genuine legal interpretation from strategic justification for AWS development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.41).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three competing readings of the IHL distinction/proportionality kernel. This outcomes-based reading influences the human-agency and categorical-prohibition readings by providing a counter-narrative for AWS legality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
