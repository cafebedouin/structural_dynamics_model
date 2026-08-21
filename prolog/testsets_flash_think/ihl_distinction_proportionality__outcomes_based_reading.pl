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
 *   human_readable: IHL Outcomes-Based Compliance for Autonomous Systems
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'outcomes-based' reading of International
 *   Humanitarian Law (IHL) regarding autonomous weapons systems (AWS). It
 *   posits that IHL obligations for distinction and proportionality are met
 *   if AWS demonstrably achieve performance equal to or exceeding human
 *   operators, emphasizing a technology-neutral approach where law governs
 *   outcomes, not means. This is one reading of the broader
 *   'ihl_distinction_proportionality' kernel, contested by
 *   'human_agency_reading' and 'categorical_prohibition_reading' siblings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.45).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.55).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "IHL Outcomes-Based Compliance for Autonomous Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '87d1e92d-9f47-4319-b54c-8f4edd2d4db4').
narrative_ontology:cs_kernel_codification('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', formalized).
narrative_ontology:cs_authority_grounding('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', lineage).
narrative_ontology:cs_interpretation_layer_present('87d1e92d-9f47-4319-b54c-8f4edd2d4db4').
narrative_ontology:cs_reading_relation('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_axiom('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', foundational, ihl_is_technology_neutral).
narrative_ontology:cs_axiom_status(ihl_is_technology_neutral, holdable).
narrative_ontology:cs_axiom_grounding('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', ihl_is_technology_neutral, conventional).
narrative_ontology:cs_axiom('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', foundational, performance_is_sufficient_for_compliance).
narrative_ontology:cs_axiom_status(performance_is_sufficient_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', performance_is_sufficient_for_compliance, instrumental).
narrative_ontology:cs_reference_frame('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', ihl_technology_neutrality_doctrine).
narrative_ontology:cs_drift_state('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', contemporary_aws_development, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87d1e92d-9f47-4319-b54c-8f4edd2d4db4', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_forces).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians_interpretive_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_if_metrics_fail).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to integrate autonomous weapons systems (AWS) for perceived operational advantages (precision, speed, reduced risk to own personnel). This reading provides a legal pathway for such integration by focusing on measurable performance.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_forces, agenda_setter,
    institutional, biographical, constrained, global).

% Develop and sell AWS. This outcomes-based reading creates a market for their technology by defining a path to IHL compliance based on technical metrics, rather than categorical prohibitions or human agency requirements.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, arbitrage, global).

% Bodies like the ICRC and legal scholars who interpret and uphold IHL. This reading challenges their traditional interpretive authority by shifting the focus from human judgment and categorical principles to technical performance, potentially eroding the normative force of IHL.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ihl_custodians_interpretive_authority, payer,
    institutional, generational, constrained, global).

% Bear the ultimate risk if autonomous systems, despite meeting performance metrics, fail to adequately distinguish combatants from civilians or apply proportionality in practice, leading to unintended harm. Their safety depends on the robustness and verifiability of the technical performance claims.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_if_metrics_fail, payer,
    powerless, immediate, trapped, global).

% Advocate for stricter controls or outright prohibitions on AWS, often emphasizing human dignity and the Martens Clause. This outcomes-based reading largely excludes their arguments by framing the debate around technical performance rather than fundamental ethical or legal principles concerning means.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, human_rights_advocates, excluded,
    organized, biographical, mobile, global).

% Analyze the ethical implications of AWS. They observe the debate, often highlighting the challenges of defining and verifying 'human-equivalent' performance, and the potential for unforeseen consequences or accountability gaps.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, ai_ethicists, observer,
    moderate, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the integration of advanced autonomous weapons systems into military doctrine and procurement with the existing framework of International Humanitarian Law, by proposing a performance-based standard for compliance.
% TRANSFER_FUNCTION: Transfers the primary locus of IHL compliance assessment from human moral judgment and categorical prohibitions to demonstrable technical performance metrics, potentially transferring risk and interpretive authority in the process.
% ABSENT_VOICES: Advocates for a categorical prohibition on autonomous weapons and those who insist on irreducible human moral judgment in lethal decision-making are largely excluded from this framing, as their core arguments pertain to the means of warfare rather than solely its outcomes.
% DISAPPEARANCE_RATIONALE: If this outcomes-based reading vanished, the legal and ethical debate around autonomous weapons systems would revert to more restrictive interpretations, likely leading to significant delays or outright prohibitions on their development and deployment, forcing military forces and defense contractors to fundamentally rethink their strategies and investments.
% FOUNDING_PROBLEM: How to reconcile the rapid technological advancements in autonomous systems with the enduring principles of International Humanitarian Law, specifically regarding distinction and proportionality, to enable military innovation while maintaining legal compliance.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and defense technologists actively attest to the ongoing challenge of integrating new technologies within legal frameworks. However, IHL custodians and human rights advocates corroborate the existence of the problem but contest this specific outcomes-based solution as a valid interpretation of IHL.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The `extractiveness` is moderate (0.45) because this reading benefits military efficiency and defense contractors by opening a path for AWS deployment, while potentially extracting interpretive authority from IHL custodians and imposing risks on civilian populations. `Suppression` is moderate (0.55) as it actively suppresses alternative interpretations that would impose stricter limits on AWS. `Theater_ratio` is low (0.15) because the core of this reading is about demonstrable, measurable performance, not mere appearance, though the verifiability of such performance is an open question. The metrics reflect a growing acceptance and institutionalization of this interpretation over the interval.
 *
 * PERSPECTIVAL GAP:
 *   Military forces and defense contractors perceive this reading as a pragmatic and necessary adaptation of IHL to technological realities, enabling innovation. In contrast, IHL custodians and human rights advocates view it as a dangerous erosion of fundamental principles, potentially leading to an accountability gap and increased risk for civilians.
 *
 * DIRECTIONALITY LOGIC:
 *   Military forces and defense contractors are clear beneficiaries, gaining legal justification and market opportunities for AWS. IHL custodians bear the cost of diminished interpretive authority, and civilian populations bear the ultimate risk if technical performance fails. Human rights advocates are structurally excluded from this framing, as their core arguments are about means, not just outcomes.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_metrics_verifiability,
    'Can distinction and proportionality performance for autonomous systems truly be measured and verified to a degree that equals or exceeds human judgment, especially in complex, dynamic combat environments?',
    'Extensive empirical testing, independent auditing of AWS performance in simulated and real-world scenarios, and expert consensus on verifiable metrics.',
    'If verifiable, this reading gains significant legitimacy, reducing extraction from civilian populations. If unverifiable, the reading''s foundation collapses, increasing extraction from civilians and interpretive authority, pushing towards more restrictive interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_metrics_verifiability, empirical, 'The empirical challenge of measuring and verifying AWS performance against IHL standards.').

omega_variable(
    accountability_gap_resolution,
    'In the event of IHL violations by autonomous systems operating under this outcomes-based reading, where does legal and moral accountability ultimately reside?',
    'Development of clear legal frameworks for command responsibility, manufacturer liability, and programmer culpability specific to AWS, tested through international jurisprudence.',
    'If a robust accountability framework is established, the perceived risk to civilian populations and the erosion of IHL''s normative force are reduced. If an accountability gap persists, the constraint''s extractiveness from victims is amplified, pushing towards a Snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_gap_resolution, conceptual, 'The challenge of assigning accountability for AWS-caused IHL violations under an outcomes-based framework.').

omega_variable(
    ihl_technology_neutrality_scope,
    'Does IHL''s principle of technology-neutrality extend to the delegation of lethal decision-making to machines, or are there inherent limits to this neutrality when human agency is removed?',
    'Further development of international customary law, advisory opinions from international courts, and state practice clarifying the boundaries of technology-neutrality in the context of AWS.',
    'If technology-neutrality is deemed to have limits that preclude machine-decided killing, this outcomes-based reading would be foreclosed by the ''human_agency_reading'' or ''categorical_prohibition_reading''. If it is affirmed to extend fully, this reading gains stronger legal grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ihl_technology_neutrality_scope, conceptual, 'The conceptual boundary of IHL''s technology-neutrality principle in relation to autonomous lethal decision-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t2015, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ihl__tr_t2020, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(ihl__tr_t2025, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2025, 0.15).
narrative_ontology:measurement(ihl__tr_t2030, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2030, 0.17).
narrative_ontology:measurement(ihl__tr_t2035, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 2035, 0.18).

% Extraction over time
narrative_ontology:measurement(ihl__be_t2015, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(ihl__be_t2020, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2020, 0.4).
narrative_ontology:measurement(ihl__be_t2025, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2025, 0.45).
narrative_ontology:measurement(ihl__be_t2030, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2030, 0.48).
narrative_ontology:measurement(ihl__be_t2035, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 2035, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t2015, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(ihl__su_t2020, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(ihl__su_t2025, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement(ihl__su_t2030, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2030, 0.58).
narrative_ontology:measurement(ihl__su_t2035, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 2035, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ihl_distinction_proportionality' kernel, each representing a distinct interpretation of IHL's application to autonomous weapons systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
