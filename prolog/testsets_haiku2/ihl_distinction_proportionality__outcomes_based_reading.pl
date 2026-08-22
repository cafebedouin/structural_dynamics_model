% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__outcomes_based_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Outcomes-Based Autonomous Weapons Compliance (IHL Distinction/Proportionality)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   The outcomes-based reading of IHL distinction and proportionality
 *   obligations interprets compliance as a technical performance question: if
 *   an autonomous system demonstrably achieves target-discrimination and
 *   collateral-damage restraint equal to or exceeding human operators, it
 *   satisfies IHL requirements. This reading is one of three contested
 *   interpretations of a contested kernel — the contested kernel is 'what
 *   does IHL require of autonomous systems?' The outcomes-based reading
 *   treats the answer as 'demonstrable performance parity' and is promoted by
 *   military technologists and defense contractors; the human-agency and
 *   categorical-prohibition readings treat it as requiring irreducible human
 *   moral judgment or absolute prohibition respectively. The authored metrics
 *   describe substantial extraction: military and contractor beneficiaries
 *   gain operational freedom and market share while humanitarian law
 *   custodians lose interpretive authority and civilians lose participation
 *   in the metric choice that governs their protection. Suppression is high
 *   because maintaining this reading requires active suppression of
 *   alternative interpretations — human-agency objections must be reframed as
 *   'non-technical resistance,' humanitarian law expertise must be
 *   subordinated to engineering judgment.
 *
 * KEY AGENTS:
 *   - Military operators — gain tactical freedom, operational speed, force multiplication under outcomes-based compliance pathway
 *   - Defense contractors — capture autonomous weapons market, licensing, and standards-setting influence
 *   - Humanitarian law custodians (ICRC, UN bodies) — lose interpretive authority, relegated to observer role in metric validation
 *   - Civilian populations — powerless, trapped, bear risk if metrics fail or embed blind spots
 *   - States adopting outcomes-based reading — agenda-setters, determine compliance thresholds and metrics
 *   - Metric developers — organized agenda-setters who design the technical parameters operationalizing 'performance parity'
 *   - Civil society observers — constrained observers who challenge metrics but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.62).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.58).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based Autonomous Weapons Compliance (IHL Distinction/Proportionality)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '9e43f455-c1fc-4f41-83b8-1e0359cfdb40').
narrative_ontology:cs_kernel_codification('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', formalized).
narrative_ontology:cs_authority_grounding('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', extraction).
narrative_ontology:cs_reading_relation('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', ihl_distinction_proportionality__human_agency_reading, forecloses).
narrative_ontology:cs_reading_relation('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', ihl_distinction_proportionality__categorical_prohibition_reading, forecloses).
narrative_ontology:cs_axiom('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', foundational, technology_neutral_compliance_criterion).
narrative_ontology:cs_axiom_status(technology_neutral_compliance_criterion, holdable).
narrative_ontology:cs_axiom_grounding('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', technology_neutral_compliance_criterion, instrumental).
narrative_ontology:cs_axiom('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', foundational, performance_parity_satisfies_humanitarian_duty).
narrative_ontology:cs_axiom_status(performance_parity_satisfies_humanitarian_duty, holdable).
narrative_ontology:cs_axiom_grounding('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', performance_parity_satisfies_humanitarian_duty, empirically_contingent).
narrative_ontology:cs_reference_frame('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', ihl_outcome_centered_authority).
narrative_ontology:cs_drift_state('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', contemporary_autonomous_weapons_deployment_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9e43f455-c1fc-4f41-83b8-1e0359cfdb40', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_efficiency_gains).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, ihl_interpretive_authority).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_protection_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operators).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, metric_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain operational speed, consistency, and reduced cognitive load through autonomous target acquisition and engagement. Under this reading, autonomous systems become lawful when technical metrics demonstrate parity with human performance on distinction and proportionality. Military advantages include reaction times shorter than human processing, elimination of fatigue degradation, and scalable force multipliers. Exit constraint: geopolitical competition pressures adoption; unilateral restraint risks strategic disadvantage.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operators, beneficiary,
    institutional, generational, constrained, global).

% Capture market share in autonomous weapons development, licensing, and integration services. The outcomes-based reading legitimates autonomous systems as products; technical compliance certification becomes a commercial pathway. Revenue flows from military procurement budgets and international licensing. Firms can exit by choosing non-autonomous product lines, but competitive pressure and military demand incentivize autonomous development.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Custodians (International Committee of the Red Cross, UN humanitarian bodies, national military legal advisors) lose interpretive authority over the meaning of distinction and proportionality. Under the outcomes-based reading, compliance becomes a technical certification question (do metrics pass?) rather than a legal judgment question (does the constraint fit the law's intent?). The reading subordinates law to engineering, which erodes the custodians' gatekeeping role. Exit is constrained by treaty obligations and institutional mandates; they cannot simply withdraw from IHL interpretation.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, humanitarian_law_custodians, payer,
    institutional, generational, constrained, global).

% Bear the compliance risk if technical metrics fail in practice or if the metric choice itself embeds blind spots. If a system achieves 95% distinction accuracy (the claimed benchmark) but the 5% failure rate concentrates on children or medical personnel, the metrics pass while law fails. Civilians cannot exit zones of autonomous operation and cannot participate in the metric choice that governs whether systems targeting near them are lawful.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations, payer,
    powerless, immediate, trapped, global).

% Set the compliance threshold by declaring which metrics constitute 'distinction/proportionality performance equal to or exceeding human operators' and certify systems against those metrics. States gain tactical freedom to deploy autonomous systems where technical compliance is demonstrated. Exit constraint: geopolitical competition and military doctrine lock states into autonomous weapons adoption; unilateral restraint is treated as strategic vulnerability.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_adopting_outcomes_reading, agenda_setter,
    institutional, generational, constrained, global).

% Are structurally barred from the outcomes-based framework because they maintain that IHL requires irreducible human moral judgment. Under the outcomes-based reading their position is treated as non-compliance or as a competing interpretation that the reading supersedes through technical performance claims. They cannot advocate for human-agency requirements without being labeled as rejecting the legal framework itself.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_maintaining_human_agency_reading, excluded,
    institutional, generational, trapped, global).

% Researchers, standards bodies, and technical committees that design the distinction/proportionality metrics themselves. They benefit from research funding, professional prestige, and consulting contracts as metrics become authoritative. They set the technical parameters that operationalize 'performance equal to or exceeding human operators.' Exit is available but incentivized against by funding structures and institutional capture.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, metric_developers, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, metric_developers, beneficiary).

% Human rights organizations, humanitarian NGOs, and academic analysts observe and challenge the metric choices, benchmark claims, and implementation gaps. They document failures and advocate for categorical prohibition or human-agency requirements. Their observational role is constrained by lack of enforcement power; military adoption pressures move faster than advocacy can counter.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civil_society_observers, observer,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a technical pathway for autonomous weapons systems to be lawful under International Humanitarian Law if they demonstrate measurable distinction and proportionality performance. Solves the coordination problem of how to interpret abstract humanitarian law principles in the presence of new military technology, by translating obligation categories into technical metrics and compliance thresholds.
% TRANSFER_FUNCTION: Transfers interpretive authority from humanitarian law custodians to military technologists and contractor-led standards bodies. Also transfers risk: under a human-agency reading, the military bears legal responsibility if systems fail; under outcomes-based reading, responsibility migrates to the metric designers if the metrics were wrong. Gains move to military efficiency and defense sectors; costs move to interpretive authority and civilian protection regimes.
% ABSENT_VOICES: States and humanitarian bodies that maintain categorical prohibition readings are excluded from the framework as implemented — their objections are treated as non-technical resistance rather than legitimate legal interpretation. Civilians affected by autonomous targeting have no seat in the metric design or certification process. Private contractors in smaller states with less AI capacity are structurally excluded from metric-setting bodies dominated by advanced military powers.
% DISAPPEARANCE_RATIONALE: If the outcomes-based reading disappeared and human-agency or categorical prohibition readings took hold, military doctrine would shift away from autonomous target engagement, defense contractor product lines would be reoriented toward human-operator support tools, and the interpretive authority of humanitarian law custodians would be restored. Geopolitical competition structures would reorganize around shared restraint or mutual prohibition rather than around technical performance parity.
% FOUNDING_PROBLEM: IHL's distinction and proportionality obligations were written for human decision-makers; autonomous systems present a novel question: can machines satisfy legal obligations that were premised on human moral agency? The outcomes-based reading solves this by decoupling the legal obligation from agency type and reframing it as a performance question.
% FOUNDING_PROBLEM_CORROBORATION: Military strategists and defense contractors attest the problem is live and that outcomes-based resolution is necessary for operational effectiveness. Humanitarian law custodians (ICRC, UN humanitarian experts) attest the founding problem is a category mistake — IHL principles of humanity and Martens Clause precepts are irreducibly about human moral agency and cannot be satisfied by performance metrics alone. Technical researchers working on autonomous systems report significant uncertainties in metric validity and generalization; some corroborate the custodians' position that metric parity is unachievable, others that performance equivalence is technically possible but interpretation remains contested.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62 at interval end, rising from 0.38) because the reading creates asymmetric benefits: military/contractor gains are concentrated and direct (operational freedom, market share), while costs are diffuse (erosion of custodian authority, civilian risk displacement). The rising trajectory reflects increasing adoption pressure and institutional lock-in as autonomous systems are deployed and metric-validation practices become entrenched. Suppression is high (0.58 at interval end) because the reading's persistence depends on actively suppressing alternative interpretations: humanitarian law expertise must be reframed as 'non-technical' or 'obstructionist,' categorical-prohibition readings must be treated as outside the legal framework rather than as competing legitimate readings, and human-agency objections must be absorbed into technical benchmarking rather than addressed as principled legal claims. Theater ratio is substantial (0.48) because metric validation activities that appear to be objective technical compliance are actually performing the role of authority displacement — the metrics are not discovered from law but constructed to make a preferred reading operationally dominant. The metrics embody choices (sensitivity vs. specificity trade-offs, benchmark selection, error-rate acceptability) that are normative but are dressed as technical. The time series run on one shared grid; projections from t=25 onward reflect competition-driven acceleration of autonomous weapons adoption coupled with plateauing suppression (the initial suppression of alternative interpretations reaches ceiling as institutional capture stabilizes).
 *
 * PERSPECTIVAL GAP:
 *   Military operators and defense contractors (beneficiary seats) perceive the outcomes-based reading as legitimating, enabling, and rational: 'if we can build systems that perform as well as humans, the law should permit them.' Humanitarian law custodians and civilian populations (payer seats) perceive it as a category error that evacuates the law of its normative content: 'performance metrics cannot answer questions about moral agency and human dignity.' The engine will compute these as divergent perceived types from the same structural data — beneficiaries will see coordination and rope-like stability, payers will see extraction and tangled-rope or snare-like characteristics. This divergence is the signal the corpus is built to detect; the authored metrics and claimed type support both computations by honestly reporting the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (military operators, defense contractors) derive d near 0.15–0.25 because they gain operational freedom and market share while bearing minimal enforcement cost; the reading removes constraint on their action. Payers (humanitarian custodians, civilian populations) derive d near 0.75–0.90 because they lose authority/participation and bear risk while gaining nothing; the reading concentrates control away from them. Humanitarian custodians are institutional payers with constrained exit (treaty obligations prevent withdrawal); civilians are powerless and trapped (no exit options, no participation). Military operators are institutional but have arbitrage options (can choose non-autonomous doctrine alternatives, though geopolitical pressure constrains this). The asymmetry is structural and stable across the interval.
 *
 * MANDATROPHY ANALYSIS:
 *   The outcomes-based reading carries a live mandatrophy signal: its founding mandate (operationalizing IHL obligations for novel technology) is substantially live, but the reading's persistence increasingly depends on suppressing the authority structures that originally authored that mandate (humanitarian law custodians). The reading claims to solve a coordination problem ('how do we apply IHL to autonomous systems?') but in doing so it transfers the problem-solving authority from law to engineering. This is not a case where the original mandate has become inapplicable; rather, the constraint's operation is increasingly protected by suppressing the parties whose interpretive role the original mandate established. Classically this is a mandatrophy signal: the constraint persists partly because questioning it has been made costly (suppression rising, civilian participation suppressed, custodian authority eroded). The R5 mismatch (founding_problem_status = 'contested' + disappearance_verdict = 'world_rearranges') is also classically mandatrophy: the founding problem is disputed but the constraint's removal would force reorganization, indicating the constraint's persistence is defended even where its justification is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_equivalence_measurement_ambiguity,
    'What metrics operationalize ''distinction/proportionality performance equal to or exceeding human operators''? Do different metric choices lead to different compliance verdicts for the same system?',
    'Comparative technical analysis: run the same autonomous system through multiple proposed metrics (e.g., confusion-matrix recall vs. precision vs. F1 score vs. scenario-based Bayesian evaluation). If divergent verdicts result, the metric choice embeds normative judgment disguised as technical objectivity.',
    'If metrics are underdetermined, the outcomes-based reading transfers authority not to an objective standard but to whoever chooses which metric counts. This converts moderate extractiveness into high extractiveness (authority capture via metric selection). If metrics are robust, extractiveness remains moderate and the reading''s legitimacy increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performance_equivalence_measurement_ambiguity, empirical, 'Whether compliance determination is metric-robust or metric-contingent').

omega_variable(
    metric_generalization_domain_shift,
    'Do autonomous systems that achieve distinction/proportionality benchmarks in training/test scenarios maintain performance in novel conflict zones with different civilian/combatant ratios, architecture, or engagement patterns?',
    'Field deployment data and post-deployment analysis: track system performance as it encounters scenarios outside its training distribution. If performance degrades in novel domains, the outcomes-based reading''s compliance claim is undermined (parity was achieved in constrained test conditions, not in real operation).',
    'If domain shift is severe, the reading collapses back toward human-agency or categorical-prohibition positions (systems cannot be trusted to satisfy IHL in operational variability). If domain generalization is robust, the outcomes-based reading gains empirical support. This is the difference between a modular technical compliance regime (outcomes-based) and a context-dependent judgment regime (human-agency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_generalization_domain_shift, empirical, 'Whether performance equivalence is achievable across operational domains or only in constrained benchmarks').

omega_variable(
    interpretive_authority_displacement_intent,
    'Is the shift from humanitarian law custodian judgment to technical metric validation an unintended consequence of the outcomes-based reading, or is authority displacement part of the reading''s appeal to its advocates?',
    'Analysis of how the outcomes-based reading was adopted and promoted: do military strategists and defense contractors emphasize the ''objective technical compliance'' framing precisely to remove discretion from humanitarian law experts? Are there statements or strategic documents indicating intentional authority transfer?',
    'If authority displacement is intentional, the reading is straightforwardly extractive (a coordinated effort to transfer authority from a constraining source). If it is unintended, the reading may retain legitimacy as an inadvertent side effect of technical operationalization. Either way, the reading''s persistence becomes partly dependent on suppressing the authority displacement question itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_displacement_intent, conceptual, 'Whether interpretive authority displacement is a designed feature or an unintended consequence').

omega_variable(
    metric_bias_and_protected_category_errors,
    'Do the distinction/proportionality metrics embed systematic bias against protected categories (children, medical personnel, women in combatant-ambiguous attire)? Do error rates concentrate on minority populations or specific demographic groups?',
    'Fairness audits of autonomous systems: disaggregate performance metrics by protected category and population demographics. If error concentration is systematic, the ''performance equivalence'' claim is false — the system matches or exceeds human performance on average but fails on vulnerable subgroups.',
    'If protected-category bias is substantial, civilians in those categories bear outsized risk and the payer population is not homogeneous — some civilians bear much higher extraction costs. This would refine the civilian_populations seat into multiple sites with different d values and exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_bias_and_protected_category_errors, empirical, 'Whether performance equivalence masks systematic bias against protected categories').

omega_variable(
    kernel_reading_coexistence_vs_foreclosure,
    'Can the outcomes-based, human-agency, and categorical-prohibition readings coexist in a single IHL legal framework, or does acceptance of one reading logically foreclose the others?',
    'Legal and philosophical analysis: do the readings represent competing but mutually-tolerant interpretations that different states or military doctrines can adopt separately? Or does adopting one reading require rejecting the epistemic foundations of the others?',
    'If readings coexist, the outcomes-based reading is one legitimate interpretation among others (coexists_with relation). If adoption of outcomes-based forecloses human-agency reading (by treating human-agency concerns as non-technical obstruction), the relation is forecloses. The engine computes this from cs_structure.reading_relations; this omega documents the ground for the declared relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence_vs_foreclosure, conceptual, 'Whether the three kernel readings are logically compatible or mutually exclusive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(ihl__tr_t25, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement(ihl__tr_t35, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ihl__be_t25, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(ihl__be_t35, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(ihl__su_t25, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(ihl__su_t35, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.18).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'IHL distinction and proportionality requirements for autonomous weapons systems.' The kernel has three structural interpretations: outcomes-based (this story), human-agency, and categorical-prohibition. Each reading instantiates a different constraint with a different ε and beneficiary/victim structure. The outcomes-based reading permits autonomous systems where technical metrics demonstrate compliance parity; this generates moderate extractiveness (military/contractor gain, custodian authority loss) and requires active suppression of alternative readings to persist. The human-agency reading treats compliance as requiring irreducible human moral judgment and generates higher extractiveness (categorical constraint on military freedom). The categorical-prohibition reading generates the highest extractiveness for military operations (absolute prohibition) but zero extractiveness for humanitarian law custodians (their authority is fully respected). The three stories are linked via this affects_constraints edge; the comparison across readings reveals how reading choice drives structural classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
