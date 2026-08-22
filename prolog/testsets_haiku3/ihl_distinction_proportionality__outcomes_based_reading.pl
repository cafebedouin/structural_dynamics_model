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
 *   human_readable: Outcomes-Based IHL Compliance for Autonomous Weapons Systems
 *   domain: international_humanitarian_law / military_ethics / technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the outcomes-based reading of IHL
 *   distinction and proportionality obligations: autonomous weapons systems
 *   are lawful if their measured performance in distinguishing civilians and
 *   assessing proportionality equals or exceeds human operator performance.
 *   The reading is technology-neutral — law governs the results, not the
 *   means — and compliance is indexed to technical metrics rather than
 *   categorical prohibitions or irreducible human-agency requirements. The
 *   constraint creates a structural asymmetry: military efficiency and
 *   contractor profit flow from adopting the systems; humanitarian law
 *   custodians and civilian populations bear the cost if metrics fail or omit
 *   relevant distinctions. This is a tangled rope: genuine coordination
 *   function (standardizing compliance via measurable performance) paired
 *   with asymmetric extraction (authority transfer to metrics-setters,
 *   responsibility attribution collapse, civilian protection dependency on
 *   metric validity).
 *
 * KEY AGENTS:
 *   - military_operations_command: agenda-setter, institutional power, arbitrage-grade exit — adopts systems when metrics permit
 *   - defense_contractors: beneficiary, powerful, arbitrage exit — profit from expanded autonomous deployment
 *   - international_humanitarian_law_custodians: payer, institutional power, constrained exit — lose interpretive authority; cannot revise compliance standard
 *   - civilian_populations: payer, powerless, trapped exit — bear cost if metrics fail
 *   - technology_ethics_researchers: observer seat — measure whether promised performance holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__outcomes_based_reading, 0.58).
domain_priors:suppression_score(ihl_distinction_proportionality__outcomes_based_reading, 0.67).
domain_priors:theater_ratio(ihl_distinction_proportionality__outcomes_based_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__outcomes_based_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__outcomes_based_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__outcomes_based_reading, "Outcomes-Based IHL Compliance for Autonomous Weapons Systems").
narrative_ontology:topic_domain(ihl_distinction_proportionality__outcomes_based_reading, "international_humanitarian_law / military_ethics / technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__outcomes_based_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__outcomes_based_reading, '66233efd-eb62-44cf-a11d-ba50e3e02187').
narrative_ontology:cs_kernel_codification('66233efd-eb62-44cf-a11d-ba50e3e02187', fixed_text).
narrative_ontology:cs_authority_grounding('66233efd-eb62-44cf-a11d-ba50e3e02187', extraction).
narrative_ontology:cs_interpretation_layer_present('66233efd-eb62-44cf-a11d-ba50e3e02187').
narrative_ontology:cs_reading_relation('66233efd-eb62-44cf-a11d-ba50e3e02187', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('66233efd-eb62-44cf-a11d-ba50e3e02187', ihl_distinction_proportionality__human_agency_reading, coexists_with).
narrative_ontology:cs_axiom('66233efd-eb62-44cf-a11d-ba50e3e02187', foundational, technical_performance_equivalence_establishes_legal_permissibility).
narrative_ontology:cs_axiom_status(technical_performance_equivalence_establishes_legal_permissibility, holdable).
narrative_ontology:cs_axiom_grounding('66233efd-eb62-44cf-a11d-ba50e3e02187', technical_performance_equivalence_establishes_legal_permissibility, instrumental).
narrative_ontology:cs_axiom('66233efd-eb62-44cf-a11d-ba50e3e02187', secondary, measurable_metrics_proxy_humanitarian_principle).
narrative_ontology:cs_axiom_status(measurable_metrics_proxy_humanitarian_principle, holdable).
narrative_ontology:cs_axiom_grounding('66233efd-eb62-44cf-a11d-ba50e3e02187', measurable_metrics_proxy_humanitarian_principle, empirically_contingent).
narrative_ontology:cs_reference_frame('66233efd-eb62-44cf-a11d-ba50e3e02187', ihl_principles_as_outcomes_governance).
narrative_ontology:cs_drift_state('66233efd-eb62-44cf-a11d-ba50e3e02187', contemporary_autonomous_weapons_deployment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('66233efd-eb62-44cf-a11d-ba50e3e02187', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_operations_command).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, international_humanitarian_law_custodians).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, military_field_operators).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__outcomes_based_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, military_field_operators).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__outcomes_based_reading, non_state_armed_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts autonomous weapons systems when their measured distinction and proportionality performance meets or exceeds human operators. Sets procurement criteria around technical compliance metrics. Justifies the transition as reducing operator fatigue errors and enabling faster threat response in complex environments. Operates under the outcomes-based reading: if the machine performs better, the law permits it.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_operations_command, agenda_setter,
    institutional, generational, arbitrage, global).

% Develop and sell autonomous weapons systems. Benefit from a permissive regulatory framework that accepts technical performance metrics as the compliance standard. Market expansion depends on military adoption; the outcomes-based reading creates a straightforward path to certification without categorical prohibition or irreducible human-agency requirements.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, defense_contractors, beneficiary,
    powerful, generational, arbitrage, global).

% Operate autonomous systems in theaters where distinction and proportionality failures carry immediate consequences (civilian casualties, war crimes liability). They bear personal accountability risk if the autonomous system's performance diverges from the promised metrics or if metrics fail to capture battlefield complexity. They also potentially benefit from reduced tactical burden and faster response.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, military_field_operators, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, military_field_operators, beneficiary).

% Include the International Committee of the Red Cross, humanitarian treaty bodies, and international courts. They have custodianship authority over IHL interpretation and enforcement. The outcomes-based reading subordinates their interpretive role to technical metrics: if the machine meets the performance threshold, their capacity to object on Martens Clause grounds or human-dignity-per-se principles is structurally preempted. They pay in interpretive authority and cannot revise the compliance standard without challenging the fundamental premise.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, international_humanitarian_law_custodians, payer,
    institutional, generational, constrained, global).

% Subject to autonomous weapons systems deployed by belligerents. They bear the cost if metrics fail in practice, if unmeasured distinctions are missed, or if proportionality calculations omit their civilian status. They have no participation in the metrics-setting process and cannot exit the conflict zone. Their protection depends on whether the measured performance actually translates to field behavior.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, global).

% May adopt autonomous systems if they can access them. Benefit from lower operational overhead; pay through increased international scrutiny and the risk that their use violates emerging customary law norms. They operate under the same outcomes-based reading if they deploy the systems, though their ability to certify performance is limited.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, non_state_armed_groups, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__outcomes_based_reading, non_state_armed_groups, payer).

% Advocate for categorical prohibition or irreducible human-agency requirements. They are excluded from the outcomes-based reading's operative framing: the constraint structurally marginalizes their objections by treating compliance as a technical matter. They would argue that Martens Clause principles demand categorical prohibition, but the outcomes-based reading reframes IHL as performance-governance rather than principled humanitarian boundary-setting.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, states_opposing_autonomous_weapons, excluded,
    institutional, generational, constrained, global).

% Study autonomous weapons performance, metric validity, and field-deployment outcomes. They hold an observational seat: whether the promised technical performance is actually achieved; whether metrics capture the moral distinctions IHL seeks; whether the reading's core premise (performance equivalence = legal equivalence) holds under real conditions.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__outcomes_based_reading, technology_ethics_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__outcomes_based_reading, military_operations_command).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__outcomes_based_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes distinction and proportionality compliance via measurable technical performance metrics, enabling automated system deployment while maintaining humanitarian law obligations through objective thresholds rather than case-by-case human judgment.
% TRANSFER_FUNCTION: Moves interpretive authority from humanitarian law custodians and irreducible human judgment to technical performance standards set by military procurement and contractor validation. Transfers operational burden from human operators to autonomous systems, and shifts responsibility attribution from individual operators to systems designers and military command.
% ABSENT_VOICES: States and movements advocating categorical prohibition or irreducible human-agency requirements are structurally excluded from the operative framing. Their objections on Martens Clause grounds (humanity, public conscience) or on principal-agent grounds are categorized as resistance to legitimate technical governance rather than as legitimate objections to the reading's core premise.
% DISAPPEARANCE_RATIONALE: If this outcomes-based compliance framework vanished, military procurement would revert to categorical prohibition, irreducible human-agency requirements, or case-by-case humanitarian review. Autonomous weapons deployment would halt or face renewed international negotiation. The constraint structures the adoption trajectory; removing it would reshape the landscape of lethal-force governance.
% FOUNDING_PROBLEM: Human operators in high-complexity, fast-moving combat environments make distinguishing-civilian-from-combatant and proportionality errors at measurable rates. Fatigue, cognitive overload, and incomplete information lead to civilian casualties and violations of IHL. Autonomous systems with superior sensor fusion and millisecond reaction time could reduce these error rates.
% FOUNDING_PROBLEM_CORROBORATION: Military operations command and defense contractors attest the founding problem is live and autonomous systems are the solution. Technology ethics researchers and field studies document human operator error rates under stress. International humanitarian law custodians attest the founding problem exists but contest whether autonomous systems actually solve it without introducing new failure modes (adversarial sensor spoofing, metric gaming, accountability collapse). No consensus external corroboration exists; the problem's status rides on which reading's framing is adopted.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__outcomes_based_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__outcomes_based_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__outcomes_based_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__outcomes_based_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__outcomes_based_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58 terminal) because the outcomes-based reading transfers interpretive authority from humanitarian custodians to technical metrics, which are set by military procurement and contractor validation — a concentration of authority over the compliance standard itself. Suppression is higher (0.67) because the constraint actively excludes categorical-prohibition and human-agency readings through the framing that compliance is technical rather than principled; alternatives are classified as resistance to legitimate governance rather than as legitimate objections. Theater ratio rises from 0.31 to 0.42 because enforcement increasingly defends the metrics-based framing against objections, not the underlying humanitarian principles. Resistance is high (0.72) because the constraint meets sustained objection from humanitarian custodians, states favoring prohibition, and ethics researchers questioning metric validity. The measurement series are shared across all three metrics on a single time grid (0, 5, 10, 15, 20, 25), with observed basis for t=0 and t=5 (early deployment phase) and projected basis for later points (modeling further adoption and contestation).
 *
 * PERSPECTIVAL GAP:
 *   From the military operations and contractor seat, the constraint is legitimate technical governance: compliance is objective, performance-based, and removes categorical dogma from an engineering problem. From the humanitarian custodians' seat, the same structure is authority capture: technical metrics become a proxy for humanitarian principle, compliance is outsourced to designers and procurement, and Martens Clause objections are categorized as opposition to legitimate governance. The engine computes this divergence from power, exit options, and beneficiary/victim structure; the authored claim (tangled_rope) does not adjudicate which perspective is correct. Seat divergence here is structural and deep.
 *
 * DIRECTIONALITY LOGIC:
 *   Military operations and contractors sit at the beneficiary end (d near 0.0–0.2): they collect expanded operational latitude and profit. Humanitarian custodians sit at the target end (d near 0.8–1.0): they lose interpretive authority and cannot exit without abandoning their institutional role. Civilian populations sit near full target (d near 1.0): powerless, trapped, dependent on metric validity they do not set. Field operators sit intermediate (d near 0.5–0.6): they benefit from reduced burden but carry personal accountability risk if metrics fail. States opposing autonomy sit excluded but adjacent to full target (d near 0.75): constrained exit, institutional power, but structurally marginalized by the outcomes-based framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (human operator error in distinction and proportionality) is live and real. The founding solution (outcomes-based metrics enabling autonomous deployment) appears to solve it. But the constraint itself creates a secondary problem: metrics become targets for gaming, civilian-protection responsibility migrates from operators to systems, and humanitarian custodians lose their authority to object on principle. A mandatrophy reading would note that the original coordination function (ensuring distinction and proportionality) persists as stated, but the extraction function (authority concentration and responsibility collapse) has grown to dominate enforcement. Theater ratio rise (0.31→0.42) signals that more enforcement energy goes to defending the metrics-based reading against objections than to validating the metrics' humanitarian legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_validity_capture_gap,
    'Do the technical metrics (distinction accuracy rate, proportionality assessment error rate) actually capture IHL''s humanitarian principles, or do they create proxy targets that can be gamed while humanitarian principles remain violated?',
    'Field deployment data: compare metric compliance to humanitarian casualty outcomes, adversarial metric-spoofing attempts, post-deployment reviews from humanitarian organizations and casualty monitoring.',
    'If metrics and humanitarian outcomes diverge significantly, the reading''s legitimacy collapses and the constraint operates as pure extraction (authority capture without humanitarian delivery). If they align closely, the reading''s technical governance framing is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_validity_capture_gap, empirical, 'Whether technical metrics proxy humanitarian principles or enable gaming.').

omega_variable(
    responsibility_attribution_under_autonomy,
    'When an autonomous system violates distinction or proportionality in the field, who bears legal and moral responsibility — the operator, the commander, the system designer, or no one? Does the outcomes-based reading resolve or obscure this question?',
    'International criminal court cases involving autonomous system deployment; state practice in domestic accountability for autonomous weapon incidents; treaty evolution clarifying operator vs. designer vs. command liability.',
    'If responsibility becomes diffuse or attributionally incoherent, the constraint enables accountability escape for humanitarian violations. If responsibility is clearly allocated, civilian protection may be maintained. The reading''s legitimacy depends on whether it clarifies or obscures accountability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsibility_attribution_under_autonomy, conceptual, 'Responsibility allocation under autonomous IHL compliance.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the outcomes-based reading genuinely coexist with categorical-prohibition and human-agency readings, or does technical adoption create irreversible path dependency that forecloses the alternatives?',
    'International humanitarian law treaty evolution; state practice and customary law drift; willingness of military powers to revert to categorical prohibition if autonomous deployment proves uncontrollable or generates humanitarian disasters.',
    'If adoption creates lock-in, the reading''s acceptance amounts to foreclosure of alternatives without explicit principled choice. If reversion remains possible, the readings remain genuinely coexistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether outcomes-based adoption forecloses or coexists with categorical prohibition.').

omega_variable(
    humanitarian_custodian_authority_legitimacy,
    'What is the normative basis for humanitarian custodian authority over IHL compliance, and does outcomes-based technical governance legitimate or undermine that authority?',
    'Foundational IHL texts (Geneva Conventions, Additional Protocols, Martens Clause); international court rulings on humanitarian custodian standing; state acceptance or rejection of custodian objections to autonomous deployment.',
    'If custodian authority is foundational to IHL legitimacy, outcomes-based governance that marginalizes custodian objections undermines IHL itself. If custodian authority is advisory only, technical governance may be compatible with humanitarian principles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_custodian_authority_legitimacy, conceptual, 'Legitimacy and status of humanitarian custodian authority under technical governance.').

omega_variable(
    civilian_population_protection_dependency,
    'Are civilian populations adequately protected by the outcomes-based reading if metric validation is incomplete, if adversarial actors manipulate systems, or if metrics omit context-dependent distinctions (e.g., cultural or political factors affecting civilian status identification)?',
    'Comparative casualty rates in conflicts using outcomes-based autonomous systems vs. human-only operations; adversarial attacks on autonomous targeting systems; field reviews documenting missed distinctions.',
    'If protection degrades, the reading''s legitimacy as humanitarian governance collapses. If protection improves or stabilizes, the reading''s technical framing is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_population_protection_dependency, empirical, 'Adequacy of civilian protection under outcomes-based compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__outcomes_based_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 0, 0.31).
narrative_ontology:measurement(ihl__tr_t5, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 10, 0.39).
narrative_ontology:measurement(ihl__tr_t15, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(ihl__tr_t20, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(ihl__tr_t25, ihl_distinction_proportionality__outcomes_based_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ihl__be_t5, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(ihl__be_t15, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(ihl__be_t20, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(ihl__be_t25, ihl_distinction_proportionality__outcomes_based_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(ihl__su_t5, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(ihl__su_t15, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(ihl__su_t20, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(ihl__su_t25, ihl_distinction_proportionality__outcomes_based_reading, suppression_requirement, 25, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__outcomes_based_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ihl_distinction_proportionality__outcomes_based_reading, 0.12).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__categorical_prohibition_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__outcomes_based_reading, ihl_distinction_proportionality__human_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel: ihl_distinction_proportionality. Three separate constraint stories instantiate the three readings: outcomes_based (this file), categorical_prohibition, and human_agency. Each reading authors its own ε, beneficiary/victim structure, and type, independent of the others. They coexist as live positions held by different parties in ongoing contestation over IHL's proper interpretation. The network links document that these are not three independent constraints but three readings of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ihl_distinction_proportionality__outcomes_based_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
