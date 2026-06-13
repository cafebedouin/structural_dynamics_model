% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Acceptable Risk Framework
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   Energy policy risk frameworks embed a choice about which tail events
 *   matter: catastrophic-tail-dominant prioritizes avoiding low-probability,
 *   high-consequence events (nuclear accidents) even if this increases
 *   aggregate expected harm from more-frequent, lower-consequence events
 *   (fossil fuel mortality). This constraint captures the policy arrangement
 *   that operationalizes this reading of acceptable risk in energy decisions.
 *   The constraint is READING ONE of a three-way kernel contest
 *   (catastrophic_tail_dominant vs. expected_value_dominant vs.
 *   option_value_preserving). Each reading produces a structurally different
 *   constraint with different beneficiary/victim sets and different
 *   suppression mechanisms. This story instantiates ONLY the
 *   catastrophic-tail-dominant reading: nuclear accidents enter the victim
 *   set with infinite weight; fossil fuel deaths are discounted as
 *   distributed/reversible; the policy enforcement suppresses nuclear pathway
 *   development.
 *
 * KEY AGENTS:
 *   - fossil_fuel_producers: Institutional beneficiary — fossil pathways remain policy-acceptable under aggregate-harm-discounting logic
 *   - nuclear_pathway_developers: Organized payer — face suppression of expansion via infinite-tail-cost regulatory burden
 *   - risk_averse_policymakers: Institutional agenda-setter — enforce the framework by restricting nuclear approvals, allowing fossil infrastructure
 *   - distributed_harm_communities: Powerless payer — coal mining and air-pollution populations bear actual mortality cost, discounted by framework
 *   - future_generations: Analytical payer (non-agent) — lose option-value of nuclear infrastructure from current suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.79).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Acceptable Risk Framework").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, '73ce4783-34f1-406c-8517-d52e21d53825').
narrative_ontology:cs_kernel_codification('73ce4783-34f1-406c-8517-d52e21d53825', distributed).
narrative_ontology:cs_authority_grounding('73ce4783-34f1-406c-8517-d52e21d53825', extraction).
narrative_ontology:cs_interpretation_layer_present('73ce4783-34f1-406c-8517-d52e21d53825').
narrative_ontology:cs_reading_relation('73ce4783-34f1-406c-8517-d52e21d53825', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('73ce4783-34f1-406c-8517-d52e21d53825', acceptable_risk_energy__option_value_preserving, influences).
narrative_ontology:cs_axiom('73ce4783-34f1-406c-8517-d52e21d53825', foundational, catastrophic_outcomes_lexicographically_prior).
narrative_ontology:cs_axiom_status(catastrophic_outcomes_lexicographically_prior, holdable).
narrative_ontology:cs_axiom_grounding('73ce4783-34f1-406c-8517-d52e21d53825', catastrophic_outcomes_lexicographically_prior, deontological).
narrative_ontology:cs_axiom('73ce4783-34f1-406c-8517-d52e21d53825', secondary, distributed_harm_asymmetric_discounting).
narrative_ontology:cs_axiom_status(distributed_harm_asymmetric_discounting, holdable).
narrative_ontology:cs_axiom_grounding('73ce4783-34f1-406c-8517-d52e21d53825', distributed_harm_asymmetric_discounting, instrumental).
narrative_ontology:cs_reference_frame('73ce4783-34f1-406c-8517-d52e21d53825', nuclear_safety_imperative_post_fukushima).
narrative_ontology:cs_drift_state('73ce4783-34f1-406c-8517-d52e21d53825', contemporary_climate_acceleration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('73ce4783-34f1-406c-8517-d52e21d53825', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_policymakers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_tail_risk).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_pathway_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, distributed_harm_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_incumbents).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, tail_risk_asymmetry_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, catastrophic_outcome_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear energy producers operate under the catastrophic-tail framework, which assigns infinite or near-infinite disutility to low-probability accidents (Chernobyl, Fukushima scale events). This suppresses nuclear pathway development and expansion despite lower mortality-per-unit-energy than fossil fuels. They must absorb insurance costs, regulatory scrutiny intensification, and project cancellations that track catastrophic risk weightings rather than aggregate expected mortality.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_industry_incumbents, payer,
    institutional, generational, constrained, global).

% Benefit from catastrophic-tail prioritization because fossil fuel deaths (coal mining accidents, respiratory disease, pollution) are distributed across millions, diffuse in causation, and manifest over decades. The framework discounts these distributed harms as reversible or individually small, assigning them lower aggregate disutility than a single catastrophic event. This allows fossil fuel expansion to persist despite higher total mortality.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_producers, beneficiary,
    institutional, generational, arbitrage, global).

% Adopt and enforce the catastrophic-tail framework because it aligns with voter risk aversion and media attention patterns: individual catastrophes are politically salient and create public backlash, while distributed deaths are statistically abstract. Policymakers enforce this by restricting nuclear approvals, imposing stringent catastrophic-outcome litigation avoidance, and allowing fossil infrastructure to expand under weaker risk-aggregation scrutiny.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, risk_averse_policymakers, agenda_setter,
    institutional, biographical, mobile, national).

% Bear the enforcement cost of catastrophic-tail suppression: innovation funding is redirected, construction permits require infinite-damage insurance, and public opposition is legitimized by the framework's axiom that catastrophic risk dominates aggregate calculation. They cannot exit the constraint without abandoning nuclear development altogether.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_pathway_developers, payer,
    organized, biographical, constrained, global).

% Coal mining regions, air-pollution-exposed populations, and future-disease-burden communities bear the actual mortality cost of fossil fuel dominance. But because these harms are distributed (no single event triggers political crisis), the catastrophic-tail framework discounts their aggregate into invisibility. They have no seat at risk-framework deliberation and no exit from exposure.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, distributed_harm_communities, payer,
    powerless, biographical, trapped, local).

% Bear the option-value cost of suppressing nuclear pathways: if climate change accelerates or fossil alternatives fail, future decision-makers inherit a depleted nuclear infrastructure and fewer energy options. The framework's suppression of nuclear development narrows their choice set, which is itself a form of harm — they lose optionality that current risk-averse decisions foreclosed.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_tail_risk, payer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_energy__catastrophic_tail_dominant, future_generations_tail_risk).

% Risk-neutral economists and decision theorists who argue aggregate expected mortality should dominate risk frameworks are structurally excluded from policy deliberation. Their positions are labeled as indifferent to catastrophe, and the catastrophic-tail framework's enforcement machinery (media framing, political vocabulary, regulatory doctrine) suppress their voice in risk-acceptability deliberations.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, expected_value_adherents, excluded,
    moderate, biographical, constrained, global).

% Research institutions studying risk frameworks, decision theory, and energy policy document the choice architecture embedded in catastrophic-tail prioritization. They measure the mortality costs of suppressing low-death-rate energy pathways and the distributional consequences of the framework's emphasis on tail events over aggregate harm.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, decision_theory_academies, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_producers).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective risk tolerance across energy pathways by establishing a shared principle: catastrophic outcomes receive infinite or near-infinite disutility weighting, preventing any aggregate-harm calculation from justifying them. This reduces disagreement about acceptable energy risk by creating a lexicographic priority (avoid catastrophe first, optimize aggregate harm second).
% TRANSFER_FUNCTION: Moves political legitimacy and investment capital from nuclear and renewable pathways (which face infinite-tail-cost scrutiny) to fossil fuel pathways (whose distributed mortality is discounted as reversible and statistically abstract). The framework transfers regulatory compliance costs from fossil infrastructure to nuclear development, and transfers future decision flexibility from future generations to current policymakers.
% ABSENT_VOICES: Expected-value decision theorists, distributed-harm communities (coal regions, air-pollution populations), and future generations are structurally absent from risk-framework deliberation. Expected-value voices are labeled reckless; distributed-harm communities lack the political salience of catastrophic events; future generations have no present agency. The framework's enforcement suppresses these positions from the policy table.
% DISAPPEARANCE_RATIONALE: If catastrophic-tail dominance as a policy framework disappeared, energy-pathway prioritization would shift: nuclear and renewables would compete on aggregate expected mortality and lifecycle costs rather than catastrophic-event probability; fossil fuel expansion would face stronger aggregate-harm scrutiny; distributed-harm communities would gain policy voice relative to catastrophic-event aversion. Investment capital and regulatory approval would redistribute.
% FOUNDING_PROBLEM: Early nuclear deployment in the 1950s-1970s lacked adequate accident prevention infrastructure and public understanding of tail risks. Chernobyl (1986) and Fukushima (2011) demonstrated that rare catastrophic accidents could cause multi-generational harm and zone-scale economic collapse. The framework was built to prevent repeating these large-scale coordination failures by elevating catastrophic-outcome prevention above other risk tradeoffs.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear safety advocates attest the founding problem (preventing another Fukushima-scale event) remains live and justifies the current framework. Expected-value risk researchers and energy economists attest the problem has been substantially mitigated (modern reactor designs have passive safety, distributed grid redundancy reduces grid-failure catastrophe) and the framework now suppresses lower-mortality pathways. Climate scientists and energy analysts outside the nuclear/fossil advocacy set attest the framework creates a binding constraint that prioritizes a specific type of risk over aggregate harm, with policy consequences not originally intended.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) and rising: the framework systematically redirects investment and regulatory approval from low-catastrophe-risk pathways (nuclear, renewables) to high-distributed-harm pathways (fossil). The rise from 0.45 to ~0.67 over 20 years reflects increasing fossil lock-in and accelerating nuclear suppression as the framework's logic compounds. Theater ratio climbs from 0.28 to 0.42: as modern reactors become safer, the actual catastrophic-risk reduction flattens, but regulatory and political activity defending the framework intensifies — more enforcement theater per unit of actual risk reduction. Suppression requirement is high (0.79 at endpoint) and rising: enforcing catastrophic-tail dominance requires suppressing expected-value voices, discounting distributed-harm salience, and preventing alternative decision frameworks from reaching policy deliberation. The framework's persistence depends on actively maintaining the lexicographic priority (catastrophe dominates aggregate), which is structurally non-obvious and requires enforcement. Accessibility collapse is 0.71: once the framework is understood (catastrophic events get infinite weight), alternatives collapse for policymakers seeking legitimacy — any other framework can be attacked as 'indifferent to catastrophe.' Resistance is 0.64: climate scientists, energy economists, and distributed-harm advocates actively push back, but their voices lack the political salience of catastrophic-event aversion.
 *
 * PERSPECTIVAL GAP:
 *   From the policymaker seat, the framework is coordination: it provides a stable, communicable principle (catastrophic outcomes dominate) that enables decisions without endless risk-tradeoff negotiation. From the fossil-fuel-beneficiary seat, the framework is invisible coordination benefiting them. From the nuclear-developer seat, the framework is pure extraction: suppression with no compensating benefit. From the distributed-harm seat, the framework is an asymmetric enforcement mechanism that discounts their actual mortality. The engine computes these divergences from the structural data; the author's claim (tangled_rope) is the reading's own framing, not a predetermined endpoint.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel producers have low directionality (~0.25, beneficiary end): the framework's logic flows to their benefit; they capture investment diverted from nuclear; they face weaker aggregate-harm scrutiny. Nuclear incumbents have high directionality (~0.75, target end): the same logic extracts from them via suppression and project cancellation. Policymakers sit near symmetric (~0.5): they balance genuine catastrophe-aversion (a real preference) against aggregate-harm tradeoffs, but the framework enforces one reading of that tradeoff. Distributed-harm communities have the highest directionality (~0.85, deep target): they bear actual fossil-fuel mortality, are discounted by the framework's logic, have no voice, and cannot exit. Future generations are analytical payees: they lose option-value from current suppression, but as non-agents they do not inhabit the per-seat classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing Fukushima-scale events) is contested as to whether it remains live. Modern reactor designs have passive safety; distributed grid infrastructure reduces grid-failure cascade risk; decades of operational learning have hardened nuclear against the specific failure modes that Fukushima exposed. Advocates for the catastrophic-tail framework attest the problem is live (another accident is possible); expected-value critics attest the problem is substantially solved and the framework now suppresses pathways that would reduce overall mortality. The framework's persistence depends on maintaining the affirmative that catastrophic risk is the binding constraint — if that claim becomes widely falsified, the enforcement machinery (regulatory restrictions, investment redirection, political suppression of alternatives) would lack mandate. A mandatrophy would manifest as: growing failure of the framework to prevent low-probability events (because suppression isn't preventing accidents, just denying development), combined with accelerating aggregate harm from fossil lock-in (because the framework's trade-off became visibly bad). We are not yet at mandatrophy, but the gap between founding problem (actual catastrophe prevention) and current function (blocking alternative pathways) is widening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_vs_aggregate_incommensurability,
    'Is the prioritization of catastrophic-outcome avoidance over aggregate expected harm a genuine structural necessity (incommensurable risk types requiring lexicographic priority), or a contingent policy choice that could be reframed as one input to expected-value calculation?',
    'Decision-theoretic analysis of whether catastrophic-risk aversion is irreducible or derivable from standard expected-utility with appropriate utility weighting. Empirical observation of whether other policy domains (medical risk, climate risk, financial regulation) adopt similar lexicographic structures or use aggregate expected harm as primary metric.',
    'If lexicographic priority is structurally necessary, the constraint''s extraction is coordination cost. If it is a choice, the extraction is asymmetric policy preference disguised as structural necessity — the reading would shift from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_vs_aggregate_incommensurability, conceptual, 'Whether catastrophic-tail dominance is irreducibly necessary or a policy choice.').

omega_variable(
    distributed_harm_discounting_mechanism,
    'Why does the framework discount distributed mortality (coal mining deaths, air pollution disease) as reversible or individually small, while treating nuclear catastrophic mortality as irreversible and system-scale? Is this discounting justified by actual reversibility, or by political salience and media attention structure?',
    'Comparison of actual harm reversibility: coal disease burden and air pollution carry multi-generational health costs and are not reversible on meaningful timescales. Chernobyl exclusion zone and Fukushima displacement are also multi-generational. If both are irreversible, the asymmetric weighting reflects political structure, not harm structure. Survey of risk-framework discussions in policy to quantify whether distributed-harm voices receive equivalent air-time to catastrophic-event voices.',
    'If discounting reflects political salience rather than harm reality, the framework''s suppression of expected-value voices is unjustified extraction. The constraint would reclassify toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributed_harm_discounting_mechanism, empirical, 'Whether distributed-harm discounting reflects actual reversibility or political attention structure.').

omega_variable(
    option_value_foreclosure_harm,
    'What is the actual harm from suppressing nuclear pathway development on future decision-making flexibility? If climate change accelerates or fossil-fuel alternatives fail more suddenly than projected, do future generations inherit a significantly depleted choice set?',
    'Modeling of energy-pathway scenarios under climate acceleration and fossil-infrastructure failure. Quantification of the infrastructure, workforce, and R&D capital required to rebuild nuclear capacity if future circumstances demand it. Comparison to the cost of maintaining nuclear pathway development today.',
    'If option-value loss is substantial (high cost to rebuild, slow timeline, irreplaceable expertise), the framework''s suppression of nuclear development is imposing a civilizational-timescale extraction on future generations. The constraint''s victim set would expand and the classification would shift toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_foreclosure_harm, empirical, 'Whether suppressing nuclear development forecloses future decision-making flexibility with real harm.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the catastrophic-tail-dominant reading logically foreclose the expected-value-dominant reading, or do both readings coexist as live positions within the broader energy-policy framework?',
    'Logical analysis of the core premises: catastrophic-tail-dominance claims catastrophic outcomes are incommensurable with aggregate harm; expected-value-dominance claims all outcomes are commensurable through expected-utility weighting. These premises are contradictory IF held in the same framework. However, in practice, different decision-makers and agencies hold different readings simultaneously. The question is whether one reading''s logical structure forces rejection of the other, or whether they coexist as competing framings.',
    'If the reading forecloses expected-value-dominance (contradiction), then the three readings are not equally live — the catastrophic-tail reading has logical superiority. If they coexist, the readings compete in practice but neither is logically mandatory. This affects how the engine treats disagreement: foreclosure => the reading has argument from necessity; coexistence => disagreement is political/preference-based, not necessity-based.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether catastrophic-tail-dominance forecloses or coexists with expected-value-dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(acce_tr_t25, projected).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(acce_tr_t30, projected).
narrative_ontology:measurement(acce_tr_t35, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(acce_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(acce_be_t25, projected).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(acce_be_t30, projected).
narrative_ontology:measurement(acce_be_t35, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(acce_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 5, 0.69).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 15, 0.76).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(acce_su_t25, projected).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(acce_su_t30, projected).
narrative_ontology:measurement(acce_su_t35, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 35, 0.79).
narrative_ontology:measurement_basis(acce_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'acceptable_risk_energy' kernel. The three readings represent competing policy frameworks for energy risk tolerance: catastrophic-tail-dominance (this story) prioritizes avoiding low-probability high-consequence events; expected-value-dominance minimizes aggregate expected mortality; option-value-preservation maintains multiple pathways to preserve future flexibility. Each reading produces a different structural constraint (different ε, different beneficiary/victim alignment, different suppression character). The three readings interact: adoption of one framework suppresses the others from policy deliberation. They are linked as a constraint family via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
