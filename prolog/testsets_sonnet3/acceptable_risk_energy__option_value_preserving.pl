% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Reading of Acceptable Energy Risk
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   This story instantiates the option-value-preserving reading of the
 *   contested 'acceptable risk in energy policy' kernel: the position that
 *   under deep uncertainty about future costs, climate outcomes, and
 *   technology trajectories, the rational risk posture is to keep multiple
 *   generation pathways viable rather than commit irreversibly to one. As
 *   authored, the arrangement genuinely solves a real decision-theoretic
 *   problem (irreversibility under uncertainty) but has been captured, in
 *   practice, by incumbent fossil and nuclear operators whose continued
 *   operation is the concrete content of 'preserved flexibility.' The victim
 *   set — renewable developers denied faster deployment, fossil-adjacent
 *   communities, nuclear-risk-bearing localities, and future generations
 *   bearing delayed decarbonization — pays the cost of a hedge whose
 *   beneficiaries are structurally identifiable incumbents, not an
 *   undifferentiated future public. This is a distinct constraint from the
 *   catastrophic_tail_dominant reading (which would foreclose nuclear and
 *   fossil expansion on tail-risk grounds) and from the
 *   expected_value_dominant reading (which would resolve toward whichever
 *   pathway minimizes mortality-per-TWh, likely accelerating nuclear and
 *   squeezing fossil harder than this reading does). Each reading has its own
 *   epsilon and its own beneficiary/victim structure; they are linked here
 *   only through the shared kernel, not merged into one story.
 *
 * KEY AGENTS:
 *   - grid_planning_authorities: agenda_setter (institutional/arbitrage) — administers the diversification mandate
 *   - incumbent_fossil_generators: beneficiary (powerful/arbitrage) — retained operating life
 *   - incumbent_nuclear_operators: beneficiary (powerful/constrained) — retained license and subsidy
 *   - renewable_only_developers: payer (moderate/constrained) — displaced deployment speed
 *   - frontline_fossil_pollution_communities: payer (powerless/trapped) — ongoing health burden
 *   - nuclear_accident_risk_bearing_localities: payer (powerless/trapped) — tail-risk exposure
 *   - future_generations_facing_delayed_decarbonization: payer (powerless/trapped, non-agent) — committed emissions
 *   - decarbonization_advocacy_coalitions: excluded (organized/constrained) — rival reading not admitted as threshold-legitimate
 *   - climate_and_risk_economists: observer (analytical) — assesses whether hedging is genuine or captured
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.48).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Reading of Acceptable Energy Risk").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '3b01d207-4e8d-4df5-8250-0d8d618ccece').
narrative_ontology:cs_kernel_codification('3b01d207-4e8d-4df5-8250-0d8d618ccece', distributed).
narrative_ontology:cs_authority_grounding('3b01d207-4e8d-4df5-8250-0d8d618ccece', distributed).
narrative_ontology:cs_reading_relation('3b01d207-4e8d-4df5-8250-0d8d618ccece', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('3b01d207-4e8d-4df5-8250-0d8d618ccece', acceptable_risk_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('3b01d207-4e8d-4df5-8250-0d8d618ccece', foundational, irreversible_commitment_under_unresolved_uncertainty_is_the_primary_risk).
narrative_ontology:cs_axiom_status(irreversible_commitment_under_unresolved_uncertainty_is_the_primary_risk, holdable).
narrative_ontology:cs_axiom_grounding('3b01d207-4e8d-4df5-8250-0d8d618ccece', irreversible_commitment_under_unresolved_uncertainty_is_the_primary_risk, instrumental).
narrative_ontology:cs_axiom('3b01d207-4e8d-4df5-8250-0d8d618ccece', secondary, pathway_diversity_has_positive_value_independent_of_expected_outcome).
narrative_ontology:cs_axiom_status(pathway_diversity_has_positive_value_independent_of_expected_outcome, holdable).
narrative_ontology:cs_axiom_grounding('3b01d207-4e8d-4df5-8250-0d8d618ccece', pathway_diversity_has_positive_value_independent_of_expected_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('3b01d207-4e8d-4df5-8250-0d8d618ccece', portfolio_diversification_as_prudent_hedging).
narrative_ontology:cs_drift_state('3b01d207-4e8d-4df5-8250-0d8d618ccece', post_renewable_cost_collapse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3b01d207-4e8d-4df5-8250-0d8d618ccece', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_fossil_generators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, grid_planning_authorities).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_diversification_consultancies).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_only_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, frontline_fossil_pollution_communities).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, future_generations_facing_delayed_decarbonization).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, nuclear_accident_risk_bearing_localities).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, deep_uncertainty_precludes_single_pathway_commitment).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, real_options_theory_applies_to_infrastructure_choice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets integrated resource plans that formally require maintaining a portfolio of generation types on the stated grounds of reliability and hedging against technological and geopolitical surprise. Administers the licensing, subsidy, and capacity-market rules that keep fossil and nuclear plants operable rather than retired. Can revise the portfolio mandate but bears none of the pollution or foreclosure costs directly, and its own institutional continuity depends on the diversification mandate remaining unresolved.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, grid_planning_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Existing coal and gas plants that would face early retirement under a decarbonization-committed pathway. The option-value framing keeps their capacity contracted and their asset value intact by making 'premature closure' a named risk the planning authority is charged with avoiding. They lobby to keep the flexibility framing in place because it directly extends their operating life.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_fossil_generators, beneficiary,
    powerful, biographical, arbitrage, national).

% Nuclear fleet operators who benefit from being classified alongside fossil assets as a 'pathway worth preserving,' which secures continued public subsidy, extended license terms, and insulation from being crowded out by cheaper renewables-plus-storage builds. Their exit options are more constrained than fossil's because plant decommissioning is costly and slow, but the option-value mandate protects them from forced closure either way.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_nuclear_operators, beneficiary,
    powerful, generational, constrained, national).

% Risk-analysis firms and think tanks that produce the scenario modeling and 'real options' valuations used to justify keeping pathways open. Their revenue depends on the uncertainty framing remaining unresolved — a settled answer (either full decarbonization or full fossil lock-in) eliminates the demand for their ongoing hedging analysis.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_diversification_consultancies, beneficiary,
    organized, biographical, mobile, national).

% Wind, solar, and storage developers who can meet demonstrated demand at lower cost than maintained fossil and nuclear capacity, but are structurally disadvantaged by capacity-market rules and interconnection queues designed to preserve pathway diversity. They bear the cost of the constraint through slower deployment, curtailment, and capital that could have built out faster absent the mandated hedge.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_only_developers, payer,
    moderate, biographical, constrained, national).

% Communities living near retained fossil generation who continue to bear particulate and combustion-byproduct health burdens for every additional year the plants stay open under the flexibility rationale. They have no meaningful voice in the portfolio planning process and cannot relocate the plants or, in most cases, themselves.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, frontline_fossil_pollution_communities, payer,
    powerless, biographical, trapped, local).

% Populations living within the plume radius of maintained nuclear facilities who carry low-probability, high-severity tail risk so that the nuclear pathway remains 'preserved.' Their exposure is a direct cost of keeping the option open rather than resolving toward either full nuclear buildout (with commensurate safety investment) or phase-out.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_accident_risk_bearing_localities, payer,
    powerless, civilizational, trapped, regional).

% Bear the accumulated climate cost of every year fossil capacity is retained as a hedge rather than retired, since emissions committed today constrain the future climate envelope regardless of later pathway resolution. Not present as an agent in any current decision process.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_generations_facing_delayed_decarbonization, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_energy__option_value_preserving, future_generations_facing_delayed_decarbonization).

% Argue that deep uncertainty is being used to justify indefinite delay rather than genuine hedging, and that a committed renewables pathway would resolve more uncertainty than it preserves. They submit comments in planning dockets but the diversification mandate is treated as a threshold requirement not open to their central objection.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, decarbonization_advocacy_coalitions, excluded,
    organized, generational, constrained, national).

% Study whether the option-value framing genuinely reduces decision-theoretic regret under uncertainty or instead functions as an institutionalized justification for retaining incumbent assets. Their findings feed into the omega variables below rather than into the immediate planning process.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_and_risk_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under genuine deep uncertainty about future technology costs, climate sensitivity, and geopolitical fuel security, maintaining a diversified generation portfolio can preserve real option value — the ability to pivot pathways as information resolves, rather than locking in an irreversible bet that later proves wrong.
% TRANSFER_FUNCTION: Moves continued operating life, subsidy, and regulatory protection to incumbent fossil and nuclear asset owners; moves capital deployment speed and market access away from lower-cost renewable developers; moves accumulated health and climate costs to fossil-adjacent communities and future populations who receive no compensating share of the 'preserved flexibility.'
% ABSENT_VOICES: Frontline pollution communities and future generations have no seat in integrated resource planning processes; decarbonization coalitions are present but treated as advocates to be weighed against incumbents rather than as holders of a legitimate rival reading of acceptable risk.
% DISAPPEARANCE_RATIONALE: If the option-value-preserving mandate were removed, capacity markets and licensing rules built around 'pathway diversity' would need to be rewritten, incumbent fossil and nuclear assets would face accelerated retirement or committed long-term operation on their own merits, and renewable deployment would no longer compete against an artificially preserved hedge — the entire integrated resource planning apparatus organized around this reading would need to reconstitute around a different acceptable-risk standard.
% FOUNDING_PROBLEM: Energy investments are long-lived and capital-intensive, made under genuine uncertainty about future costs, climate outcomes, and technology trajectories; committing irreversibly to one pathway risks large regret if the future diverges from the planning assumption.
% FOUNDING_PROBLEM_CORROBORATION: Grid planning authorities and diversification consultancies attest the uncertainty problem remains fully live and requires ongoing hedging. Independent decision-theory researchers outside the incumbent-benefiting set corroborate that genuine option value exists in principle, but several (cited in the omega below) find that in practice the diversification mandate has been calibrated to protect specific incumbent assets well past the point where uncertainty resolution would justify closure — an assessment decarbonization coalitions and some climate economists share but incumbents dispute.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).
:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) sits moderate: real option value is a genuine decision-theoretic good, so this reading is not pure extraction, but the beneficiary set (incumbent asset owners, consultancies whose business model depends on unresolved uncertainty) is concrete and identifiable, which pulls extraction above a pure-coordination baseline. Suppression (0.48) is moderate because the mandate actively disadvantages both poles — accelerated decarbonization advocates AND expected-value-minimizing nuclear expansion advocates — without fully foreclosing either; it holds the middle by regulatory and market-design force, not by persuasion alone. Theater ratio (0.31) reflects that a nontrivial share of the 'preserving optionality' framing in planning documents functions to justify decisions (asset retention) already made on other grounds, and this share has grown over the measured interval as the empirical case for least-cost renewables-plus-storage has strengthened even as the diversification mandate has not budged.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid planning authorities and incumbent generators sit near the beneficiary end: the authority's institutional relevance and the incumbents' asset value both depend on the diversification mandate persisting. Renewable developers and both classes of frontline/tail-risk communities sit near the target end: they bear displaced deployment, health burden, or catastrophic tail exposure respectively, with no institutional lever to alter the portfolio mandate. Future generations are declared non-agent (they cannot appear in the current decision process) but are retained as a payer group because the emissions-lock-in cost is real and structurally attributable to this reading's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — genuine deep uncertainty counseling against irreversible pathway commitment — remains partially live: real options theory is a legitimate framework and uncertainty about long-run technology costs has not fully resolved. But the founding_problem_status is authored as contested rather than clearly live, because independent decision-theory analysis (per the omega below) increasingly finds that uncertainty has resolved further than the mandate's continued rigidity reflects, particularly on cost trajectories for renewables-plus-storage versus new nuclear and fossil-with-carbon-capture. The tangled_rope classification (rather than a clean rope) captures exactly this: a real coordination function (hedging under uncertainty) riding alongside asymmetric extraction (specific incumbents protected, specific powerless populations paying) — mislabeling this as pure extraction would erase the genuine option-value logic; mislabeling it as pure coordination would erase the identifiable, non-diffuse beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_hedge_vs_incumbent_shield,
    'Is the diversification mandate functioning as a genuine real-options hedge against unresolved technological and climate uncertainty, or has it been captured as a durable shield protecting specific incumbent fossil and nuclear assets past the point where the uncertainty it was built to manage has actually resolved?',
    'Track whether portfolio composition requirements update in response to resolved uncertainty (e.g., falling renewable-plus-storage costs, improved climate sensitivity estimates) or remain static regardless of new information; a mandate that never updates despite resolving uncertainty is evidence of capture rather than genuine option preservation.',
    'If capture-dominant, effective extraction is higher than the moderate value authored here and the classification should drift toward snare; if genuine-hedge-dominant, extraction should be lower and the classification drifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_hedge_vs_incumbent_shield, empirical, 'Whether the option-value framing is genuine hedging or captured incumbent protection.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three sibling readings of acceptable_risk_energy disagree — is it a factual disagreement about the shape of the uncertainty distribution, or a values disagreement about how to weight tail risk versus expected value versus flexibility?',
    'Decompose the disagreement into (a) empirical claims about probability distributions over future costs/accidents/climate outcomes, which are in principle resolvable by better data, and (b) normative claims about risk-weighting (precautionary vs. expected-value vs. flexibility-preserving), which are not resolvable by data alone. Map each sibling reading''s central claim onto this decomposition.',
    'If the disagreement is primarily normative, none of the three readings should be expected to converge regardless of evidence, and all three should be maintained as permanently coexisting constraints rather than expecting eventual resolution to a single reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the sibling readings disagree empirically or normatively, and what that implies for eventual convergence.').

omega_variable(
    opportunity_cost_measurement,
    'How should the opportunity cost borne by renewable developers and future generations from delayed pathway resolution be measured and weighed against the option value the mandate claims to preserve?',
    'Comparative counterfactual modeling: estimate deployment trajectories and cumulative emissions under a committed-renewables scenario versus the actual diversified-portfolio trajectory, using the same uncertainty assumptions the option-value framing itself relies on.',
    'If counterfactual modeling shows the preserved option value is smaller than the foregone decarbonization benefit, the coordination function claimed for this reading is weaker than authored and the extraction score should be revised upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(opportunity_cost_measurement, empirical, 'Whether the claimed option value exceeds the opportunity cost it imposes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.14).
narrative_ontology:measurement(acce_tr_t4, acceptable_risk_energy__option_value_preserving, theater_ratio, 4, 0.17).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__option_value_preserving, theater_ratio, 8, 0.2).
narrative_ontology:measurement(acce_tr_t12, acceptable_risk_energy__option_value_preserving, theater_ratio, 12, 0.24).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__option_value_preserving, theater_ratio, 16, 0.27).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.29).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__option_value_preserving, theater_ratio, 24, 0.31).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(acce_be_t4, acceptable_risk_energy__option_value_preserving, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__option_value_preserving, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(acce_be_t12, acceptable_risk_energy__option_value_preserving, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__option_value_preserving, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__option_value_preserving, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t4, acceptable_risk_energy__option_value_preserving, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__option_value_preserving, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(acce_su_t12, acceptable_risk_energy__option_value_preserving, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__option_value_preserving, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__option_value_preserving, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the acceptable_risk_energy kernel. catastrophic_tail_dominant would foreclose both fossil and nuclear pathways on precautionary grounds; expected_value_dominant would resolve toward whichever pathway minimizes mortality-per-TWh (likely favoring nuclear expansion and rapid fossil phase-out). This reading (option_value_preserving) keeps both pathways nominally open on deep-uncertainty grounds. Each reading has a distinct epsilon, distinct beneficiary/victim sets, and distinct classification; they are not merged. The upstream influence runs from this reading toward the others insofar as maintained fossil/nuclear capacity under this reading changes the resource and legitimacy conditions the other two readings must contend with (e.g., prolonged fossil retention narrows the expected-value reading's near-term feasible set).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
