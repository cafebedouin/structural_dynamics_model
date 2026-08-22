% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option-Value-Preserving Standard of Acceptable Energy Risk
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   Energy regulators and grid planners increasingly justify continued
 *   licensing of both nuclear and fossil generation — alongside renewables
 *   buildout — on the grounds that deep, non-probabilizable uncertainty about
 *   technology trajectories and catastrophic tail risk makes maintaining
 *   multiple pathways the epistemically prudent choice. The framing has
 *   genuine coordination content: irreversible early commitment to any single
 *   pathway under real model uncertainty risks costly lock-in. But the same
 *   framing systematically protects incumbent nuclear and fossil asset value,
 *   slows renewable-only market entry, and defers residual risk and cost onto
 *   host communities and future generations who have no seat in the
 *   standard-setting process.
 *
 * KEY AGENTS:
 *   - grid_planning_authorities: agenda-setter administering the acceptable-risk standard (institutional/analytical exit)
 *   - incumbent_nuclear_operators and incumbent_fossil_operators: organized beneficiaries whose continued licensing rides on the optionality framing
 *   - diversified_utility_holding_companies: powerful beneficiary with arbitrage exit, capturing the flexibility value the standard exists to preserve
 *   - renewable_only_developers: moderate-power payer competing against protected incumbents
 *   - communities_near_legacy_fossil_and_nuclear_sites and future_generations_bearing_delayed_transition_costs: powerless, trapped payers bearing residual and inherited risk
 *   - rapid_decarbonization_advocates: excluded organized voice arguing for the tail-dominant reading
 *   - risk_decision_theorists: analytical observers assessing whether option-value preservation is a defensible decision rule or a captured framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.51).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option-Value-Preserving Standard of Acceptable Energy Risk").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '5e4c4f27-91dd-4fe3-aa36-4608d5270213').
narrative_ontology:cs_kernel_codification('5e4c4f27-91dd-4fe3-aa36-4608d5270213', distributed).
narrative_ontology:cs_authority_grounding('5e4c4f27-91dd-4fe3-aa36-4608d5270213', distributed).
narrative_ontology:cs_reading_relation('5e4c4f27-91dd-4fe3-aa36-4608d5270213', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('5e4c4f27-91dd-4fe3-aa36-4608d5270213', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('5e4c4f27-91dd-4fe3-aa36-4608d5270213', foundational, irreversible_commitment_under_deep_uncertainty_is_itself_a_harm).
narrative_ontology:cs_axiom_status(irreversible_commitment_under_deep_uncertainty_is_itself_a_harm, holdable).
narrative_ontology:cs_axiom_grounding('5e4c4f27-91dd-4fe3-aa36-4608d5270213', irreversible_commitment_under_deep_uncertainty_is_itself_a_harm, instrumental).
narrative_ontology:cs_axiom('5e4c4f27-91dd-4fe3-aa36-4608d5270213', secondary, pathway_diversity_has_positive_hedge_value_independent_of_point_estimates).
narrative_ontology:cs_axiom_status(pathway_diversity_has_positive_hedge_value_independent_of_point_estimates, holdable).
narrative_ontology:cs_axiom_grounding('5e4c4f27-91dd-4fe3-aa36-4608d5270213', pathway_diversity_has_positive_hedge_value_independent_of_point_estimates, empirically_contingent).
narrative_ontology:cs_reference_frame('5e4c4f27-91dd-4fe3-aa36-4608d5270213', multi_pathway_hedge_under_non_probabilizable_uncertainty).
narrative_ontology:cs_drift_state('5e4c4f27-91dd-4fe3-aa36-4608d5270213', post_storage_cost_decline_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5e4c4f27-91dd-4fe3-aa36-4608d5270213', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, grid_planning_authorities).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, diversified_utility_holding_companies).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_only_developers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, communities_near_legacy_fossil_and_nuclear_sites).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, future_generations_bearing_delayed_transition_costs).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, rapid_decarbonization_advocates).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, deep_uncertainty_justifies_pathway_diversity).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, premature_lock_in_is_itself_a_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set integrated resource plans and licensing criteria that formally weight 'preserving optionality' as an acceptable-risk criterion, allocating capacity targets and approvals across nuclear, fossil, and renewable pathways. They administer the standard and could revise it toward a single dominant pathway, but bear none of the direct cost of keeping multiple pathways alive.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, grid_planning_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Depend on the optionality framing to keep license renewals, subsidies, and capacity payments flowing even where a strict expected-value or catastrophic-tail standard would phase them out faster. Optionality language lets them argue their fleet is a hedge against renewable intermittency and fossil lock-in.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_nuclear_operators, beneficiary,
    organized, generational, constrained, national).

% Use the same optionality logic to justify continued permitting and capacity-market participation as 'dispatchable backbone' insurance against decarbonization pathways stalling. Their continued operation is protected by the same acceptable-risk standard that a tail-dominant or expected-value standard would foreclose much faster.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators, beneficiary,
    organized, generational, constrained, national).

% Hold mixed nuclear, fossil, and renewable assets and lobby regulators to keep the optionality standard in place because it protects the value of their entire portfolio rather than forcing write-downs on any single asset class. They shift capital opportunistically between pathways as political winds change, extracting flexibility value the standard exists to preserve for them specifically.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, diversified_utility_holding_companies, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, diversified_utility_holding_companies, agenda_setter).

% Compete for interconnection queue slots, capacity payments, and financing against incumbents whose continued viability is guaranteed by the optionality standard rather than by comparative performance. Every year multiple pathways are kept 'open' is a year of slower queue clearance and capital reallocation away from renewable-only portfolios.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_only_developers, payer,
    moderate, biographical, constrained, national).

% Continue to bear localized pollution, accident-tail exposure, and waste-storage risk from plants that a stricter standard would have retired sooner. The abstract decision-theoretic value of 'keeping options open' is realized elsewhere; the concrete residual risk is realized where they live.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, communities_near_legacy_fossil_and_nuclear_sites, payer,
    powerless, biographical, trapped, local).

% Cannot participate in the present decision but inherit whichever residual carbon budget, waste inventory, or stranded-asset bill results from pathways kept open longer than a single-pathway commitment would have allowed. Their interests enter only through discounting assumptions embedded in the planning models.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_generations_bearing_delayed_transition_costs, payer,
    powerless, civilizational, trapped, global).

% Argue that under a catastrophic-tail-dominant reading, fossil pathways should already be foreclosed regardless of optionality value, and that 'preserving flexibility' functions as a rhetorical delay mechanism. They participate in public comment but the planning framework's formal decision criteria are not built around their tail-risk framing.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, rapid_decarbonization_advocates, excluded,
    organized, generational, constrained, national).

% Study whether option-value preservation under deep uncertainty is a defensible decision rule (robust to model misspecification) or a captured framing that launders continued dual-pathway investment as prudence. They can formally model regret-minimization versus expected-value versus tail-avoidance criteria but do not administer the standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, risk_decision_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, diversified_utility_holding_companies).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under genuine deep (non-probabilizable) uncertainty about technology cost trajectories, catastrophe probabilities, and policy durability, keeping more than one energy pathway technically and financially viable preserves the ability to correct course later without irreversible stranding — a real hedge against model error that a single committed pathway cannot offer.
% TRANSFER_FUNCTION: Moves capacity payments, licensing continuity, and regulatory protection from a faster-closing single-pathway standard toward incumbent nuclear and fossil operators and diversified asset holders, at the cost of renewable-only developers' market access, host communities' ongoing residual risk exposure, and future generations' inherited carbon and waste burden.
% ABSENT_VOICES: Future generations have no seat in the proceeding at all. Host communities near legacy sites participate through limited local hearings with little formal weight in the planning criteria. Rapid decarbonization advocates are present but their catastrophic-tail framing is treated as one input among several rather than as the dominant decision criterion.
% DISAPPEARANCE_RATIONALE: If the option-value-preserving standard were replaced overnight by either the tail-dominant or expected-value-dominant reading, licensing and capacity-market rules would shift rapidly: a tail-dominant standard would accelerate fossil and possibly nuclear phase-out; an expected-value standard would reallocate capacity payments strictly by mortality-per-TWh, likely favoring nuclear over fossil and squeezing peaker plants. Incumbent portfolios, interconnection queues, and stranded-asset exposure would all reorganize.
% FOUNDING_PROBLEM: Energy planners faced genuine deep uncertainty in the late 20th and early 21st centuries about renewable cost curves, storage economics, nuclear safety and waste trajectories, and the pace of climate damage — situations where assigning firm probabilities to catastrophic or dominant-pathway outcomes was not credible, so keeping multiple pathways alive was framed as the epistemically honest response to genuine unknowns.
% FOUNDING_PROBLEM_CORROBORATION: Grid planning authorities and diversified utility holding companies attest the deep-uncertainty problem remains live, citing continued volatility in storage costs and policy durability. Independent decision theorists and rapid decarbonization advocates outside the beneficiary set counter that renewable and storage cost trajectories have substantially de-risked since the early framing was adopted, and that continued invocation of 'deep uncertainty' now functions largely to protect incumbent asset value rather than to hedge a genuinely open question.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42 at interval end) rather than high: the coordination function is real — genuine deep uncertainty about cost curves and catastrophe probabilities makes pathway diversification a defensible hedge, not pure pretext. But extraction is rising over the interval as renewable and storage cost trajectories de-risk many of the original unknowns while incumbents continue invoking 'optionality' to protect asset value, a widening gap between the standard's founding justification and its current function. Suppression (0.51) reflects moderate active suppression of BOTH extremes: the standard structurally resists both the catastrophic-tail-dominant push to foreclose fossil/nuclear pathways rapidly and the expected-value-dominant push to rank pathways strictly by mortality metrics — it exists specifically to prevent either sibling reading from becoming the sole decision criterion. Theater ratio is modest but rising (0.28) as 'preserving optionality' increasingly functions as a rhetorical delay mechanism in specific proceedings rather than a live epistemic hedge.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid planning authorities set the standard but bear none of its direct costs — analytical exit, agenda-setter role. Incumbent nuclear and fossil operators and diversified utility holders are structural beneficiaries: the standard's persistence directly protects their asset value and licensing continuity, pushing their directionality toward the beneficiary end. Renewable-only developers, host communities, and future generations are structural targets: they bear competitive disadvantage, residual local risk, and inherited transition costs respectively, pushing directionality toward the target end. Future generations receive the highest effective extraction despite zero formal voice because their exit option is fully trapped (civilizational time horizon, no participation channel) — the derivation chain correctly amplifies their exposure even though no explicit override is needed here.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) prevents two mislabelings. First, it prevents treating the entire arrangement as pure extraction: the coordination function — hedging against irreversible lock-in under genuine deep uncertainty — is real and would be lost if the standard were abolished rather than reformed. Second, it prevents treating the arrangement as pure coordination (rope): the beneficiary concentration among incumbent and diversified asset holders, combined with active suppression of both sibling readings and a widening extraction trend, shows the coordination function has been substantially captured to protect incumbent portfolios beyond what the founding uncertainty justifies. The founding_problem_status is authored as contested rather than dead precisely because corroboration is split: incumbents (interested parties) say the uncertainty is still live; independent decision theorists and excluded advocates (outside the beneficiary set) say it has substantially resolved. That split is the signal a tangled_rope classification is built to preserve rather than erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_uncertainty_vs_captured_framing,
    'Is the deep-uncertainty justification for pathway preservation still epistemically live today, or has it become a captured framing that protects incumbent asset value after the underlying uncertainty has substantially resolved?',
    'Compare the confidence intervals on renewable/storage cost projections and nuclear/fossil catastrophic-tail probabilities used at the standard''s founding versus current best estimates; if current intervals are substantially narrower and consistently favor one pathway, the deep-uncertainty premise has weakened even though the standard has not.',
    'If the uncertainty premise has resolved, effective extraction is higher than currently measured and the classification should trend toward snare; if the uncertainty remains genuinely deep, the tangled_rope classification with moderate extraction is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_uncertainty_vs_captured_framing, empirical, 'Whether the founding deep-uncertainty premise is still empirically live.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the disagreement between this reading and the catastrophic_tail_dominant / expected_value_dominant siblings live — in the empirical inputs (probability estimates), the decision rule (maximin/regret vs. expected utility vs. option value), or the normative weighting of future/local versus present/diffuse interests?',
    'Decompose each sibling reading''s formal decision procedure and identify whether feeding identical probability and cost inputs into each procedure still produces divergent pathway rankings (decision-rule disagreement) or whether the readings actually differ chiefly in their input probability estimates (empirical disagreement).',
    'If the disagreement is primarily a decision-rule dispute, no amount of better data resolves it and the three readings remain permanently coexisting normative positions; if primarily empirical, better catastrophe-probability and cost-trajectory data could eventually collapse the readings toward convergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether kernel disagreement is empirical or decision-theoretic in nature.').

omega_variable(
    future_generation_representation_adequacy,
    'Can any formal weighting scheme (discount rate, intergenerational equity adjustment) adequately represent future generations'' interests in a standard-setting process from which they are structurally absent, or does their absence constitute an irreducible legitimacy gap regardless of the discount rate chosen?',
    'Compare outcomes under alternative discounting/representation schemes against outcomes from processes that included formal future-interest advocates (e.g., ombudsperson-for-future-generations models in some jurisdictions) to see whether representation changes pathway rankings.',
    'If representation changes outcomes substantially, the current standard''s victim classification for future generations understates the extraction; if discounting adequately proxies their interests, the current classification is reasonably calibrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_representation_adequacy, preference, 'Whether formal discounting can substitute for actual representation of absent future generations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t4, acceptable_risk_energy__option_value_preserving, theater_ratio, 4, 0.15).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_energy__option_value_preserving, theater_ratio, 8, 0.18).
narrative_ontology:measurement(acce_tr_t12, acceptable_risk_energy__option_value_preserving, theater_ratio, 12, 0.21).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_energy__option_value_preserving, theater_ratio, 16, 0.24).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.26).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_energy__option_value_preserving, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(acce_be_t4, acceptable_risk_energy__option_value_preserving, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_energy__option_value_preserving, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(acce_be_t12, acceptable_risk_energy__option_value_preserving, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_energy__option_value_preserving, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_energy__option_value_preserving, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t4, acceptable_risk_energy__option_value_preserving, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_energy__option_value_preserving, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(acce_su_t12, acceptable_risk_energy__option_value_preserving, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_energy__option_value_preserving, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_energy__option_value_preserving, suppression_requirement, 24, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the acceptable_risk_energy kernel, each authored as a separate ε-invariant constraint per the decomposition principle. option_value_preserving keeps both nuclear and fossil pathways viable and authors moderate extraction (0.42) reflecting a real but increasingly captured coordination function. catastrophic_tail_dominant would author a different ε reflecting rapid pathway foreclosure prioritizing worst-case avoidance over aggregate expected harm. expected_value_dominant would author yet another ε reflecting strict mortality-per-TWh ranking. The three are linked via affects_constraints rather than merged, since each reading produces a distinct beneficiary/victim structure and a distinct classification — averaging or hedging across them inside one file would violate the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
