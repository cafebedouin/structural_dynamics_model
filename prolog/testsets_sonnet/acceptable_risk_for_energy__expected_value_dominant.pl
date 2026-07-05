% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Acceptable Risk Standard for Nuclear Energy Policy
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This story instantiates the expected-value-dominant reading of the
 *   acceptable-risk-for-energy kernel: nuclear energy's acceptability is
 *   determined by annualized expected cost (probability of an adverse event
 *   times its consequence magnitude) weighed against annualized climate and
 *   cost benefits relative to fossil alternatives. Under this reading,
 *   because the annual probability of a severe nuclear accident is low, even
 *   a large consequence magnitude produces a small expected-value product,
 *   which nuclear then clears comfortably — exiting it from the victim-heavy
 *   category that a tail-dominant or comparative-risk reading would place it
 *   in. Waste disposal, similarly, is treated as an engineering cost line
 *   item to be annualized and amortized rather than an open-ended,
 *   non-consenting intergenerational liability. This is a structurally
 *   distinct constraint from its siblings (catastrophic_tail_dominant,
 *   comparative_risk_dominant), not a different observation angle on the same
 *   one — each reading produces a different victim set, a different
 *   suppression profile, and a different treatment of waste, so per the
 *   epsilon-invariance principle they are authored as three separate stories
 *   linked via network edges, not as one story with a measurement parameter.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.32).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.28).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.32).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Acceptable Risk Standard for Nuclear Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'b7989623-dc22-4dc3-b906-812840c7d7c9').
narrative_ontology:cs_kernel_codification('b7989623-dc22-4dc3-b906-812840c7d7c9', formalized).
narrative_ontology:cs_authority_grounding('b7989623-dc22-4dc3-b906-812840c7d7c9', expertise).
narrative_ontology:cs_interpretation_layer_present('b7989623-dc22-4dc3-b906-812840c7d7c9').
narrative_ontology:cs_reading_relation('b7989623-dc22-4dc3-b906-812840c7d7c9', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('b7989623-dc22-4dc3-b906-812840c7d7c9', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('b7989623-dc22-4dc3-b906-812840c7d7c9', foundational, annualized_probability_weighted_consequence_is_sufficient_acceptability_metric).
narrative_ontology:cs_axiom_status(annualized_probability_weighted_consequence_is_sufficient_acceptability_metric, holdable).
narrative_ontology:cs_axiom_grounding('b7989623-dc22-4dc3-b906-812840c7d7c9', annualized_probability_weighted_consequence_is_sufficient_acceptability_metric, instrumental).
narrative_ontology:cs_axiom('b7989623-dc22-4dc3-b906-812840c7d7c9', secondary, waste_stewardship_cost_is_boundable_and_amortizable).
narrative_ontology:cs_axiom_status(waste_stewardship_cost_is_boundable_and_amortizable, holdable).
narrative_ontology:cs_axiom_grounding('b7989623-dc22-4dc3-b906-812840c7d7c9', waste_stewardship_cost_is_boundable_and_amortizable, empirically_contingent).
narrative_ontology:cs_reference_frame('b7989623-dc22-4dc3-b906-812840c7d7c9', actuarial_risk_parity_across_energy_sources).
narrative_ontology:cs_drift_state('b7989623-dc22-4dc3-b906-812840c7d7c9', post_fukushima_regulatory_reassessment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b7989623-dc22-4dc3-b906-812840c7d7c9', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, grid_decarbonization_planners).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, ratepayers_in_low_carbon_grids).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_agencies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, host_community_residents_near_reactor_sites).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations_managing_waste_repositories).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, populations_within_low_probability_exclusion_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate reactors under a licensing and cost-benefit regime that treats annual expected cost (probability times consequence) as the acceptability metric. This framing lets them site, insure, and finance plants against a favorable expected-value comparison to fossil alternatives, and they actively lobby regulators and standard-setting bodies to keep the expected-value frame dominant over tail-focused alternatives.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, nuclear_utility_operators, agenda_setter).

% Design long-term decarbonization pathways and need a stable, quantifiable risk metric that lets nuclear compete on paper against coal and gas. The expected-value frame gives them a defensible number to put in planning documents; without it, nuclear's role in decarbonization models becomes harder to justify against public tail-risk aversion.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, grid_decarbonization_planners, beneficiary,
    institutional, generational, constrained, national).

% Receive lower-carbon, often lower-cost electricity where nuclear capacity is retained or built under this framework. They rarely evaluate the underlying risk math themselves and inherit the acceptability judgment made on their behalf by regulators and utilities.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, ratepayers_in_low_carbon_grids, beneficiary,
    moderate, biographical, constrained, regional).

% Use expected-value nuclear risk accounting to argue nuclear belongs in the climate mitigation portfolio, trading a bounded, probability-weighted nuclear risk against the much larger, near-certain expected cost of continued fossil combustion.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_agencies, beneficiary,
    institutional, civilizational, constrained, global).

% Bear the concentrated, place-bound consequence side of the probability x consequence product: if the rare event occurs, they absorb evacuation, contamination, and property loss regardless of how favorable the annual expected value looked in the planning documents. Their local, low-probability exposure is averaged away in a national expected-value calculation they did not design and cannot easily exit given housing and employment ties to the region.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, host_community_residents_near_reactor_sites, payer,
    powerless, biographical, trapped, local).

% Inherit spent fuel and repository stewardship obligations that extend far beyond any single generation's planning horizon. Under this reading, waste disposal is treated as a solvable, cost-annualizable engineering challenge rather than an open intergenerational liability, which discounts their non-consenting exposure to near zero in the acceptability calculation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations_managing_waste_repositories, payer,
    powerless, civilizational, trapped, national).

% Live within zones that carry non-trivial consequence magnitude if a rare event occurs, but whose low annual probability keeps the expected-value product low enough to pass the acceptability threshold. They experience the full consequence tail as a lived possibility, not as an averaged annual figure.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, populations_within_low_probability_exclusion_zones, payer,
    powerless, biographical, trapped, regional).

% Set and enforce the expected-value acceptability threshold in licensing decisions, choosing probability models, consequence valuations, and discount rates that determine which plants pass. Their methodological choices are the load-bearing structure of this reading of the kernel.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, risk_assessment_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that irreversibility, intergenerational burden, and correlated catastrophic failure modes cannot be captured by a single expected-value scalar. They participate in comment periods and litigation but the governing methodology itself is set upstream of their input, and their framing is treated as a minority objection rather than a co-equal acceptability standard.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_advocacy_groups, excluded,
    moderate, generational, constrained, national).

% Study nuclear risk relative to competing energy sources and note that this reading's exit of nuclear from the victim-heavy category depends entirely on the expected-value frame being the operative comparison; under a comparative-risk or tail-dominant frame the same facility could classify very differently.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, comparative_energy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, diffuse).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, quantifiable, comparable metric (probability x consequence, annualized) that lets regulators, financiers, and grid planners make siting, insurance, and portfolio decisions about nuclear energy without requiring case-by-case philosophical adjudication of every possible accident scenario.
% TRANSFER_FUNCTION: Moves acceptability determination away from communities bearing concentrated low-probability consequence exposure and toward centralized regulatory and utility bodies who can average that exposure into a favorable annualized figure; also moves financing and siting certainty toward utility operators by giving them a defensible, bounded risk number.
% ABSENT_VOICES: Host communities near reactor sites and future generations managing waste have no seat in setting the discount rates, consequence valuations, or probability models that determine the threshold; tail-risk advocacy groups are procedurally present in comment periods but structurally outside the methodology-setting process itself.
% DISAPPEARANCE_RATIONALE: If the expected-value-dominant frame were replaced overnight by a catastrophic-tail-dominant standard, several existing and planned nuclear facilities would fail licensing or relicensing review, insurance and financing structures built on annualized risk figures would need to be rebuilt around worst-case reserve requirements, and climate policy portfolios that count on nuclear capacity would need substitute low-carbon generation.
% FOUNDING_PROBLEM: Early nuclear regulation needed a tractable way to compare a technology with a very low annual accident probability but potentially large consequence magnitude against conventional energy sources with more continuous, visible risk profiles (mining deaths, air pollution, combustion emissions), so that licensing decisions could proceed on a bounded, actuarial basis rather than stalling indefinitely on worst-case scenario analysis.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear utility operators and regulators attest the expected-value framework remains the correct and necessary basis for licensing, citing its consistency with actuarial practice in other high-consequence industries. Independent risk theorists, tail-risk advocacy groups, and some post-Fukushima regulatory reviews (e.g. national commission reports following the 2011 accident) attest that the framework understates correlated, irreversible, and intergenerational consequence classes structurally excluded from an annualized expected-value product — corroboration exists outside the beneficiary set but is contested rather than settled.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.32) and rising slowly: the expected-value frame itself does not manufacture large near-term transfers, but as more capacity is sited and relicensed under this standard, the diffuse concentration of consequence-bearing exposure on host communities and future waste stewards compounds. Suppression is comparatively low and slightly declining (0.28) because this reading does not need to actively silence tail-risk framing to operate — it wins by methodological default in the venues where licensing decisions are actually made (technical regulatory bodies favor quantifiable, comparable metrics), so overt suppression of dissenting voices is less necessary than in a regime that had to actively argue down a competing standard. Theater ratio is low (0.22): the cost-benefit modeling is functionally real technical work, not primarily performative, though its share of purely justificatory modeling (post-hoc validation of pre-favored siting decisions) grows modestly over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator and utility seat, this is a rope: a genuine, technically defensible coordination mechanism that lets a real decision (site this reactor or not) get made without infinite regress into worst-case argument. From the host-community seat, the same structure looks like an enforced averaging-away of their concentrated, non-consenting exposure into a national statistic that was favorable before their local consequence magnitude was even known. The engine should register this divergence directly from the beneficiary/victim declarations and exit options, not from any claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Utility operators, grid planners, and climate agencies are declared beneficiaries because the expected-value frame directly produces the licensing, financing, and portfolio outcomes they need — their directionality sits near the full-beneficiary end. Host community residents, exclusion-zone populations, and future waste stewards are declared victims: they bear the concentrated consequence side of the probability x consequence product without having shaped the probability model, discount rate, or consequence valuation that determined it was acceptable, and their trapped exit options (residential, generational, non-consenting) push their directionality toward the full-target end. Ratepayers are a diffuse, low-intensity beneficiary rather than a victim — they receive the aggregate benefit (lower-carbon, often cheaper power) without individually bearing sited consequence risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing a tractable, non-paralyzing way to compare nuclear risk to conventional energy risk — remains partially live (nuclear licensing decisions still need some workable standard), which argues against pure mandatrophy. But the specific methodological choices embedded in the expected-value frame (which discount rate, which consequence valuation, whether correlated/irreversible failure modes are excluded from the product) have hardened past their original justification and are defended by beneficiaries using the same 'we need a tractable standard' argument that justified the original framework, even where post-Fukushima analysis suggests the product formulation itself excludes the exact failure modes that matter most. This is exactly the discrimination the classification exists to make: a real coordination function persists, but the specific parametrization has drifted toward extraction-favorable settings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expected_value_product_excludes_correlated_failure,
    'Does the probability x consequence product formulation structurally exclude correlated, irreversible, and intergenerational failure modes that a catastrophic-tail framing would weight far more heavily?',
    'Compare licensing decisions and consequence valuations made under this reading against post-accident regulatory reviews (e.g., national commission findings after major nuclear accidents) to determine whether the excluded failure modes were in fact material to the outcomes observed.',
    'If the exclusion is structural and material, this reading''s favorable treatment of nuclear risk is a methodological artifact of the chosen risk metric rather than a genuine resolution of the underlying acceptability question — supporting reclassification toward tangled_rope with the excluded consequence classes as unrecognized victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expected_value_product_excludes_correlated_failure, conceptual, 'Whether the expected-value product formulation itself is the mechanism suppressing tail-risk visibility.').

omega_variable(
    waste_disposal_engineering_vs_open_liability,
    'Is nuclear waste disposal genuinely a solvable, boundable engineering challenge amortizable within an annual cost framework, or is it an open-ended intergenerational liability whose true cost cannot be annualized without arbitrary discount-rate assumptions?',
    'Track long-horizon repository performance, cost overruns, and discount-rate sensitivity in the handful of operating or near-operating deep geological repositories (e.g., Onkalo) over multi-decade intervals.',
    'If waste truly proves boundable at modeled cost, this reading''s treatment is vindicated as engineering-adequate coordination; if costs or custodial requirements diverge substantially from modeled assumptions, future generations were treated as a discounted-away victim class under a reading that claimed the problem was solved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_disposal_engineering_vs_open_liability, empirical, 'Whether waste disposal amortization assumptions hold over multi-generational timescales.').

omega_variable(
    kernel_framing_choice_is_itself_contested,
    'Is the choice to evaluate nuclear acceptability via expected-value dominance (rather than tail-dominance or comparative-risk framing) itself a neutral technical decision, or a framing choice that structurally favors incumbent nuclear operators and grid planners who need a bounded, comparable metric?',
    'Examine which stakeholders participated in setting the methodological defaults (discount rates, consequence valuations, probability models) in the regulatory bodies that adopted this frame, and whether tail-risk-affected communities had comparable standing in that process.',
    'If the framing choice itself was made predominantly by beneficiary-aligned technical bodies without proportionate host-community input, the reading''s ''neutral methodology'' status is undermined and the constraint''s classification should weight the procedural capture more heavily than the surface technical defensibility of the metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_is_itself_contested, conceptual, 'Whether the selection of expected-value dominance as the governing frame was itself procedurally captured by beneficiary interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 8, 0.14).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 16, 0.17).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 24, 0.19).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 32, 0.21).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 24, 0.29).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 32, 0.31).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 24, 0.29).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 32, 0.28).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.1).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the acceptable_risk_for_energy kernel. catastrophic_tail_dominant treats irreversibility and intergenerational burden as dominating the acceptability calculus regardless of low annual probability, placing nuclear firmly in a victim-heavy classification. comparative_risk_dominant treats nuclear acceptability as relative to the risk of displaced energy sources with no absolute threshold. This reading (expected_value_dominant) treats the annualized probability x consequence product as the acceptability determinant, which exits nuclear from the victim-heavy category when the product is favorable, treats waste disposal as an annualizable engineering cost, and requires comparatively low suppression of tail-risk framing because the methodology wins by regulatory default rather than active argument. Each reading has its own epsilon, beneficiary/victim structure, and classification; they are linked here rather than merged into one parametrized story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
