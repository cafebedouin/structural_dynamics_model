% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Acceptable Risk Framework Preserving Nuclear and Fossil Option Value Under Deep Uncertainty
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   The 'acceptable risk' framework in energy policy is contested across
 *   three readings. This story instantiates the option_value_preserving
 *   reading: it holds that deep uncertainty about future technology costs,
 *   climate damages, and geopolitical conditions justifies keeping both
 *   nuclear and fossil pathways viable rather than committing to a single
 *   transition trajectory. The constraint operates through reliability
 *   standards, capacity markets, and licensing regimes that treat pathway
 *   diversity as a system requirement. It extracts from ratepayers and
 *   delayed-transition parties to subsidize incumbent firm capacity, while
 *   coordinating a multi-pathway portfolio that no single actor would
 *   maintain voluntarily. The sibling readings — catastrophic_tail_dominant
 *   (prioritizes avoiding low-probability catastrophes) and
 *   expected_value_dominant (minimizes aggregate expected harm via
 *   mortality-per-TWh) — would produce different beneficiary/victim
 *   structures and different ε values.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.42).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.55).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Acceptable Risk Framework Preserving Nuclear and Fossil Option Value Under Deep Uncertainty").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, 'e4a3c735-22bc-4c07-9f69-99c113df676e').
narrative_ontology:cs_kernel_codification('e4a3c735-22bc-4c07-9f69-99c113df676e', distributed).
narrative_ontology:cs_authority_grounding('e4a3c735-22bc-4c07-9f69-99c113df676e', practice).
narrative_ontology:cs_interpretation_layer_present('e4a3c735-22bc-4c07-9f69-99c113df676e').
narrative_ontology:cs_reading_relation('e4a3c735-22bc-4c07-9f69-99c113df676e', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('e4a3c735-22bc-4c07-9f69-99c113df676e', acceptable_risk_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('e4a3c735-22bc-4c07-9f69-99c113df676e', foundational, irreversibility_of_pathway_closure_creates_option_loss).
narrative_ontology:cs_axiom_status(irreversibility_of_pathway_closure_creates_option_loss, holdable).
narrative_ontology:cs_axiom_grounding('e4a3c735-22bc-4c07-9f69-99c113df676e', irreversibility_of_pathway_closure_creates_option_loss, empirically_contingent).
narrative_ontology:cs_axiom('e4a3c735-22bc-4c07-9f69-99c113df676e', foundational, deep_uncertainty_requires_portfolio_not_optimization).
narrative_ontology:cs_axiom_status(deep_uncertainty_requires_portfolio_not_optimization, holdable).
narrative_ontology:cs_axiom_grounding('e4a3c735-22bc-4c07-9f69-99c113df676e', deep_uncertainty_requires_portfolio_not_optimization, empirically_contingent).
narrative_ontology:cs_reference_frame('e4a3c735-22bc-4c07-9f69-99c113df676e', post_oil_crisis_energy_security_paradigm).
narrative_ontology:cs_drift_state('e4a3c735-22bc-4c07-9f69-99c113df676e', post_paris_agreement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4a3c735-22bc-4c07-9f69-99c113df676e', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, grid_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, energy_security_policymakers).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, ratepayers_bearing_stranded_asset_risk).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_developers_facing_delayed_transition).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, communities_near_legacy_infrastructure).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_adaptation_budget_holders).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, deep_uncertainty_justifies_pathway_diversity).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, real_options_theory_applies_to_energy_system_design).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__option_value_preserving, premature_closure_creates_irreversible_option_loss).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains existing reactor fleet and develops advanced designs under the premise that baseload and firm capacity are irreplaceable. Gains policy support for life extensions and new builds from the option-value framing. Exit means accepting stranded asset losses and workforce dissolution; constrained by capital intensity and regulatory licensing.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_industry, beneficiary,
    institutional, generational, constrained, national).

% Retains gas-fired generation as 'bridge' and 'firming' capacity under the option-value rationale. Gains continued market access and deferred write-downs. Exit means accelerated transition risk; constrained by reserve valuation and political economy of producing regions.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, fossil_fuel_producers, beneficiary,
    institutional, biographical, constrained, national).

% Administers reliability standards and capacity markets that encode the option-value logic. Their reliability mandate gives them structural authority to define what counts as 'acceptable risk.' Exit means regulatory redesign of their mandate; constrained by statutory obligation and institutional inertia.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, grid_operators, agenda_setter,
    institutional, biographical, constrained, regional).

% Set national energy strategy invoking security of supply, geopolitical resilience, and affordability. Benefit from a framework that justifies maintaining domestic fuel cycles and dispatchable capacity. Exit means shifting to a different risk paradigm; mobile because policy frames can change with administration.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_security_policymakers, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, energy_security_policymakers, beneficiary).

% Pay above-market rates for capacity kept online for option value, and bear future decommissioning liabilities through regulated recovery. Organized through consumer advocates and industrial user groups. Exit means leaving the service territory or self-generating; constrained by geography and capital requirements.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, ratepayers_bearing_stranded_asset_risk, payer,
    organized, biographical, constrained, regional).

% Face curtailment, interconnection queues, and market designs that value firm capacity over energy. Lose revenue and investment certainty when option-value logic prioritizes incumbent pathways. Mobile because capital can deploy to more favorable jurisdictions; powerful because of falling technology costs and corporate procurement demand.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_developers_facing_delayed_transition, payer,
    powerful, biographical, mobile, national).

% Bear localized pollution, health, and safety externalities from plants kept running for system option value. Rarely compensated at a level reflecting the systemic benefit they subsidize. Trapped by housing markets, community ties, and lack of political voice in regional reliability decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, communities_near_legacy_infrastructure, payer,
    moderate, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, communities_near_legacy_infrastructure, excluded).

% Face opportunity costs when capital allocated to maintaining option-value pathways crowds out adaptation and resilience investment. Constrained by fiscal rules and intergenerational equity mandates that are not enforced against energy system decisions.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_adaptation_budget_holders, payer,
    moderate, generational, constrained, national).

% Provide the formal real-options and deep-uncertainty literature that underwrites the option-value framing. Their work is cited by all sides; they do not collect rents from the constraint but their models shape its legitimacy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, decision_theory_scholars, observer,
    analytical, civilizational, analytical, universal).

% Argue that tail risks (nuclear accidents, climate tipping points) dominate the option-value calculus and demand pathway elimination, not preservation. Excluded from reliability-centric governance forums; mobile because their frame travels across jurisdictions through transnational advocacy networks.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, catastrophic_risk_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of maintaining a portfolio of energy technologies under conditions where no single metric (cost, emissions, reliability) dominates and the future state space is fundamentally uncertain. The constraint coordinates investment, regulation, and operation across multiple pathways so that society does not irrevocably commit to one trajectory before learning resolves key uncertainties.
% TRANSFER_FUNCTION: Transfers capital and risk from ratepayers and climate-adaptation budgets to incumbent nuclear and fossil capacity owners, mediated through regulated cost recovery and capacity markets. Also transfers option value from future generations (who lose pathway flexibility if climate damages accelerate) to current system operators who value near-term reliability.
% ABSENT_VOICES: Frontline communities bearing localized externalities of preserved pathways are structurally excluded from reliability planning forums. Future generations who bear the climate opportunity cost of delayed transition have no seat. Catastrophic-risk advocates who would eliminate rather than preserve certain pathways are excluded from the 'acceptable risk' definition process.
% DISAPPEARANCE_RATIONALE: If the option-value framing vanished overnight, capacity markets and reliability standards would shift to least-cost or emissions-minimizing criteria. Nuclear plants facing relicensing would close; gas plants would face accelerated retirement schedules; renewable deployment would accelerate but reliability events might increase during transition. The energy system would reorganize around a different risk calculus.
% FOUNDING_PROBLEM: Post-1970s energy crises created a paradigm where 'running out' and 'brownouts' were the dominant failure modes. The option-value framing emerged to justify maintaining diverse domestic supply chains against oil embargoes and later, against the perceived intermittency risk of early renewables. It was built to solve energy security under supply uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and energy security agencies attest the founding problem (supply security under uncertainty) remains live, citing geopolitical volatility and electrification-driven demand growth. Renewable industry analysts and climate economists attest the problem has shifted: the dominant uncertainty is now climate damage trajectory, not fuel supply, and the option-value framing preserves the wrong pathways. Independent system operator studies and IPCC mitigation pathways corroborate the shifted-problem reading.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.42) reflects that the constraint transfers significant but not total system costs to preserve optionality — it is not pure rent extraction because the coordination function (reliability under uncertainty) is real and valued. Suppression (0.55) is moderate: the constraint operates through regulatory and market design rather than direct coercion, but it structurally excludes alternative risk framings and pathway-elimination policies. Theater ratio (0.25) acknowledges that reliability services are genuinely provided, but a growing share of enforcement maintains incumbents beyond their coordination necessity. Accessibility collapse (0.65) is moderately high because once the option-value frame is accepted, eliminating a pathway looks like irreversible folly — but alternatives (pure renewables, demand-side) remain conceptually available. Resistance (0.48) is moderate: organized payer groups and excluded voices resist but have not displaced the framework.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (grid operators), the constraint is genuine coordination — they see reliability events avoided and cannot know the counterfactual where a different portfolio would have worked. From the payer seats (ratepayers, renewable developers), the same structure operates as extraction maintaining incumbents past their economic justification. From the excluded seat (catastrophic-risk advocates), the constraint is a category error — it preserves pathways whose tail risks dominate the option-value calculus. The engine computes this divergence; the authored claim (tangled_rope) reflects that all three perspectives have structural grounding.
 *
 * DIRECTIONALITY LOGIC:
 *   Grid operators and energy security policymakers are agenda_setters with institutional power and constrained exit — they define and administer the constraint. Nuclear and fossil producers are beneficiaries with institutional power but constrained exit (capital lock-in). Ratepayers, renewable developers, frontline communities, and climate budget holders are payers with varying power and exit: renewable developers are powerful and mobile; frontline communities are moderate and trapped; ratepayers are organized but constrained. Catastrophic-risk advocates are excluded but organized and mobile. Decision-theory scholars are analytical observers. The engine will compute per-seat directionality from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s supply security) has partially shifted but not vanished — geopolitical energy security remains salient. However, the climate externality and renewable cost decline have created a new dominant uncertainty that the option-value framing does not center. The constraint persists because the institutional machinery (reliability standards, capacity markets) has captured the option-value logic as its own justification. This is not pure mandatrophy (the coordination function is still invoked) but shows mandatrophy dynamics: the arrangement's persistence is increasingly explained by the institutions that administer it rather than the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of the measured extractiveness is the necessary cost of genuine multi-pathway coordination under deep uncertainty, versus rent extraction by incumbents using the coordination framing as cover?',
    'Counterfactual system modeling: optimize a portfolio under deep uncertainty with and without the incumbent-preserving constraints; the cost difference isolates the coordination premium. Compare to observed cost transfers.',
    'If the coordination premium is small relative to observed transfers, the constraint leans snare; if large, it leans rope. The tangled_rope claim rests on both being substantial.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Separability of coordination function from extraction in option-value energy policy').

omega_variable(
    deep_uncertainty_vs_known_trajectory,
    'Is the uncertainty facing energy systems still ''deep'' (unknown probability distributions over outcomes) or has it resolved into ''risk'' (known distributions) where expected-value optimization dominates?',
    'Track convergence of climate-economy integrated assessment models, renewable cost curves, and geopolitical scenario analyses. If distributions narrow and converge, deep uncertainty recedes.',
    'If uncertainty has resolved to risk, the option-value justification weakens and the constraint''s claimed_type should shift toward snare or piton. If deep uncertainty persists, tangled_rope remains defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_uncertainty_vs_known_trajectory, empirical, 'Epistemic status of uncertainty in energy system planning').

omega_variable(
    kernel_reading_relations,
    'Does the option_value_preserving reading structurally coexist with, foreclose, or merely influence its sibling readings (catastrophic_tail_dominant, expected_value_dominant)?',
    'Analyze whether a single governance framework can simultaneously maintain pathway diversity (this reading), eliminate high-tail-risk pathways (catastrophic reading), and optimize expected mortality (expected-value reading). Map actual policy conflicts.',
    'If forecloses: the kernel admits no stable multi-reading equilibrium — one reading must dominate. If coexists_with: the kernel is a permanent multi-frame contest. If influences: this reading shifts the viability conditions for siblings without resolving the contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between option-value, catastrophic-tail, and expected-value readings of acceptable risk').

omega_variable(
    suppression_mechanism_regulatory_vs_market,
    'Is the constraint''s suppression primarily structural (regulatory barriers to pathway elimination) or market-mediated (capacity payments that make exit uneconomic)?',
    'Decompose suppression_requirement measurements into regulatory (licensing, reliability standards) and market (capacity market clearing prices, regulated cost recovery) components over the interval.',
    'If regulatory suppression dominates, the constraint is more amenable to policy reform; if market-mediated, it reflects deeper capital lock-in and may persist despite regulatory change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_regulatory_vs_market, empirical, 'Regulatory vs market-mediated suppression in energy pathway preservation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 1975, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1975, acceptable_risk_energy__option_value_preserving, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(acce_tr_t1990, acceptable_risk_energy__option_value_preserving, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_energy__option_value_preserving, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(acce_tr_t2015, acceptable_risk_energy__option_value_preserving, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(acce_tr_t2025, acceptable_risk_energy__option_value_preserving, theater_ratio, 2025, 0.25).
narrative_ontology:measurement(acce_tr_t2035, acceptable_risk_energy__option_value_preserving, theater_ratio, 2035, 0.3).

% Extraction over time
narrative_ontology:measurement(acce_be_t1975, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(acce_be_t1990, acceptable_risk_energy__option_value_preserving, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(acce_be_t2015, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(acce_be_t2025, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement(acce_be_t2035, acceptable_risk_energy__option_value_preserving, base_extractiveness, 2035, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1975, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1975, 0.3).
narrative_ontology:measurement(acce_su_t1990, acceptable_risk_energy__option_value_preserving, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(acce_su_t2015, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(acce_su_t2025, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement(acce_su_t2035, acceptable_risk_energy__option_value_preserving, suppression_requirement, 2035, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, nuclear_relicensing_regime).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, capacity_market_design).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, renewable_interconnection_queue).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, climate_adaptation_funding).

% DUAL FORMULATION NOTE:
% Part of the acceptable_risk_energy constraint family with catastrophic_tail_dominant and expected_value_dominant readings. This reading preserves both nuclear and fossil pathways; catastrophic_tail_dominant eliminates nuclear; expected_value_dominant eliminates coal/gas. The three readings share the kernel 'acceptable risk' but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, organized, 0.8).
constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, powerful, 0.65).
constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, moderate, 0.7).
constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
