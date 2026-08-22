% ============================================================================
% CONSTRAINT STORY: climate_response_action__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__mitigation_priority, []).

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
 *   constraint_id: climate_response_action__mitigation_priority
 *   human_readable: Growth-Compatible Mitigation Priority (Below-2°C via Emissions Reduction, Innovation, and Carbon Markets)
 *   domain: Climate Policy / Political Economy / Intergenerational Ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation_priority reading of the
 *   climate_response_action kernel: the dominant post-Rio, post-Paris
 *   framework that pursues below-2°C warming through emissions reductions,
 *   technological substitution, and carbon markets, while treating continued
 *   GDP growth as a non-negotiable constraint on any acceptable policy
 *   pathway. This is a distinct constraint from the sibling readings, not an
 *   observable variant of one constraint — the adaptation_priority reading
 *   accepts warming as inevitable and reallocates resources to resilience;
 *   the degrowth_transformation reading rejects the growth constraint itself
 *   as the thing to be abandoned. Each reading has its own beneficiary/victim
 *   structure and its own epsilon; this file's epsilon (0.63) describes the
 *   mitigation_priority arrangement as it actually operates — a negotiated
 *   compromise that has allowed real coordination (a shared target, real
 *   capital flows to renewables) to coexist with real extraction (offset
 *   markets that launder continued emissions, cost-deferral onto the Global
 *   South and the future).
 *
 * KEY AGENTS:
 *   - innovation_capacity_economies: primary beneficiary (institutional/arbitrage) — captures technology rents and sets negotiation terms
 *   - carbon_market_intermediaries: beneficiary (organized/mobile) — collects transaction fees independent of real abatement
 *   - incumbent_high_emitting_industries: beneficiary-payer (powerful/constrained) — buys time and compliance flexibility via offsets
 *   - global_south_frontline_states: primary payer (moderate/trapped) — accepts deferred adaptation financing as price of participation
 *   - future_generations: primary payer (powerless/trapped, civilizational horizon) — bears risk of unproven negative-emissions technology
 *   - informal_carbon_offset_communities: payer (powerless/trapped) — bears local costs of offset-project land enrollment
 *   - small_island_developing_states: excluded/payer (powerless/trapped) — 2°C target set by parties for whom it is survivable
 *   - multilateral_climate_institutions: agenda_setter (institutional/analytical) — administers and has institutional stake in this reading's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__mitigation_priority, 0.63).
domain_priors:suppression_score(climate_response_action__mitigation_priority, 0.52).
domain_priors:theater_ratio(climate_response_action__mitigation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, extractiveness, 0.63).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_action__mitigation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__mitigation_priority, "Growth-Compatible Mitigation Priority (Below-2°C via Emissions Reduction, Innovation, and Carbon Markets)").
narrative_ontology:topic_domain(climate_response_action__mitigation_priority, "Climate Policy / Political Economy / Intergenerational Ethics").

domain_priors:requires_active_enforcement(climate_response_action__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__mitigation_priority, '322401e0-5048-4594-9331-dac0b40de3e6').
narrative_ontology:cs_kernel_codification('322401e0-5048-4594-9331-dac0b40de3e6', distributed).
narrative_ontology:cs_authority_grounding('322401e0-5048-4594-9331-dac0b40de3e6', distributed).
narrative_ontology:cs_reading_relation('322401e0-5048-4594-9331-dac0b40de3e6', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('322401e0-5048-4594-9331-dac0b40de3e6', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('322401e0-5048-4594-9331-dac0b40de3e6', foundational, growth_compatibility_is_a_binding_constraint).
narrative_ontology:cs_axiom_status(growth_compatibility_is_a_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('322401e0-5048-4594-9331-dac0b40de3e6', growth_compatibility_is_a_binding_constraint, instrumental).
narrative_ontology:cs_axiom('322401e0-5048-4594-9331-dac0b40de3e6', foundational, technological_substitution_can_decouple_emissions_from_output).
narrative_ontology:cs_axiom_status(technological_substitution_can_decouple_emissions_from_output, holdable).
narrative_ontology:cs_axiom_grounding('322401e0-5048-4594-9331-dac0b40de3e6', technological_substitution_can_decouple_emissions_from_output, empirically_contingent).
narrative_ontology:cs_reference_frame('322401e0-5048-4594-9331-dac0b40de3e6', rio_kyoto_growth_compatible_consensus).
narrative_ontology:cs_drift_state('322401e0-5048-4594-9331-dac0b40de3e6', post_paris_implementation_gap_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('322401e0-5048-4594-9331-dac0b40de3e6', '').
narrative_ontology:cs_kernel_id(climate_response_action__mitigation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, innovation_capacity_economies).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, incumbent_high_emitting_industries).
narrative_ontology:constraint_beneficiary(climate_response_action__mitigation_priority, multilateral_climate_institutions).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, global_south_frontline_states).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, informal_carbon_offset_communities).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, small_island_developing_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_action__mitigation_priority, incumbent_high_emitting_industries).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, green_growth_decoupling_thesis).
narrative_ontology:constraint_vindicates(climate_response_action__mitigation_priority, carbon_pricing_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States and firms with mature R&D bases, patent portfolios in renewables, batteries, and carbon capture, and deep capital markets. They set the terms of the mitigation framework at COP negotiations and in bilateral technology-transfer deals, and capture rents from licensing green technology to states without comparable capacity. Their GDP growth continues largely uninterrupted because emissions cuts are financed through efficiency gains and offset purchases rather than output contraction.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, innovation_capacity_economies, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, innovation_capacity_economies, agenda_setter).

% Exchanges, verification bodies, project developers, and brokers who originate, certify, and trade offset credits and allowances. They collect fees and margins on every transaction regardless of whether the underlying emissions reduction is real; verification scandals (overcounted forest credits, phantom reductions) have not removed them from the transaction chain because the market's continued operation is their revenue base.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Fossil-intensive sectors (heavy industry, aviation, shipping) that face compliance costs under emissions caps but can purchase offsets or delay hard decarbonization through negotiated exemptions and transition timelines. They bear real but manageable costs while retaining the option to substitute purchased credits for structural change, which is a subsidy relative to the degrowth alternative that would foreclose their business model outright.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, incumbent_high_emitting_industries, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, incumbent_high_emitting_industries, payer).

% UNFCCC bodies, the IPCC's policy-facing summaries, and multilateral development banks that administer the below-2°C target, certify national pledges, and channel climate finance. Their institutional survival and mandate are tied to the mitigation-through-markets-and-innovation framework remaining the dominant reading; adopting degrowth or hard adaptation-first framing would require redesigning the institutions themselves.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, multilateral_climate_institutions, agenda_setter,
    institutional, generational, analytical, global).

% States with high climate vulnerability and low historical emissions who must accept the below-2°C mitigation architecture as the price of participating in climate finance mechanisms, even though it defers the adaptation investment they need now in favor of financing emissions cuts in the Global North and technology purchases they cannot afford. They have little leverage to renegotiate the framework's priorities.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, global_south_frontline_states, payer,
    moderate, generational, trapped, continental).

% Bear the residual warming, ecological damage, and any carbon-removal technology shortfall that the current framework assumes will be resolved later. They have no seat in current negotiations; the mitigation-priority reading's reliance on future negative-emissions technology at scale transfers risk directly onto them if that technology underperforms.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Indigenous and rural communities whose land is enrolled in forestry or conservation offset projects, often without full consent or benefit-sharing, so that distant emitters can claim compliance. Displacement, restricted land use, and unequal revenue distribution are the local cost of a credit that primarily benefits the purchasing firm's compliance ledger.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, informal_carbon_offset_communities, payer,
    powerless, biographical, trapped, regional).

% Face existential territorial loss under any warming trajectory materially above 1.5°C, yet the mitigation-priority framework's negotiated target of 'below 2°C' and its growth-compatibility constraint were set largely by parties for whom 2°C is survivable. Their preferred 1.5°C-with-loss-and-damage framing is acknowledged rhetorically but structurally subordinated to the growth-compatible pathway's timeline and cost allocation.
narrative_ontology:constraint_stakeholder(climate_response_action__mitigation_priority, small_island_developing_states, excluded,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(climate_response_action__mitigation_priority, small_island_developing_states, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_action__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common global target (below 2°C), a shared accounting and verification infrastructure (carbon markets, national pledges, MRV systems), and a channel for capital and technology to flow toward decarbonization without requiring any single actor to unilaterally absorb the full cost of transition.
% TRANSFER_FUNCTION: Moves near-term financial and technological resources toward innovation-capacity economies and market intermediaries (who supply the abatement technology and the trading infrastructure), while moving residual climate risk, adaptation costs, and land-use burdens onto the Global South, offset-hosting communities, and future generations who did not set the target or its cost-allocation terms.
% ABSENT_VOICES: Small island states pressing for a 1.5°C-binding target rather than 2°C; offset-hosting communities whose land is enrolled in credit-generating projects without full free, prior, and informed consent; future generations who bear the compounding risk of assumed-but-unproven negative-emissions technology at scale.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framework vanished overnight, the negotiated below-2°C target, the carbon market infrastructure, and the growth-compatibility assumption would all dissolve — national pledges would need to be renegotiated under a different premise (e.g., degrowth-transformation or adaptation-priority), carbon credit markets would lose their regulatory anchor, and climate finance flows currently routed through this architecture would have to be rebuilt around a different allocation of costs.
% FOUNDING_PROBLEM: By the early 1990s it was clear that unmitigated emissions growth risked catastrophic warming, but no political consensus existed for restructuring the global economy away from growth; the mitigation-priority framework was built to make emissions reduction politically and economically compatible with continued GDP growth in both developed and developing economies, using technology and market mechanisms as the compatibility device.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group III reports and independent climate economists outside the carbon-market industry attest that the growth-compatibility premise is increasingly strained by the gap between pledged mitigation and required emissions trajectories, and that negative-emissions technology is not yet deployed at the scale the framework assumes; multilateral institutions and innovation-economy governments continue to attest the framework remains adequate and merely needs faster implementation, not structural revision.
narrative_ontology:disappearance_verdict(climate_response_action__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__mitigation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__mitigation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_action__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__mitigation_priority, 0.63, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.63) reflects a genuine and rising gap between the framework's coordination promise (a shared global target, functioning capital flows to abatement technology) and its actual cost allocation: offset markets have repeatedly been shown to overstate real reductions (verified forestry and REDD+ credit scandals), and the growth-compatibility constraint means the hardest, most disruptive cuts are persistently deferred rather than made, with the deferral cost landing on regions and future actors with no seat in the framework's design. Suppression (0.52) is moderate rather than extreme: no single enforcer coerces compliance, but the framework's dominance in multilateral finance access means states effectively cannot access climate capital without accepting its terms, which functions as structural suppression of the sibling readings even without direct coercion. Theater ratio (0.44) and its rising trajectory track the growing share of the framework's activity that is compliance accounting and offset certification rather than physical emissions reduction — a rising Goodhart signature.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of innovation-capacity economies and multilateral institutions, this reads as functioning coordination: a shared target, credible finance mechanisms, technology diffusion. From the seat of Global South frontline states and offset-hosting communities, the same structure reads as extraction — their costs are structurally deferred or externalized so that the growth-compatibility constraint can be honored elsewhere. The engine computes these divergent seat classifications from the declared power/exit/scope data; this story does not force convergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Innovation-capacity economies and carbon market intermediaries sit near the full-beneficiary end: they collect rents (technology licensing, transaction fees) with mobile or arbitrage-grade exit and institutional/organized power. Incumbent high-emitting industries are a hybrid — real payers of compliance cost, but the offset market functions as a subsidized escape valve relative to the degrowth alternative, so their effective directionality sits closer to symmetric than a naive victim reading would suggest. Global South frontline states, informal offset-hosting communities, and future generations anchor the target end: trapped or powerless, they absorb costs (deferred adaptation finance, land dispossession, residual climate risk) generated by a framework whose terms they did not set.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no political will existed in the 1990s for degrowth, so mitigation had to be growth-compatible to be adoptable at all — was genuinely live at founding and remains partially live (political economies still resist growth-negative policy). But three decades of accumulating extraction (rising base_extractiveness, rising theater_ratio) alongside persistent failure to close the emissions gap suggest the arrangement has drifted from 'necessary political compromise' toward 'institutionalized cost-deferral machine.' Classifying this as tangled_rope rather than snare or rope preserves both halves: it is not pure extraction (real coordination and real technology deployment occur) and it is not pure coordination (the beneficiary/victim asymmetry is structural, not incidental, and requires active enforcement via finance-access conditionality to persist).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    negative_emissions_feasibility_ambiguity,
    'Is large-scale negative-emissions technology (direct air capture, enhanced weathering, BECCS) a genuinely feasible bridge that justifies deferring harder near-term cuts, or is it a speculative assumption that functions to license continued emissions now at the cost of future generations?',
    'Track deployed negative-emissions capacity against IPCC pathway assumptions over the next two decades; a persistent and widening gap between assumed and deployed capacity would indicate the feasibility assumption is being used as cover rather than as a credible plan.',
    'If technologically infeasible at required scale, the mitigation_priority reading''s effective extraction from future generations is substantially higher than currently measured, since the promised offset for delayed action would not materialize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negative_emissions_feasibility_ambiguity, empirical, 'Whether assumed future carbon removal is a credible bridge or a deferral mechanism.').

omega_variable(
    carbon_market_integrity_ambiguity,
    'Do carbon markets, on net, produce real additional emissions reductions, or do they primarily launder continued emissions through non-additional or overcounted credits?',
    'Independent auditing of offset project additionality across major registries (forestry, renewable energy, industrial gas) compared against counterfactual baseline emissions.',
    'If markets are predominantly non-additional, the beneficiary status of carbon_market_intermediaries and the coordination-function claim for the whole framework weaken substantially, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_market_integrity_ambiguity, empirical, 'Whether carbon markets deliver real abatement or mostly compliance theater.').

omega_variable(
    growth_compatibility_as_natural_constraint,
    'Is the requirement that climate policy remain GDP-growth-compatible a fixed political/economic reality that any workable framework must respect, or a constructed constraint that primarily protects incumbent economic beneficiaries from having to accept contraction?',
    'Comparative study of economies that have pursued sufficiency-oriented or post-growth policy experiments (e.g., municipal or national degrowth pilots) to assess whether growth-compatibility is empirically necessary for political durability or an artifact of incumbent preference.',
    'If constructed rather than necessary, the mitigation_priority reading''s core distinguishing axiom (growth-compatibility) loses its claim to being a neutral constraint and reads instead as a beneficiary-protecting design choice, strengthening the case for reclassification toward snare at the level of the growth constraint itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_compatibility_as_natural_constraint, conceptual, 'Whether GDP-growth-compatibility is a real constraint or a beneficiary-protective framing choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__mitigation_priority, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_action__mitigation_priority, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(clim_tr_t1997, climate_response_action__mitigation_priority, theater_ratio, 1997, 0.25).
narrative_ontology:measurement(clim_tr_t2005, climate_response_action__mitigation_priority, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(clim_tr_t2015, climate_response_action__mitigation_priority, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(clim_tr_t2020, climate_response_action__mitigation_priority, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2024, climate_response_action__mitigation_priority, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_action__mitigation_priority, base_extractiveness, 1992, 0.32).
narrative_ontology:measurement(clim_be_t1997, climate_response_action__mitigation_priority, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(clim_be_t2005, climate_response_action__mitigation_priority, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(clim_be_t2015, climate_response_action__mitigation_priority, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(clim_be_t2020, climate_response_action__mitigation_priority, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(clim_be_t2024, climate_response_action__mitigation_priority, base_extractiveness, 2024, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_action__mitigation_priority, suppression_requirement, 1992, 0.3).
narrative_ontology:measurement(clim_su_t1997, climate_response_action__mitigation_priority, suppression_requirement, 1997, 0.34).
narrative_ontology:measurement(clim_su_t2005, climate_response_action__mitigation_priority, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(clim_su_t2015, climate_response_action__mitigation_priority, suppression_requirement, 2015, 0.44).
narrative_ontology:measurement(clim_su_t2020, climate_response_action__mitigation_priority, suppression_requirement, 2020, 0.48).
narrative_ontology:measurement(clim_su_t2024, climate_response_action__mitigation_priority, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__mitigation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__mitigation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_response_action kernel. mitigation_priority (this file) concentrates near-term cost on emissions-reducing sectors and defers adaptation and residual-risk cost to the Global South and future generations, while preserving growth. adaptation_priority accepts warming and reallocates toward resilience investment, producing a different beneficiary/victim split (protecting vulnerable populations now at the cost of deferred mitigation ambition). degrowth_transformation rejects the growth-compatibility axiom entirely, producing a third distinct extraction profile (near-term consumption contraction in high-consuming economies, no reliance on speculative carbon removal). All three share the same underlying kernel commitment — that climate response requires SOME actionable priority — but instantiate structurally different constraints with different epsilon values; none is a measurement variant of another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
