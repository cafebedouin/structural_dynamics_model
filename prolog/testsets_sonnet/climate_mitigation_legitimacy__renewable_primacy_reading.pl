% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable-Primacy Reading of Climate Mitigation Legitimacy
 *   domain: energy policy / climate mitigation / technology governance
 *
 * SUMMARY:
 *   This constraint captures the renewable-primacy claim within the larger
 *   contested kernel of 'climate mitigation legitimacy': that renewables plus
 *   storage can decarbonize grids faster and more cheaply than nuclear
 *   expansion, and that policy, capital, and permitting priority should be
 *   organized around that premise. The claim functions as a coordination
 *   device for capital allocation and standard-setting, but it also
 *   structurally disadvantages nuclear construction firms, nuclear-dependent
 *   regions, and their workforces, whose multi-decade capital cycles and
 *   existing plans lose legitimacy and financing priority under this framing.
 *   The reading is one of four live readings of the same kernel —
 *   baseload_necessity, portfolio_pragmatism, and degrowth_sufficiency are
 *   separate constraints, not alternate measurements of this one; per the
 *   ε-invariance principle each carries its own ε, beneficiary/victim
 *   structure, and classification.
 *
 * KEY AGENTS:
 *   - utility_scale_solar_developers: Primary beneficiary (organized/mobile) — captures policy and capital priority under this framing
 *   - battery_storage_manufacturers: Beneficiary (organized/mobile) — market share depends on storage-substitutes-baseload doctrine
 *   - nuclear_construction_firms: Primary target (powerful/constrained) — stranded capital and licensing delay under this framing
 *   - nuclear_dependent_grid_regions: Secondary target (moderate/trapped) — reliability exposure from deprioritized firming capacity
 *   - existing_nuclear_workforce: Diffuse target (powerless/trapped) — career and community disruption
 *   - grid_reliability_regulators: Analytical observer (institutional/analytical) — adjudicates between competing technology claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.52).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.4).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable-Primacy Reading of Climate Mitigation Legitimacy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy policy / climate mitigation / technology governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'd0b2b3b1-d786-46b6-a8fb-1788e04eb370').
narrative_ontology:cs_kernel_codification('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', distributed).
narrative_ontology:cs_authority_grounding('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', distributed).
narrative_ontology:cs_reading_relation('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', foundational, storage_backed_variable_generation_is_sufficient_for_reliability).
narrative_ontology:cs_axiom_status(storage_backed_variable_generation_is_sufficient_for_reliability, holdable).
narrative_ontology:cs_axiom_grounding('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', storage_backed_variable_generation_is_sufficient_for_reliability, empirically_contingent).
narrative_ontology:cs_axiom('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', foundational, shorter_capital_cycles_dominate_decarbonization_speed_calculus).
narrative_ontology:cs_axiom_status(shorter_capital_cycles_dominate_decarbonization_speed_calculus, holdable).
narrative_ontology:cs_axiom_grounding('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', shorter_capital_cycles_dominate_decarbonization_speed_calculus, instrumental).
narrative_ontology:cs_reference_frame('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', cost_declining_renewable_deployment_regime).
narrative_ontology:cs_drift_state('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', post_2020s_grid_reliability_stress_events, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0b2b3b1-d786-46b6-a8fb-1788e04eb370', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, grid_scale_wind_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_advocacy_groups).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_construction_firms).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_dependent_grid_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, existing_nuclear_workforce).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, capital_locked_utility_ratepayers).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, levelized_cost_of_energy_supremacy_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__renewable_primacy_reading, storage_learning_curve_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build large solar and wind portfolios financed on short capital cycles; benefit directly when policy, subsidy allocation, and interconnection queues privilege renewable-plus-storage projects over nuclear licensing tracks. Can redeploy capital across jurisdictions quickly if a market turns unfavorable.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_solar_developers, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, utility_scale_solar_developers, agenda_setter).

% Sell storage capacity whose market share depends on the premise that storage substitutes for baseload rather than merely supplementing it. Global supply chains give them exit if any single jurisdiction reweights toward nuclear.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, battery_storage_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Operate wind assets whose interconnection priority and capacity-market treatment improve when regulators adopt the renewable-primacy framing over a portfolio or baseload framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_scale_wind_operators, beneficiary,
    organized, biographical, mobile, regional).

% Lobby regulators and standard-setting bodies to encode LCOE comparisons and storage learning-curve projections into official decarbonization pathways, shaping which capital gets legitimated as 'the fast, cheap route' and which is framed as a stranded-asset risk.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_advocacy_groups, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, distributed_energy_advocacy_groups, agenda_setter).

% Carry multi-decade capital commitments and licensing timelines that this reading treats as a delay cost rather than a decarbonization asset. When policy capital and political attention shift to renewables-plus-storage as the legitimate pathway, financing, permitting priority, and public support for new nuclear projects erode, stranding sunk engineering and site investment.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_construction_firms, payer,
    powerful, generational, constrained, national).

% Regions whose grids were built around existing or planned nuclear baseload face reliability and cost exposure if renewable-primacy framing diverts investment away from firming capacity their geography or industrial base actually needs. Cannot easily relocate the grid or its industrial customers.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_dependent_grid_regions, payer,
    moderate, biographical, trapped, regional).

% Skilled workers in plant construction, operation, and the specialized supply chain face career and community disruption when public and regulatory legitimacy shifts away from nuclear expansion; retraining into renewables is not always geographically or skill-wise available.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, existing_nuclear_workforce, payer,
    powerless, biographical, trapped, local).

% Ratepayers in jurisdictions with partially built or recently cancelled nuclear projects absorb stranded-cost surcharges when the renewable-primacy framing prevails and nuclear projects are abandoned mid-construction, without having chosen either technology path themselves.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, capital_locked_utility_ratepayers, payer,
    powerless, biographical, trapped, regional).

% Evaluate resource-adequacy and reliability filings across technology pathways; must adjudicate between competing LCOE, capacity-value, and reliability claims from the renewable-primacy and baseload-necessity camps without an uncontested empirical standard.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_reliability_regulators, observer,
    institutional, generational, analytical, national).

% Argue dispatchable nuclear baseload is structurally necessary at scale and that renewable-primacy framing undercounts firming, transmission, and land-use costs. Present in the broader policy debate but this reading's own institutional framing treats their position as the alternative to be displaced, not a co-equal input.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_necessity_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__renewable_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__renewable_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation, permitting priority, and grid-planning standards around a single technology pathway (renewables plus storage) so that finance, supply chains, and regulatory timelines can move in a common direction without hedging across incompatible generation paradigms.
% TRANSFER_FUNCTION: Moves policy attention, subsidy allocation, interconnection queue priority, and public legitimacy away from nuclear capital projects and existing nuclear-dependent regions, toward renewable and storage developers and the jurisdictions and advocacy networks that have built expertise and market position around that pathway.
% ABSENT_VOICES: Nuclear engineers, plant communities, and baseload-necessity advocates raise reliability and land-use-intensity objections but are structurally positioned as the incumbent orthodoxy to be displaced rather than as co-equal parties in the standard-setting process that ratifies LCOE and storage-learning-curve doctrine.
% DISAPPEARANCE_RATIONALE: If the renewable-primacy framing lost its grip on policy and financial legitimacy overnight, stranded renewable project pipelines dependent on favorable interconnection queues would lose priority, capital would re-flow toward nuclear licensing and portfolio approaches, and regions currently deprioritizing baseload investment would face a scramble to re-secure firm capacity — the allocation of decarbonization capital would visibly reorganize.
% FOUNDING_PROBLEM: Decarbonization requires deploying enormous new generation and storage capacity within a narrow window; renewable-primacy framing was built to concentrate scarce capital, permitting attention, and political will onto the pathway its proponents judged fastest and cheapest per ton of carbon avoided, rather than diffusing effort across competing technologies.
% FOUNDING_PROBLEM_CORROBORATION: Renewable industry associations and allied climate policy institutes attest the founding problem (urgency plus cost-effectiveness) remains live and renewables-plus-storage remains the fastest path. Independent grid reliability studies from transmission operators and some national laboratories, sitting outside both the renewable and nuclear industry associations, report that storage-only firming assumptions embedded in this framing understate multi-day and seasonal reliability gaps in several grid geographies — corroboration for the founding problem's continued urgency is strong, but corroboration for the specific renewables-plus-storage-sufficiency claim is contested by parties outside the beneficiary set.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52) because the reading transfers real policy capital and legitimacy away from nuclear-committed actors toward renewable and storage developers, but this transfer runs through genuine coordination value (concentrating capital on a fast-moving decarbonization pathway), not naked rent extraction — hence tangled_rope rather than snare. Suppression (0.4) reflects the institutional and rhetorical work required to keep nuclear framed as slow/expensive rather than as a co-equal pathway; it is moderate, not severe, because nuclear advocates retain organized voice and some regulatory standing. Theater ratio rises modestly over the interval (0.15→0.28) as LCOE-supremacy and storage-learning-curve doctrines increasingly function as legitimating narrative layered atop genuine cost declines. Accessibility collapse is moderate (0.35): alternative framings (portfolio, baseload-necessity) remain institutionally live, unlike a true mountain where alternatives vanish. Resistance is substantial (0.62) because nuclear-aligned actors actively contest the framing in regulatory and legislative venues.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (solar/wind/storage developers, advocacy groups) this reads as legitimate technical coordination around the fastest, cheapest decarbonization pathway. From the payer seats (nuclear firms, dependent regions, workforce, stranded ratepayers) the same institutional momentum reads as an enforced technology preference that forecloses their capital and career investments regardless of site-specific reliability needs. The engine computes these as different per-seat types from the same structural data; the divergence is the finding, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (solar/wind/storage developers, advocacy groups) sit near the subsidy end of directionality: their capital cycles align with and are reinforced by the framing's institutional adoption, and they retain mobile or arbitrage-grade exit if a jurisdiction reweights. Victims (nuclear firms, dependent regions, workforce, ratepayers) sit near the target end: their capital and careers are structurally locked into multi-decade nuclear timelines that lose legitimacy and financing priority under this framing, and their exit options range from constrained (firms can pursue projects elsewhere) to trapped (workforce and ratepayers cannot relocate their sunk investment or community).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — urgent, capital-constrained decarbonization — remains live by nearly all accounts; what is contested is whether the specific renewable-primacy solution to that problem is itself still correctly calibrated or has hardened into an institutional orthodoxy that persists partly because incumbent renewable/storage capital now depends on its continued legitimacy. Classifying this as tangled_rope rather than snare or mountain prevents two errors: treating it as pure extraction (it does solve a real coordination problem — concentrating capital fast) and treating it as natural law (it is a contested empirical and policy claim with an identifiable beneficiary structure and organized opposition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lcoe_versus_system_cost_ambiguity,
    'Does levelized cost of energy (LCOE) comparison — which favors renewables-plus-storage — capture the true system cost of full decarbonization, or does it systematically undercount firming, transmission, and multi-day/seasonal storage costs that would favor a baseload-inclusive portfolio?',
    'Full system-cost modeling (not marginal LCOE) across multiple grid geographies and weather years, ideally conducted by parties without a stake in either technology''s market share, comparing realized decarbonization cost and reliability outcomes.',
    'If LCOE comparison systematically undercounts system-level costs, this reading''s core empirical claim weakens substantially and the constraint''s classification shifts toward a more extractive framing (the coordination benefit is smaller than claimed relative to the capital diverted from nuclear). If LCOE tracks true system cost well, the coordination function is more clearly genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lcoe_versus_system_cost_ambiguity, empirical, 'Whether the core cost-comparison metric underlying this reading is a fair system-cost proxy or a metric-substitution favoring renewables.').

omega_variable(
    capital_cycle_framing_versus_stranded_asset_framing,
    'Is treating nuclear''s long capital cycle as a ''delay cost'' the correct framing, or does it improperly discount the decades-long decarbonization value nuclear plants deliver once built, making short renewable capital cycles look artificially superior by comparing apples (fast deployment) to oranges (total lifecycle decarbonization delivered)?',
    'Lifecycle carbon-abatement-per-dollar analysis normalized across differing plant lifespans and capacity factors, cross-checked against realized (not projected) construction timelines for both technology classes in comparable regulatory environments.',
    'If nuclear''s longer lifecycle offsets its slower deployment when properly normalized, the ''nuclear as capital sink'' framing central to this reading is partly an artifact of time-horizon selection rather than a structural fact, weakening the case for treating nuclear firms as victims of a purely extractive dynamic and supporting a portfolio_pragmatism reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_cycle_framing_versus_stranded_asset_framing, conceptual, 'Whether the delay-cost framing of nuclear capital cycles is structurally fair or a framing choice that favors this reading.').

omega_variable(
    cs_framing_kernel_versus_institution,
    'Is the correct kernel here the technical decarbonization-pathway claim itself, or the deeper legitimacy narrative of cost-effectiveness and urgency that both renewable and nuclear advocates invoke to justify their preferred institutional authority over capital allocation?',
    'Trace whether disputes in regulatory proceedings center on the technical claim (LCOE, storage learning curves) or on the meta-level legitimacy of which expert community''s cost projections should govern resource-adequacy standards.',
    'If the deeper legitimacy narrative is the true kernel, this reading''s axioms may need reformulation around institutional epistemic authority rather than pure cost-comparison; the current framing (technical claim as kernel) was chosen because that is where the manifest-declared reading contest is explicitly located, but the alternative framing would shift which axioms are foundational versus secondary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_versus_institution, conceptual, 'Whether the kernel is best modeled as the technical cost claim or the underlying institutional legitimacy claim it rides on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 4, 0.28).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__renewable_primacy_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the climate_mitigation_legitimacy kernel, each authored as a separate constraint story per the ε-invariance principle: renewable_primacy_reading (this story — renewables+storage sufficiency, nuclear as capital sink), baseload_necessity_reading (dispatchable nuclear as structurally required), portfolio_pragmatism_reading (technology-neutral mix optimal), and degrowth_sufficiency_reading (demand reduction obviates the generation race). Each carries a distinct ε, beneficiary/victim structure, and classification rather than being a measurement variant of a single constraint. The readings are linked bidirectionally via affects_constraints to support contamination and legitimacy-contest propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
