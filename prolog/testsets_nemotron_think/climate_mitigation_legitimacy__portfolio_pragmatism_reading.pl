% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Technology-Neutral Portfolio Decarbonization Mandate
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The portfolio pragmatism reading of climate mitigation legitimacy asserts
 *   that optimal decarbonization requires a technology-neutral framework
 *   deploying both nuclear and renewable energy according to regional
 *   least-cost optimization. This reading emerged from integrated assessment
 *   modeling showing that single-technology pathways face higher cost and
 *   reliability risks. It is instantiated as a coordination constraint:
 *   governments and financial institutions adopt 'technology-neutral'
 *   criteria for clean energy subsidies, carbon credits, and permitting,
 *   intending to let market competition determine the optimal mix. In
 *   practice, the constraint's operation reveals tensions: nuclear's high
 *   capital costs and long lead times make it dependent on sustained policy
 *   support that renewables no longer need; 'neutrality' criteria are often
 *   defined by nuclear-advocating institutions; and developing nations face
 *   pressure to adopt nuclear despite unfavorable economics. The constraint
 *   coordinates genuine risk diversification but also extracts transition
 *   costs onto vulnerable communities while privileging incumbent nuclear
 *   institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.35).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.25).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Technology-Neutral Portfolio Decarbonization Mandate").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '01018a13-1343-4f5e-bf1d-b4c5355fbf11').
narrative_ontology:cs_kernel_codification('01018a13-1343-4f5e-bf1d-b4c5355fbf11', distributed).
narrative_ontology:cs_authority_grounding('01018a13-1343-4f5e-bf1d-b4c5355fbf11', expertise).
narrative_ontology:cs_interpretation_layer_present('01018a13-1343-4f5e-bf1d-b4c5355fbf11').
narrative_ontology:cs_reading_relation('01018a13-1343-4f5e-bf1d-b4c5355fbf11', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('01018a13-1343-4f5e-bf1d-b4c5355fbf11', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('01018a13-1343-4f5e-bf1d-b4c5355fbf11', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('01018a13-1343-4f5e-bf1d-b4c5355fbf11', foundational, technology_neutrality_principle).
narrative_ontology:cs_axiom_status(technology_neutrality_principle, holdable).
narrative_ontology:cs_axiom_grounding('01018a13-1343-4f5e-bf1d-b4c5355fbf11', technology_neutrality_principle, empirically_contingent).
narrative_ontology:cs_axiom('01018a13-1343-4f5e-bf1d-b4c5355fbf11', foundational, regional_optimization_principle).
narrative_ontology:cs_axiom_status(regional_optimization_principle, holdable).
narrative_ontology:cs_axiom_grounding('01018a13-1343-4f5e-bf1d-b4c5355fbf11', regional_optimization_principle, empirically_contingent).
narrative_ontology:cs_axiom('01018a13-1343-4f5e-bf1d-b4c5355fbf11', secondary, portfolio_diversification_hedges_uncertainty).
narrative_ontology:cs_axiom_status(portfolio_diversification_hedges_uncertainty, holdable).
narrative_ontology:cs_axiom_grounding('01018a13-1343-4f5e-bf1d-b4c5355fbf11', portfolio_diversification_hedges_uncertainty, empirically_contingent).
narrative_ontology:cs_reference_frame('01018a13-1343-4f5e-bf1d-b4c5355fbf11', integrated_assessment_modeling_consensus).
narrative_ontology:cs_drift_state('01018a13-1343-4f5e-bf1d-b4c5355fbf11', post_ira_2025, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('01018a13-1343-4f5e-bf1d-b4c5355fbf11', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_agnostic_project_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_workers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_in_high_cost_regions).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_dependent_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, international_financial_institutions).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_neutrality_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, regional_optimization_over_ideological_purity).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__portfolio_pragmatism_reading, portfolio_diversification_hedges_uncertainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains policy legitimacy and access to decarbonization funding streams previously reserved for renewables. Must still compete on cost and deployment speed; high capital requirements and long lead times constrain exit from the constraint's requirements.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, global).

% Retains dominant position in new capacity additions but loses exclusive claim on decarbonization funding. Mobile exit options through established supply chains and falling cost curves; can adapt to technology-neutral frameworks without existential threat.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_industry, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_industry, agenda_setter).

% Gains planning flexibility to optimize system reliability and cost across technology options. Identity-locked because grid reliability mandate makes them the institutional anchor of any decarbonization framework; cannot exit the constraint without abandoning core mission.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, grid_operators, beneficiary,
    institutional, biographical, identity_locked, national).

% Can deploy whichever technology optimizes project economics in each market. Mobile exit options — if portfolio mandates become prescriptive in practice, can shift capital to regions with genuine technology neutrality.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, technology_agnostic_project_developers, beneficiary,
    moderate, biographical, mobile, global).

% Bear transition costs (job loss, community decline) without guaranteed place in new portfolio. Trapped by geographic concentration, skill specificity, and political economy of transition; the constraint's technology neutrality does not address their displacement.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_workers, payer,
    organized, biographical, trapped, regional).

% Absorb cost premiums where portfolio mandates force deployment of higher-cost technologies (e.g., nuclear in markets without supply chains, renewables without storage). Constrained exit — cannot easily change utility service territory.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, ratepayers_in_high_cost_regions, payer,
    powerless, immediate, constrained, local).

% Face structural economic decline as constraint accelerates fossil phase-out without commensurate investment in replacement industries. Trapped by place-based assets and limited mobility.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_dependent_communities, payer,
    powerless, generational, trapped, regional).

% Set portfolio targets, allocate subsidies, and define 'technology neutrality' in practice. Arbitrage exit — can shift policy emphasis between nuclear and renewables based on domestic industrial policy goals.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, national_governments, agenda_setter,
    institutional, biographical, arbitrage, national).

% Channel climate finance through technology-neutral frameworks, gaining leverage over national energy policies. Benefit from expanded lending portfolio; arbitrage across borrower countries.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, international_financial_institutions, beneficiary).

% Monitor whether portfolio approach delivers actual emissions reductions or becomes cover for delayed action. Analytical exit — can shift advocacy to alternative framings (renewable primacy, degrowth) if portfolio underperforms.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_advocacy_organizations, observer,
    organized, generational, analytical, global).

% Face portfolio mandates from international finance that may not match least-cost domestic pathways (e.g., nuclear pushed where renewables+storage are cheaper). Constrained by finance access; would object to one-size-fits-all technology neutrality.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, developing_nation_energy_ministries, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global capital allocation and policy support across competing low-carbon technologies to hedge against single-technology failure risk and exploit regional comparative advantage.
% TRANSFER_FUNCTION: Moves public subsidies, carbon finance, and regulatory preference from fossil fuels toward both nuclear and renewable deployment proportionally to assessed system value in each region.
% ABSENT_VOICES: Communities hosting nuclear waste or renewable extraction (lithium, cobalt); indigenous peoples affected by large-scale energy infrastructure; fossil fuel workers without transition pathways; developing nations pressured into technology choices that serve export industries rather than domestic least-cost decarbonization.
% DISAPPEARANCE_RATIONALE: If the technology-neutral portfolio mandate vanished, climate finance would revert to technology-specific silos (renewables-only or nuclear-only), stranded asset risk would concentrate in whichever technology loses policy favor, and regional optimization would be replaced by ideological or industrial policy-driven deployment.
% FOUNDING_PROBLEM: Single-technology bets (renewables-only or nuclear-only) create unacceptable risk of decarbonization failure due to resource limits, supply chain bottlenecks, or integration challenges; a portfolio approach hedges across uncertainties.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) emphasizes portfolio approaches; IEA Net Zero by 2050 (2021, 2023 updates) models significant roles for both nuclear and renewables; National Academies (US) and similar bodies in EU/UK/Japan confirm multi-technology necessity. Corroboration comes from assessment bodies outside the direct beneficiary industries.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).
:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects moderate asymmetric costs: fossil workers and ratepayers bear transition burdens while nuclear and renewable industries capture policy rents. Suppression (0.25) is low-moderate — the constraint operates through financial incentives and permitting reform, not direct coercion, but financial exclusion of non-compliant projects is real. Theater ratio (0.15) is low because the coordination function (risk hedging) is genuine and actively used by planners. Accessibility collapse (0.45) is moderate — alternative framings (renewable primacy, degrowth) remain live but are marginalized in formal finance channels. Resistance (0.55) is significant from both renewable purists and nuclear skeptics, plus affected communities. The measurement series shows extractiveness and suppression rising 2015-2030 as portfolio mandates embed in climate finance, then stabilizing as the framework matures.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear industry seat, this constraint is a rope — it solves the coordination problem of gaining equal policy footing. From fossil worker seats, it is a snare — extraction without voice. From grid operator seats, it is genuine coordination (rope). From developing nation seats, it is a tangled rope — coordination imposed by external finance with asymmetric terms. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear industry and grid operators are structural beneficiaries (d near 0.1-0.2) — they gain policy access and planning authority. Renewable industry is near-symmetric beneficiary (d ~0.3) — gains from overall decarbonization push but loses exclusive subsidy claim. Fossil fuel workers and dependent communities are full targets (d ~0.85-0.95) — bear costs with minimal voice. Ratepayers in high-cost regions are strong targets (d ~0.75). Governments and IFIs are agenda-setters with arbitrage exit (d ~0.1). Climate advocates are analytical observers (d ~0.5). Developing nations are excluded with constrained exit (d ~0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (single-technology risk) remains live per IPCC/IEA corroboration. However, if renewables+storage achieve deep decarbonization cheaper than nuclear in most regions, the portfolio mandate's nuclear component becomes extractive mandatrophy — maintained by nuclear industry lobbying and institutional inertia rather than system need. The constraint currently sits at the rope/tangled_rope boundary; the engine's computed type will reveal whether coordination or extraction dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_neutrality_achievability,
    'Can technology neutrality be achieved in practice given entrenched nuclear supply chains, regulatory frameworks, and institutional path dependencies that favor incumbent technologies?',
    'Track deployment outcomes in jurisdictions with explicit technology-neutral policies (e.g., UK CfD auctions, US IRA tax credits) — does nuclear deployment increase without targeted subsidies, or does ''neutrality'' require nuclear-specific support?',
    'If neutrality is unachievable without nuclear-specific subsidies, the constraint''s extractiveness is understated — the portfolio mandate becomes a vehicle for nuclear rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_neutrality_achievability, empirical, 'Whether technology-neutral policy frameworks can overcome nuclear''s structural disadvantages without becoming de facto nuclear subsidies.').

omega_variable(
    capital_diversification_extraction_boundary,
    'Does ''moderate capital diversification'' into nuclear extract capital from faster/cheaper decarbonization via renewables, given nuclear''s high capital intensity and long lead times?',
    'Compare marginal abatement cost curves under portfolio mandates vs. renewable-only scenarios across regions; measure opportunity cost of capital allocated to nuclear that could have deployed more renewables faster.',
    'If portfolio mandates systematically divert capital from higher-return renewable deployment, the constraint''s extraction from ratepayers and climate mitigation effectiveness increases substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_diversification_extraction_boundary, conceptual, 'Whether the coordination benefit of diversification is outweighed by the opportunity cost of nuclear capital intensity.').

omega_variable(
    committer_structure_portfolio_pragmatism,
    'How does this reading''s structural relationship to the climate_mitigation_legitimacy kernel differ from its sibling readings?',
    'Compare the four readings'' beneficiary/victim structures, claimed coordination functions, and empirical corroboration bases to map the kernel''s contestation geometry.',
    'If portfolio pragmatism''s beneficiary set (nuclear + renewables) is strictly broader than siblings'', it may be a meta-coordination reading that absorbs rather than resolves the kernel''s contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_portfolio_pragmatism, conceptual, 'This constraint is one reading of the contested kernel climate_mitigation_legitimacy. Sibling readings: baseload_necessity_reading, renewable_primacy_reading, degrowth_sufficiency_reading. This reading''s distinguishing structural delta: neither technology privileged a priori; regional variation in optimal mix; moderate capital diversification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_tr_t2015, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_tr_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_tr_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2025, 0.14).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_tr_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2030, 0.15).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_tr_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2040, 0.15).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_tr_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 2050, 0.15).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_be_t2015, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2015, 0.2).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_be_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2020, 0.25).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_be_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2025, 0.3).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_be_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2030, 0.33).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_be_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2040, 0.35).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_be_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 2050, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_su_t2015, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_su_t2020, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2020, 0.2).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_su_t2025, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2025, 0.23).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_su_t2030, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2030, 0.25).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_su_t2040, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2040, 0.25).
narrative_ontology:measurement(climate_mitigation_portfolio_pragmatism_su_t2050, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 2050, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_finance_architecture).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_licensing_reform).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_permitting_streamlining).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, just_transition_funding).

% DUAL FORMULATION NOTE:
% Part of the climate_mitigation_legitimacy constraint family. This reading (portfolio_pragmatism) coexists with baseload_necessity_reading (nuclear essential), renewable_primacy_reading (renewables sufficient), and degrowth_sufficiency_reading (demand reduction primary). All four share the kernel 'what legitimizes a decarbonization pathway?' but instantiate different constraints with different beneficiary/victim structures and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
