% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Mitigation-Priority Climate Response Obligation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'mitigation_priority' reading of
 *   the climate_response_obligation kernel. The reading holds that
 *   intergenerational justice requires rapid decarbonization to minimize
 *   warming, treating the carbon budget as a hard constraint that allocates
 *   remaining emissions overwhelmingly to essential needs and historical
 *   emitters. The constraint operates as a tangled rope: it solves a genuine
 *   planetary-scale coordination problem (finite atmospheric commons) while
 *   extracting transition costs from the current generation —
 *   disproportionately from the Global North and fossil capital — to benefit
 *   future generations who cannot reciprocate. The engine will compute
 *   per-seat classifications from the structural data below; the claimed_type
 *   (tangled_rope) is the author's structural judgment, independent of the
 *   authored metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.62).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Mitigation-Priority Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'ba927bda-675d-451d-b5ea-5a26b2d243f6').
narrative_ontology:cs_kernel_codification('ba927bda-675d-451d-b5ea-5a26b2d243f6', formalized).
narrative_ontology:cs_authority_grounding('ba927bda-675d-451d-b5ea-5a26b2d243f6', lineage).
narrative_ontology:cs_interpretation_layer_present('ba927bda-675d-451d-b5ea-5a26b2d243f6').
narrative_ontology:cs_reading_relation('ba927bda-675d-451d-b5ea-5a26b2d243f6', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('ba927bda-675d-451d-b5ea-5a26b2d243f6', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('ba927bda-675d-451d-b5ea-5a26b2d243f6', foundational, intergenerational_justice_requires_minimizing_warming).
narrative_ontology:cs_axiom_status(intergenerational_justice_requires_minimizing_warming, holdable).
narrative_ontology:cs_axiom_grounding('ba927bda-675d-451d-b5ea-5a26b2d243f6', intergenerational_justice_requires_minimizing_warming, deontological).
narrative_ontology:cs_axiom('ba927bda-675d-451d-b5ea-5a26b2d243f6', foundational, historical_emissions_create_disproportionate_mitigation_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_create_disproportionate_mitigation_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ba927bda-675d-451d-b5ea-5a26b2d243f6', historical_emissions_create_disproportionate_mitigation_obligation, conventional).
narrative_ontology:cs_axiom('ba927bda-675d-451d-b5ea-5a26b2d243f6', secondary, carbon_budget_is_finite_and_non_negotiable).
narrative_ontology:cs_axiom_status(carbon_budget_is_finite_and_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('ba927bda-675d-451d-b5ea-5a26b2d243f6', carbon_budget_is_finite_and_non_negotiable, empirically_contingent).
narrative_ontology:cs_reference_frame('ba927bda-675d-451d-b5ea-5a26b2d243f6', pre_industrial_carbon_budget_integrity).
narrative_ontology:cs_drift_state('ba927bda-675d-451d-b5ea-5a26b2d243f6', post_paris_agreement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ba927bda-675d-451d-b5ea-5a26b2d243f6', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, current_generation_transition_bearers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_north_mitigation_bearers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_capital).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, current_generation_transition_bearers).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_principle).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, carbon_budget_finitude).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, historical_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the climate system stabilized by today's mitigation. They cannot act, negotiate, or exit the arrangement; they are the ultimate beneficiaries of a constraint they had no hand in creating. The constraint's legitimacy rests entirely on their structural position as the party for whom the arrangement is built.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Present-day populations in low-lying, arid, or heat-exposed regions who gain most from avoided warming. They bear the earliest and sharpest costs of any mitigation failure, but have no structural power to enforce the constraint and no exit from their geographic exposure.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% The living population that pays the transition costs: higher energy prices, labor market dislocation, consumption changes, and tax burdens to fund decarbonization. They also receive co-benefits (cleaner air, technology spillovers, avoided near-term damages). Exit is constrained — they cannot opt out of the energy system or the policy regime without severe personal cost.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, current_generation_transition_bearers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, current_generation_transition_bearers, beneficiary).

% Industrialized economies assigned disproportionate mitigation burden under historical responsibility frameworks. They possess the capital and technology to decarbonize but face domestic political resistance to cost-bearing. Exit is constrained by treaty architecture and reputational costs; arbitrage via carbon leakage is actively suppressed by border adjustment mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_north_mitigation_bearers, payer,
    institutional, biographical, constrained, global).

% Owners of fossil fuel reserves, extraction infrastructure, and carbon-intensive capital stock. The constraint renders a large fraction of their assets stranded. They deploy political influence to delay, weaken, or carve out the constraint; their exit options are constrained by the physical specificity of their assets (a coal plant cannot be repurposed as a solar farm) and by increasing financial sector exclusion.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_capital, payer,
    powerful, biographical, constrained, global).

% Developing economies arguing that mitigation-priority frameworks foreclose their remaining carbon space for poverty alleviation and industrialization. They are present in negotiations but structurally excluded from setting the mitigation agenda — the carbon budget is allocated before their claims are heard. Their exit from the constraint would mean rejecting the global climate regime entirely.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_development_claimants, excluded,
    organized, biographical, constrained, global).

% UNFCCC, IPCC, national climate ministries, carbon markets, and financial regulators that administer the constraint. They set mitigation targets, design policy instruments, verify compliance, and manage the transfer of resources. They benefit from the constraint's institutional architecture (mandates, budgets, staffing) but face pressure to demonstrate effectiveness.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_governance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Philosophers, climate ethicists, and long-term governance scholars who evaluate the constraint's moral architecture. They do not bear costs or collect benefits from the constraint's operation; they analyze its justification, its distributive profile, and its fidelity to the intergenerational justice claim.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, intergenerational_ethics_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes global mitigation effort to stay within a finite carbon budget, preventing catastrophic warming that would impose irreversible harms on future generations. Solves the collective action problem of a global commons (atmospheric carbon capacity) where no single actor can secure the outcome alone and free-riding is the dominant strategy without a binding framework.
% TRANSFER_FUNCTION: Moves mitigation costs (capital redeployment, energy system transformation, consumption reduction) from the current generation — disproportionately from the Global North and fossil capital — to secure a stabilized climate for future generations and climate-vulnerable populations. The transfer is mediated by carbon pricing, regulation, subsidy redirection, and climate finance flows.
% ABSENT_VOICES: Future generations are structurally absent — they cannot speak, vote, or litigate. Global South development claimants are procedurally present but substantively excluded from the carbon budget allocation that precedes their input. Fossil fuel workers and communities in transition are often consulted but rarely empowered to shape the transition's pace or terms.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority obligation vanished overnight, emissions would revert to business-as-usual trajectories within years. The carbon budget would be exhausted, locking in 3°C+ warming. Future generations would inherit a radically destabilized climate system; climate-vulnerable populations would face existential threats; fossil capital would avoid stranding but at the cost of systemic collapse. The global energy system, financial architecture, and geopolitical order would reorganize around adaptation and loss-and-damage rather than prevention.
% FOUNDING_PROBLEM: The discovery that atmospheric CO2 accumulation from fossil combustion creates a cumulative, irreversible warming effect that disproportionately harms those who did not cause it — future generations and the globally poor. The problem was recognized scientifically by the 1970s, politically by the 1990s (UNFCCC), and has been the founding justification for every subsequent climate agreement.
% FOUNDING_PROBLEM_CORROBORATION: IPCC assessment reports (scientific consensus outside the benefiting parties), the UNFCCC treaty text (negotiated by 197 parties including non-beneficiaries), and the consistent testimony of climate-vulnerable nations at COPs corroborate that the founding problem — cumulative, irreversible, disproportionate harm — remains live and is worsening. No credible scientific body contests the core causal chain.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62 at interval end) reflects the real resource transfer from current to future generations and from fossil-intensive incumbents to the commons. Suppression (0.58) captures the active enforcement needed: border carbon adjustments, subsidy phase-outs, financial regulation, and the political suppression of fossil capital's resistance. Theater ratio (0.28) acknowledges that a growing share of climate governance activity performs urgency (net-zero pledges, long-term targets) while near-term emissions continue rising. Accessibility collapse (0.42) is moderate — alternatives (adaptation, geoengineering, degrowth) remain conceptually available but are increasingly foreclosed by the shrinking carbon budget. Resistance (0.67) is high and rising, reflecting fossil capital's political counter-mobilization, Global North domestic pushback, and Global South procedural objections.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (governance institutions) experiences the constraint as coordination infrastructure it builds and maintains. The payer seats (current generation, Global North, fossil capital) experience it as enforced extraction with shrinking exit. The beneficiary seats (future generations, vulnerable populations) experience it as a promise whose fulfillment they cannot verify or enforce. The engine computes this divergence from the structural data — the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and climate-vulnerable populations are structural beneficiaries (d near 0.0) — they receive the stabilized climate without paying transition costs. Current generation transition bearers and Global North mitigation bearers are targets (d near 0.7-0.8) — they pay costs with constrained exit. Fossil capital sits at high d (0.85+) — targeted extraction via stranding, with constrained exit due to asset specificity. Climate governance institutions are agenda_setters with arbitrage-grade exit (they administer the constraint and can move between regimes). The derivation chain from beneficiary/victim declarations + power + exit produces this gradient; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cumulative, irreversible, disproportionate harm) is live and worsening per IPCC and UNFCCC corroboration. The constraint has not outlived its function — the carbon budget continues to shrink and the coordination problem intensifies. However, the extraction profile has shifted: early phases (1992-2005) were more rope-like (low extraction, coordination-dominant); recent phases show rising extraction as the budget tightens and the burden falls on harder-to-abate sectors and resistant incumbents. The tangled_rope classification captures this hybridity. Mandatrophy is not resolved; the constraint's function remains live but its extraction burden is concentrating on increasingly resistant parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does the mitigation_priority reading of climate_response_obligation instantiate a distinct constraint from the adaptation_priority and degrowth_reading readings, or are they observable variants of the same structural arrangement?',
    'Compare epsilon, beneficiary/victim sets, and enforcement mechanisms across the three readings. If epsilon differs by >0.15 or beneficiary/victim sets are disjoint, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story linked by network.affects_constraints. If not distinct, the kernel frame is analytical scaffolding and only one constraint story should exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s declared readings map to structurally distinct constraints per ε-invariance.').

omega_variable(
    mitigation_burden_allocation,
    'Is the disproportionate Global North mitigation burden a structural feature of this reading (historical responsibility doctrine) or a contested political negotiation that could shift?',
    'Track burden-sharing outcomes across COPs and carbon budget allocation methodologies. If the Global North share of mitigation cost converges toward per-capita or GDP-proportional allocation, the structural delta for this reading changes.',
    'If the burden allocation shifts, the victim set and directionality profile for global_north_mitigation_bearers changes, altering per-seat classifications and the constraint''s tangled_rope balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_burden_allocation, empirical, 'Stability of the historical responsibility burden allocation in the mitigation_priority reading.').

omega_variable(
    fossil_capital_stranding_mechanism,
    'Does fossil capital''s victim status reflect genuine stranding (assets become worthless) or managed decline (compensated phase-out, carbon capture retrofits, state buyouts)?',
    'Analyze financial flows: stranded asset write-downs vs. transition compensation, CCS deployment rates, state acquisition of fossil assets. Compare to the reading''s structural claim that fossil capital bears extraction.',
    'If stranding is managed/compensated, fossil capital shifts from victim toward beneficiary (state-backed transition), changing the extraction asymmetry and potentially reclassifying the constraint toward rope or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fossil_capital_stranding_mechanism, empirical, 'Whether fossil capital''s extraction is genuine stranding or compensated transition.').

omega_variable(
    intergenerational_discount_rate,
    'What effective discount rate does the constraint''s policy architecture apply to future damages, and does it match the reading''s intergenerational justice claim?',
    'Extract implied discount rates from integrated assessment models used in policy (DICE, REMIND, MESSAGE) and compare to the near-zero rates the intergenerational justice principle requires.',
    'If policy models use discount rates >2%, the constraint''s operational extraction from future generations (via under-mitigation) contradicts its claimed beneficiary structure — a false summit risk for the mitigation_priority reading itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, empirical, 'Alignment between the reading''s intergenerational justice claim and the discount rates embedded in its policy implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__mitigation_priority, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(clim_tr_t1997, climate_response_obligation__mitigation_priority, theater_ratio, 1997, 0.12).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__mitigation_priority, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(clim_tr_t2009, climate_response_obligation__mitigation_priority, theater_ratio, 2009, 0.19).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__mitigation_priority, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(clim_tr_t2021, climate_response_obligation__mitigation_priority, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__mitigation_priority, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__mitigation_priority, base_extractiveness, 1992, 0.18).
narrative_ontology:measurement(clim_be_t1997, climate_response_obligation__mitigation_priority, base_extractiveness, 1997, 0.22).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__mitigation_priority, base_extractiveness, 2005, 0.28).
narrative_ontology:measurement(clim_be_t2009, climate_response_obligation__mitigation_priority, base_extractiveness, 2009, 0.32).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__mitigation_priority, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(clim_be_t2021, climate_response_obligation__mitigation_priority, base_extractiveness, 2021, 0.53).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__mitigation_priority, base_extractiveness, 2025, 0.62).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__mitigation_priority, base_extractiveness, 2015, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__mitigation_priority, suppression_requirement, 1992, 0.25).
narrative_ontology:measurement(clim_su_t1997, climate_response_obligation__mitigation_priority, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__mitigation_priority, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(clim_su_t2009, climate_response_obligation__mitigation_priority, suppression_requirement, 2009, 0.42).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__mitigation_priority, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(clim_su_t2021, climate_response_obligation__mitigation_priority, suppression_requirement, 2021, 0.53).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__mitigation_priority, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, carbon_budget_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_subsidy_regime).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_finance_architecture).

% DUAL FORMULATION NOTE:
% This constraint (mitigation_priority) and its siblings (adaptation_priority, degrowth_reading) form the climate_response_obligation constraint family. They share the kernel (the obligation to respond to climate change) but instantiate different constraints with distinct ε values, beneficiary/victim sets, and enforcement mechanisms. mitigation_priority has the highest ε (0.62) because it allocates the steepest near-term costs to the current generation and fossil capital. adaptation_priority likely has lower ε (burden shifted to future adaptation costs) but higher suppression (locking in warming). degrowth_reading has a different victim set (high-consumption populations) and coordination type (resource_allocation vs global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
