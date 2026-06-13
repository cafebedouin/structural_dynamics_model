% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__portfolio_optimization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__portfolio_optimization_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__portfolio_optimization_reading
 *   human_readable: Climate Mitigation via Portfolio Optimization (Nuclear-Inclusive Reading)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The portfolio-optimization reading of climate mitigation mandates that
 *   all low-carbon electricity sources be maximized, with nuclear explicitly
 *   treated as necessary for reliable baseload generation. This reading
 *   emerged in early 2010s climate modeling and became embedded in IPCC
 *   scenarios, national decarbonization policy, and energy-sector planning.
 *   It coordinates around a technical premise: wind and solar intermittency
 *   requires complementary dispatchable generation, which only nuclear (or
 *   fossil with carbon capture) can provide at scale without demand
 *   reduction. The constraint classifies as tangled_rope: it solves a genuine
 *   coordination problem (integrating variable renewables with dispatchable
 *   backup) AND operates asymmetrically to extract from fossil operators and
 *   renewable contractors while subsidizing nuclear. This constraint is ONE
 *   READING of the contested kernel 'climate_mitigation_imperative'. The
 *   sibling readings (opportunity_cost and systems_transition) emerge from
 *   fundamentally different premises about what 'mitigation' means and what
 *   the founding problem requires. This reading is neither the only coherent
 *   framing nor obviously correct — the entire corpus of readings is needed
 *   to map the contestation space.
 *
 * KEY AGENTS:
 *   - nuclear_power_operators: Institutional beneficiary and co-agenda-setter. Receive capital subsidies, regulatory streamlining, grid priority. Set the technical framing.
 *   - fossil_fuel_infrastructure_operators: Primary victim. Face accelerated retirement mandates and stranded assets under the portfolio-optimization classification.
 *   - renewable_energy_contractors: Secondary victim. Deploy capital expecting symmetric incentives but face backup charges and depressed grid value under baseload-priority framing.
 *   - climate_rapid_transition_coalitions: Tertiary victim and excluded voice. Advocate alternative pathways but are treated as technically illegitimate by the consensus engineering framing.
 *   - energy_market_regulators: Institutional co-agenda-setter. Author and enforce the mandate through capacity payments and grid codes. Claim analytical neutrality but are substantially shaped by nuclear-operator input.
 *   - grid_stability_engineers: Observers. Provide the technical testimony that legitimates the baseload premise. Do not benefit or pay directly.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, 0.67).
domain_priors:suppression_score(climate_mitigation_imperative__portfolio_optimization_reading, 0.58).
domain_priors:theater_ratio(climate_mitigation_imperative__portfolio_optimization_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(climate_mitigation_imperative__portfolio_optimization_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__portfolio_optimization_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__portfolio_optimization_reading, "Climate Mitigation via Portfolio Optimization (Nuclear-Inclusive Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__portfolio_optimization_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__portfolio_optimization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__portfolio_optimization_reading, '6a876197-5d9a-4ad8-8e26-c1ca497ef5cc').
narrative_ontology:cs_kernel_codification('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', formalized).
narrative_ontology:cs_authority_grounding('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', extraction).
narrative_ontology:cs_interpretation_layer_present('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc').
narrative_ontology:cs_reading_relation('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', climate_mitigation_imperative__opportunity_cost_reading, forecloses).
narrative_ontology:cs_reading_relation('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', climate_mitigation_imperative__systems_transition_reading, coexists_with).
narrative_ontology:cs_axiom('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', foundational, baseload_generation_technically_necessary).
narrative_ontology:cs_axiom_status(baseload_generation_technically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', baseload_generation_technically_necessary, empirically_contingent).
narrative_ontology:cs_axiom('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', foundational, technology_neutral_carbon_optimization).
narrative_ontology:cs_axiom_status(technology_neutral_carbon_optimization, holdable).
narrative_ontology:cs_axiom_grounding('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', technology_neutral_carbon_optimization, instrumental).
narrative_ontology:cs_axiom('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', secondary, centralized_generation_infrastructure_required).
narrative_ontology:cs_axiom_status(centralized_generation_infrastructure_required, overridden).
narrative_ontology:cs_axiom_grounding('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', centralized_generation_infrastructure_required, empirically_contingent).
narrative_ontology:cs_reference_frame('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', portfolio_technology_neutrality).
narrative_ontology:cs_drift_state('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', post_battery_cost_collapse_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6a876197-5d9a-4ad8-8e26-c1ca497ef5cc', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, nuclear_power_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, baseload_generation_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__portfolio_optimization_reading, large_industrial_power_users).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_infrastructure_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_contractors).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, distributed_generation_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__portfolio_optimization_reading, climate_rapid_transition_coalitions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__portfolio_optimization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__portfolio_optimization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__portfolio_optimization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__portfolio_optimization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.67) but not extreme because the constraint coordinates a real technical function (baseload-variability matching) alongside extractive redistribution. The 0.67 reading represents extractiveness of the capital-allocation asymmetry: fossil operators lose stranded assets; renewables contractors face backup charges; rapid-transition coalitions lose policy priority. But the coordination benefit (grid stability) is real and is captured in the mid-range extraction value, not at extreme snare levels. Suppression is moderate-high (0.58) because the constraint's persistence requires active exclusion of alternative framings from technical legitimacy — rapid-transition and systems-transition readings are prevented from entering formal energy planning processes, not through violent suppression but through regulatory capture and professional-consensus gatekeeping. Theater is rising (0.32→0.42) because the constraint's justification is increasingly performative: baseload necessity was empirically urgent in 2015 when battery storage cost $500/kWh; by 2024 it is $150/kWh and falling, yet the policy framing has not adjusted. The theater-ratio rise reflects growing gap between the founding technical premise and the constraint's continued operation. Accessibility collapse is moderate (0.71) because alternatives DO exist (some grids are demonstrating renewables+storage viability) but they are systematically excluded from legitimate policy space by professional consensus and regulatory capture — not collapsed by physical law but by institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-operator seat (agenda-setter), the constraint appears as a necessary technical framework to prevent climate policy failure — the baseload premise is correct, the portfolio approach is neutral and technocratic, and extraction is incidental to coordination. From the renewable-contractor seat (payer), the same constraint appears as regulatory capture disguised as technical necessity — the baseload premise is increasingly empirically questionable, the extraction is intentional, and the coordination benefit has been inflated to justify subsidy flows. From the rapid-transition-coalition seat (excluded), the constraint appears as a delay mechanism: it treats speed of deployment as secondary to technology-mix diversification, thereby protecting incumbent nuclear operators while slower timelines delay climate action. The engine computes these divergent directionalities from the stakeholder structure: nuclear operators sit at d≈0.1 (near-beneficiary); fossil operators sit at d≈0.95 (near-target); renewable contractors sit at d≈0.75 (moderate target). The constraint is tangled_rope from every seat, but the extraction REASON differs: genuine-necessity (nuclear framing) vs. rent-seeking-rationalization (payer framing) vs. delay-mechanism (excluded framing).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: nuclear operators collect subsidies and are named explicit beneficiaries. Baseload advocates collect institutional deference and are named beneficiaries. Large industrial users collect preferential pricing and are named beneficiaries. Fossil operators have stranded assets imposed on them and are named victims. Renewable contractors have grid-value suppressed and are named victims. Rapid-transition coalitions have policy access denied and are named victims. Incumbent fossil laborers have regional jobs eliminated via top-down mandate and are structurally victimized though not formally named (they are too diffuse for the formal beneficiary/victim declaration, but are visible in the stakeholder layer). The directionality derivation follows: beneficiary seats get d≈0.05-0.20 (low extraction experienced), victim seats get d≈0.70-0.95 (high extraction experienced). The agenda-setter seat (energy regulators) would ordinarily sit at d≈0.5 (symmetric), but their operational alignment with nuclear operators and their regulatory capture by the industry pushes them toward d≈0.35 (light-payer position, though they frame themselves as analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy arises when the constraint's founding problem is dead or satisfied but the constraint persists. This constraint shows early signs. The founding problem (2015): 'wind and solar alone cannot ensure grid stability because storage is too expensive and demand-response is too diffuse.' Status today (2024-2026): Storage costs have fallen 80%; distributed demand-response technologies have proliferated; several regions (Denmark, Costa Rica, California in summer) have achieved 50%+ renewable supply with stable grid operation. The founding problem is NOT fully dead — some grids still require baseload; seasonal storage remains expensive in temperate regions. But the problem is CONTESTED (multiple readings claim different status) and the policy constraint has NOT adapted to evidence of reduced necessity. The measurement series show extractiveness rising slightly even as the technical premise weakens (t=0: 0.54 extractiveness when storage cost ~$500/kWh; t=30: 0.67 extractiveness when storage cost ~$150/kWh). This pattern is mandatrophy risk: the constraint persists not because the founding problem requires it but because beneficiary coalitions have institutional power to maintain it. The theater_ratio rise (0.32→0.42) is evidence of this drift: more of the constraint's operation is now devoted to defending the technical premise against contrary evidence (theater) rather than to solving the original coordination problem (function). Theater >0.40 is a warning threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseload_necessity_empirical_drift,
    'As battery storage costs fall and demand-response technologies mature, what is the region-specific threshold at which ''baseload necessity'' transitions from empirically real to empirically contestable?',
    'Grid operator analysis and real-world system operation data from regions (Denmark, Costa Rica, Hawaii, parts of Germany) achieving 50%+ renewable supply with stated grid stability. Document what backup mechanisms actually operate; compare to portfolio-optimization predictions.',
    'If the threshold is lower than current policy assumes (e.g., 70% renewable feasible in temperate climates by 2030), the founding technical premise of baseload-necessity weakens and the constraint shifts toward piton classification (performance-maintained fiction). If threshold remains high, the premise holds and tangled_rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseload_necessity_empirical_drift, empirical, 'Whether baseload necessity is universal engineering fact or region-contingent and technology-contingent.').

omega_variable(
    regulatory_capture_vs_technical_consensus,
    'How much of the portfolio-optimization framing''s institutional persistence is due to genuine technical necessity versus institutional power of nuclear operators and grid-operator unions?',
    'Comparative analysis of energy policy across jurisdictions with different nuclear fleet sizes (high-nuclear countries like France vs. low-nuclear countries like Denmark) and their renewable penetration rates and grid reliability. If low-nuclear countries achieve high renewable penetration with similar stability as high-nuclear countries, the baseload premise is weaker than consensus suggests; if they show instability, the premise is stronger.',
    'Evidence of regulatory capture would push classification toward snare; evidence of genuine technical necessity would strengthen tangled_rope classification. The question determines whether the extraction component is essential to the coordination function (tangled_rope) or is incidental rent-seeking disguised as coordination (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_vs_technical_consensus, empirical, 'Whether portfolio-optimization framing reflects technical reality or institutional power concentration.').

omega_variable(
    systems_transition_reading_incompatibility,
    'Are portfolio-optimization and systems-transition readings logically foreclosed from coexisting, or can they coexist as different valid frameworks adopted by different actors?',
    'Examine whether a jurisdiction could simultaneously: (a) adopt portfolio-optimization technology allocation (all low-carbon sources including nuclear), AND (b) mandate democratic governance and co-ownership of energy infrastructure (systems-transition requirement). If both can hold in one legal framework, they coexist; if the portfolio-optimization''s centralized deployment model forecloses democratic governance, they foreclose.',
    'If they coexist, the two readings are different policy choices, not logical contradictions, and the contestation is preference-based (which framing prioritizes which values). If they foreclose, the kernel has genuine logical divisions and one reading is necessarily rejected if the other is adopted. This affects how the constraint family should be modeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systems_transition_reading_incompatibility, conceptual, 'Whether portfolio-optimization and systems-transition readings are compatible in a single framework.').

omega_variable(
    founding_problem_corroboration_gap,
    'Can independent energy analysts, grid operators, and climate scientists from outside the nuclear industry attest whether the founding problem (baseload necessity from intermittency) is still live at the claimed urgency level?',
    'Systematic literature review of energy transition studies from non-nuclear-aligned institutions (universities, national labs with no nuclear fleet stake, IEA analyses in low-nuclear countries). Document whether consensus has shifted toward ''baseload necessity is conditional'' or remains ''baseload necessity is universal''.',
    'If independent analysts attest founding problem is now conditional (not universal), the constraint shows mandatrophy signs: it persists by institutional inertia, not by meeting the original problem condition. If consensus remains ''baseload necessary'', the constraint''s justification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_corroboration_gap, empirical, 'Whether the founding problem''s urgency is independently attested or only by industry-aligned actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__portfolio_optimization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__portfolio_optimization_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__portfolio_optimization_reading, base_extractiveness, 30, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__portfolio_optimization_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__portfolio_optimization_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__portfolio_optimization_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, climate_mitigation_imperative__systems_transition_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, fossil_fuel_stranded_asset_exposure).
narrative_ontology:affects_constraint(climate_mitigation_imperative__portfolio_optimization_reading, renewable_energy_interconnection_rules).

% DUAL FORMULATION NOTE:
% Portfolio-optimization is one of three structurally distinct readings of the 'climate_mitigation_imperative' kernel. The kernel is the commitment to rapid decarbonization; the readings differ on what counts as rapid, what role centralized baseload plays, and who decides. Each reading instantiates a different constraint with different ε, different beneficiary/victim structure, and (typically) different computed type. This story is portfolio_optimization_reading; see opportunity_cost_reading and systems_transition_reading for sibling constraints. The three readings coexist as live positions held by different actors and institutions; they are not resolved by evidence alone because they make different normative commitments about what 'mitigation' prioritizes (speed, baseload certainty, or democratic control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__portfolio_optimization_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
