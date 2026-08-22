% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Technology Legitimacy Kernel: Reliability Primacy Reading
 *   domain: energy_policy/climate_mitigation
 *
 * SUMMARY:
 *   The reliability-primacy reading defines technological legitimacy for
 *   climate mitigation by a single criterion: dispatchable, baseload-capable
 *   generation. Under this reading, nuclear power is the primary legitimate
 *   decarbonization pathway due to its 90%+ capacity factor and
 *   dispatchability; intermittent renewables must pay for battery storage to
 *   'qualify' as baseload-equivalent; and ratepayers bear the cost of
 *   whatever grid stability architecture the reading mandates. This
 *   constraint is ONE of three competing readings of the same kernel
 *   (technology legitimacy for climate mitigation). The other
 *   readings—velocity-primacy (speed of deployment) and precautionary
 *   (reversibility and bounded legacy risk)—would classify technologies
 *   differently and allocate capital to different sources. All three readings
 *   claim authority over the same legitimacy question but use mutually
 *   exclusive measurement criteria. The claim and metrics are independent:
 *   the constraint is CLAIMED as tangled_rope (genuine coordination problem
 *   solved: grid stability + asymmetric extraction imposed via legitimacy
 *   gatekeeping) while the authored metrics describe a constraint whose
 *   extraction is increasing over time (0.54 → 0.68), whose suppression of
 *   alternative readings is rising (0.58 → 0.72), and whose theater_ratio is
 *   creeping upward (0.28 → 0.41)—indicators that the coordination function
 *   is being outweighed by gatekeeping function.
 *
 * KEY AGENTS:
 *   - Grid stability advocates (institutional): set and enforce the reliability-primacy criterion through grid codes and permitting standards; agenda-setter
 *   - Nuclear operators (institutional): primary beneficiaries under the reading; profit from legitimacy premium and preferential capital access
 *   - Renewable energy developers (organized): pay through reduced legitimacy, higher storage-compliance costs, delayed permitting; constrained exit
 *   - Distributed generation advocates (moderate): identity-locked to incompatible architectural vision; excluded from the reading's operational standards
 *   - Ratepayers (powerless): trapped; absorb reliability costs through grid bills
 *   - Storage manufacturers (powerful): indirect beneficiaries; made necessary by the reading's architectural requirement
 *   - Competing climate readings (global): velocity-primacy and precautionary readings hold the kernel jointly with reliability-primacy; excluded from this reading's operational authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Technology Legitimacy Kernel: Reliability Primacy Reading").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_mitigation").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '84f85dff-f893-486a-a33d-222841a85814').
narrative_ontology:cs_kernel_codification('84f85dff-f893-486a-a33d-222841a85814', distributed).
narrative_ontology:cs_authority_grounding('84f85dff-f893-486a-a33d-222841a85814', extraction).
narrative_ontology:cs_interpretation_layer_present('84f85dff-f893-486a-a33d-222841a85814').
narrative_ontology:cs_reading_relation('84f85dff-f893-486a-a33d-222841a85814', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('84f85dff-f893-486a-a33d-222841a85814', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('84f85dff-f893-486a-a33d-222841a85814', foundational, dispatchable_baseload_necessity).
narrative_ontology:cs_axiom_status(dispatchable_baseload_necessity, holdable).
narrative_ontology:cs_axiom_grounding('84f85dff-f893-486a-a33d-222841a85814', dispatchable_baseload_necessity, empirically_contingent).
narrative_ontology:cs_axiom('84f85dff-f893-486a-a33d-222841a85814', foundational, stability_primacy_over_velocity).
narrative_ontology:cs_axiom_status(stability_primacy_over_velocity, holdable).
narrative_ontology:cs_axiom_grounding('84f85dff-f893-486a-a33d-222841a85814', stability_primacy_over_velocity, deontological).
narrative_ontology:cs_reference_frame('84f85dff-f893-486a-a33d-222841a85814', baseload_stability_framework).
narrative_ontology:cs_drift_state('84f85dff-f893-486a-a33d-222841a85814', battery_cost_breakthrough_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('84f85dff-f893-486a-a33d-222841a85814', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_advocates).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, incumbent_baseload_producers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, distributed_generation_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, storage_technology_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Nuclear power plants operate at 90%+ capacity factors and provide uninterruptible baseload generation. Under the reliability-primacy reading, they are the primary legitimate decarbonization pathway. Operators shape permitting standards, technical codes, and grid-integration rules to favor baseload architectures. They benefit from the legitimacy premium and receive deployment capital preferentially.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Utilities, grid operators, and grid-reliability engineers who argue that grid stability is the binding constraint on decarbonization. They set technical codes, permitting criteria, and interconnection standards that measure a technology's legitimacy by its dispatchability and baseload capability. They define what counts as grid stability and enforce the measurement.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_stability_advocates, agenda_setter,
    institutional, generational, analytical, national).

% Solar and wind developers must either accept lower legitimacy status (requiring costly battery storage to qualify as baseload-equivalent) or exit the climate-mitigation framing entirely. Their projects face permitting delays, grid-connection standards that penalize variability, and reduced access to capital streams designated for 'legitimate' technologies. Storage costs are born by developers or passed to consumers.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, renewable_energy_developers, payer,
    organized, biographical, constrained, national).

% Rooftop solar, microgrids, and demand-response advocates who argue for decentralized resilience instead of centralized baseload. They cannot participate in the reliability-primacy framing without accepting its architectural premises. Their legitimacy requires proving their systems stabilize grids; the rules are designed assuming centralized control. Professional identity fuses with advocacy for distributed models, making exit from the constraint conceptually incoherent.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, distributed_generation_advocates, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__reliability_primacy_reading, distributed_generation_advocates, excluded).

% End-use consumers who pay grid bills. As the system requires more expensive storage to accommodate intermittent renewables (per the reliability-primacy rule), storage costs are recovered through rates. Ratepayers absorb the cost of whatever architectural commitment the reading mandates. They cannot exit the grid or renegotiate the technical standards underlying their bills.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers_bearing_reliability_costs, payer,
    powerless, immediate, trapped, regional).

% Battery and storage manufacturers benefit indirectly: the reliability-primacy reading makes storage a necessary cost to legitimize intermittent renewables. They profit from the architectural requirement without bearing the cost visibility. Their technology becomes a compliance tool rather than an option.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, storage_technology_manufacturers, beneficiary,
    powerful, biographical, arbitrage, global).

% Models showing that rapid renewable deployment (without full baseload substitution) could meet climate targets on 1.5°C pathways are treated as non-authoritative under this reading's framing. The constraint privileges grid-stability models over climate-carbon-budget models. The analytical seat observes the trade-off between decarbonization speed and baseload stability.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_scientists_and_decarbonization_models, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(technology_legitimacy_kernel__reliability_primacy_reading, climate_scientists_and_decarbonization_models).

% Regulatory bodies that set legitimacy criteria for climate-mitigation technologies via grid codes, interconnection standards, and capital allocation. They operationalize the reliability-primacy reading in rule-making. Their enforcement machinery certifies which technologies count as legitimate, shaping investment flows and deployment patterns.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_decarbonization_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Proponents of velocity-primacy (speed of deployment) and precautionary readings (bounded worst-case risk) hold incompatible legitimacy criteria. Velocity-primacy argues renewables should be deployed as fast as possible despite intermittency; precautionary reading prioritizes reversibility and bounded legacy. All three readings claim the same kernel (technology legitimacy for climate) but use mutually exclusive measurement criteria. Excluded from THIS reading's operational standards.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, competing_climate_readings, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_operators).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__reliability_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the grid stability problem: establishes a unified technical criterion (dispatchability and baseload capability) that allows grid operators, utilities, and technology developers to coordinate on infrastructure investment without renegotiating stability requirements technology-by-technology. Provides a common measurement frame for what counts as a legitimate climate solution.
% TRANSFER_FUNCTION: Transfers deployment capital, permitting priority, and grid-connection authority from variable renewable developers toward baseload sources (primarily nuclear) and storage providers. Transfers reliability costs from grid operators onto ratepayers in the form of higher bills for grid-stabilization infrastructure. Transfers legitimacy status from intermittent renewables (lower status, requires costly compliance) to baseload technologies (higher status, presumptively authorized).
% ABSENT_VOICES: Climate scientists emphasizing decarbonization velocity; engineers advocating demand-response and microgrids; low-income ratepayers who would otherwise renegotiate grid architecture; communities bearing long-term nuclear waste storage costs; renewable developers who view baseload requirements as architectural lock-in rather than technical necessity. None of these parties set the legitimacy criteria.
% DISAPPEARANCE_RATIONALE: If this reading disappeared—if technology legitimacy were uncoupled from baseload capability—capital allocation would shift within months to projects meeting velocity or precautionary criteria instead. Grid operating standards would be rewritten. Nuclear plant economics would worsen (no legitimacy premium). Renewables would deploy faster (lower storage requirements). The entire decarbonization architecture would reorganize around whichever reading replaced reliability-primacy.
% FOUNDING_PROBLEM: Early decarbonization pathways assumed variable renewables could substitute for fossil baseload without additional grid stability infrastructure. This created 'duck curve' grid instability, blackout risk, and plausibility crises for renewable-dominated grids. The reliability-primacy reading was established to ensure decarbonization does not produce grid fragility.
% FOUNDING_PROBLEM_CORROBORATION: Grid operators and utility engineers attest the founding problem remains live, citing ongoing integration challenges in high-renewable grids. Climate modelers and renewable-deployment advocates attest the problem is substantially solved through storage cost decreases, demand flexibility, and transmission expansion—and that the reading persists to protect baseload incumbency rather than grid safety. Regulatory filings and grid-stability studies from operators show increasing renewable penetration without cascading failures; independent cost analyses show storage costs now competitive with nuclear.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 because the reading gates legitimacy on a single criterion (baseload capability) that benefits nuclear and penalizes renewables, independent of actual carbon reduction or climate benefit. The criterion is decoupled from marginal climate utility: a new geothermal plant with lower capacity factor might reduce emissions faster than waiting for nuclear construction, but counts as less legitimate under the reading. Suppression is high (0.72) and rising because the reading's persistence requires active enforcement against velocity-primacy and precautionary competing readings—grid codes must exclude them, capital-allocation mechanisms must overweight baseload, technical standards must penalize variability. Theater rises (0.28 → 0.41) because early in the interval grid stability was a real coordination constraint; as battery costs fall and demand-response scales, the stability rationale becomes more theatrical cover for baseload preference. The measurement series models a tangled rope slowly metastasizing toward snare: genuine coordination function (grid stability) remains, but extraction grows as the reading's authority hardens and alternatives are suppressed. Accessibility collapse sits at 0.61: renewable developers perceive alternatives (velocity-primacy, precautionary readings) but these alternatives are administratively foreclosed within the regulatory frame; the reading's authority prevents exit to a different legitimacy criterion.
 *
 * PERSPECTIVAL GAP:
 *   Grid operators experience the reading as neutral technical governance; renewable developers experience it as gatekeeping; ratepayers experience it as cost with no negotiation. Each seat computes a different type from the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators: d ≈ 0.15 (beneficiary; high power; arbitrage exit; gains from legitimacy premium and capital allocation). Grid-stability advocates: d ≈ 0.25 (beneficiary; institutional; analytical exit; they set and administer the reading, collecting authority and standard-setting power). Renewable developers: d ≈ 0.78 (target; organized but weaker than utilities; constrained exit; pay through storage compliance and delayed permitting; cannot renegotiate the criterion). Ratepayers: d ≈ 0.88 (most-extracted; powerless; trapped; bear reliability costs with zero participation in criterion-setting). Distributed advocates: d ≈ 0.75 (target; identity-locked exit; their professional coherence depends on rejecting the reading's architectural premises; exit means abandoning the decarbonization mission as they understand it). Effectiveness extraction is amplified for trapped and identity-locked targets; beneficiaries with arbitrage options see effective extraction inverted to subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy risk: the founding problem (grid instability from high renewable penetration) is substantially solved by falling battery costs and improved demand-response, yet the reliability-primacy reading persists because it has become institutionalized. Utilities have rewritten grid codes around baseload assumptions; regulatory capital-allocation mechanisms favor nuclear; competitive rules exclude competing readings. The coordination function (grid stability) no longer justifies the extraction (legitimacy gatekeeping), but enforcement has hardened. This is classic tangled-rope → piton drift: the coordination rationale is increasingly theatrical (theater_ratio rising 0.28 → 0.41), while suppression of alternatives hardens (0.58 → 0.72). The measurement series captures a constraint that solved a real problem but now persists through institutional inertia. The three-reading contest (reliability-primacy vs. velocity-primacy vs. precautionary) means that the 'founding problem solved' verdict is itself contested—grid operators attest stability is still fragile; modelers attest stability is empirically robust. This contestation is modeled in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_problem_persistence,
    'Is grid instability from high renewable penetration still a binding constraint on decarbonization, or has it been substantially solved by falling battery costs and improved demand-response?',
    'Comparative grid-stability analysis across regions with high renewable penetration (Denmark, Costa Rica, Hawaii, parts of California). Measure: cascading failures, reserve margin requirements, frequency-regulation costs over time. If failure rates stay low and reserve margins decline while renewable fraction increases, the binding constraint has moved.',
    'If the founding problem is solved, the reliability-primacy reading transitions from justified coordination constraint to institutionalized gatekeeping (mandatrophy). The extracted value persists but is no longer justified by stability necessity. Type classification would shift from tangled_rope (coordination + asymmetric extraction) toward piton (atrophied function, theatrical maintenance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_persistence, empirical, 'Whether grid instability remains the binding constraint on renewable deployment.').

omega_variable(
    kernel_reading_mutual_exclusivity,
    'Can a single institutional framework hold multiple sibling readings simultaneously, or does embracing one reading logically foreclose the others?',
    'Policy jurisdiction that attempts to implement two readings jointly (e.g., require baseload capability AND reward velocity); measure whether the dual implementation creates internal contradictions or requires auxiliary trades (e.g., higher storage costs, longer timelines). Alternatively, analyze the logical structure of the three readings'' core axioms: do they share incompatible factual premises or incompatible values?',
    'If mutual exclusivity is logical (not just institutional competition), the readings coexist_with each other in a fundamental way—they are live normative disagreements without Archimedean resolution point. The constraint story would carry an omega documenting that the KERNEL itself is contested not just empirically but normatively. If mutual exclusivity is only institutional (could be jointly satisfied with sufficient capital), the contest is resolvable by removing gatekeeping constraints and letting multiple readings operate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_mutual_exclusivity, conceptual, 'Whether the three readings are logically incompatible or merely institutionally competitive.').

omega_variable(
    legitimacy_criterion_independence_from_climate_utility,
    'Is the reliability-primacy criterion (dispatchability + baseload capability) actually necessary for climate mitigation, or is it an architectural preference that benefits certain incumbent technologies?',
    'Compare: (a) climate models using optimized technology mix (unconstrained baseload requirement) vs. (b) climate models constrained to baseload-only technologies. If both meet 1.5°C target with comparable total cost and timeline, the criterion is architectural preference. If baseload constraint raises cost or extends timeline significantly, it is justified.',
    'If independence is confirmed (baseload criterion is preference not necessity), the constraint is exposed as false-summit mountain: claimed natural (grid stability necessity) but actually constructed (technology gatekeeping). The beneficiary structure (nuclear operators, baseload incumbents) becomes the causal mechanism. Type would shift from tangled_rope toward snare (extraction riding on false coordination story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_criterion_independence_from_climate_utility, empirical, 'Whether dispatchable baseload is technically necessary or architecturally preferred.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of competing readings (velocity-primacy, precautionary) structural (regulatory barriers, capital gatekeeping) or internalized (renewable developers and climate advocates accept baseload premise as natural/necessary)?',
    'Post-reading suppression trajectory: if a jurisdiction removes regulatory barriers (opens capital to velocity-aligned technologies, stops penalizing intermittency in grid codes), measure whether competing readings mobilize advocacy and investment. If suppression persists after removal of structural barriers, it is partly internalized (the reading has become common sense to some stakeholders).',
    'If suppression is structural, removing barriers would rapidly mobilize competing readings and rebalance legitimacy allocation. If suppression is internalized, the constraint carries its gatekeeping function even after institutional removal—renewable developers would carry ''reliability primacy'' reasoning with them into new institutional frames. This informs the cost and sustainability of alternative policy approaches.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression of competing readings.').

omega_variable(
    kernel_codification_status_ambiguity,
    'Is the legitimacy kernel ''technology must be X for climate mitigation'' a formally written rule (grid codes, climate policy statutes) or an implicit operational standard (how capital allocation and permitting actually work)?',
    'Inventory: (a) explicitly written criteria (grid codes, interconnection standards, capital-allocation policy text) naming dispatchability/baseload as requirements; (b) implicit criteria observable only in permitting decisions and capital flows. If the criterion appears only implicitly, it is harder to reform and carries higher suppression.',
    'Explicit codification makes the reading visible and contestable—alternative readings can point to the written rule and propose amendment. Implicit codification is harder to challenge because it appears as ''how things are done'' rather than a contestable criterion. The legibility of the kernel affects the feasibility of competing readings mobilizing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_codification_status_ambiguity, empirical, 'Whether the reliability-primacy criterion is formally written or implicit operational practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(tech_tr_t4, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 4, 0.31).
narrative_ontology:measurement(tech_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(tech_tr_t12, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(tech_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(tech_be_t4, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 4, 0.59).
narrative_ontology:measurement(tech_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(tech_be_t12, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(tech_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(tech_su_t4, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(tech_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(tech_su_t12, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(tech_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__reliability_primacy_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel spawns three mutually competing constraint stories, each instantiating a different reading of the same kernel. The reliability-primacy reading (this story) centers on dispatchability and baseload capability as the legitimacy criterion. The velocity-primacy reading centers on deployment speed within carbon budget timelines. The precautionary reading centers on bounded worst-case risk and reversibility. All three share the same kernel (what makes a technology legitimate for climate mitigation) but produce different beneficiary/victim structures and different extraction mechanisms. The three readings coexist in policy debate and institutional competition; none has achieved hegemonic consensus. Each reading's persistence depends partly on suppressing the others' operational authority. Network links represent the structural mutual pressure: reliability-primacy's emphasis on baseload delays velocity-primacy's fast-deployment agenda; precautionary reading's risk-bounds create pressure against both baseload incumbency and speed-at-risk. Constraint family decomposition follows OQ-26 ε-invariance: each reading has a distinct ε-referent (the standing arrangement the reading evaluates), distinct beneficiary/victim sets, and distinct type. Linking them via network.affects_constraints enables the engine to detect when one reading's strengthening suppresses others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__reliability_primacy_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
