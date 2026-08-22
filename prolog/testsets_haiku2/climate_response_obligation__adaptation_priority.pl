% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Framework
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the 'adaptation-priority' reading of the
 *   contested kernel 'climate response obligation.' The reading frames
 *   current warming as locked-in by physics and prior emissions, rendering
 *   costly near-term mitigation inefficient, and proposes investment in
 *   adaptive capacity (resilience infrastructure, technological adjustment,
 *   managed retreat) as the rational response. The current generation wealthy
 *   nations and fossil capital benefit by deferring transition costs; future
 *   generations and the Global South bear the unmitigated climate impacts and
 *   disproportionately sparse adaptation resources. The constraint functions
 *   as active enforcement: policy frameworks, funding mechanisms, and
 *   intellectual property regimes are structured to prioritize adaptation
 *   investment over mitigation technology, excluding rival framings
 *   (mitigation-priority, degrowth) from decision authority. The kernel
 *   contest is irreducible—the three readings constitute fundamentally
 *   different obligations to different actors, and no single factual
 *   discovery will reconcile them.
 *
 * KEY AGENTS:
 *   - current_generation_wealthy_nations: Institutional agenda-setter; avoids transition costs; controls adaptation investment flows and policy framing
 *   - fossil_fuel_capital: Powerful beneficiary; extraction deferred; adaptation investment allows continued operational life without phase-out
 *   - future_generations: Institutional victim; bear unmitigated climate impacts; excluded from present decision-making authority
 *   - global_south_developing_nations: Institutional victim; climate-vulnerable populations; adaptation capacity structurally sparse relative to impacts
 *   - adaptation_technology_vendors: Powerful beneficiary; capture significant share of adaptation investment; profit margins depend on adaptation-priority framing
 *   - climate_scientists_mitigation_advocates: Excluded seats; would argue the reading misrepresents physical inevitability and suppresses cost-effective mitigation options
 *   - intergenerational_justice_framework: Analytical observer; reads the constraint as violating duties to future welfare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.78).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.71).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Climate Response Framework").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'c58497c9-18e4-4bb3-8683-ca385e2bc8b4').
narrative_ontology:cs_kernel_codification('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', distributed).
narrative_ontology:cs_authority_grounding('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', extraction).
narrative_ontology:cs_interpretation_layer_present('c58497c9-18e4-4bb3-8683-ca385e2bc8b4').
narrative_ontology:cs_reading_relation('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', foundational, warming_baseline_acceptance).
narrative_ontology:cs_axiom_status(warming_baseline_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', warming_baseline_acceptance, empirically_contingent).
narrative_ontology:cs_axiom('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', foundational, present_consumption_priority).
narrative_ontology:cs_axiom_status(present_consumption_priority, holdable).
narrative_ontology:cs_axiom_grounding('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', present_consumption_priority, instrumental).
narrative_ontology:cs_reference_frame('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', rational_response_to_locked_in_warming).
narrative_ontology:cs_drift_state('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', contemporary_climate_impact_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c58497c9-18e4-4bb3-8683-ca385e2bc8b4', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, current_generation_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, fossil_fuel_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, adaptation_technology_vendors).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_developing_nations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, climate_vulnerable_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nation_citizens).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, wealthy_nation_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the global climate response framework through UNFCCC and multilateral institutions; frame adaptation as primary response; defer transition costs to future; maintain current consumption and production systems. Their exit option is arbitrage: they can shift to alternative framings if pressure rises, but do so strategically to protect near-term interests. They control capital flows for adaptation investment and policy authority.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, current_generation_wealthy_nations, agenda_setter,
    institutional, biographical, arbitrage, global).

% Avoid immediate phase-out obligations by accepting adaptation-priority framing; defer divestment and transition costs decades into future; continue operations under present business model; profit from adaptation technology investments (carbon capture, geoengineering). Their exit option is mobile: they can relocate to less-regulated jurisdictions or shift to adaptation-technology portfolios if pressure rises. They benefit from deferral and from the legitimacy adaptation-priority framing provides for continued operations.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, fossil_fuel_capital, beneficiary,
    institutional, biographical, mobile, global).

% Profit from large-scale adaptation investment (resilience infrastructure, climate-resilient agriculture, managed retreat technology, desalination, cooling systems). Their exit option is arbitrage: they profit equally from mitigation or adaptation investment; framing matters only insofar as it directs capital flows. They benefit from adaptation-priority because it guarantees sustained investment streams and excludes lower-cost mitigation alternatives that might reduce their market.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, adaptation_technology_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Avoid near-term transition costs (energy price spikes, infrastructure disruption, consumption constraints); maintain consumption levels; pay adaptation taxes/insurance and face incremental climate impacts (heat, flood, drought) which wealthy-nation infrastructure can largely buffer. Their exit option is constrained: they can exit domestically (migrate to less-impacted regions within nation) but face carbon lock-in via infrastructure and supply chains. They are net beneficiaries under this reading because adaptation investment flows to their regions and their adaptive capacity is highest.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, wealthy_nation_citizens, payer).

% Inherit a world 2-3°C warmer than pre-industrial baseline, with compound adaptation deficits: ecosystems degraded, infrastructure inadequate, technology still incomplete, social instability from climate migration and resource conflict. They bear the full cost of deferred mitigation (higher warming baseline) plus adaptation deficits (insufficient preparation). Their exit option is trapped: they cannot exit the generation they are born into, cannot exit the climate they inherit, have no say in present decision-making that determines their climate burden.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Bear disproportionate climate impacts (heat, water stress, agricultural collapse) while receiving sparse adaptation funding and technology transfer; excluded from policy authority that frames adaptation-priority; depend on wealthy-nation institutions for adaptation capital and technology access. Their exit option is constrained: they cannot escape the climate impacts or the resource dependence on wealthy nations' adaptation investment decisions. They are net payers under this reading: they absorb the unmitigated warming while adaptation investment concentrates in wealthy regions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_developing_nations, payer,
    moderate, generational, constrained, global).

% Live in regions of extreme climate impact (small islands, drought zones, flood plains, arctic communities) with minimal adaptive capacity; face managed retreat or ecosystem collapse; identity fused with place and livelihood (pastoralist, island inhabitant, subsistence farmer); exit means cultural death. Their exit option is identity_locked: leaving means abandoning the identity, livelihood, kinship structure, and territorial claim that constitutes who they are. They are net payers: they suffer the highest climate impacts with the lowest adaptation resources.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_vulnerable_communities, payer,
    powerless, biographical, identity_locked, local).

% Argue that rapid decarbonization is cost-effective and necessary; that 2-3°C warming is not inevitable and results from policy choices to continue fossil fuels; that adaptation-priority framing suppresses mitigation investment and externalizes costs. They are structurally excluded from policy authority when adaptation-priority framing dominates; their alternative reading (mitigation_priority) is treated as unrealistic or economically irrational. Their exit option is constrained: they can organize politically, but institutional power concentrates in agenda-setter hands.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, mitigation_advocates, excluded,
    organized, civilizational, constrained, global).

% Analyze whether the adaptation-priority reading satisfies duties to future generations; argue that accepting preventable harm when mitigation is still possible violates intergenerational justice principles; observe structural asymmetry in burden distribution. They have no decision authority but provide normative framework for evaluating the constraint's legitimacy.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, intergenerational_justice_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, current_generation_wealthy_nations).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns global climate response investment and policy around a shared assumption (2-3°C warming is locked in by prior emissions) such that adaptation planning can proceed with unified timelines, technology standards, and capital allocation; solves collective action problem around accepting new climate baseline and coordinating resilience infrastructure.
% TRANSFER_FUNCTION: Moves resources (capital, technology, research capacity, planning authority) from mitigation-priority framing to adaptation-priority investment; defers transition costs from present to future; externalizes climate damage costs from wealthy nations to Global South and future generations; transfers fossil-fuel phase-out obligations across generational boundary.
% ABSENT_VOICES: Mitigation-priority advocates (would argue the reading misrepresents physics and suppresses cost-effective alternatives); future generations (would argue they are bearing externalized costs); Global South subsistence communities (would argue adaptation resources are distributed unequally); degrowth theorists (would argue both adaptation and mitigation leave planetary boundaries unsustained). These voices are structurally excluded from UNFCCC decision authority when adaptation-priority framing dominates institutional consensus.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing and its suppression of alternative climate responses vanished, policy authority would shift toward mitigation-priority investment, transition timelines would accelerate, fossil-fuel phase-out obligations would tighten, and adaptation investment would reorient from current-generation protection toward future-generation preparedness. The constraint's disappearance would change capital flows, technology development trajectories, and intergenerational burden distribution fundamentally.
% FOUNDING_PROBLEM: How should humanity respond rationally to warming that is already locked in by past emissions and cannot be reversed within current decision horizons? Given the delay between emissions and climate response, and given that mitigation requires costly near-term transition, should policy optimize for adaptation to the new baseline rather than paying twice (transition costs + adaptation to residual warming)?
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists (IPCC) attest that substantial warming is locked in by existing greenhouse gas inventory and that near-term warming will occur regardless of mitigation choices. Wealthy-nation governments and fossil-capital institutions attest that adaptation should be prioritized given cost-benefit analysis favoring near-term consumption over transition. Mitigation advocates and intergenerational-justice theorists (from OUTSIDE the benefiting-party set) attest that the founding problem is misframed: that aggressive near-term mitigation still prevents the worst outcomes, that adaptation capacity is structurally unequal, and that accepting higher warming externally costs onto future generations and the Global South without their consent. The corroboration is split: the physical premise (warming is locked in) is attested from outside benefiting parties; the policy conclusion (adaptation should be prioritized) is attested only by parties who benefit from deferral.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the reading's core move is deferring costs from present to future and from wealthy to poor: current consumption continues undiminished; future generations inherit degraded climate + adaptation deficits. Suppression is also high (0.71) because the framing actively excludes mitigation-priority and degrowth readings from policy authority—'inevitability' closes the decision space; rival framings are treated as unrealistic or economically irrational. Theater is moderate-high (0.48) because a genuine adaptive-capacity problem exists (infrastructure, technology, planning for 2-3°C warming are real public goods), but the framing's primary function is to legitimize present inaction, not to deliver adaptation effectiveness. The measurement series show rising extractiveness and suppression over the interval (0-30 years): as warming damages accumulate and adaptation costs rise, the constraint's function shifts more fully toward extraction and less toward coordination; theater ratio remains moderate, indicating ongoing genuineness of adaptive activity coupled with performance of necessity. All three metrics share a single time grid (0, 10, 20, 30); the basis field distinguishes observed data (0-20) from projected scenarios (30).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (future generations, Global South) and the agenda-setter seat (wealthy nations) should diverge sharply: from the wealthy-nation institutional perspective, the reading is rational adaptation to new climate baseline—they control technology, capital, and choice sets. From the future-generation perspective, the same structure is deferred harm externalization—they inherit the climate and the adaptation deficit simultaneously. From the Global South perspective, the reading is institutional capture: wealthy nations externalize their adaptation costs onto those least able to bear them. The engine computes these divergences from power × exit asymmetry; the authored claim/metric gap is intentional (claimed tangled_rope; metrics show high extraction and suppression) to flag that the reading's own framing as 'rational cooperation' may mask asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Current-generation wealthy nations sit at high directionality (near 1.0, full target-ish, though they define themselves as beneficiaries): they set the agenda, avoid costs, and benefit from deferral. Fossil capital is similarly positioned (d near 0.2-0.4: nominally beneficiary, but structurally target under mitigation-priority reading—this story locks them as beneficiary via the reading's framing). Future generations and Global South are locked as victims (d near 0.9): they have trapped/identity_locked exit options (born into the climate; no exit from generation or geography), institutional powerlessness, and bear costs they did not choose. Adaptation-technology vendors sit near symmetric (d ≈ 0.5): they benefit from adaptation investment but also profit from mitigation scenarios; their role depends on which reading governs. The engine computes d from power + exit + beneficiary/victim declarations; the authored directionality overrides are not needed here because the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem for this reading: 'How do we respond to warming that is already locked in by past emissions?' The founding problem status: contested. Future generations and mitigation advocates say the founding problem is misframed—warming is not fully locked; aggressive near-term mitigation still avoids the worst outcomes. Wealthy nations and fossil capital say the problem is live and requires adaptation focus. The disappearance verdict is world_rearranges: if this constraint (adaptation-priority policy framework + suppression of mitigation investment) vanished, wealthy nations would face pressure to transition sooner, fossil capital would phase out faster, and future generations' climate damages would be reduced. The mandatrophy question: does the adaptation-priority obligation still serve its original function (rational response to locked-in warming) or has it become primarily a mechanism for deferring transition costs? The measurement series suggest the latter: as warming and adaptation costs accumulate (extractiveness rising), and as the decision space for alternatives remains suppressed (suppression rising), the constraint increasingly functions as rent extraction masked by adaptation theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inevitability_vs_constructed_pathway,
    'Is 2-3°C warming genuinely inevitable given current physics and locked-in emissions, or is the ''inevitability'' frame a constructed narrative that forecloses political choices around near-term mitigation?',
    'IPCC-aligned modeling comparing scenarios where aggressive mitigation begins now vs. scenarios accepting higher warming. Assess whether the claimed inevitability depends on assuming current policy trajectories continue unchanged.',
    'If the warming is contingent on policy paths we control, the reading''s referent constraint shifts from ''acceptance of physical limit'' to ''political choice to prioritize current consumption over future welfare.'' Classification would shift from coordination to extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inevitability_vs_constructed_pathway, empirical, 'Whether the ''inevitability'' is physical fact or policy choice framed as physics').

omega_variable(
    adaptation_capacity_distribution,
    'Can adaptation genuinely protect the Global South and poorest populations, or is adaptation capacity so unevenly distributed that the reading''s implicit claim of ''resilience for all'' functions as cover for unequal harm distribution?',
    'Empirical analysis of adaptation investment flows by nation, wealth quintile, and climate-impact severity. Compare adaptive capacity (capital, technology, institutional infrastructure) across regions against projected climate impacts.',
    'If adaptation is structurally unequal (wealthy regions can adapt; poor regions cannot), the constraint operates as a mechanism for externalizing costs onto those with lowest exit options and highest vulnerability. The ''adaptation'' frame then masks a snare operating across generations and geography.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_capacity_distribution, empirical, 'Whether adaptation is universally accessible or concentrates protection in wealthy regions').

omega_variable(
    suppression_of_mitigation_alternatives,
    'Does the adaptation-priority framing actively suppress political investment in rapid mitigation technologies, behavioral change, and system transition, or is the framing orthogonal to mitigation choices?',
    'Policy-level analysis: does adoption of adaptation-priority language in governance correlate with reduction in near-term mitigation investment, carbon pricing, or transition infrastructure? Compare jurisdictions with strong adaptation-priority messaging vs. those emphasizing mitigation.',
    'If the framing suppresses mitigation, it functions as an enforced extraction from future generations. If mitigation and adaptation are pursued independently, the constraint is pure coordination around resilience and the suppression score overstates the coercive component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_mitigation_alternatives, empirical, 'Whether adaptation-priority narrative actively suppresses mitigation alternatives or is independent of them').

omega_variable(
    kernel_reading_contest,
    'What is the ontological status of the kernel—''climate response obligation''—under different readings?',
    'Philosophical and institutional analysis of how the three readings (adaptation_priority, mitigation_priority, degrowth_reading) constitute the kernel differently: as a duty to current welfare (adaptation), duty to future generations (mitigation), or duty to planetary limits (degrowth). No empirical resolution exists; this is a reading-dependent question.',
    'The referent of the constraint—what obligation exists and to whom—differs across readings. Each reading has its own ε, its own beneficiary/victim structure. They cannot be reconciled into a single constraint; they are three readings of one kernel, each with valid internal structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the three readings describe the same obligation differently or constitute fundamentally different obligations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_obligation__adaptation_priority, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clim_tr_t10, climate_response_obligation__adaptation_priority, theater_ratio, 10, 0.41).
narrative_ontology:measurement(clim_tr_t20, climate_response_obligation__adaptation_priority, theater_ratio, 20, 0.46).
narrative_ontology:measurement(clim_tr_t30, climate_response_obligation__adaptation_priority, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_obligation__adaptation_priority, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(clim_be_t10, climate_response_obligation__adaptation_priority, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(clim_be_t20, climate_response_obligation__adaptation_priority, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(clim_be_t30, climate_response_obligation__adaptation_priority, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_obligation__adaptation_priority, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(clim_su_t10, climate_response_obligation__adaptation_priority, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(clim_su_t20, climate_response_obligation__adaptation_priority, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(clim_su_t30, climate_response_obligation__adaptation_priority, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_financial_transfer__adaptation_funds).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, fossil_fuel_phase_out__delay_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'climate_response_obligation.' The sibling readings are 'mitigation_priority' and 'degrowth_reading', each with its own ε, beneficiary/victim structure, and type. All three readings share the kernel but have different directionality profiles. This story's ε (0.78) is specific to the adaptation-priority reading and reflects the reading's structural asymmetry: current generation benefits, future generations and Global South bear unmitigated costs. Sibling readings will author different ε values reflecting their own structural asymmetries. The network links all three to enable family-level analysis of how the readings compete for policy authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
