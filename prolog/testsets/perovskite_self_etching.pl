% ============================================================================
% CONSTRAINT STORY: perovskite_self_etching
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perovskite_self_etching, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perovskite_self_etching
 *   human_readable: The 2D Perovskite Machinability Constraint
 *   domain: technological/semiconductors
 *
 * SUMMARY:
 *   The 2D perovskite machinability constraint models the dominance of
 *   destructive, incompatible lithography techniques when applied to soft
 *   lead halide perovskites. Traditional photolithography—designed for rigid
 *   silicon and inorganic semiconductors—causes material degradation in
 *   halide perovskites through photoresist solvent attack, UV-induced
 *   decomposition, and ion beam damage. This constraint forces the perovskite
 *   research community into a bind: accept severe yield losses (40-60%), pay
 *   for expensive equipment workarounds, or abandon perovskite integration
 *   entirely. The constraint exhibits all six DR types from different
 *   structural positions, revealing how institutional dependence on
 *   silicon-era tooling creates extraction that resembles natural law but is
 *   actually contingent on the absence of viable alternatives. Theater ratio
 *   (0.58) reflects that much traditional lithography activity is
 *   non-functional for perovskites—expensive resist coatings and UV curing
 *   are performed out of protocol compliance, not because they advance the
 *   fabrication goal. Extractiveness (0.52) is moderate-to-high because the
 *   extraction is enforced through both material incompatibility and supply
 *   chain lock-in. Suppression (0.68) is high because researchers have no
 *   practical alternatives within institutional timescales.
 *
 * KEY AGENTS:
 *   - Perovskite Device Yield: Primary victim (powerless/trapped) — forced to accept destructive protocols; degradation unavoidable within established supply chain
 *   - Perovskite Research Community: Secondary victim (moderate/constrained) — faces capital barriers to alternative fabrication; also benefits from 'open problem' status and publication opportunity
 *   - Established Lithography Vendors: Primary beneficiary (institutional/arbitrage) — capture customer dependency through constraint; extract through equipment/process consulting lock-in
 *   - Silicon Foundry Incumbents: Secondary beneficiary (institutional/arbitrage) — maintain supply chain dominance; constraint ensures perovskite researchers remain integrated into silicon ecosystem
 *   - Alternative Fabrication Coalition: Organized agents (organized/constrained) — nanoimprint, self-assembly, direct-write communities; building exit pathway with plausible 5-10 year sunset
 *   - Silicon Foundry Protocol Standards: Institutional actor (institutional/arbitrage) — maintains inherited fab design rules through standardization inertia, not functional compatibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perovskite_self_etching, 0.52).
domain_priors:suppression_score(perovskite_self_etching, 0.68).
domain_priors:theater_ratio(perovskite_self_etching, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perovskite_self_etching, extractiveness, 0.52).
narrative_ontology:constraint_metric(perovskite_self_etching, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(perovskite_self_etching, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perovskite_self_etching, tangled_rope).
narrative_ontology:human_readable(perovskite_self_etching, "The 2D Perovskite Machinability Constraint").
narrative_ontology:topic_domain(perovskite_self_etching, "technological/semiconductors").

domain_priors:requires_active_enforcement(perovskite_self_etching).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perovskite_self_etching, established_lithography_vendors).
narrative_ontology:constraint_beneficiary(perovskite_self_etching, silicon_ecosystem_incumbents).
narrative_ontology:constraint_victim(perovskite_self_etching, perovskite_device_yield).
narrative_ontology:constraint_victim(perovskite_self_etching, cost_reduction_pathway).
narrative_ontology:constraint_victim(perovskite_self_etching, alternative_fabrication_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEROVSKITE DEVICE YIELD (SNARE) — Cannot escape destructive lithography protocols. Soft organic-inorganic halide perovskites degrade under traditional photoresist solvents, UV exposure, and ion beam etching. The constraint forces researchers to either accept 40-60% yield loss or abandon perovskite integration altogether. No alternative pathway exists within the established supply chain — trapped with maximum experienced extraction.
constraint_indexing:constraint_classification(perovskite_self_etching, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PEROVSKITE RESEARCH COMMUNITY (TANGLED ROPE) — Constrained by capital costs of alternative fabrication tools (nanoimprint, direct writing, self-assembly) and institutional dependence on established fabs. But also benefits from the constraint's status as an 'open problem' — publication pipeline, grant funding for 'perovskite integration,' and prestige for solving machinability drive research effort. Mixed experience: genuine extraction (forced to use suboptimal techniques) but also coordination benefit (field cohesion, research opportunity).
constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED LITHOGRAPHY VENDORS (ROPE) — Experiences the constraint as coordination of their existing installed base. Every perovskite research group that attempts traditional lithography becomes a customer for expensive equipment upgrades, process consulting, and consumables. The constraint ensures perovskite researchers remain dependent on the silicon ecosystem's tools. Net beneficiary — extraction runs toward these actors through customer lock-in.
constraint_indexing:constraint_classification(perovskite_self_etching, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE FABRICATION COALITION (SCAFFOLD) — Organized agents developing self-assembly, nanoimprint, direct-write, and solvent-free patterning technologies see the machinability constraint as a temporary bottleneck with a sunset. These emerging techniques are maturing (nanoimprint resolution now <10nm, self-assembly patterning approaches commercialization). The coalition has agency and a plausible exit timeline — 5-10 years for alternative methods to reduce destructive lithography dependency. Low effective extraction because agency and sunset path are evident.
constraint_indexing:constraint_classification(perovskite_self_etching, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SILICON FOUNDRY PROTOCOL STANDARDS (PITON) — Traditional fab design rules and process windows (photoresist compatibility, resist tone, etching chemistries) were optimized for silicon and inorganic semiconductors. Applying them to perovskites is substantially performative — the protocols persist because they are standardized and integrated into supply chains, not because they are optimal for soft materials. Theater ratio (0.58) reflects that much of the fabrication activity (expensive resist coating, UV curing, ion beam prep) is non-functional for perovskites — it's inherited ritual from the silicon playbook. The standards persist through institutional inertia despite low functional verification for this material class.
constraint_indexing:constraint_classification(perovskite_self_etching, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational timescale, the machinability constraint exhibits both genuine coordination (perovskite research depends on access to patterning infrastructure) and asymmetric extraction (the coordination is enforced through material damage, yield loss, and supply chain lock-in). The constraint persists because silicon-era lithography is the only standardized pathway, not because it is inherently necessary for 2D perovskites. The extractiveness is moderate-to-high (0.52) because the extraction is enforced through technical suppression (material incompatibility) rather than explicit coercion. Suppression (0.68) is high: researchers have no practical alternatives within institutional timescales.
constraint_indexing:constraint_classification(perovskite_self_etching, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perovskite_self_etching_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perovskite_self_etching, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perovskite_self_etching, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(perovskite_self_etching, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(perovskite_self_etching, TR),
    TR >= 0.70.

:- end_tests(perovskite_self_etching_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The constraint extracts from perovskite researchers through multiple mechanisms: forced adoption of expensive, incompatible lithography equipment; yield loss during patterning (40-60%); and institutional dependence on silicon supply chains. The extraction is not as severe as a pure snare (0.70+) because some workarounds exist (self-assembly, direct-write prototyping) at laboratory scale, and the research community has some agency through grant funding and publication incentives. However, the extraction is higher than pure coordination (Rope, <0.35) because the incompatibility is material and suppliers profit from the dependency. Suppression (0.68): High. Significant barriers prevent escape: (1) lack of standardized alternatives within established fabs, (2) capital cost of dedicated soft-material fabrication infrastructure, (3) institutional inertia (design rules, process protocols, supplier contracts), (4) potential IP barriers to alternative technologies. But suppression is not total (0.85+) because alternative fabrication methods are maturing and some research groups have demonstrated workarounds at small scale. Theater ratio (0.58): Moderate-to-high. Traditional lithography protocols applied to perovskites are substantially performative: resist coating, UV curing, resist development, and ion beam prep are executed per silicon foundry standards but do not serve their intended function for soft materials. Much activity is protocol compliance rather than functional necessity. Theater has increased over the interval as more research groups adopted traditional lithography without understanding material incompatibility, leading to more performative process activity (elaborate troubleshooting, process optimization for incompatible techniques, expensive workarounds).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a wide perspectival gap between suppliers and victims. Lithography vendors and silicon foundries see a coordination mechanism (Rope) — perovskite integration uses their standardized tools, creating customer lock-in and pulling the research community deeper into their ecosystem. The perovskite research community sees mixed coordination and extraction (Tangled Rope) — they use the constraint to justify funding and publication pipelines, but also suffer real yield loss and cost penalties. The perovskite device yield sees pure extraction (Snare) — no benefit, only damage from incompatible chemistry. The alternative fabrication coalition sees a temporary problem with a sunset (Scaffold) — emerging technologies are approaching viability, and they have agency to accelerate the transition. The silicon foundry protocols see their own ritual as degraded (Piton) — the inherited design rules persist through standardization inertia, not because they serve perovskites. The analytical observer sees a tangled rope with institutional extraction (Tangled Rope) — genuine coordination (patterning infrastructure is necessary), but enforced through material incompatibility and supply chain lock-in rather than explicit coercion. The perspectival gap is large because supplier benefit (Rope) and victim cost (Snare) are almost perfectly opposed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (lithography vendors, silicon foundries) experience low directionality (d ≈ 0.1-0.2): they benefit from the constraint and have arbitrage exit options (can shift to other customers, other materials). Their effective extractiveness (chi) is negative or near-zero — they see the constraint as beneficial coordination. Victims (perovskite device yield, alternative fabrication researchers) experience high directionality (d ≈ 0.75-0.90): they bear the cost of incompatibility and have constrained or trapped exit options. Their effective extractiveness is amplified by high d values — they experience the constraint as severe extraction. The research community (moderate power) experiences d ≈ 0.55-0.65: they have some constrained exit options (can pursue alternative materials, can prototype at lab scale) but face institutional barriers to full escape. Their chi is moderate-to-high, reflecting mixed extraction and coordination. The alternative fabrication coalition (organized power) experiences d ≈ 0.40-0.50: they have organized capacity to develop and deploy alternatives, giving them lower directionality than trapped victims. Their effective extractiveness reflects mixed experience — they benefit from the constraint's status as an 'open problem' but bear extraction costs during the transition period.
 *
 * MANDATROPHY ANALYSIS:
 *   The machinability constraint resolves mandatrophy by disambiguating apparent naturalization from actual institutional contingency. The mountain perspective ('perovskites are inherently incompatible with all lithography') would naturalize what is actually a contingent technical-institutional arrangement. The omega variables (material damage mechanism primacy, alternative fabrication maturity) distinguish between fundamental material limits and contingent supply chain dominance. If omega_1 (material damage mechanism) resolves toward 'contingent to specific process chemistries,' the constraint collapses toward Rope (coordination with alternative materials-compatible resists). If omega_2 (alternative fabrication timeline) resolves toward '5-10 year maturity,' the scaffold sunset becomes real and extractiveness declines. If omega_3 (supply chain lock-in degree) reveals that 70%+ of suppression is institutional rather than material, the constraint is purely extractive (Snare), not natural law. The mandatrophy resolution is that the constraint is empirically Tangled Rope (mixed coordination of fabrication access + extraction through incompatibility), with a plausible sunset contingent on alternative fabrication maturation, not an immutable natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_damage_mechanism_primacy,
    'Are perovskite damage mechanisms under traditional lithography fundamental material limitations or contingent to specific process chemistries?',
    'Mechanistic analysis of photoresist solvent compatibility with halide perovskites; identification of alternative resist chemistries (inert fluoropolymers, sol-gel oxides) that do not trigger degradation; controlled experiments with modified process windows',
    'If fundamental: the constraint approaches Mountain status (inherent to the material). If contingent: alternative resist families exist, collapsing extractiveness to 0.25-0.35 (Rope). This is the primary axis of uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(material_damage_mechanism_primacy, empirical, 'Whether perovskite damage is material-fundamental or process-contingent').

omega_variable(
    alternative_fabrication_maturity_timeline,
    'Will nanoimprint lithography, self-assembly patterning, or direct-write techniques achieve cost-parity and yield-parity with traditional lithography for 2D perovskites within 5-10 years?',
    'Technology readiness level assessment; comparison of unit economics ($/chip), defect density, and throughput for emerging methods vs traditional lithography on test structures; roadmap tracking from equipment vendors and research groups',
    'If timeline is realistic: scaffold classification confirmed, sunset path is real. If timeline slips beyond 15 years: scaffold downgrade to tangled_rope (mixed extraction + coordination with uncertain exit). If timeline extends to >20 years: reclassifies toward snare (extraction persists indefinitely).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fabrication_maturity_timeline, empirical, 'Timeline for cost-parity of alternative fabrication methods').

omega_variable(
    silicon_supply_chain_lock_in_degree,
    'How much of the perovskite machinability problem is enforced by institutional dependence on silicon foundries vs inherent material incompatibility?',
    'Comparative analysis of perovskite yield in dedicated soft-material fabs (few exist globally) vs silicon fabs; capital cost analysis for building vertical integration; survey of research groups that bypassed traditional lithography entirely',
    'If institutional dependence dominates (>70% of suppression): beneficiary extraction is largely through lock-in, not material necessity — reclassifies extractiveness toward 0.65+ (pure snare). If material incompatibility dominates (>60% of suppression): constraint approaches Mountain or Rope (depending on availability of material-compatible alternatives). This determines whether the constraint is contingent or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silicon_supply_chain_lock_in_degree, empirical, 'Degree of silicon supply chain lock-in vs material incompatibility').

omega_variable(
    cross_licensing_barrier_strength,
    'Do alternative fabrication technologies (nanoimprint, self-assembly) face IP barriers or materials-based restrictions that prevent rapid deployment for perovskites?',
    'Patent landscape analysis; licensing availability and cost for emerging patterning tools; materials compatibility data from vendor technical specifications; identification of IP control concentration',
    'If barriers are strong: alternative coalition faces institutional extractiveness in addition to technical challenges — mandatrophy becomes multidimensional (extraction by incumbents + extraction by IP holders). If barriers are weak: alternatives scale rapidly, collapsing the constraint toward Rope (pure coordination). This determines whether the scaffold sunset is technically feasible or blocked by institutional mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_licensing_barrier_strength, conceptual, 'IP and licensing barriers to alternative fabrication deployment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perovskite_self_etching, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pero_tr_t0, perovskite_self_etching, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pero_tr_t5, perovskite_self_etching, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pero_tr_t10, perovskite_self_etching, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pero_be_t0, perovskite_self_etching, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pero_be_t5, perovskite_self_etching, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(pero_be_t10, perovskite_self_etching, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perovskite_self_etching, resource_allocation).
narrative_ontology:affects_constraint(perovskite_self_etching, lead_halide_perovskite_stability).
narrative_ontology:affects_constraint(perovskite_self_etching, perovskite_solar_cell_certification_gap).
narrative_ontology:affects_constraint(perovskite_self_etching, semiconductor_manufacturing_vertical_integration).

% DUAL FORMULATION NOTE:
% The machinability constraint is downstream of material-level lead halide perovskite chemistry (instability, bandgap tunability) and upstream of device-level challenges (certification, encapsulation). The constraint represents a distinct structural phenomenon: the dominance of silicon-era lithography techniques when applied to soft materials. Alternative fabrication methods (nanoimprint, self-assembly, direct-write) are maturing as a parallel pathway that bypasses this constraint entirely, making it a temporary institutional lock-in rather than a fundamental material limit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(perovskite_self_etching, institutional, 0.15).
constraint_indexing:directionality_override(perovskite_self_etching, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
