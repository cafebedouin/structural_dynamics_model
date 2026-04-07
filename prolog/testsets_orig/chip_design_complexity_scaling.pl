% ============================================================================
% CONSTRAINT STORY: chip_design_complexity_scaling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chip_design_complexity_scaling, []).

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
 *   constraint_id: chip_design_complexity_scaling
 *   human_readable: Chip Design Complexity Scaling Constraint
 *   domain: semiconductor_engineering/design_methodology
 *
 * SUMMARY:
 *   Chip design complexity has grown exponentially over four decades,
 *   following Moore's Law and increasing design freedom (heterogeneous
 *   architectures, power management, security features). This growth creates
 *   an extraction mechanism: as design complexity exceeds human cognitive
 *   capacity, teams must rely on proprietary design automation tools,
 *   specialized expertise, and access to manufacturing process design kits.
 *   The constraint appears natural — complexity management requires tools —
 *   but closer analysis reveals a hybrid structure where genuine coordination
 *   (tools enable impossible designs) is layered with institutional
 *   extraction (tool licensing, IP moats, process gatekeeping). The
 *   constraint exhibits all six types from different perspectives: snare for
 *   startups trapped by tool costs and knowledge barriers, rope for tool
 *   vendors solving coordination problems, tangled rope for large firms
 *   gaining both benefit and asymmetric advantage, piton for standardized
 *   methodologies that persist through institutional inertia rather than
 *   functional necessity, scaffold for open-source EDA initiatives building
 *   alternative pathways, and (falsely) mountain for analytical observers who
 *   naturalize institutional extraction as inherent complexity scaling.
 *
 * KEY AGENTS:
 *   - Startup Design Team: Primary victim (powerless/trapped) — cannot access tool infrastructure, process information, or manufacturing capacity without accepting extraction
 *   - Design Automation Vendor: Primary beneficiary (institutional/arbitrage) — captures high licensing revenue; provides genuine coordination service; has arbitrage exit options through customer diversification
 *   - Large Semiconductor Firm: Secondary beneficiary (powerful/mobile) — benefits from coordination and extraction; has mobile exit options (internal tools, acquisition of design expertise, process relationships)
 *   - Academic Research Group: Secondary victim (moderate/constrained) — benefits from reduced-cost licenses and open tools but constrained by lack of manufacturing access and publication delays
 *   - Standardized Methodology: Institutional structure (institutional/arbitrage) — maintains procedural legitimacy through inertia despite reduced functional value at advanced nodes
 *   - Open-Source EDA Movement: Organized victim-response (organized/constrained) — building alternative pathways with genuine coordination benefit and sunset potential
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional extraction as inherent to complexity scaling
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chip_design_complexity_scaling, 0.52).
domain_priors:suppression_score(chip_design_complexity_scaling, 0.58).
domain_priors:theater_ratio(chip_design_complexity_scaling, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chip_design_complexity_scaling, extractiveness, 0.52).
narrative_ontology:constraint_metric(chip_design_complexity_scaling, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(chip_design_complexity_scaling, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chip_design_complexity_scaling, tangled_rope).
narrative_ontology:human_readable(chip_design_complexity_scaling, "Chip Design Complexity Scaling Constraint").
narrative_ontology:topic_domain(chip_design_complexity_scaling, "semiconductor_engineering/design_methodology").

domain_priors:requires_active_enforcement(chip_design_complexity_scaling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chip_design_complexity_scaling, design_automation_vendors).
narrative_ontology:constraint_beneficiary(chip_design_complexity_scaling, large_semiconductor_firms).
narrative_ontology:constraint_victim(chip_design_complexity_scaling, startup_design_teams).
narrative_ontology:constraint_victim(chip_design_complexity_scaling, academic_research_groups).
narrative_ontology:constraint_victim(chip_design_complexity_scaling, design_methodology_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STARTUP DESIGN TEAM (SNARE) — Trapped by absolute complexity scaling. Cannot design competitive chips without: proprietary design tools ($5M+/year), specialized training (3-5 year ramp), access to manufacturing process design kits (PDKs, restricted), and verification infrastructure. No exit: the complexity floor is enforced by physics and process technology, making suppression structural. Startups must accept extraction through tool licensing, IP constraints, and manufacturing gatekeeping or abandon chip design entirely.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DESIGN AUTOMATION VENDOR (ROPE) — Experiences the constraint as coordination mechanism. Provides tools that enable designers to manage complexity scaling; enables the entire chip design ecosystem; benefits from licensing revenue and industry standardization. The extraction they capture (high tool costs) funds the genuine service of complexity management. Net coordination view — they solve a real problem.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LARGE SEMICONDUCTOR FIRM (TANGLED ROPE) — Benefits from coordination (access to design tools, manufacturing, ecosystem expertise) but also extracts from ecosystem through: patent moats on advanced design techniques, privileged access to process technology ahead of competitors, ability to negotiate favorable tool pricing. Both coordination and asymmetric extraction present. Significant agency (mobile exit options) but also locked into tool/fab relationships.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC RESEARCH GROUP (TANGLED ROPE) — Access to reduced-cost academic tool licenses and open-source EDA tools provides genuine coordination benefit for research and education. But constrained by: limited access to cutting-edge process design kits, inability to tape out advanced nodes, and publication constraints that slow methodology innovation from reaching practice. Mixed benefit and burden.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STANDARDIZED DESIGN METHODOLOGY (PITON) — Industry-standard methodologies (design-for-manufacturability, timing-closure flows, sign-off procedures) have become largely performative. Designers follow standard steps (place-and-route, static timing analysis, power delivery verification) that are necessary but insufficient for advanced nodes where new failure modes dominate (electromigration, power delivery noise, thermal hotspots). High theater ratio (0.64) reflects procedures maintained through institutional inertia: design teams follow methodology because it's the standard, not because it robustly ensures correctness.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN-SOURCE EDA MOVEMENT (SCAFFOLD) — OpenROAD, IHP PDK, and similar initiatives represent organized efforts to build alternative pathways that bypass proprietary tool gatekeeping. They see the complexity scaling as solvable through distributed design methodology innovation. Constrained (requires sustained funding and community effort) but providing genuine coordination benefit with a sunset: if open tools reach feature parity with commercial tools, the proprietary extraction mechanism loses force. Estimated sunset: 5-15 years depending on process node and application domain.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COMPLEXITY GROWTH VIEW (MOUNTAIN) — From first principles, chip complexity scales exponentially with feature count and design freedom (Moore's Law). This growth is governed by physics and information theory, not institutions: you cannot design complex systems without tools to manage state space, and managing state space is inherently extractive (requires cognitive resources, computational power, verification overhead). The constraint appears immutable — complexity must increase, and someone must bear the cost. However, the structural data reveals this as a false summit: the acceleration of extraction beyond physics-constrained levels is driven by proprietary tool licensing, gatekeeping of process information, and IP moats — not by physics alone.
constraint_indexing:constraint_classification(chip_design_complexity_scaling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chip_design_complexity_scaling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chip_design_complexity_scaling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chip_design_complexity_scaling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chip_design_complexity_scaling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chip_design_complexity_scaling, TR),
    TR >= 0.70.

:- end_tests(chip_design_complexity_scaling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through multiple mechanisms: (1) tool licensing creates direct financial capture (~$5-15M/year for large firms), (2) PDK restrictions create information asymmetry that inflates design risk for those without access, (3) expertise concentration in large firms creates dependency on hiring or acquisition. The rising trajectory (0.28 → 0.52 over 20 years) reflects that complexity scaling has outpaced the rate at which tool innovation and skill distribution have caught up — the gap between design challenge and available solutions has widened. Suppression (0.58): Moderate-high. Multiple barriers reinforce the extraction: (1) tool cost creates direct financial suppression, (2) specialized knowledge (timing closure, power delivery, design-for-manufacturability) requires years to accumulate, (3) manufacturing access is restricted by long-term fab partnerships and process technology agreements, (4) open-source alternatives exist but lag commercial tools in capability and support. Suppression is not total because some actors (large firms, well-funded startups) can overcome barriers. Theater ratio (0.64): Moderate-high. Industry-standard design methodologies (static timing analysis, place-and-route, sign-off procedures) have become substantially performative. Designers follow these steps because they are the standard and because process complexity requires some verification process, but the procedures have reduced functional validity as failure modes shift (electromigration, power delivery, thermal effects dominate advanced nodes more than timing). The theater has increased as the gap between what methodologies address and what actually fails at advanced nodes has grown.
 *
 * PERSPECTIVAL GAP:
 *   The most acute perspectival gap separates the startup (snare) from the tool vendor (rope). The startup experiences the constraint as inescapable extraction: they must pay high licensing costs, wait 3-5 years for expertise ramp, accept reduced feature sets due to tool limitations, and accept gatekeeping restrictions on advanced process nodes. The tool vendor experiences the constraint as a coordination service: they solve the real problem of managing design complexity, enable businesses that would otherwise be impossible, and deserve licensing revenue proportional to their value. Both perspectives are structurally correct from within their position. The snare classification for startups is not wrong; neither is the rope classification for vendors. The gap reveals that the coordination problem (complexity management) is real, but the institutional solution (proprietary tool licensing and gatekeeping) concentrates extraction among those who can pay while excluding those who cannot, despite lower-cost alternatives existing (open-source EDA). The tangled rope perspective (large firms gaining both coordination benefit and asymmetric advantage) reveals the extraction mechanism: large firms can internalize design automation costs and manufacturing relationships, converting the constraint from external suppression into competitive advantage over smaller rivals.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural position. Startups (powerless/trapped) have d ≈ 0.95 (full targets): they bear suppression with no exit. Tool vendors (institutional/arbitrage) have d ≈ 0.05 (beneficiaries): they collect extraction while providing service. Large firms (powerful/mobile) have d ≈ 0.50 (both): they benefit from coordination and extraction but can also extract from smaller competitors. Academic groups (moderate/constrained) have d ≈ 0.65 (net targets): they face significant barriers but have some institutional protection and benefit from reduced-cost access. Open-source initiatives (organized/constrained) have d ≈ 0.55 (both): they are constrained by lack of resources but have organized agency and genuine alternative-building capacity. The directionality distribution shows strong asymmetry: powerless actors bear maximum extraction, organized actors have agency to challenge extraction, and institutional/powerful actors benefit disproportionately.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH NETWORK DECOMPOSITION: The constraint decomposes into two structurally distinct claims: (1) Complexity scaling itself (ε ≈ 0.15, Mountain) — the inherent information-theoretic cost of managing design state space increases with complexity; (2) Institutional gatekeeping of tools and process information (ε ≈ 0.52, Tangled Rope) — the extraction mechanism that amplifies the complexity problem beyond its inherent cost. The false summit (mountain perspective) occurs when these are conflated. The analytics correctly separates them: complexity scaling is real and somewhat inescapable (lower extraction), but the institutional restrictions on tool access and process information are contingent and can be reformed (higher extraction). The tangled rope classification holds because genuine coordination (tools enable complex designs) coexists with institutional extraction (gatekeeping and licensing restrict access), and active enforcement (patent moats, licensing agreements, IP restrictions) maintains both simultaneously. The mandatrophy resolves to: complexity management requires coordination (rope), but current institutional arrangements layer extraction onto that coordination (tangled rope), and open-source alternatives represent a genuine sunset pathway to reduced extraction (scaffold).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_floor_location,
    'What portion of design complexity scaling is inherent to physics/information theory vs. contingent on institutional choices (tool licensing, IP restrictions, process gatekeeping)?',
    'Comparative analysis of open-source vs proprietary tool capabilities at the same process node; measurement of design effort required with optimal information disclosure vs actual information asymmetry; correlation of complexity scaling slope with institutional changes in tool/PDK access',
    'If floor is high (>70% inherent): the snare classification is justified as physics-enforced extraction. If floor is lower (<50% inherent): extraction is primarily institutional, suggesting the scaffold sunset is real and accessible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_floor_location, empirical, 'Inherent vs institutional components of complexity scaling').

omega_variable(
    alternative_design_methodology_sufficiency,
    'Can alternative design methodologies (chiplet composition, simplified standard cells, reduced design margin) manage complexity scaling with comparable power/performance/area tradeoffs to monolithic designs, or are they fundamentally limited?',
    'Longitudinal tracking of chiplet-based designs vs monolithic designs across technology generations; measurement of aggregate system complexity and design effort; comparison of risk/cost tradeoffs',
    'If alternative methodologies are sufficiently competitive: the constraint is coordination failure (Rope/Scaffold from more perspectives) rather than extraction. If monolithic designs remain strongly preferred: the snare mechanism is robust because complexity scaling forces the tradeoff into proprietary tools.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_design_methodology_sufficiency, empirical, 'Whether alternative design methodologies can compete with monolithic designs').

omega_variable(
    design_tool_feature_parity_timeline,
    'Can open-source EDA tools achieve feature parity with commercial tools within 10-15 years, or is the proprietary advantage structural and persistent?',
    'Roadmap analysis of OpenROAD, IHP, and similar projects; measurement of design capability gaps (timing accuracy, DFM coverage, optimization quality); tracking of commercial tool innovation pace vs open-source effort scaling',
    'If parity achievable: scaffold perspective confirmed, and the extraction window is temporary. If not: the snare mechanism persists, and open-source tools remain secondary for advanced-node design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(design_tool_feature_parity_timeline, empirical, 'Timeline for open-source EDA feature parity').

omega_variable(
    suppression_mechanism_composition,
    'Is the measured suppression (0.58) primarily tool cost barriers, skill/knowledge barriers, manufacturing gatekeeping, or some combination?',
    'Decomposition of suppression across multiple dimensions: cost surveys of tool licensing, analysis of ramp-time and knowledge transfer, mapping of PDK access restrictions by firm size and geography, measurement of design tape-out barriers',
    'If tool cost dominates: open-source tools address the constraint directly. If manufacturing gatekeeping dominates: the constraint persists regardless of tool availability. If knowledge/skill dominates: the constraint is solvable through education/training rather than institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Composition of suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chip_design_complexity_scaling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chip_design_tr_t0, chip_design_complexity_scaling, theater_ratio, 0, 0.48).
narrative_ontology:measurement(chip_design_tr_t10, chip_design_complexity_scaling, theater_ratio, 10, 0.58).
narrative_ontology:measurement(chip_design_tr_t20, chip_design_complexity_scaling, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(chip_design_be_t0, chip_design_complexity_scaling, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(chip_design_be_t10, chip_design_complexity_scaling, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(chip_design_be_t20, chip_design_complexity_scaling, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chip_design_complexity_scaling, resource_allocation).
narrative_ontology:affects_constraint(chip_design_complexity_scaling, semiconductor_manufacturing_access).
narrative_ontology:affects_constraint(chip_design_complexity_scaling, design_expertise_concentration).
narrative_ontology:affects_constraint(chip_design_complexity_scaling, process_technology_gatekeeping).

% DUAL FORMULATION NOTE:
% The chip design complexity constraint decomposes into inherent complexity scaling (physics/information theory, lower extractiveness) and institutional gatekeeping (tool licensing, PDK restrictions, IP moats, higher extractiveness). Each operates with different ε values and should be evaluated separately. The present story captures the hybrid institutional constraint; the decomposed physics constraint would show ε ≈ 0.15 (mountain-adjacent) and would represent the coordination problem absent institutional mediation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chip_design_complexity_scaling, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
