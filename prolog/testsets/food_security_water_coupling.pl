% ============================================================================
% CONSTRAINT STORY: food_security_water_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_food_security_water_coupling, []).

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
 *   constraint_id: food_security_water_coupling
 *   human_readable: Food Security-Water Resource Coupling
 *   domain: environmental/agricultural/economic
 *
 * SUMMARY:
 *   Food security and water availability are locked together through
 *   agricultural water demand, creating a tangled rope constraint where
 *   coordination (global trade redistributing water-embedded commodities,
 *   transboundary water treaties, food security provisioning) coexists with
 *   asymmetric extraction (aquifer depletion concentrated on vulnerable
 *   farmers, hidden water costs in commodity prices, downstream riparian
 *   states absorbing flow reductions). The constraint exhibits all six DR
 *   types from different perspectives. Over the 50-year interval,
 *   extractiveness has increased from 0.35 to 0.58 as aquifer depletion has
 *   accelerated, commodity prices have decoupled from water availability
 *   through subsidy architectures, and climate variability has increased
 *   suppression (reduced precipitation in food-producing regions). Theater
 *   ratio has increased from 0.32 to 0.48, reflecting growing gap between
 *   subsidy narratives ('protecting food security') and actual outcomes
 *   (accelerating depletion). The constraint is not naturally immutable —
 *   viable exit pathways exist through precision agriculture, alternative
 *   proteins, and reallocation of commodity crop subsidies toward
 *   drought-resistant crops — but these pathways remain constrained by
 *   institutional inertia and concentrated beneficiary opposition. The
 *   scaffold perspective identifies a sunset clause around 20-30 year horizon
 *   if innovation coalitions can scale alternative proteins and
 *   water-productivity technologies.
 *
 * KEY AGENTS:
 *   - Small-holder groundwater-dependent farmers: Primary victims (powerless/trapped) — face aquifer depletion with no exit options; bear full cost of unsustainable extraction
 *   - Groundwater-dependent communities: Victims (powerless/trapped/constrained) — face water scarcity, health impacts, and livelihood loss as aquifers deplete
 *   - Industrial agriculture-export complex: Primary beneficiaries (institutional/arbitrage) — capture profit margin on water-intensive commodities while externalizing depletion costs
 *   - Commodity exporters: Beneficiaries (organized/institutional) — gain economy-of-scale advantages and foreign exchange through water-intensive crop export; absorb internal depletion costs through externalization
 *   - Food-importing nations: Mixed (moderate/constrained) — benefit from cheap food security but exposed to strategic vulnerability and hidden water externality
 *   - Upstream riparian states: Organized extractors (organized/constrained) — benefit from upstream water diversion; constrained by transboundary treaties and sovereignty claims embedded in extraction
 *   - Downstream riparian states: Victims (organized/constrained) — absorb flow reductions from upstream extraction; cannot easily exit without regional political restructuring
 *   - Water-productivity innovation coalition: Organized agents building exit pathways (organized/constrained) — UN FAO, agricultural research institutes, precision irrigation developers, alternative protein platforms
 *   - Agricultural subsidy system: Institutional theater-maintainer (institutional/arbitrage) — maintains performative subsidy architecture that masks water costs and subsidizes depletion
 *   - Analytical observer: Risks false summit (analytical/analytical) — tempted to naturalize contingent institutional coupling as inherent agricultural limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(food_security_water_coupling, 0.58).
domain_priors:suppression_score(food_security_water_coupling, 0.65).
domain_priors:theater_ratio(food_security_water_coupling, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(food_security_water_coupling, extractiveness, 0.58).
narrative_ontology:constraint_metric(food_security_water_coupling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(food_security_water_coupling, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(food_security_water_coupling, tangled_rope).
narrative_ontology:human_readable(food_security_water_coupling, "Food Security-Water Resource Coupling").
narrative_ontology:topic_domain(food_security_water_coupling, "environmental/agricultural/economic").

domain_priors:requires_active_enforcement(food_security_water_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(food_security_water_coupling, industrial_agriculture_operators).
narrative_ontology:constraint_beneficiary(food_security_water_coupling, water_extraction_infrastructure_owners).
narrative_ontology:constraint_beneficiary(food_security_water_coupling, commodity_export_nations).
narrative_ontology:constraint_victim(food_security_water_coupling, small_holder_farmers).
narrative_ontology:constraint_victim(food_security_water_coupling, groundwater_dependent_communities).
narrative_ontology:constraint_victim(food_security_water_coupling, downstream_riparian_states).
narrative_ontology:constraint_victim(food_security_water_coupling, future_food_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GROUNDWATER-DEPENDENT FARMER (SNARE) — Trapped in unsustainable aquifer depletion cycle. No exit options: agriculture is sole livelihood, irrigation is mandatory for competitive yields in water-scarce regions, and groundwater is the only available water source. Suppression is maximum — externally enforced by commodity market prices and internally by necessity. Experiences pure extraction: bears full depletion cost while benefiting only marginally from global food commodity supply.
constraint_indexing:constraint_classification(food_security_water_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FOOD-IMPORTING NATION (TANGLED ROPE) — Constrained by dependence on water-intensive crop imports but benefits from cheap food security. Genuine coordination function exists (global trade redistributes water-embedded in commodities). Asymmetric extraction embedded: constraint forces reliance on exporting nations' water depletion, creating hidden vulnerability. Can theoretically exit through domestic production but faces high reconfiguration costs. Mixed experience: benefits from cheap food offset by strategic vulnerability and water externalization.
constraint_indexing:constraint_classification(food_security_water_coupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL AGRICULTURE-EXPORT COMPLEX (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences constraint as pure coordination: global water redistribution through commodity trade enables economies of scale and competitive advantage. Can exit costlessly (shift sourcing, arbitrage water-rich vs water-scarce regions). Effective extraction runs toward this agent — they capture margin on water-embedded commodities while externalizing depletion costs.
constraint_indexing:constraint_classification(food_security_water_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UPSTREAM RIPARIAN STATE (TANGLED ROPE) — Organized institutional actor with constrained exit. Coordination function: transboundary water treaties enable shared basin management and mutual security. Asymmetric extraction embedded: upstream state benefits from extraction of shared water for agriculture/energy but cannot fully exit (water rights are embedded in sovereignty claim, hydroelectric infrastructure, agricultural economy). Downstream states absorb flow reduction costs. Both cooperation (treaty framework) and asymmetric extraction (water diversion) are real.
constraint_indexing:constraint_classification(food_security_water_coupling, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: WATER-PRODUCTIVITY INNOVATION COALITION (SCAFFOLD) — Organized agents (UN FAO, agricultural research institutes, drought-resistant crop programs) see the coupling as a temporary coordination failure with a sunset clause: precision irrigation, crop breeding, alternative proteins, and virtual water trade reallocation are building pathways to decouple food security from unsustainable water extraction. Low effective extraction because the coalition has agency and a clear exit mechanism. Suppression declines over the generational horizon as technologies mature and norms shift.
constraint_indexing:constraint_classification(food_security_water_coupling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: AGRICULTURAL SUBSIDY SYSTEM (PITON) — Performative institutional mechanism maintaining water-intensive crop production in water-scarce regions through price support and externality masking. Subsidy structure creates illusion of sustainable agriculture while subsidizing aquifer depletion. Theater is high (subsidies presented as food security protection) while functional verification is low (actual outcomes are depletion acceleration). Maintained through political inertia rather than structural necessity. Alternative mechanisms (virtual water trade, precision agriculture, alternative proteins) exist but institutional resistance persists.
constraint_indexing:constraint_classification(food_security_water_coupling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some coupling between food production and water is inherent to agriculture: crops require water, drier regions have less water, and the gap between food demand and local water availability is a structural feature of human geography. This perspective sees the constraint as immutable natural law. However, the structural data contradicts the mountain classification — the engine will detect this as a false summit, revealing that the 'inherent to agriculture' framing naturalizes what is actually a contingent choice between sustainability pathways.
constraint_indexing:constraint_classification(food_security_water_coupling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(food_security_water_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(food_security_water_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(food_security_water_coupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(food_security_water_coupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(food_security_water_coupling, TR),
    TR >= 0.70.

:- end_tests(food_security_water_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts through multiple mechanisms: (1) commodity price structure that decouples food cost from water cost, hiding externality in subsidy architecture; (2) temporal asymmetry — benefits accrue immediately to exporters while depletion costs accumulate over decades; (3) geographic asymmetry — water-scarce food-producing regions bear depletion while food-consuming regions externalize cost through imports. The value of 0.58 reflects that genuine coordination exists (global trade does solve immediate food availability problem) but extraction is substantial and growing. Starting value of 0.35 reflects lower global water stress 50 years ago; current 0.58 reflects accelerating depletion and climate variability. Suppression (0.65): High and structural. Multiple suppression mechanisms: (1) agricultural commodity structure tied to water-intensive crops (wheat, corn, rice); (2) subsidy architecture that makes water-intensive production more profitable than alternatives; (3) infrastructure lock-in (irrigation systems designed for high-volume extraction); (4) market lock-in (commodity prices driven by lowest-cost producers, forcing water depletion to remain competitive); (5) informational suppression (water costs hidden in agricultural subsidies, not visible in commodity prices); (6) temporal suppression (depletion is gradual, reaching critical thresholds only after decades of committed extraction). Theater ratio (0.48): Moderate and increasing. Subsidy narratives present water-intensive agriculture as 'protecting food security' while actual outcomes accelerate depletion and undermine long-term food security. Agricultural research investment framed as 'productivity' while measuring yield per capita rather than yield per unit water. Water use efficiency improvements framed as sustainability while applied to increase total extraction volume (Jevons paradox effect).
 *
 * PERSPECTIVAL GAP:
 *   Largest gaps: (1) Trapped farmer (snare, d≈0.95) vs industrial exporter (rope, d≈0.05) — same constraint yields opposite classification. (2) Agricultural subsidy system (piton, sees own degradation) vs commodity export complex (rope, sees pure benefit). (3) Civilizational observer (mountain, naturalizes coupling) vs scaffold coalition (scaffold, sees contingent sunset). These gaps arise from: (a) asymmetric exit options — beneficiaries can arbitrage, victims are locked in; (b) temporal framing — immediate beneficiaries vs long-term vulnerability; (c) information asymmetry — extraction costs hidden in subsidy architecture; (d) scale mismatch — local depletion externalized to global commodity price structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives d from (beneficiary/victim status, power level, exit options, scope). Beneficiaries with arbitrage → low d (≈0.05-0.20). Victims with constrained exit → high d (≈0.65-0.95). Mixed agents (food importers, upstream riparians) → moderate d (≈0.50-0.70). The piton perspective shows an institutional actor that sees its own mechanism as degraded — the subsidy system persists not because it functions but because alternatives haven't fully replaced it. This is the diagnostic signature of inertial maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL EXEMPLAR: The coupling constraint resolves mandatrophy by showing that all six types are legitimate structural readings. The snare (trapped farmer) is real — depletion is happening, suppression is real, extraction is experienced. The rope (exporter) is real — global trade genuinely provides coordination benefit and low friction. The tangled rope (importer, riparian) is real — mixed coordination and extraction coexist. The scaffold (innovation coalition) is real — viable exits exist with 20-30 year horizon. The piton (subsidy system) is real — institutional theater is measurable (theater_ratio = 0.48) and increasing. The mountain (false summit) is a real misclassification diagnostic — it reveals where natural-law framing naturalizes contingent institutional architecture. No single type 'wins' — the perspectival family IS the accurate model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    virtual_water_accounting_validity,
    'Does virtual water trade genuinely redistribute water stress or merely mask extraction through commodity trade semantics?',
    'Hydrological analysis of actual water saved in exporting vs importing regions; accounting for non-tradeable water footprints and local depletion irreversibility',
    'If valid: virtual water is real exit pathway for import-dependent nations. If semantic: trade is illusion of water transfer while both exporting and importing regions deplete local groundwater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(virtual_water_accounting_validity, empirical, 'Whether virtual water trade genuinely redistributes hydrological stress').

omega_variable(
    yield_plateau_vs_intensification_logic,
    'Are agricultural water requirements driven by physics of photosynthesis/transpiration or by intensification choices (monoculture, commodity crop focus, subsidized prices)?',
    'Comparison of water efficiency in diverse cropping systems vs industrial monoculture; crop-mix analysis for caloric output per unit water across different agricultural models',
    'If physics-driven: coupling is near-immutable. If choice-driven: coupling is a tangled rope held together by agricultural policy architecture, not natural limits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(yield_plateau_vs_intensification_logic, empirical, 'Whether water requirements are physics-constrained or policy-driven').

omega_variable(
    alternative_protein_substitution_feasibility,
    'Can plant-based and cellular agriculture technologies scale to replace water-intensive commodity crop production within a 20-year horizon?',
    'Technology readiness assessment; cost trajectory analysis; infrastructure capacity modeling for alternative protein production',
    'If feasible: scaffold sunset is real — constraint will dissolve as production pathways shift. If infeasible: coupling persists as structural constraint independent of innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_protein_substitution_feasibility, empirical, 'Whether alternative proteins can replace water-intensive commodity production').

omega_variable(
    aquifer_recovery_reversibility,
    'Once an aquifer reaches critical depletion threshold, is recharge physically possible or has the extraction created an irreversible state change?',
    'Hydrological modeling of specific overexploited aquifers; paleoclimate data on recharge rates under current and future climatic regimes',
    'If reversible: constraint can be broken by halting extraction. If irreversible: constraint becomes mountain-like (immutable past extraction embedded in future water availability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(aquifer_recovery_reversibility, empirical, 'Whether aquifer depletion is reversible or creates permanent state change').

omega_variable(
    distributional_incidence_obfuscation,
    'How much of the measured suppression is structural (physical barriers to exit) vs deliberate framing (hiding water costs in commodity prices, masking depletion through subsidy rhetoric)?',
    'Audit of agricultural subsidy allocation and depletion externality accounting; comparison of farmer-perceived vs actual long-term cost; policy analysis of alternative framings tested',
    'If structural: suppression is immutable without major economic restructuring. If framing-dependent: suppression persists through active narrative maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_incidence_obfuscation, conceptual, 'Extent to which suppression is structural vs narrative-maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(food_security_water_coupling, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fswc_tr_t0, food_security_water_coupling, theater_ratio, 0, 0.32).
narrative_ontology:measurement(fswc_tr_t15, food_security_water_coupling, theater_ratio, 15, 0.42).
narrative_ontology:measurement(fswc_tr_t30, food_security_water_coupling, theater_ratio, 30, 0.48).
narrative_ontology:measurement(fswc_tr_t45, food_security_water_coupling, theater_ratio, 45, 0.55).

% Extraction over time
narrative_ontology:measurement(fswc_be_t0, food_security_water_coupling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fswc_be_t15, food_security_water_coupling, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fswc_be_t30, food_security_water_coupling, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(fswc_be_t45, food_security_water_coupling, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(food_security_water_coupling, resource_allocation).
narrative_ontology:affects_constraint(food_security_water_coupling, aquifer_depletion_irreversibility).
narrative_ontology:affects_constraint(food_security_water_coupling, agricultural_subsidy_architecture).
narrative_ontology:affects_constraint(food_security_water_coupling, transboundary_water_rights_allocation).
narrative_ontology:affects_constraint(food_security_water_coupling, climate_precipitation_variability).

% DUAL FORMULATION NOTE:
% Food security-water coupling decomposes into multiple structurally distinct constraints: (1) aquifer_depletion_irreversibility (ε≈0.08, mountain — once certain thresholds are crossed, recovery is physically impossible); (2) agricultural_subsidy_architecture (ε≈0.72, snare — subsidy structure actively incentivizes depletion); (3) transboundary_water_rights_allocation (ε≈0.55, tangled rope — genuine coordination embedded in asymmetric extraction). The coupling story unifies these at the level of food security provisioning. Each story has different ε because they answer different questions about the same domain. The network links show how deterioration in one constraint (aquifer irreversibility crossing threshold) would affect others (subsidy effectiveness, rights allocation viability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(food_security_water_coupling, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
