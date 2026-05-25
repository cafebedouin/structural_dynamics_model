% ============================================================================
% CONSTRAINT STORY: nitrogen_phosphorus_cycle_disruption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nitrogen_phosphorus_cycle_disruption, []).

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
 *   constraint_id: nitrogen_phosphorus_cycle_disruption
 *   human_readable: Nitrogen and Phosphorus Cycle Disruption
 *   domain: biogeochemical/environmental
 *
 * SUMMARY:
 *   The nitrogen and phosphorus cycle disruption is a planetary-scale
 *   constraint that arises from the systematic replacement of biological
 *   nutrient cycling with synthetic chemical inputs. The Haber-Bosch
 *   synthesis of ammonia (1909) solved a genuine coordination problem:
 *   agricultural soils were being depleted faster than natural nitrogen
 *   fixation could replenish them, creating a food security crisis.
 *   Industrial nitrogen fertilizers enabled population growth from ~1.6
 *   billion (1900) to >8 billion today. However, the constraint has
 *   metamorphosed: synthetic inputs now represent a massive uncontrolled
 *   experiment in biogeochemical manipulation. Excess nitrogen and phosphorus
 *   leach into aquatic systems, creating dead zones where oxygen depletion
 *   kills most life; accumulate in atmospheric pools threatening the ozone
 *   layer and altering climate forcing; and gradually degrade soil structure
 *   and microbial communities that biological nutrient cycling depends on.
 *   The constraint exhibits tangled rope structure: genuine coordination
 *   function (reliable nutrient supply for food production) coexists with
 *   severe asymmetric extraction (externalities absorbed by aquatic
 *   ecosystems, downstream communities, and the planetary nitrogen-phosphorus
 *   balance). Industrial agriculture and fertilizer manufacturers benefit
 *   from the constraint; aquatic ecosystems and long-term soil fertility bear
 *   the costs.
 *
 * KEY AGENTS:
 *   - Aquatic Ecosystems: Primary victims (powerless/trapped) — dead zones, eutrophication, oxygen depletion; no exit option; absorb nutrient runoff from continental-scale drainage basins
 *   - Downstream Communities: Primary victims (powerless/trapped) — water treatment costs, drinking water contamination, fisheries collapse, loss of recreation; geographically trapped; dependent on affected aquatic systems
 *   - Industrial Agriculture Sector: Primary beneficiary (institutional/arbitrage) — obtains reliable, scalable nutrient inputs at low cost; externalizes pollution costs; can relocate operations or shift supply chains
 *   - Fertilizer Manufacturers: Primary beneficiary (institutional/arbitrage) — global commodity market worth ~$200B annually; extraction through price markup and market control; arbitrage options across regions
 *   - Regional Farming Communities: Secondary actor (moderate/constrained) — benefit from coordination (soil fertility, productivity) but also bear suppression (debt, crop contracts, long-term soil degradation, conversion barriers)
 *   - Regenerative Agriculture Networks: Organized challenger (organized/constrained) — building alternative pathways (cover crops, nitrogen-fixing legumes, composting); constraint from resource requirements and knowledge barriers, but perceive sunset in biological cycling restoration
 *   - Regulatory Bodies: Institutional actor (institutional/arbitrage) — maintain nutrient runoff standards and monitoring frameworks; largely performative (theater ratio 0.55); captured by agricultural interests; functional enforcement weak
 *   - Analytical Observer: Civilizational context (analytical/analytical) — biogeochemical perspective reveals the hybrid structure: coordination function coexisting with catastrophic externalities at planetary scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nitrogen_phosphorus_cycle_disruption, 0.58).
domain_priors:suppression_score(nitrogen_phosphorus_cycle_disruption, 0.68).
domain_priors:theater_ratio(nitrogen_phosphorus_cycle_disruption, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nitrogen_phosphorus_cycle_disruption, extractiveness, 0.58).
narrative_ontology:constraint_metric(nitrogen_phosphorus_cycle_disruption, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nitrogen_phosphorus_cycle_disruption, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nitrogen_phosphorus_cycle_disruption, tangled_rope).
narrative_ontology:human_readable(nitrogen_phosphorus_cycle_disruption, "Nitrogen and Phosphorus Cycle Disruption").
narrative_ontology:topic_domain(nitrogen_phosphorus_cycle_disruption, "biogeochemical/environmental").

domain_priors:requires_active_enforcement(nitrogen_phosphorus_cycle_disruption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nitrogen_phosphorus_cycle_disruption, industrial_agriculture).
narrative_ontology:constraint_beneficiary(nitrogen_phosphorus_cycle_disruption, synthetic_fertilizer_manufacturers).
narrative_ontology:constraint_beneficiary(nitrogen_phosphorus_cycle_disruption, agribusiness_operators).
narrative_ontology:constraint_victim(nitrogen_phosphorus_cycle_disruption, aquatic_ecosystems).
narrative_ontology:constraint_victim(nitrogen_phosphorus_cycle_disruption, downstream_communities).
narrative_ontology:constraint_victim(nitrogen_phosphorus_cycle_disruption, long_term_soil_fertility).
narrative_ontology:constraint_victim(nitrogen_phosphorus_cycle_disruption, global_atmospheric_nitrogen_balance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AQUATIC ECOSYSTEMS (SNARE) — Dead zones, algal blooms, oxygen depletion in coastal waters and freshwater systems. Ecosystems cannot exit; they absorb runoff from hundreds of miles upstream. Maximum extraction with no alternative pathways. Trapped at every scale — local, regional, continental — the nutrient flood is structurally unavoidable given current agricultural practices.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM COMMUNITIES (SNARE) — Water treatment costs escalate, drinking water contamination risks increase, fisheries collapse, recreation value disappears. Communities absorb externalities from agricultural regions with no exit option and no compensation mechanism. Trapped by geography and economic dependency on affected aquatic resources.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL FARMING COMMUNITIES (TANGLED ROPE) — Face genuine coordination problem: nutrient cycling is essential to soil fertility and food production. Synthetic fertilizers solved the historical coordination problem of depleted soils (the Haber-Bosch process enabled population growth). But the constraint now exhibits asymmetric extraction: nitrogen-phosphorus runoff benefits industrial monoculture (externality dumping) while degrading regional soil quality long-term. Communities are constrained by debt, crop contracts, and lack of alternative inputs, but they also benefit from coordination around nutrient supply. Neither pure coordination nor pure extraction.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INDUSTRIAL AGRICULTURE & FERTILIZER MANUFACTURERS (ROPE) — Experience the constraint as coordination: large-scale nutrient management that enables monoculture productivity and profit. They have arbitrage options (can relocate operations, shift supply chains, exit specific regions). Net beneficiaries. The constraint from their perspective is a working coordination system — nutrient supply chains, price signals, production scaling. They do not experience the suppression that downstream agents experience.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGENERATIVE AGRICULTURE MOVEMENT (SCAFFOLD) — Organized agents (agroecology researchers, organic farming networks, soil conservation groups, some regulatory bodies) are building alternative nutrient cycling pathways: cover cropping, intercropping, composting, nitrogen-fixing legumes, rotational grazing. These reconstruct biological nutrient cycling with lower synthetic inputs. The movement sees the industrial disruption as a temporary coordination failure being solved through regenerative practices with a sunset clause: as ecological agriculture scales, synthetic fertilizer dependency declines. Current suppression is moderate — conversion costs and knowledge barriers, but not absolute structural barriers.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGULATORY FRAMEWORKS (PITON) — Agricultural pollution regulations (Clean Water Act, Water Framework Directive, nutrient runoff limits) are largely performative. Enforcement is weak, implementation lags, loopholes are abundant, and regulations often lag scientific understanding of ecosystem impacts. The regulatory apparatus maintains the appearance of control through monitoring and advisory programs while actual runoff persists. Theater ratio reflects that many regulations exist but their functional impact on actual nutrient cycling disruption is limited. Maintained through institutional inertia and political capture by agricultural interests.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational biogeochemical view, the constraint is a hybrid: genuine coordination around nutrient cycling for food production coexists with asymmetric extraction of atmospheric and aquatic commons. The disruption solves a real problem (soil fertility) while creating catastrophic externalities (dead zones, atmospheric nitrogen saturation, cascade extinctions). The analytical view sees both the coordination function and the extraction mechanism. This is the correct classification for the constraint's true structure.
constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nitrogen_phosphorus_cycle_disruption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nitrogen_phosphorus_cycle_disruption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nitrogen_phosphorus_cycle_disruption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nitrogen_phosphorus_cycle_disruption, TR),
    TR >= 0.70.

:- end_tests(nitrogen_phosphorus_cycle_disruption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint delivers genuine coordination benefits (food security, soil fertility) but with severe asymmetric extraction. The beneficiaries (industrial agriculture, fertilizer industry) capture productivity gains while victims absorb externalities (dead zones, atmospheric nitrogen saturation, soil microbial degradation). The extraction is not as high as pure snares because the coordination function is real and non-trivial — without synthetic inputs, global food production would drop catastrophically and many would face starvation. But extraction is substantial because beneficiaries have not internalized externality costs or invested in alternative pathways. Suppression (0.68): High. Multiple barriers prevent transition: (1) Regulatory capture — agricultural interests block enforcement of nutrient standards; (2) Infrastructure lock-in — fertilizer manufacturing capacity, supply chains, distribution systems are optimized for synthetic inputs; (3) Knowledge barriers — regenerative techniques require different labor, equipment, and expertise; (4) Financial barriers — conversion costs, debt structures tied to industrial monoculture; (5) Scale mismatch — biological cycling methods face higher per-unit labor and land requirements, creating pressure to revert when commodity prices drop. Theater ratio (0.55): Moderate-high. Regulations exist (Clean Water Act, nutrient standards, monitoring programs) but enforcement is weak and loopholes abundant. The regulatory apparatus creates appearance of control while actual runoff persists at harmful levels. This reflects that the constraint's response has shifted from denial (1970s-1990s: industry claimed no problem) to performance (2000s-present: regulations exist, agencies measure, but transformation doesn't follow). The theater ratio increased over the interval as scientific understanding of impacts grew but regulatory transformation remained limited. Interval trajectory: Extractiveness increased from 0.22 (1980s: industrial system optimized for productivity, externalities not yet quantified) to 0.58 (present: externality costs are measured and known, but system persists). Theater ratio increased from 0.35 (1980s: minimal regulatory oversight) to 0.55 (present: regulatory framework exists but functional impact limited). This reflects growing awareness coupled with institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   Each perspective produces a different classification type despite the same base_properties metrics. This gap reveals structural heterogeneity: the constraint means fundamentally different things to different agents. Aquatic ecosystems cannot perceive coordination benefit (snare). Industrial beneficiaries perceive coordination with low extraction (rope). Farming communities perceive both (tangled rope). Regenerative networks perceive temporary extraction with exit path (scaffold). Regulatory systems perceive degraded function (piton). The analytical observer perceives the full hybrid structure (tangled rope). None of these are measurement error — they reflect genuine structural differences in how the constraint operates from different positions. The gap is the diagnostic signal that this is not a simple Mountain (inevitable law) or Rope (pure coordination). It is a complex constraint with multiple structural functions operating simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) measure each agent's structural position relative to the extraction flow. Aquatic ecosystems: d ≈ 0.95 (full targets, zero beneficiary position, trapped exit). Downstream communities: d ≈ 0.92 (victims with geographic entrapment). Industrial agriculture: d ≈ 0.05 (full beneficiaries, arbitrage exit). Fertilizer manufacturers: d ≈ 0.08 (beneficiaries, global market arbitrage). Regional farmers: d ≈ 0.60 (mixed position — benefit from coordination, suppressed by conversion barriers). Regenerative networks: d ≈ 0.55 (constrained exit options, some beneficiary role in ecosystem restoration). Regulatory bodies: d ≈ 0.35 (institutional, arbitrage exit, but captured by industry interests so directionality leans toward beneficiary). The analytical observer: d ≈ 0.72 (unaligned with power flow, sees full structure). Derivation: beneficiary status for industrial agents maps to low d via the sigmoid f(d); victim status for aquatic agents maps to high d; trapped/constrained exit amplifies d for victims; arbitrage exit dampens d for beneficiaries. The chi formula χ = ε × f(d) × σ(S) applies scope modifier σ(global) = 1.2, amplifying effective extraction for the planetary-scale scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The nitrogen-phosphorus cycle disruption resolves the mandatrophy by demonstrating how a genuine coordination solution can metamorphose into an extraction mechanism when externalities are unpriced. The Haber-Bosch process WAS a pure coordination achievement: it solved the soil depletion crisis. But industrial application without externality pricing transformed it into a snare for aquatic systems and a tangled rope for the global system. The mandatrophy resolution is to recognize that tangled rope classification is correct and stable: the constraint solves real coordination problems (food security, soil fertility) while extracting massively from unpriced commons (aquatic systems, atmosphere, soil microbial communities). The resolution is NOT to reclassify as pure snare (ignoring the real coordination function) or pure rope (ignoring the catastrophic externalities). It is to maintain tangled rope classification AND identify the causal mechanisms of extraction: (1) externality non-pricing, (2) regulatory capture preventing enforcement, (3) infrastructure lock-in, (4) knowledge barriers to alternatives. Addressing these mechanisms would shift the constraint: if externalities were fully priced, snare components would be constrained; if regenerative techniques scaled, scaffold sunset would accelerate. The mandatrophy is resolved by recognizing the constraint's hybrid nature and identifying which components are amenable to intervention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_ecosystem_recovery,
    'At what nutrient concentration threshold do aquatic ecosystems undergo irreversible collapse rather than oscillating recovery?',
    'Long-term ecosystem monitoring; analysis of recovery trajectories in different coastal dead zones; tipping point identification in nutrient loading dynamics',
    'If threshold is near current concentrations: the snare is approaching a phase transition to permanent degradation. If threshold is higher: some recovery potential remains with intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_ecosystem_recovery, empirical, 'Irreversibility threshold for eutrophication in aquatic ecosystems').

omega_variable(
    regenerative_agriculture_scalability,
    'Can regenerative agriculture practices scale to global crop production levels without synthetic fertilizer inputs?',
    'Life-cycle assessments of regenerative vs industrial systems; regional scaling experiments; soil carbon and nutrient density tracking; global food security modeling',
    'If scalable: scaffold perspective is structural and sunset is real. If constrained to niche production: scaffold is aspirational and synthetic fertilizer dependency persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regenerative_agriculture_scalability, empirical, 'Whether regenerative agriculture can scale globally').

omega_variable(
    regulatory_capture_depth,
    'To what degree does agricultural industry capture of regulatory bodies prevent enforcement of nutrient runoff standards?',
    'Analysis of regulation writing timelines vs industry input; cross-national comparison of enforcement intensity and outcomes; campaign finance and revolving-door tracking in agricultural agencies',
    'If capture is deep: piton classification is correct and regulations are primarily theatrical. If capture is partial: regulations have some functional impact, reducing piton features.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of industry capture in nutrient pollution regulation').

omega_variable(
    nitrogen_fixation_efficiency_ceiling,
    'Is there a thermodynamic or biological ceiling on nitrogen fixation efficiency that will perpetually require synthetic inputs for current global population levels?',
    'Theoretical analysis of biological nitrogen fixation limits; crop yield modeling under 100% biological inputs; population-land-area optimization',
    'If ceiling exists at current population levels: industrial fertilizers are non-negotiable, snare is permanent feature of global agriculture. If ceiling is substantially higher: transitional shift is possible, scaffold perspective is viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nitrogen_fixation_efficiency_ceiling, empirical, 'Thermodynamic limits on biological nitrogen fixation').

omega_variable(
    externality_capitalization_mechanism,
    'Can nutrient externality costs be accurately priced and capitalized into fertilizer costs without distorting agricultural economies?',
    'Economic models pricing externalities (water treatment, ecosystem restoration, dead zone management); agricultural system resilience analysis under true-cost pricing; political economy of price transmission',
    'If capitalizable: constraint could shift from snare to constrained extraction with market feedback. If capitalization breaks economics: industrial system requires subsidy, tangled rope with hidden extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_capitalization_mechanism, empirical, 'Whether nutrient externality costs can be priced into fertilizer market').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nitrogen_phosphorus_cycle_disruption, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npcd_tr_t0, nitrogen_phosphorus_cycle_disruption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(npcd_tr_t20, nitrogen_phosphorus_cycle_disruption, theater_ratio, 20, 0.48).
narrative_ontology:measurement(npcd_tr_t40, nitrogen_phosphorus_cycle_disruption, theater_ratio, 40, 0.55).
narrative_ontology:measurement(npcd_tr_t10, nitrogen_phosphorus_cycle_disruption, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(npcd_be_t0, nitrogen_phosphorus_cycle_disruption, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(npcd_be_t20, nitrogen_phosphorus_cycle_disruption, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(npcd_be_t40, nitrogen_phosphorus_cycle_disruption, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(npcd_be_t10, nitrogen_phosphorus_cycle_disruption, base_extractiveness, 10, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nitrogen_phosphorus_cycle_disruption, resource_allocation).
narrative_ontology:affects_constraint(nitrogen_phosphorus_cycle_disruption, coastal_dead_zone_formation).
narrative_ontology:affects_constraint(nitrogen_phosphorus_cycle_disruption, soil_microbial_community_degradation).
narrative_ontology:affects_constraint(nitrogen_phosphorus_cycle_disruption, atmospheric_nitrogen_saturation).
narrative_ontology:affects_constraint(nitrogen_phosphorus_cycle_disruption, agricultural_debt_accumulation).

% DUAL FORMULATION NOTE:
% The nitrogen-phosphorus cycle disruption is a complex constraint family encompassing multiple mechanistically distinct phenomena: (1) nutrient runoff eutrophication (aquatic snare, ε=0.72), (2) soil cycling disruption (agricultural tangled rope, ε=0.55), (3) atmospheric nitrogen pollution (biospheric snare, ε=0.68), (4) synthetic fertilizer market structure (industrial rope, ε=0.15). Each has different ε values and different beneficiary/victim structures. This story models the unified constraint from the civilizational analytical perspective (ε=0.58, tangled rope). Downstream stories should decompose by ecosystem domain and scale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nitrogen_phosphorus_cycle_disruption, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
