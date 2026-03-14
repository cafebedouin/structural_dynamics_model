% ============================================================================
% CONSTRAINT STORY: ocean_acidification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ocean_acidification, []).

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
 *   constraint_id: ocean_acidification
 *   human_readable: Ocean Acidification as Structural Extraction
 *   domain: environmental/biogeochemical
 *
 * SUMMARY:
 *   Ocean acidification represents a structural extraction mechanism where
 *   atmospheric carbon dumping by fossil fuel industries is externalized as a
 *   cost borne by marine calcifying organisms and human communities dependent
 *   on ocean fisheries. The constraint exhibits the full diagnostic signature
 *   of a snare: high extractiveness (0.68), high suppression (0.72), and
 *   asymmetric cost distribution with no coordination benefit for victims.
 *   The acidification process is mediated through chemistry (immutable
 *   Henderson-Hasselbalch equilibrium) but the extraction mechanism is
 *   institutional (externality pricing defaults to zero, regulatory capacity
 *   is absent, beneficiaries have political power to maintain the system).
 *   From different structural positions, the same constraint classifies as
 *   natural law (analytical), degraded governance theater (piton), pure
 *   extraction (snare, powerless victims), coordination mechanism (rope,
 *   beneficiaries), temporary problem with exit (scaffold, organized
 *   mitigation agents), and mixed coordination-extraction (tangled rope,
 *   industrial actors). The theater ratio (0.58) reflects that climate
 *   governance maintains ritual commitment (Paris Agreement, corporate ESG)
 *   while actual acidification continues; governance institutions classify as
 *   piton (0.70+ theater specifically for international frameworks) because
 *   compliance is performative rather than functional.
 *
 * KEY AGENTS:
 *   - Marine Calcifiers (Pteropods, Corals, Mollusks): Powerless/trapped victims — experience maximum extraction with no exit options or coordination benefit
 *   - Subsistence Fishing Communities: Moderate/constrained victims — face fishery collapse and costly relocation or occupational exit
 *   - Island Nations and Coastal Populations: Powerless/trapped victims — face combined extraction from acidification (fishery loss) and sea level rise
 *   - Fossil Fuel Extractors and Carbon-Intensive Industries: Institutional/arbitrage beneficiaries — primary beneficiaries of carbon externalization; experience constraint as coordination enabling their profit extraction
 *   - Industrial Aquaculture and Shellfish Operations: Powerful/mobile hybrid actors — benefit from coordination systems but face direct extraction from ocean chemistry; have capacity to adapt or lobby for subsidies
 *   - Climate Mitigation Coalition (Scientists, Island Nation Representatives, Environmental NGOs): Organized/constrained agents — perceive exit pathway (carbon neutrality) with sunset logic; currently constrained by political and economic barriers
 *   - International Climate Governance Institutions (UNFCCC, National Climate Agencies): Institutional/arbitrage actors maintaining theater — governance framework persists through institutional inertia and industry capture despite low functional mitigation; classify as piton
 *   - Analytical Observer: Civilizational perspective — risks naturalizing contingent policy arrangement (carbon externalization) as immutable chemistry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ocean_acidification, 0.68).
domain_priors:suppression_score(ocean_acidification, 0.72).
domain_priors:theater_ratio(ocean_acidification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ocean_acidification, extractiveness, 0.68).
narrative_ontology:constraint_metric(ocean_acidification, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ocean_acidification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ocean_acidification, snare).
narrative_ontology:human_readable(ocean_acidification, "Ocean Acidification as Structural Extraction").
narrative_ontology:topic_domain(ocean_acidification, "environmental/biogeochemical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ocean_acidification, fossil_fuel_extractors).
narrative_ontology:constraint_beneficiary(ocean_acidification, carbon_intensive_industries).
narrative_ontology:constraint_victim(ocean_acidification, marine_calcifiers).
narrative_ontology:constraint_victim(ocean_acidification, pteropod_populations).
narrative_ontology:constraint_victim(ocean_acidification, coral_reef_ecosystems).
narrative_ontology:constraint_victim(ocean_acidification, shellfish_fisheries).
narrative_ontology:constraint_victim(ocean_acidification, island_nations).
narrative_ontology:constraint_victim(ocean_acidification, subsistence_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARINE CALCIFIERS (SNARE) — Pteropods, corals, mollusks, and foraminifera cannot exit the ocean or adjust their shell chemistry. Maximum extraction: their survival depends on calcium carbonate saturation states that are declining irreversibly at current emission rates. No coordination benefit; pure cost. The constraint is external coercion — atmospheric CO2 concentrations are imposed on marine biology without consent or negotiation mechanism.
constraint_indexing:constraint_classification(ocean_acidification, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSISTENCE FISHING COMMUNITIES (SNARE) — Bear immediate extraction through fishery collapse; exit options are expensive (geographic relocation, occupational retraining) but theoretically available. Experience the constraint as pure extraction with minimal coordination benefit. Cannot alter ocean chemistry; can only absorb costs or migrate. Regional scope because impact concentrates in specific fishing zones.
constraint_indexing:constraint_classification(ocean_acidification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FOSSIL FUEL EXTRACTORS (ROPE) — Experience the constraint as pure coordination mechanism: the ability to externalize carbon costs is the primary benefit of the extraction system. From their perspective, acidification is the coordination solution to the problem 'how do we avoid internalizing emission costs?' The constraint enforces their access to atmospheric carbon dump. Net beneficiary with maximal arbitrage (can relocate capital, offshore operations, influence policy).
constraint_indexing:constraint_classification(ocean_acidification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDUSTRIAL AQUACULTURE (TANGLED ROPE) — Benefits from coordination of production and distribution systems but faces direct extraction as acidification increases hatchery failures and disease susceptibility. Powerful enough to lobby for subsidies; mobile enough to relocate operations or adjust to acidification (feed additives, selective breeding). Hybrid: genuine coordination of supply chains exists alongside asymmetric extraction from ocean chemistry.
constraint_indexing:constraint_classification(ocean_acidification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CLIMATE MITIGATION COALITION (SCAFFOLD) — Organized agents (climate scientists, island nation representatives, environmental NGOs) perceive the constraint as temporary: carbon neutrality pathways, renewable transition, ocean alkalinization research represent genuine exit strategies with sunset logic. Constrained by current political inertia but see a structural pathway to resolving acidification within 50-100 years if emissions decline. Theater ratio for this perspective is lower than the fossil fuel narrative — the technical solutions exist; barriers are political and economic, not scientific.
constraint_indexing:constraint_classification(ocean_acidification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL CLIMATE GOVERNANCE (PITON) — The UNFCCC, Paris Agreement, and national climate commitments maintain theatrical compliance (emissions targets that miss deadlines, net-zero pledges without enforcement, corporate ESG metrics decoupled from actual emission reductions) while the underlying extraction mechanism persists. Theater ratio high (0.58 baseline, but 0.70+ for governance institutions specifically) — the ritual of commitment is maintained despite structural failure to reduce acidification. The framework has low functional verification capacity; enforcement is largely performative. Persists through institutional inertia and industry capture of negotiating spaces.
constraint_indexing:constraint_classification(ocean_acidification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a physics/chemistry perspective, ocean acidification is governed by immutable carbonate equilibrium chemistry: increased atmospheric pCO2 inexorably drives ocean pH downward. The constraint appears as natural law — the Henderson-Hasselbalch equation, carbonate saturation kinetics, and species-specific calcification thresholds are invariant across all contexts. However, the structural data contradicts this: the extraction flow (atmospheric carbon dumping) and suppression mechanism (lack of externality pricing) are institutional choices, not physical laws. The mountain classification is a false summit — the engine will flag this as naturalization of a contingent policy arrangement.
constraint_indexing:constraint_classification(ocean_acidification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ocean_acidification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ocean_acidification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ocean_acidification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ocean_acidification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ocean_acidification, TR),
    TR >= 0.70.

:- end_tests(ocean_acidification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting irreversible ecosystem damage accumulation and economic costs to fishing communities. The value has increased from 0.35 in 1950 to 0.68 by 2050 as atmospheric CO2 crossed 400 ppm threshold and exceeded carbonate saturation horizons in key fishing zones. The measurement trajectory shows acceleration: extraction compounds as pH decline deepens and tipping points in ecosystem resilience are approached. Suppression (0.72): High, reflecting multiple barriers to mitigation: sunk costs in fossil fuel infrastructure, political capture of climate negotiations by extractive industries, information asymmetries about acidification severity, and cognitive distance between atmospheric emissions and ocean chemistry (temporal lag ~30 years between emissions and full acidification realization). Theater ratio (0.58): Moderate-high, reflecting governance institutions' performative compliance (Paris Agreement targets that miss deadlines, corporate net-zero pledges without enforcement, IPCC reports that document problem without binding mechanisms). International climate negotiations maintain theatrical commitment while actual emission reductions lag targets by 3-5 GtCO2/year annually. The theater ratio increased from 0.25 (1950, when problem was scientifically recognized but policy was absent) to 0.58 (2050, when governance framework exists but enforcement capacity remains minimal). This trajectory indicates Goodhart drift: the metric (Paris Agreement alignment) became substitute for actual goal (emission reduction), resulting in institutions optimizing for appearance of compliance rather than actual mitigation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across the observation site. Marine calcifiers experience pure snare (trapped/powerless) — they cannot alter ocean pH and bear 100% of costs. Subsistence communities experience snare with constrained rather than trapped exit (high cost to relocate but theoretically possible). Fossil fuel extractors experience rope (institutional/arbitrage) — the constraint solves their cost externalization problem. Industrial aquaculture experiences tangled rope (powerful/mobile) — mixed coordination (production systems) and extraction (ocean chemistry). Climate mitigation coalition experiences scaffold (organized/constrained) — they see genuine exit pathway (decarbonization) with sunset logic. Governance institutions experience piton (institutional/arbitrage) — they maintain performative ritual despite low function. Analytical observer risks mountain (analytical/analytical) — naturalizing institutional choices as physical laws. The perspectival gap reveals that no single type captures the constraint; instead, the presheaf over structural positions shows: (1) true extraction victims are least mobile and least powerful; (2) beneficiaries have maximum arbitrage options; (3) organized mitigation agents perceive genuine exit pathway contradicted by current policy; (4) governance institutions are captured (performing compliance while maintaining underlying extraction mechanism). The gap between victims' snare and beneficiaries' rope classification is maximal — no coordination benefit exists for victims, while beneficiaries experience the constraint as solving their core coordination problem (externalize costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim relationships and exit options per the framework. Fossil fuel extractors as beneficiaries with arbitrage options → d ≈ 0.05 → f(d) ≈ -0.12 → negative effective extraction for this agent (they benefit). Marine calcifiers as victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction (they bear full costs). Subsistence communities as victims with constrained exit → d ≈ 0.85 → f(d) ≈ 1.15 → high extraction. Industrial aquaculture as both beneficiary (from coordination of production systems) and victim (from ocean chemistry) with mobile exit → d ≈ 0.50 → f(d) ≈ 0.65 → moderate asymmetry. Organized mitigation agents as organized/constrained → d ≈ 0.40 → f(d) ≈ 0.40 → moderate because they have agency (organization) but face barriers (political constraints). Governance institutions as beneficiaries maintaining status quo (arbitrage) → d ≈ 0.00 → f(d) ≈ -0.12 → they experience low extraction because the system works in their institutional interest. Analytical observer as observer → d ≈ 0.73 → f(d) ≈ 1.15 → moderate, reflecting the analytical position's distance from structural positions. Scope modifier σ(S) = 1.2 (global scale, maximum complexity amplification) — ocean acidification is planetary-scale coordination failure, making verification and regulatory enforcement maximally difficult. χ = ε × f(d) × σ(S) across perspectives shows beneficiaries experiencing χ < 0 (the constraint subsidizes them) while powerless victims experience χ > 1.6 (extraction magnified by organization, trapped exit, and global scope). This directionality structure confirms snare classification: extraction flows asymmetrically from structurally powerless to structurally powerful actors.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint (ε > 0.70) resolves the mandatrophy trap by distinguishing true extraction (snare for marine calcifiers and subsistence communities) from false natural law framing (mountain classification from analytical perspective). The resolution mechanism is perspectival: (1) BENEFICIARY PERSPECTIVE: Fossil fuel extractors and their beneficiaries experience the constraint as rope — pure coordination enabling externalization. This is not false; it is their genuine structural experience. (2) VICTIM PERSPECTIVE: Marine organisms and fishing communities experience snare — pure extraction with no coordination benefit. This is not false; it is their genuine structural experience. (3) ANALYTICAL PERSPECTIVE: From civilizational/universal scope, acidification appears to follow immutable carbonate chemistry (mountain). This would be a false summit IF the extraction mechanism were purely physical. But the extraction mechanism is institutional — carbon externalization, regulatory absence, political capture. The physics is immutable; the policy is contingent. The false summit is revealed by the organizedmitigation coalition's scaffold perspective (organized/constrained agents perceive genuine exit pathway via decarbonization). If decarbonization is possible, then acidification is not immutable natural law — it is contingent institutional failure to price externalities. The mandatrophy resolves by showing that the mountain classification naturalizes what is actually a snare. The engine detects this as false summit (mountain claim contradicted by scaffold perspective showing exit pathway). (4) PITON PERSPECTIVE: Governance institutions maintain performative theater (high theater ratio) while extraction persists, confirming institutional inertia explanation. The mandatrophy is resolved: all six types are valid perspectival readings; the constraint's identity is established by the presheaf structure, not by forcing a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    acidification_threshold_irreversibility,
    'At what ocean pH threshold does marine ecosystem damage become ecologically irreversible on human timescales?',
    'Long-term paleoceanographic analysis of past pH transitions and ecosystem recovery rates; experimental studies of carbonate dissolution rates and species adaptation limits under controlled conditions',
    'If threshold < current trajectory: acidification is already locked in as permanent constraint on marine life (snare from all perspectives). If threshold > current trajectory: mitigation still offers pathway to escape (scaffold perspective validated). If threshold is species-dependent: some calcifiers trapped (snare) while others adapt (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acidification_threshold_irreversibility, empirical, 'Ecological reversibility threshold for ocean acidification').

omega_variable(
    carbon_pricing_efficacy,
    'Would internalizing the cost of acidification (via carbon tax or emissions pricing) eliminate the extraction mechanism, or would it merely shift distribution of costs without reducing suppression?',
    'Comparative analysis of carbon tax regimes and their impact on emission reduction rates vs. observable ocean acidification deceleration; counterfactual modeling of price levels required to achieve 1.5°C pathway',
    'If pricing eliminates extraction: fossil fuel constraint becomes rope or scaffold (temporary). If pricing merely redistributes costs: extraction persists under different institutional form. If price level required is economically infeasible: snare becomes structural regardless of policy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_pricing_efficacy, empirical, 'Whether carbon pricing can resolve ocean acidification extraction mechanism').

omega_variable(
    ecosystem_adaptation_versus_collapse,
    'Can marine ecosystems adapt evolutionarily or physiologically to ocean acidification faster than the current trajectory of pH decline?',
    'Genomic analysis of acidification tolerance alleles in wild populations; experimental evolution studies; comparison of adaptation rates to observed rate of pH change (0.1 pH units per decade)',
    'If adaptation matches or exceeds decline rate: snare perspectives misclassified — constraint transitions to tangled rope (some species adapt, maintain partial coordination). If adaptation lags decline: snare classification confirmed; extinction timelines become determinable. If adaptation is constrained by immigration or genetic bottlenecks: region-specific transitions from snare to rope possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_adaptation_versus_collapse, empirical, 'Marine ecosystem adaptation rate relative to acidification rate').

omega_variable(
    decoupling_feasibility,
    'Is complete economic decoupling from fossil fuels structurally feasible within the timescale required to prevent irreversible acidification (30-50 years)?',
    'Technoeconomic analysis of renewable energy deployment rates, energy storage scaling, industrial process decarbonization costs, and substitution timelines for aviation/shipping/cement; comparison to climate models'' decarbonization requirements',
    'If decoupling feasible: scaffold perspective is structural (exit pathway exists). If not feasible: snare becomes permanent condition; mitigation constraints shift to adaptation or geoengineering. If feasible but economically costs exceed political willingness: snare persists as institutional choice rather than physical constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility, empirical, 'Technical and economic feasibility of carbon-neutral transition').

omega_variable(
    suppression_mechanism_clarity,
    'Is suppression of ocean acidification mitigation primarily structural (asymmetric economic incentives favor continued extraction) or discursive (the constraint is hidden by false natural law framing)?',
    'Comparative policy analysis of countries/sectors with high acidification awareness vs. low mitigation spending; content analysis of climate negotiations for false naturalization rhetoric; tracking of climate litigation success rates',
    'If primarily structural: snare persists regardless of awareness (victims cannot exit because economics favor extraction). If primarily discursive: enhanced transparency and framing could shift perspectives and reduce suppression. If both: discursive work alone is insufficient; structural incentives must change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_clarity, empirical, 'Structural vs. discursive mechanisms sustaining suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ocean_acidification, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oa_theater_1950, ocean_acidification, theater_ratio, 0, 0.25).
narrative_ontology:measurement(oa_theater_2000, ocean_acidification, theater_ratio, 50, 0.48).
narrative_ontology:measurement(oa_theater_2050, ocean_acidification, theater_ratio, 100, 0.58).
narrative_ontology:measurement(oa_theater_2100, ocean_acidification, theater_ratio, 150, 0.62).

% Extraction over time
narrative_ontology:measurement(oa_extract_1950, ocean_acidification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oa_extract_2000, ocean_acidification, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(oa_extract_2050, ocean_acidification, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(oa_extract_2100, ocean_acidification, base_extractiveness, 150, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ocean_acidification, global_infrastructure).
narrative_ontology:affects_constraint(ocean_acidification, marine_food_web_collapse).
narrative_ontology:affects_constraint(ocean_acidification, coral_bleaching).
narrative_ontology:affects_constraint(ocean_acidification, shellfish_hatchery_failure).
narrative_ontology:affects_constraint(ocean_acidification, carbonate_saturation_state).
narrative_ontology:affects_constraint(ocean_acidification, pteropod_abundance_decline).

% DUAL FORMULATION NOTE:
% Ocean acidification decomposes into multiple structurally distinct constraints: (1) carbonate_saturation_state (ε ≈ 0.08, Mountain — immutable chemistry); (2) marine_ecosystem_acidification_response (ε ≈ 0.55, Tangled Rope — genuine coordination of ocean chemistry + asymmetric extraction from biology); (3) ocean_acidification_as_externality (ε ≈ 0.68, Snare — this story — carbon dumping institutional arrangement). Decomposition required because measuring acidification by carbonate equilibrium (ε=0.08, mountain) vs. measuring it by economic cost to fishing communities (ε=0.68, snare) yields different extractiveness values. The framework's ε-invariance principle mandates separate stories: this story focuses on the institutional extraction mechanism (who bears costs, who benefits, why suppression persists); upstream stories address the physics and biology independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ocean_acidification, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
