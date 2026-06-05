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
 *   Ocean acidification represents one of the clearest examples of a pure
 *   extraction constraint where costs are systematically externalized to
 *   organisms and communities with zero exit capacity. Since
 *   industrialization (~1750), atmospheric CO2 has risen from 280 ppm to 420+
 *   ppm, driven by fossil fuel combustion. This CO2 dissolves in seawater,
 *   forming carbonic acid and reducing ocean pH by 0.1 units (a 30% increase
 *   in acidity). For calcifying organisms whose shells and skeletons depend
 *   on dissolved carbonate, this represents a direct metabolic cost: they
 *   must expend more energy to maintain their structures as carbonate
 *   availability declines. Simultaneously, fossil fuel industries capture the
 *   benefit — atmospheric dumping enables cheap energy production without
 *   paying for waste disposal. The constraint operates through market
 *   externality: the price of coal, oil, and gas does not reflect the cost
 *   borne by pteropods and fishing communities. There is no coordination
 *   benefit for victims; the constraint is pure extraction. The suppression
 *   signature is particularly stark: calcifying organisms have no legal
 *   representation, no political voice, and no exit option — their biology is
 *   fixed. Small-scale fishing communities are trapped by economic dependency
 *   and limited access to policy institutions. The constraint's
 *   extractiveness has risen sharply over the interval (0.12 → 0.68) as CO2
 *   accumulation has crossed critical thresholds for calcifier viability.
 *   Theater ratio has also risen (0.15 → 0.58) as international climate
 *   policy has proliferated without corresponding emissions reductions — the
 *   policy apparatus performs response while the extraction mechanism
 *   accelerates.
 *
 * KEY AGENTS:
 *   - Calcifying marine organisms (pteropods, corals, mollusks, crustaceans): Primary victims (powerless/trapped) — biochemically locked into seawater carbonate chemistry; zero exit capacity; bear full extraction cost
 *   - Small-scale fishing communities (subsistence fishers, artisanal operations): Primary victims (moderate/constrained) — economically dependent on calcifier and fish stocks; high cost to exit; weak political voice
 *   - Fossil fuel industries (petroleum, coal, natural gas companies): Primary beneficiaries (institutional/arbitrage) — benefit from cheap atmospheric waste disposal; arbitrage exit options available (renewable transition) but costly
 *   - Industrial fishing operations (large-scale commercial fleets): Secondary mixed agent (powerful/mobile) — benefit from cheap energy (fossil fuel input), harmed by ecosystem collapse; mobile exit options available
 *   - Small island states and coastal nations: Secondary victims (moderate/constrained) — dependent on fisheries and tourism; political voice in climate forums but limited enforcement capacity
 *   - International climate policy apparatus (UNFCCC, IPCC, Paris Agreement): Institutional responder (institutional/arbitrage) — maintains performative policy structures; high theater ratio; limited enforcement mechanisms
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent extraction as biogeochemical law
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
narrative_ontology:constraint_beneficiary(ocean_acidification, fossil_fuel_industries).
narrative_ontology:constraint_victim(ocean_acidification, calcifying_marine_organisms).
narrative_ontology:constraint_victim(ocean_acidification, small_scale_fishers).
narrative_ontology:constraint_victim(ocean_acidification, coral_reef_ecosystems).
narrative_ontology:constraint_victim(ocean_acidification, pteropod_dependent_food_webs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CALCIFYING ORGANISMS (SNARE) — Pteropods, corals, mollusks, and crustaceans cannot exit the constraint. Their shell-forming biochemistry is locked into seawater carbonate chemistry; acidification is not a choice they can avoid. Maximum experienced extraction with zero exit capacity. The constraint operates at the biochemical level — organisms bear the full cost of pH reduction with no alternative. No coordination benefit exists; the extraction is pure.
constraint_indexing:constraint_classification(ocean_acidification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL-SCALE FISHING COMMUNITIES (SNARE) — Subsistence and artisanal fishers dependent on pteropod, coral, and shellfish stocks face high extraction with constrained but not eliminated exit options. Can potentially relocate, switch livelihoods, or migrate, but at severe economic and cultural cost. The suppression is high: local economic dependency, limited capital for transition, and weak political voice in global carbon policy. No coordination benefit — the constraint extracts livelihood while offering no reciprocal gain. Effectively a snare despite constrained exit, because exit costs are catastrophic.
constraint_indexing:constraint_classification(ocean_acidification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: FOSSIL FUEL INDUSTRIES (ROPE) — From the perspective of industrial extractors, the ocean serves as a coordination mechanism: atmosphere and ocean act as a free waste-disposal medium, enabling profitable energy production. Exit options are available (renewable energy transition, carbon capture) but carry significant capital costs. The industries experience the constraint as coordinating their profit extraction with cheap waste disposal. The negative externality is not visible from this perspective — only the coordination benefit of using the atmosphere as an unbilled sink. This is the beneficiary perspective: low effective extraction, high coordination benefit.
constraint_indexing:constraint_classification(ocean_acidification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDUSTRIAL FISHING OPERATIONS (TANGLED ROPE) — Large-scale fishing corporations have both benefits and costs within the constraint. They benefit from cheap fossil-fuel-powered fishing equipment and supply chains; they bear costs as pteropod stocks collapse and coral reef fisheries degrade. Powerful exit options exist (shift to different fish stocks, invest in aquaculture, geographic arbitrage). Mixed extraction — partial beneficiary through energy cost externalization, partial victim through ecosystem collapse. This is a mixed perspective where the same constraint delivers both coordination benefit and extraction damage, with asymmetric distribution across dimensions.
constraint_indexing:constraint_classification(ocean_acidification, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE POLICY APPARATUS (PITON) — The UN climate framework, Paris Agreement, and IPCC structures exist as institutional responses to ocean acidification and CO2 accumulation. However, the apparatus has become largely performative: international climate negotiations produce targets without enforcement mechanisms, carbon markets enable accounting games rather than emission reduction, and voluntary corporate climate commitments lack teeth. Theater ratio is high — the policy apparatus maintains ritual (summits, agreements, assessments) while emissions continue rising. The institutional machine persists through legitimacy and career incentives for climate diplomats, not through functional constraint on emissions. This is degraded institutional authority maintained by inertia.
constraint_indexing:constraint_classification(ocean_acidification, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective on biogeochemistry, the constraint could be framed as a natural law: CO2 dissolves in seawater, forming carbonic acid, lowering pH. This is a universal chemical principle — immutable. However, the source material declares beneficiaries (fossil fuel industries), which triggers false-summit detection. The 'immutability' framing naturalizes what is actually a contingent social choice: burning fossil fuels is not a law of nature, and neither is externalizing the cost. The constraint appears as a mountain only when the beneficiary structure is occluded. The engine will identify this as naturalization of extraction.
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
 *   Extractiveness (0.68): High, reflecting the magnitude of cost transfer from calcifying organisms to fossil fuel beneficiaries. The extraction is measured in gigatonnes of anthropogenic CO2 externalized; in pteropod shell dissolution; in coral bleaching and mortality; in fishing community income loss. The beneficiary (fossil fuel industries) captures the energy value; the victim bears the biogeochemical cost. The 0.68 value reflects that some mitigation exists (only ~30% of CO2 becomes dissolved CO2; some organisms can buffer; some regions are less affected) — were ocean acidification literally 100% efficacious at extracting from all calcifiers simultaneously with zero alternatives, ε would approach 0.90. The actual ε reflects partial exposure with some ecological and geographic variance. Suppression (0.72): Very high. Suppression is structural: calcifiers cannot exit their biochemistry, cannot lobby, cannot vote, cannot migrate (for sessile organisms like corals). Fishing communities face high barriers to livelihood transition, capital constraints, and weak political access to carbon policy. The suppression is not enforced by institutional violence but by economic dependency and biological lock-in. Victims cannot coordinate effectively (distributed globally, communicate only through ecosystems, lack financial resources to organize). Theater ratio (0.58): Moderate-high, reflecting that international climate policy has produced extensive institutional response (UNFCCC, Paris Agreement, IPCC assessments, national pledges) without corresponding emissions control. The ritual of climate summits and the performance of national climate commitments creates appearance of governance without solving the underlying extraction mechanism. The theater has risen over time as policy proliferation has accelerated without emissions decline.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a fundamental asymmetry in how different agents perceive the same mechanism. Fossil fuel industries see coordination (Rope) — a functioning system that enables profitable energy production. Calcifying organisms and fishing communities see snare — systematic extraction with no escape. Industrial fishing sees tangled rope — mixed benefits (cheap energy) and harms (ecosystem collapse). Policy apparatus sees piton — institutional ritual degrading but persisting. The analytical observer risks seeing mountain — framing extraction as a law of nature (CO2 + water = acid) rather than a contingent social choice (burning fossil fuels is optional; externalizing costs is chosen). The perspectival gap reveals that the 'same' constraint is structurally different depending on whether you are a beneficiary with exit options or a victim without them. The gap is not epistemological (disagreement about facts) but structural: the agents occupy different positions in the extraction flow and experience different constraint geometries.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's effective extractiveness (χ) is computed from base extractiveness (ε = 0.68), directionality (d), and scope modifier. Fossil fuel industries: d ≈ 0.05 (full beneficiary + arbitrage exit) → f(d) ≈ -0.12 → χ negative (they experience extraction flowing toward them, not away). Calcifying organisms: d ≈ 0.95 (full victim + trapped exit) → f(d) ≈ 1.42 → χ amplified (maximum experienced extraction). Fishing communities: d ≈ 0.85 (victim + constrained exit) → f(d) ≈ 1.15 → χ high (severe experienced extraction, though less than trapped organisms). Industrial fishing: d ≈ 0.50 (mixed beneficiary/victim + mobile exit) → f(d) ≈ 0.65 → χ moderate (mixed experience). The canonical scope modifier σ(global) = 1.2 applies to all, amplifying χ by 20% to reflect that ocean acidification is a planetary-scale coordination problem where evasion is impossible — no region can escape the constraint unilaterally. Beneficiaries cannot arbitrage away from liability (unlike, e.g., a carbon tax where industries can relocate); victims cannot migrate globally to unpolluted oceans (the constraint is universal). This scope-scaling reflects that global scope makes the constraint inescapable for both sides — it cements the extraction in place.
 *
 * MANDATROPHY ANALYSIS:
 *   Ocean acidification as a pure snare (ε = 0.68, suppression = 0.72, χ ≥ 0.66) faces no mandatrophy — the classification is robust. Unlike constraints that might blur between coordination and extraction (tangled ropes), ocean acidification has an unambiguous beneficiary (fossil fuel industries) and victims (calcifiers, fishing communities) with zero coordination benefit for victims. The snare gate is unambiguously satisfied. The mandatrophy resolution is therefore to confirm that this IS extraction, not to defend it as coordination. The challenge is instead the false summit omega: the constraint can be mischaracterized as a natural law (immutable biogeochemistry) when framed without beneficiary structure. The false-summit detection mechanism is critical here — by declaring beneficiaries explicitly, the schema ensures that the engine detects and flags the naturalizing interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbonate_saturation_threshold,
    'What carbonate saturation level represents the functional collapse point for calcifying organisms, and does it vary across species and ecosystem types?',
    'Laboratory and field studies measuring shell dissolution rates, metabolic costs, and fitness impacts across pH levels; mapping of critical thresholds for key species (pteropods, corals, oysters, crustaceans)',
    'If threshold varies widely: different organisms experience different extraction severity (constrains the snare classification to organisms above the threshold). If threshold is sharp and universal: all calcifiers are equally trapped (strengthens snare classification). If humans can engineer adaptation: exit options change from trapped to constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbonate_saturation_threshold, empirical, 'Carbonate saturation threshold for calcifying organism viability').

omega_variable(
    food_web_cascades_and_human_impact,
    'How severely do pteropod and small crustacean population declines cascade through food webs to human-dependent fish stocks, and at what lag time?',
    'Time-series analysis of pteropod abundance, fish recruitment, and human catch data; ecosystem modeling linking plankton to commercial species; economic cost quantification of fishery collapse relative to fossil fuel industry value extraction',
    'If cascades are rapid and severe: fishing communities transition from snare to catastrophic-collapse state in years rather than decades (accelerates victim recognition). If cascades are delayed and buffered by ecosystem redundancy: extraction appears gradual, extending suppression through false confidence in slow-onset dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(food_web_cascades_and_human_impact, empirical, 'Food web cascade severity and lag from pteropod collapse to human fishery impact').

omega_variable(
    technological_adaptation_feasibility,
    'Can marine calcifiers or dependent human communities technologically adapt (shell strengthening, pH-neutral aquaculture, ocean alkalinity enhancement) to maintain productivity in acidified oceans?',
    'Assessment of biological limits to calcifier adaptation; economic analysis of aquaculture transition costs; techno-economic evaluation of large-scale ocean alkalinity enhancement (cost, energy, side effects)',
    'If adaptation is feasible at scale: victim classification becomes constrained rather than trapped (exit options improve, though at high cost). If adaptation is infeasible: snare classification is strengthened — no exit exists except abandonment of calcifier-dependent livelihoods. If adaptation is possible for some organisms/regions but not others: constraint decomposes into multiple stories with different ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_adaptation_feasibility, empirical, 'Feasibility and scale-potential of technological adaptation to ocean acidification').

omega_variable(
    fossil_fuel_industry_exit_optionality,
    'What is the true cost to fossil fuel industries of transitioning to renewable energy and carbon capture, and does this cost structure change their exit_options classification from arbitrage to constrained?',
    'Cost-benefit analysis comparing: (a) continued fossil fuel production with carbon externalization, (b) renewable energy transition with stranded asset writedown, (c) carbon capture and sequestration retrofitting; historical precedent analysis (coal-to-gas transitions, nuclear phase-outs)',
    'If transition costs are low (< 10% capital restructuring): industries are truly arbitrage-positioned (low-cost exit exists), confirming beneficiary status. If transition costs are high (> 30% value destruction): industries are constrained rather than arbitrage, raising the possibility of institutional coercion as response. If costs vary by firm: classification requires separate perspectives per industry segment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_industry_exit_optionality, empirical, 'True transition cost from fossil fuels to renewables for industry exit options').

omega_variable(
    natural_law_versus_constructed_framing,
    'Is ocean acidification from anthropogenic CO2 a natural law (immutable chemical principle) or a constructed constraint (contingent on fossil fuel industry beneficiary structure)?',
    'Epistemological analysis: distinguishing universal biogeochemical principles (CO2 + H2O → H2CO3) from contingent social practices (burning fossil fuels for energy). Policy counterfactual: would acidification occur if CO2 emissions had been capped in 1980, and if not, is it a natural law or a policy choice?',
    'If natural law interpretation is adopted: constraint appears immutable, suppression is legitimate (organisms cannot escape physics), victim agency is irrelevant. If constructed interpretation is adopted: constraint is contingent, beneficiary status becomes salient, and policy intervention is justified. FSM signature activated: false summit detection flags the mountain perspective as naturalization when beneficiaries are declared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_versus_constructed_framing, conceptual, 'Whether ocean acidification is natural law or constructed extraction mechanism').

omega_variable(
    measurement_observable_stability,
    'Does measuring ocean acidification by pH level, carbonate saturation, or calcium carbonate dissolution rates produce the same ε value (extractiveness), or does choice of observable shift the classification?',
    'If pH-based measurement gives ε ≈ 0.68 (high snare), but carbonate saturation measurement gives ε ≈ 0.32 (coordination problem with adaptation feasibility), then the single constraint has decomposed. Each observable would require a separate constraint story with its own perspectives and metrics.',
    'If observables yield different ε: decompose into constraint family (ocean_acidification_ph_basis, ocean_acidification_carbonate_saturation_basis) linked via network. If observables are equivalent: single story is appropriate. This omega addresses ε-invariance principle enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_observable_stability, empirical, 'Whether different measurement bases for ocean acidification yield the same extractiveness metric').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ocean_acidification, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oa_tr_t0, ocean_acidification, theater_ratio, 0, 0.15).
narrative_ontology:measurement(oa_tr_t40, ocean_acidification, theater_ratio, 40, 0.42).
narrative_ontology:measurement(oa_tr_t80, ocean_acidification, theater_ratio, 80, 0.58).

% Extraction over time
narrative_ontology:measurement(oa_be_t0, ocean_acidification, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(oa_be_t40, ocean_acidification, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(oa_be_t80, ocean_acidification, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(oa_su_t0, ocean_acidification, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(oa_su_t40, ocean_acidification, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(oa_su_t80, ocean_acidification, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ocean_acidification, resource_allocation).
narrative_ontology:affects_constraint(ocean_acidification, coral_reef_collapse).
narrative_ontology:affects_constraint(ocean_acidification, pteropod_stock_decline).
narrative_ontology:affects_constraint(ocean_acidification, coastal_fisheries_degradation).
narrative_ontology:affects_constraint(ocean_acidification, fossil_fuel_industry_externality_structure).

% DUAL FORMULATION NOTE:
% Ocean acidification as a primary constraint connects to multiple downstream constraints affecting specific marine systems. Each downstream constraint (coral collapse, pteropod decline) has its own ε and perspectives reflecting the organism-specific or ecosystem-specific manifestation. The upstream constraint (this story) models the global pH/carbonate chemistry system that drives all downstream impacts. Linking enables propagation analysis — if ocean acidification worsens, all downstream constraints degrade simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ocean_acidification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
