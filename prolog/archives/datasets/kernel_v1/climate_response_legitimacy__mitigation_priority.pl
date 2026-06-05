% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__mitigation_priority, []).

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
 *   constraint_id: climate_response_legitimacy__mitigation_priority
 *   human_readable: Climate Mitigation Priority Reading: Emissions Reduction via Technology and Carbon Pricing
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The mitigation priority reading of the climate response kernel asserts
 *   that legitimate climate action consists of emissions reduction through
 *   technological innovation and carbon pricing mechanisms, while preserving
 *   economic growth in wealthy nations through 'decoupling' — increasing GDP
 *   while decreasing emissions. This constraint exhibits structural tension
 *   between its coordination function (global emissions must be reduced to
 *   stabilize atmosphere) and its extraction mechanisms (transition costs are
 *   distributed asymmetrically, technological pathways create lock-in risks,
 *   and institutional enforcement is largely performative). The reading
 *   exists in active contest with two sibling readings: the adaptation
 *   priority reading (accept warming trajectory and focus on protecting
 *   vulnerable populations) and the degrowth transformation reading
 *   (dismantle growth imperative to achieve decarbonization). This constraint
 *   story generates ONLY the mitigation priority reading; the sibling
 *   readings are separate constraints with their own ε values. The
 *   permutation of six classification types across perspectives reflects that
 *   the same structural phenomenon — embedding climate action in a
 *   technological-market framework — appears as coordination (rope from tech
 *   sector), extraction (snare from future generations if decoupling fails),
 *   performance theater (piton from governance institutions), and even
 *   physical necessity (false summit mountain from analytical observer). The
 *   core empirical uncertainty (omega variables) centers on whether
 *   decoupling at required rates is technically and economically feasible,
 *   whether carbon pricing enforcement works in practice, and whether the
 *   reading's technological commitments create path-dependent lock-in
 *   foreclosing future policy options.
 *
 * KEY AGENTS:
 *   - Current Generation in Wealthy Nations (powerful/arbitrage): Primary beneficiary — experiences mitigation priority as market opportunity and growth-preservation. Benefits from transition subsidies, technological leadership, and deferred climate impacts.
 *   - Future Generations (powerless/trapped): Primary victim — locked into climate trajectory determined by current pathway. If decoupling fails, inherits warmed world with constrained adaptation capacity. No exit option.
 *   - Workers in Carbon-Intensive Sectors (moderate/constrained): Secondary victim — face job displacement, retraining costs, wage suppression during transition. Constrained by sectoral dependency; benefits from transition support programs (asymmetrically distributed).
 *   - Renewable Tech & Finance Sector (institutional/arbitrage): Co-beneficiary with wealthy current generation. Experiences constraint as pure coordination. Market demand from carbon pricing creates dominant position for renewable manufacturers and green finance.
 *   - Incumbent Carbon-Intensive Industries (powerful/arbitrage): Mixed beneficiary-victim. Nominally constrained by carbon pricing but can extract value through regulatory capture, weak pricing, offsets, and greenwashing. Arbitrage options available.
 *   - Climate Governance Institutions (institutional/arbitrage): Maintain ritual authority through emissions accounting and NDC reviews, but actual enforcement is largely performative (theater ratio 0.68+). Inertial actors benefiting from symbolic compliance.
 *   - Climate Justice & Degrowth Movements (organized/constrained): Organized opposition recognizing mitigation priority as temporary holding structure. See constraint as insufficient and extractive; view themselves as building exit pathway to deeper transformation.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional choices as immutable physical laws. The mountain classification (physical necessity) is a false summit — the constraint is a reading of how to respond to physical limits, not the limits themselves.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_legitimacy__mitigation_priority, 0.62).
domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_legitimacy__mitigation_priority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__mitigation_priority, "Climate Mitigation Priority Reading: Emissions Reduction via Technology and Carbon Pricing").
narrative_ontology:topic_domain(climate_response_legitimacy__mitigation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__mitigation_priority, 'cce2ec62-cc10-4a22-b327-a0c33a0ec287').
narrative_ontology:cs_kernel_codification('cce2ec62-cc10-4a22-b327-a0c33a0ec287', formalized).
narrative_ontology:cs_authority_grounding('cce2ec62-cc10-4a22-b327-a0c33a0ec287', extraction).
narrative_ontology:cs_interpretation_layer_present('cce2ec62-cc10-4a22-b327-a0c33a0ec287').
narrative_ontology:cs_reading_relation('cce2ec62-cc10-4a22-b327-a0c33a0ec287', climate_response_legitimacy__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('cce2ec62-cc10-4a22-b327-a0c33a0ec287', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('cce2ec62-cc10-4a22-b327-a0c33a0ec287', foundational, decoupling_technically_feasible).
narrative_ontology:cs_axiom_status(decoupling_technically_feasible, holdable).
narrative_ontology:cs_axiom_grounding('cce2ec62-cc10-4a22-b327-a0c33a0ec287', decoupling_technically_feasible, empirically_contingent).
narrative_ontology:cs_axiom('cce2ec62-cc10-4a22-b327-a0c33a0ec287', foundational, growth_preservation_compatible_with_climate_stability).
narrative_ontology:cs_axiom_status(growth_preservation_compatible_with_climate_stability, holdable).
narrative_ontology:cs_axiom_grounding('cce2ec62-cc10-4a22-b327-a0c33a0ec287', growth_preservation_compatible_with_climate_stability, instrumental).
narrative_ontology:cs_reference_frame('cce2ec62-cc10-4a22-b327-a0c33a0ec287', technological_decoupling_possible_growth_preservable).
narrative_ontology:cs_drift_state('cce2ec62-cc10-4a22-b327-a0c33a0ec287', contemporary_post_paris_2015_to_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cce2ec62-cc10-4a22-b327-a0c33a0ec287', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, current_generation_wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__mitigation_priority, renewable_tech_manufacturers).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__mitigation_priority, vulnerable_populations_in_warming_scenarios).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS IF DECOUPLING FAILS (SNARE) — Locked into climate trajectory determined by current emissions and technological pathway dependency. If carbon pricing and renewable scale-up fail to achieve decoupling, future generations inherit a warmed world with limited adaptation capacity. No exit option; bear full cost of path dependence. Maximum experienced extraction because the constraint embeds intergenerational transfer of climate risk without guarantee of success.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKERS IN CARBON-INTENSIVE SECTORS (TANGLED ROPE) — Face genuine coordination problem (need to transition industries) but also extraction through job displacement, retraining burden, and wage suppression during transition. The mitigation priority reading distributes transition costs asymmetrically: workers bear relocation and skills barriers while corporate entities receive subsidy and market guarantees. Significant extraction but real coordination function — the constraint both enables and constrains their participation in transition.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE TECH MANUFACTURERS & GREEN FINANCE (ROPE) — Primary beneficiary under the mitigation priority reading. Experiences the constraint as pure coordination: carbon pricing creates market demand for renewables, innovation subsidies fund R&D, and international agreements establish scale-up guarantees. Net beneficiary with arbitrage options — can pivot to dominant market position during energy transition. Minimal extraction experienced; sees constraint as enabling coordination.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT CARBON-INTENSIVE INDUSTRIES (TANGLED ROPE) — Experience mixed coordination and extraction. The carbon pricing mechanism ostensibly creates incentive to transition (coordination function), but weak carbon prices, regulatory exemptions, and offsets create pathways to profit from transition without decarbonization (stranded asset concealment, greenwashing). Powerful actors can extract value through regulatory capture while nominally participating in the mitigation framework. Sophisticated extraction embedded in apparent coordination.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UNFCCC, IPCC, NATIONAL CLIMATE AUTHORITIES (PITON) — Maintain ritual legitimacy through emissions inventories, carbon accounting frameworks, and NDC (Nationally Determined Contribution) reviews, but the actual verification and enforcement mechanisms are largely performative. Theater_ratio (0.68) reflects that institutional commitments often lack binding verification, corporations can self-report emissions, and accountability is diffuse. Institutions persist through inertia and symbolic authority rather than enforcement capacity. The ritual persists because alternatives haven't fully replaced it, not because verification works reliably.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE JUSTICE MOVEMENTS & DEGROWTH ADVOCATES (SCAFFOLD) — See the mitigation priority reading as a temporary holding structure that will inevitably collapse or transition to deeper transformation. They view carbon pricing + technology as insufficient and extractive (shifting costs to vulnerable populations), but recognize mitigation priority as a staging ground for more radical demands. Sunset logic: as technological optimism falters and climate impacts accelerate, pressure builds toward degrowth or adaptation-priority readings. Exit path visible — constraint contains its own negation.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, physics imposes hard constraint: atmospheric CO₂ concentration must stabilize to avoid dangerous warming. The mitigation priority reading claims this requires emissions reduction + economic preservation simultaneously (decoupling). From this frame, the constraint appears as a law of thermodynamics and atmospheric chemistry — immutable, non-negotiable. However, the structural data (beneficiary concentration, suppression mechanisms, technology dependency risks) contradicts the mountain classification. The engine will identify this as a false summit: what appears as physical necessity is actually a contested institutional reading of how to manage the physical constraint.
constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__mitigation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_response_legitimacy__mitigation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_response_legitimacy__mitigation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_response_legitimacy__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, moderate-high): The mitigation priority reading concentrates benefits among wealthy nations, renewable tech manufacturers, and corporate actors capable of regulatory arbitrage, while distributing transition costs to workers, vulnerable populations, and future generations. The 0.58 value reflects that extraction is significant but mediated through market mechanisms (carbon pricing) and partial coordination functions (technology transfer, green finance). It is not a pure snare (would be >0.66) because genuine coordination functions exist — global emissions reduction does require technology deployment and price signals. The temporal trajectory (0.38 → 0.58) reflects accumulating extraction as carbon pricing mechanisms mature and reveal enforcement gaps (offsets, accounting fraud) and as lock-in effects of renewable infrastructure investment constrain future policy options. Suppression (0.62, moderate-high): Barriers to exit and alternative pathways include regulatory capture (incumbent industries shape carbon pricing rules), technological lock-in (early renewable investment constrains degrowth or rapid adaptation options), cultural-epistemic lock-in (dominant narrative that 'growth can continue') suppresses awareness of the reading's contingency, and institutional inertia (UNFCCC/IPCC structures have vested interest in mitigation priority). International context: low-income nations face constrained choices between accepting climate impact (adaptation inadequacy) and accepting externalized transition costs from wealthy nations (justice constraint). Theater_ratio (0.68, high): International climate institutions rely heavily on performative mechanisms: carbon accounting with verification gaps, voluntary corporate net-zero commitments with greenwashing potential, NDC reviews with no enforcement mechanism, offset markets with permanence failures. The trajectory (0.52 → 0.68) reflects that early mitigation optimism (2015-2020) has given way to recognition that institutional frameworks lack enforcement capacity. Governance institutions persist through symbolic authority rather than functional verification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's six perspectives span the full classification spectrum: rope (tech sector beneficiary), snare (future generations if decoupling fails), tangled rope (workers, incumbents), piton (governance institutions), scaffold (climate justice movements), and mountain (false summit). The perspectival gap reveals that the same structural phenomenon — embedding climate response in technological-market frameworks — is experienced as coordination by beneficiaries, extraction by victims, performance by institutions, and inevitability by analytical observers. The gap is not a measurement error but a structural fact: the mitigation priority reading genuinely benefits some agents and constrains others. The analytical observer's mountain classification is particularly revealing — it risks naturalizing the choice to pursue mitigation through decoupling as the only possible response to physical constraints, obscuring the reading's contingency relative to adaptation-priority and degrowth-transformation alternatives. False summit detection is active: the constraint declares current_generation_wealthy_nations as beneficiary, which triggers FSM evaluation (beneficiary presence on a mountain disqualifies natural law status).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation chains from three structural vectors: (1) beneficiary/victim declarations, (2) agent power level + exit options, (3) canonical d fallbacks. Current generation wealthy nations: institutional power + arbitrage exit → d ≈ 0.15 (beneficiary at low-friction option) → f(d) ≈ -0.01 → χ dampened (experienced extraction near zero). Future generations: powerless + trapped → d ≈ 0.95 (victim with no exit) → f(d) ≈ 1.42 → χ amplified (maximum experienced extraction). Workers in transition sectors: moderate power + constrained exit → d ≈ 0.65 (victim facing high but surmountable barriers) → f(d) ≈ 1.00 → χ at canonical level. Renewable tech sector: institutional power + arbitrage → d ≈ 0.05 (beneficiary with market optionality) → f(d) ≈ -0.12 → χ negative (experienced subsidy rather than extraction). Scope modifier σ(S) applied at global scope (σ=1.2): extraction harder to verify at planetary scale, enabling hidden extraction through accounting complexity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (What legitimates classifying extraction as coordination?) by revealing that the mitigation priority reading legitimates itself through four distinct mechanisms: (1) Physical necessity framing (false summit) — the atmosphere's carbon budget is real, making emissions reduction imperative; (2) Technological optimism (rope framing) — decoupling is possible and market mechanisms efficiently allocate technology; (3) Justice rhetoric (tangled rope framing) — transition costs are distributed to protect vulnerable populations; (4) Institutional inertia (piton framing) — governance structures persist despite low functional verification. These framings are not false — but neither are they the *only* way to respond to the physical constraint. The alternative readings (adaptation, degrowth) respond to the same physical reality differently. The mandatrophy is resolved by recognizing that 'legitimate climate response' is not empirically determinate — it is a contested normative choice among readings that respond to different agent preferences and values. The mitigation priority reading privileges wealthy nations' preference for growth continuity; the degrowth reading privileges intergenerational equity and environmental regeneration; the adaptation reading privileges near-term vulnerability reduction. No reading is illegitimate — but the choice between them has distributive consequences (reflected in the beneficiary/victim structure and victim set inclusion of future generations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility_threshold,
    'What global emissions reduction rate is physically required to limit warming to 1.5°C, and what decoupling rate (GDP growth + emissions reduction) has been historically achieved or is technically feasible?',
    'Comparison of IPCC carbon budget requirements vs. historical decoupling rates in high-income nations (typically 1-3% absolute emissions reduction annually while maintaining GDP growth). Test whether required rate (4-7% annually) has been achieved without recession or energy shock in any economy.',
    'If decoupling at required rates is not feasible: mitigation priority reading becomes structurally impossible — constraint reclassifies as snare or forces transition to adaptation or degrowth readings. If feasible at high cost: validates tangled rope classification but shifts victim set asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decoupling_feasibility_threshold, empirical, 'Physical feasibility of emissions decoupling from economic growth at required rates').

omega_variable(
    carbon_removal_technical_viability,
    'Can direct air capture (DAC) and other carbon dioxide removal (CDR) technologies scale to offset residual emissions (500-1000 GtCO₂ by 2100) at costs compatible with continued economic growth?',
    'Techno-economic modeling of CDR deployment: capital requirements, energy demands, storage permanence, and cost curves. Historical precedent analysis from similar scalability challenges (solar PV cost curves, battery manufacturing). Field trials and pilot plant performance data.',
    'If CDR cannot scale at required rates/costs: mitigation priority''s technological foundation collapses, revealing structural extraction through postponement of harder choices. If feasible: validates rope classification from tech sector perspective but increases technological dependency risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_removal_technical_viability, empirical, 'Scalability and cost viability of carbon dioxide removal technology deployment').

omega_variable(
    carbon_pricing_enforcement_reality,
    'What is the actual compliance rate for carbon pricing mechanisms, and what proportion of emissions claimed as ''offset'' represent genuine emissions reductions vs. accounting artifacts or permanence failures?',
    'Audit of carbon credit markets: tracking of claimed offsets through satellite verification (deforestation monitoring, renewable energy verification). Analysis of regulatory evasion (firms shifting emissions to unpriced jurisdictions, accounting fraud, offset permanence failures).',
    'If enforcement is <50% effective: carbon pricing is primarily theater, and suppression mechanisms are primarily symbolic (piton classification confirmed). If >80% effective: validates rope classification from institutional perspective. Medium effectiveness (50-80%) confirms tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_pricing_enforcement_reality, empirical, 'Real-world compliance and effectiveness of carbon pricing enforcement mechanisms').

omega_variable(
    reading_kernel_foreclosure,
    'Do the core premises of the mitigation priority reading (technological decoupling is feasible, economic growth can be preserved, carbon pricing coordinates transition) logically foreclose the degrowth transformation reading''s core premise (growth must be dismantled for decarbonization)?',
    'Logical analysis: if decoupling is feasible at required rates, does degrowth retain structural necessity? If growth can be preserved, does degrowth''s economy dismantling premise still hold? Causal vs. definitional necessity distinction.',
    'If mitigation premise forecloses degrowth: one reading eliminates the other in any consistent framework. If they coexist: both remain live options for different parties (coexists_with relation). Affects cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Logical relationship between mitigation priority and degrowth readings of the climate response kernel').

omega_variable(
    technological_path_dependency_lock,
    'Does the mitigation priority''s commitment to carbon pricing + renewable energy scale-up create irreversible lock-in to specific technological pathways that prevent future shifts toward degrowth or adaptation-priority strategies?',
    'Historical analysis of energy infrastructure path dependence (sunk costs in renewable energy, grid architecture decisions, supply chains). Modeling of counterfactual scenarios: what options remain available if 10-20 years of mitigation-priority investment lock in renewable + CCS infrastructure incompatible with lower-energy futures?',
    'If significant lock-in: future generations face constrained set of policy options, confirming victim status. If reversible: technological dependency risk is lower, classification shifts toward rope from future-generation perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_path_dependency_lock, empirical, 'Degree of irreversible path dependence created by mitigation-priority technology commitments').

omega_variable(
    vulnerable_population_transition_distribution,
    'Are transition costs of emissions reduction (job displacement, energy price increases, infrastructure disruption) distributed proportionally to historical emissions responsibility, or do they fall asymmetrically on vulnerable populations and low-income nations?',
    'Empirical analysis of carbon pricing regressive effects, just transition program coverage, international burden-sharing mechanisms. Tracking of who bears costs in practice vs. stated equity principles.',
    'If asymmetric distribution confirmed: suppression mechanisms are actively extractive (current generation externalizes costs to vulnerable). If proportional: extraction is lower, classification shifts toward rope. Affects base_properties.suppression calibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerable_population_transition_distribution, empirical, 'Fairness of transition cost distribution across populations and nations').

omega_variable(
    reading_contest_authority_grounding,
    'What authority structure grounds the legitimacy of the mitigation priority reading, and is that authority structure robust against challenge from adaptation or degrowth readings?',
    'Institutional analysis: IPCC technical authority, national climate policy structures, international agreements (Paris Accord). Assessment of whether technical expertise (IPCC) can arbitrate between readings with different normative premises (preserve growth vs. dismantle growth), or whether the readings rest on incommensurable value commitments.',
    'If technical authority cannot arbitrate: readings are genuinely incommensurable; all remain coexist_with relations. If one reading''s authority can be demonstrated as superior: foreclosure or influence relations become valid. Affects cs_structure.authority_grounding and reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_authority_grounding, conceptual, 'Authority grounding and legitimacy status of the mitigation priority reading within the kernel contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__mitigation_priority, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_mit_theater_t0, climate_response_legitimacy__mitigation_priority, theater_ratio, 0, 0.52).
narrative_ontology:measurement(clim_mit_theater_t8, climate_response_legitimacy__mitigation_priority, theater_ratio, 8, 0.68).
narrative_ontology:measurement(clim_mit_theater_t16, climate_response_legitimacy__mitigation_priority, theater_ratio, 16, 0.72).

% Extraction over time
narrative_ontology:measurement(clim_mit_extract_t0, climate_response_legitimacy__mitigation_priority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_mit_extract_t8, climate_response_legitimacy__mitigation_priority, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(clim_mit_extract_t16, climate_response_legitimacy__mitigation_priority, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_mit_suppress_t0, climate_response_legitimacy__mitigation_priority, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(clim_mit_suppress_t8, climate_response_legitimacy__mitigation_priority, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(clim_mit_suppress_t16, climate_response_legitimacy__mitigation_priority, suppression_requirement, 16, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__mitigation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, climate_response_legitimacy__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, carbon_pricing_enforcement_mechanism).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, renewable_energy_scalability).
narrative_ontology:affects_constraint(climate_response_legitimacy__mitigation_priority, intergenerational_discount_rate).

% DUAL FORMULATION NOTE:
% The climate_response_legitimacy kernel has three structurally distinct readings with different ε values and beneficiary/victim structures. mitigation_priority (ε ≈ 0.58, this constraint) centers technological coordination + growth preservation. adaptation_priority (ε ≈ 0.45, separate constraint) centers near-term resilience coordination + accepted warming. degrowth_transformation (ε ≈ 0.68, separate constraint) centers structural economy change + intergenerational equity. Each reading responds to the same physical constraint (atmospheric CO₂) but produces different constraints on policy, beneficiary/victim distributions, and risk profiles. All three are linked via affects_constraints to represent kernel interdependency: pressure on one reading (empirical failure of decoupling) creates pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__mitigation_priority, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
