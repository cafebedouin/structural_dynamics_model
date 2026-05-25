% ============================================================================
% CONSTRAINT STORY: nature_based_carbon_sink_displacement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nature_based_carbon_sink_displacement, []).

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
 *   constraint_id: nature_based_carbon_sink_displacement
 *   human_readable: Nature-Based Carbon Sink Displacement
 *   domain: climate_policy/environmental_economics
 *
 * SUMMARY:
 *   Nature-based carbon sink displacement describes a structural constraint
 *   where carbon credit mechanisms, designed to incentivize ecosystem
 *   conservation and restoration, systematically extract value from
 *   indigenous land stewards and biodiverse ecosystems while displacing
 *   emissions-reduction obligations elsewhere. The constraint operates
 *   through a hybrid mechanism: genuine coordination (carbon accounting
 *   coordinates land-use decisions with financial incentives) overlaid with
 *   asymmetric extraction (carbon value flows to financial intermediaries and
 *   commodity producers; ecosystem value and rights flow away from indigenous
 *   stewards). The extractiveness has increased from 0.38 to 0.62 over the
 *   measurement interval as carbon markets have scaled, verification
 *   standards have become more gameable, and displacement mechanisms have
 *   become more sophisticated. The theater ratio (rising from 0.52 to 0.71)
 *   reflects that carbon accounting procedures increasingly measure metrics
 *   (carbon stored) decoupled from ecological outcomes (ecosystem health,
 *   indigenous sovereignty, permanence). This constraint exhibits all six DR
 *   types from different observation points, making it a diagnostic exemplar
 *   for how indexical classification reveals competing claims about the same
 *   structural phenomenon.
 *
 * KEY AGENTS:
 *   - Indigenous Land Stewards: Primary victim (powerless/trapped) — dispossessed, their land converted to commodity production or monoculture plantations, no meaningful consent or benefit-sharing
 *   - Biodiverse Ecosystems: Primary victim (powerless/trapped) — abstract but real entity, unable to organize, bearing full cost of displacement via ecosystem degradation
 *   - Climate Mitigation Integrity: Victim at civilizational scope (analytical/snare) — locked into false solution pathway where credits substitute for emissions reductions
 *   - Small Farmers and Local Communities: Secondary victim (moderate/constrained) — constrained choice between participation in schemes (with debt risk) or land loss
 *   - Carbon Credit Financial Intermediaries: Primary beneficiary (institutional/arbitrage) — capture rent between land-use actors and climate-concerned capital; arbitrage to different instruments if carbon prices fall
 *   - Agricultural and Forest Industrial Operators: Secondary beneficiary (powerful/mobile) — convert high-biodiversity ecosystems to managed monocultures, receive carbon credits, capture commodity value; mobile actors with exit options
 *   - Verification and Standards Bodies: Organized actors (organized/constrained) — developing higher standards (FPIC, permanence, additionality) as sunset mechanism for the displacement constraint
 *   - Carbon Accounting Ritual: Institutional actor (institutional/arbitrage) — maintains performative accounting that measures carbon stored while ignoring ecosystem health and indigenous rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nature_based_carbon_sink_displacement, 0.58).
domain_priors:suppression_score(nature_based_carbon_sink_displacement, 0.62).
domain_priors:theater_ratio(nature_based_carbon_sink_displacement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nature_based_carbon_sink_displacement, extractiveness, 0.58).
narrative_ontology:constraint_metric(nature_based_carbon_sink_displacement, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(nature_based_carbon_sink_displacement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nature_based_carbon_sink_displacement, tangled_rope).
narrative_ontology:human_readable(nature_based_carbon_sink_displacement, "Nature-Based Carbon Sink Displacement").
narrative_ontology:topic_domain(nature_based_carbon_sink_displacement, "climate_policy/environmental_economics").

domain_priors:requires_active_enforcement(nature_based_carbon_sink_displacement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nature_based_carbon_sink_displacement, agricultural_commodity_producers).
narrative_ontology:constraint_beneficiary(nature_based_carbon_sink_displacement, forest_industrial_operators).
narrative_ontology:constraint_beneficiary(nature_based_carbon_sink_displacement, carbon_credit_financial_intermediaries).
narrative_ontology:constraint_victim(nature_based_carbon_sink_displacement, indigenous_land_stewards).
narrative_ontology:constraint_victim(nature_based_carbon_sink_displacement, biodiverse_ecosystems).
narrative_ontology:constraint_victim(nature_based_carbon_sink_displacement, climate_mitigation_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS STEWARDS & BIODIVERSE ECOSYSTEMS (SNARE) — Cannot exit the carbon-sink displacement regime. Lands managed for millennia with high carbon sequestration and biodiversity are converted to commodity production or monoculture plantations, then credited as 'nature-based solutions.' Stewards face legal dispossession, cultural erasure, and resource extraction with no meaningful consent mechanisms. Trapped in a system that extracts the carbon credit value while destroying the actual ecological function. Maximum extraction, high suppression via property law and capitalist land markets.
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL FARMERS & LOCAL COMMUNITIES (TANGLED ROPE) — Face constrained choice: participate in carbon credit schemes or lose land access entirely. The constraint coordinates genuine land-use decisions (which crops, forestry rotation, conservation intensity) but extracts by transferring carbon value ownership to external financial actors. Some agents benefit from carbon payments; others face debt traps when commodity prices fall and credit terms tighten. Mixed extraction and coordination with significant asymmetry.
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CARBON CREDIT INTERMEDIARIES (ROPE) — Financial actors, offset platforms, and carbon credit traders experience the constraint as pure coordination. They aggregate land-use data, verify carbon accounting, and facilitate credit sales. Net beneficiaries capturing rent between commodity producers and climate-concerned capital. Exit via arbitrage: if verification standards tighten or carbon prices fall, they pivot to different instruments. Effective extraction runs toward these actors, but they perceive it as legitimate transaction services.
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMODITY PRODUCERS & FOREST OPERATORS (TANGLED ROPE) — Coordinate land-use intensity and carbon sequestration with financial incentives. Also extract by converting high-biodiversity natural ecosystems to managed monocultures, then receiving carbon credits for 'reforestation' or 'conservation.' Mobile actors — can exit by selling land or shifting to different commodities. Beneficiaries overall, but constrained by carbon accounting rules and compliance verification. Mixed perspective: coordination of land management + extraction of ecosystem value.
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STANDARDS & VERIFICATION BODIES (SCAFFOLD) — Organized actors (VCS, Gold Standard, compliance agencies) see the carbon-sink displacement as a temporary coordination problem being solved through better additionality verification, baseline-setting standards, and indigenous land rights recognition. Standards for free, prior, and informed consent (FPIC), biodiversity safeguards, and permanence requirements are building alternative pathways with a sunset: as standards mature and enforcement tightens, displacement mechanisms lose legitimacy. Low effective extraction because these actors have agency and see a technical solution path with institutional backing.
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CARBON ACCOUNTING THEATER (PITON) — The carbon-accounting apparatus (IPCC methodologies, national greenhouse gas inventories, carbon credit registries) persists largely through institutional inertia. These systems perform climate mitigation without necessarily achieving it — a ton of carbon displaced from a biodiversity hotspot is credited as a ton mitigated, even though the ecosystem function was destroyed. The ritual persists because alternatives (carbon taxes, binding emission caps) are politically infeasible. Theater ratio is high because the accounting system measures a metric (carbon stored) that is decoupled from the actual ecological outcome (ecosystem health, indigenous rights, mitigation integrity).
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE MITIGATION INTEGRITY (SNARE) — From a civilizational/universal perspective, the carbon-sink displacement constraint extracts from the actual climate mitigation goal. Ecosystem-based carbon storage is credited as mitigation, but if the storage is temporary, non-additional, or displaces emissions-reduction obligations elsewhere, the mitigation is illusory. The constraint locks the climate system into a false solution pathway where carbon-credit financialization substitutes for real emissions reductions. Powerless victim position — climate goals cannot exit the constraint; they are bound by the accounting rules that permit displacement to persist.
constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nature_based_carbon_sink_displacement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nature_based_carbon_sink_displacement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nature_based_carbon_sink_displacement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nature_based_carbon_sink_displacement, TR),
    TR >= 0.70.

:- end_tests(nature_based_carbon_sink_displacement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts via multiple mechanisms: (1) carbon credit financialization captures ecosystem value and transfers it to external actors; (2) baseline-setting inflation enables higher credit issuance than real additionality warrants; (3) leakage and displacement move emissions to unmonitored regions, creating zero-sum or negative net mitigation. The extraction is not total (some genuine conservation occurs, some communities benefit from payments) but is substantial and systematic. Suppression (0.62): High. Indigenous stewards are suppressed through property law (land titles and capitalist markets), information asymmetry (complex carbon accounting methodologies), and political power (weak representation in carbon credit governance). Commodity producers suppress indigenous resistance through legal mechanisms and capital power. Theater ratio (0.68): High and rising. The carbon accounting system measures carbon stored (the official metric) while ignoring ecosystem health, indigenous rights, permanence, and additionality — the metrics that matter for climate integrity. The ritual persists because it provides governments and corporations with a mechanism to claim climate action while avoiding binding emissions reductions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is extreme because the structural relationship differs radically between actors. Indigenous stewards experience maximal extraction (snare). Financial intermediaries experience profitable coordination (rope). Commodity producers experience mixed coordination-extraction (tangled rope). Standards bodies experience a solvable temporary problem (scaffold). The accounting system experiences itself as degraded but inertially persistent (piton). Climate integrity experiences entrapment in a false solution (snare). These are not measurement ambiguities or observer-dependent framings — they reflect genuine structural asymmetries. The constraint simultaneously coordinates land-use decisions with financial incentives (a real coordination function) and extracts carbon value from powerless actors (a real extraction function). The mandatrophy is resolved by recognizing that this constraint is genuinely tangled rope from the beneficiary's perspective and genuinely snare from the victim's perspective. The tension is not resolvable by picking one type — it requires acknowledging the perspectival plurality.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain maps structural position to experienced extractiveness. Indigenous stewards (trapped, victim) derive d ≈ 0.95, producing f(d) ≈ 1.42 — maximum experienced extraction. Financial intermediaries (arbitrage, beneficiary) derive d ≈ 0.08, producing f(d) ≈ -0.12 — they experience negative extraction (profitable surplus). Commodity producers (mobile, mixed) derive d ≈ 0.50, producing f(d) ≈ 0.65 — moderate experienced extraction. Community actors (constrained, victim) derive d ≈ 0.85, producing f(d) ≈ 1.15 — high experienced extraction. The analytical observer's snare classification treats climate goals as trapped victims (d ≈ 0.90), reflecting that the mitigation system is locked into a false solution. These derived values track the real structural power asymmetries in the constraint: value flows toward financial intermediaries and commodity producers; costs flow toward indigenous stewards and climate integrity.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE WITH SNARE COMPONENTS: This constraint resolves the mandatrophy by demonstrating that tangled rope and snare are not contradictory — they describe the same constraint from different structural positions. From the beneficiary's perspective (financial intermediaries, commodity producers), the constraint is tangled rope: it coordinates land-use decisions with financial incentives (coordination function) while extracting carbon value (asymmetric extraction). From the victim's perspective (indigenous stewards, climate integrity), it is snare: pure extraction with no coordination benefit and no exit. The constraint is both types simultaneously because it is a hybrid mechanism with asymmetric structural positions. The mandatrophy is resolved by accepting that the classification differs by perspective, not by revising the constraint metrics. The theater ratio rising from 0.52 to 0.68 is diagnostic: the constraint's coordination function (land-use incentives) is becoming increasingly decoupled from its stated outcome (climate mitigation), indicating that the tangled rope is shifting toward piton (degraded institutional ritual). If this trajectory continues and standards enforcement tightens, the constraint may bifurcate: genuine coordination mechanisms (higher standards, FPIC, permanence verification) emerge as scaffold replacements, while the performative accounting system degrades into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    additionality_verification_gap,
    'Can carbon credit schemes reliably verify that carbon sequestration is additional (would not have occurred without the credit incentive)?',
    'Counterfactual land-use analysis: compare reforestation rates and intensities in carbon credit vs non-credit regions with similar initial conditions. Temporal analysis of land-use change timing relative to credit scheme implementation.',
    'If additionality is < 40% verified: most carbon credits represent displaced rather than mitigated carbon, making the constraint a snare for climate integrity. If > 70% verified: constraint is genuine coordination with non-trivial extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(additionality_verification_gap, empirical, 'Fraction of carbon credits representing genuinely additional sequestration').

omega_variable(
    permanence_and_reversal_risk,
    'How often do nature-based carbon sinks reverse (due to fire, drought, land-use change) within timescales relevant to climate goals, and are these reversals factored into credit issuance?',
    'Satellite monitoring of carbon-sink permanence; longitudinal tracking of project sites 10-30 years post-credit issuance. Comparison of actual reversal rates to permanence assumptions in credit accounting.',
    'If reversal rate > 30% within 30-year horizon and credits are not discounted: effectively all credits are illusory. If < 10%: permanence assumption holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(permanence_and_reversal_risk, empirical, 'Permanence of nature-based carbon sequestration and reversal risk').

omega_variable(
    ecosystem_equivalence_assumption,
    'Is a carbon-credit-issued monoculture forest ecologically equivalent to the biodiverse ecosystem it displaced, and should they receive equivalent climate credit?',
    'Biodiversity monitoring (species richness, functional diversity); carbon sequestration comparison; water cycle and soil health assessment; indigenous-defined ecosystem function metrics.',
    'If monoculture forests store equivalent carbon but <20% equivalent biodiversity: the accounting system is extracting ecosystem value while crediting carbon, making the constraint snare-like. If high biodiversity and carbon equivalence: constraint is genuinely coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_equivalence_assumption, conceptual, 'Whether credit-eligible ecosystems are functionally equivalent to displaced ecosystems').

omega_variable(
    indigenous_consent_enforcement,
    'What fraction of nature-based carbon projects have genuine free, prior, and informed consent (FPIC) from indigenous stewards, and is consent withdrawal enforced?',
    'Third-party audit of FPIC processes; legal enforcement tracking; documented cases where indigenous groups initiated project cessation and result (credit cancellation vs continued issuance).',
    'If FPIC is <50% genuine and withdrawal is not honored: constraint is snare for indigenous stewards. If >80% genuine with enforced withdrawal: constraint approaches tangled rope for this actor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_consent_enforcement, empirical, 'Authenticity and enforceability of indigenous consent in carbon projects').

omega_variable(
    leakage_and_displacement_boundaries,
    'Does carbon credit issuance for conservation in one location cause displacement of extractive activity to unmonitored locations, creating zero-sum or negative mitigation?',
    'Spatial econometric analysis of land-use change patterns; tracking of commodity production shifts to non-credit regions; global commodity flow analysis for agricultural and timber products.',
    'If leakage rate > 50%: carbon credits are reducing cumulative emissions by <50% or potentially zero. If < 20%: additionality holds at acceptable level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leakage_and_displacement_boundaries, empirical, 'Rate of emissions leakage and activity displacement to unmonitored regions').

omega_variable(
    baseline_setting_as_extraction_mechanism,
    'Are carbon credit baselines (the counterfactual ''business as usual'' scenario) systematically set at higher emission intensities than actual baseline practices, enabling higher credit issuance and extraction?',
    'Baseline audit: compare declared baselines to historical land-use and emissions intensity data for same regions. Statistical analysis of baseline inflation over time.',
    'If baselines are systematically inflated by >20%: the constraint is snare-like (extraction via inflated baselines). If baseline accuracy is <5% error: accounting holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_setting_as_extraction_mechanism, empirical, 'Baseline inflation as an extraction mechanism in carbon accounting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nature_based_carbon_sink_displacement, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nbcsd_tr_t0, nature_based_carbon_sink_displacement, theater_ratio, 0, 0.52).
narrative_ontology:measurement(nbcsd_tr_t7, nature_based_carbon_sink_displacement, theater_ratio, 7, 0.62).
narrative_ontology:measurement(nbcsd_tr_t14, nature_based_carbon_sink_displacement, theater_ratio, 14, 0.68).
narrative_ontology:measurement(nbcsd_tr_t21, nature_based_carbon_sink_displacement, theater_ratio, 21, 0.71).

% Extraction over time
narrative_ontology:measurement(nbcsd_be_t0, nature_based_carbon_sink_displacement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nbcsd_be_t7, nature_based_carbon_sink_displacement, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(nbcsd_be_t14, nature_based_carbon_sink_displacement, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(nbcsd_be_t21, nature_based_carbon_sink_displacement, base_extractiveness, 21, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nature_based_carbon_sink_displacement, resource_allocation).
narrative_ontology:affects_constraint(nature_based_carbon_sink_displacement, carbon_credit_accounting_games).
narrative_ontology:affects_constraint(nature_based_carbon_sink_displacement, indigenous_land_dispossession).
narrative_ontology:affects_constraint(nature_based_carbon_sink_displacement, baseline_inflation_mechanism).
narrative_ontology:affects_constraint(nature_based_carbon_sink_displacement, emissions_leakage_and_substitution).

% DUAL FORMULATION NOTE:
% Nature-based carbon sink displacement is a constraint family linking multiple structurally distinct phenomena: (1) the coordination of land-use intensity with financial incentives (genuine coordination problem); (2) the extraction of ecosystem value via credit financialization; (3) the accounting theater that measures carbon while ignoring permanence, additionality, and indigenous rights. Each family member has distinct ε and classification depending on the observable. Decomposition enables diagnostic clarity: the constraint exists at the intersection of these mechanisms, and only the presheaf over all observables reveals its true structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nature_based_carbon_sink_displacement, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
