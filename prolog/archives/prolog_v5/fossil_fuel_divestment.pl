% ============================================================================
% CONSTRAINT STORY: fossil_fuel_divestment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fossil_fuel_divestment, []).

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
 *   constraint_id: fossil_fuel_divestment
 *   human_readable: Fossil Fuel Divestment Movement Constraints
 *   domain: environmental_policy/economic_coordination
 *
 * SUMMARY:
 *   The fossil fuel divestment movement creates a structural constraint where
 *   climate coordination goals coexist with asymmetric extraction from
 *   stranded workers, pension beneficiaries, and developing economies. The
 *   constraint exhibits genuine coordination functions (renewable sector
 *   capital mobilization, institutional norm-setting, climate pressure
 *   amplification) alongside significant distributional costs concentrated on
 *   powerless agents. The movement demonstrates how moral urgency (carbon
 *   reduction) can justify extraction without exit support, creating Snare
 *   dynamics for trapped populations while Rope/Scaffold dynamics benefit
 *   organized climate actors. Theater ratio increases over the interval as
 *   institutional divestment becomes performative ritual: extensive public
 *   announcements coexist with persistent indirect fossil fuel exposure
 *   through supply chains, derivatives, and index fund architecture. The
 *   constraint is neither pure extraction nor pure coordination — it is
 *   Tangled Rope at the institutional level, with Snare dynamics for workers
 *   and pensioners who bear costs without exit capacity or compensation.
 *
 * KEY AGENTS:
 *   - Stranded Fossil Fuel Workers: Primary victims (powerless/trapped) — geographically and professionally locked into coal/oil regions; identity fused with industry; no arbitrage available
 *   - Pension Beneficiaries: Primary victims (powerless/trapped) — dependent on pension fund returns; face benefit cuts from portfolio reallocation; retirement security at risk
 *   - Developing Economies with Carbon Resources: Secondary victims (organized/constrained) — face revenue loss from capital flight; constrained exit through development finance negotiations
 *   - Renewable Energy Sector: Primary beneficiary (institutional/arbitrage) — direct recipient of redirected capital; experiences constraint as coordination mechanism
 *   - University Endowments and Institutional Investors: Mixed actor (institutional/constrained) — face dual pressure (fiduciary duty vs institutional legitimacy); experience mixed Tangled Rope dynamics
 *   - Climate Finance Coalition: Organized intermediary (organized/mobile) — builds exit ramps through transition finance, technology transfer, managed declining pathways
 *   - Divestment Ritual and Symbolic Capital: Institutional persistence (institutional/arbitrage) — performative mechanisms (announcements, ceremonies) generating reputational benefit decoupled from climate impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fossil_fuel_divestment, 0.58).
domain_priors:suppression_score(fossil_fuel_divestment, 0.62).
domain_priors:theater_ratio(fossil_fuel_divestment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fossil_fuel_divestment, extractiveness, 0.58).
narrative_ontology:constraint_metric(fossil_fuel_divestment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fossil_fuel_divestment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fossil_fuel_divestment, tangled_rope).
narrative_ontology:human_readable(fossil_fuel_divestment, "Fossil Fuel Divestment Movement Constraints").
narrative_ontology:topic_domain(fossil_fuel_divestment, "environmental_policy/economic_coordination").

domain_priors:requires_active_enforcement(fossil_fuel_divestment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fossil_fuel_divestment, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(fossil_fuel_divestment, climate_advocacy_organizations).
narrative_ontology:constraint_beneficiary(fossil_fuel_divestment, institutional_reputation_managers).
narrative_ontology:constraint_victim(fossil_fuel_divestment, fossil_fuel_workers).
narrative_ontology:constraint_victim(fossil_fuel_divestment, pension_beneficiaries_dependent_on_returns).
narrative_ontology:constraint_victim(fossil_fuel_divestment, developing_economies_with_carbon_resources).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRANDED WORKER (SNARE) — Geographically and professionally locked into coal/oil/gas regions with minimal alternative employment. Career capital and identity fused to industry. No arbitrage available; exit requires relocation, retraining, and loss of seniority. Maximum experienced extraction through career collapse.
constraint_indexing:constraint_classification(fossil_fuel_divestment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PENSION BENEFICIARY (SNARE) — Retirees depending on pension fund returns face benefit cuts or delayed distributions if divestment-driven portfolio reallocation produces lower returns during transition. No exit capacity — trapped between carbon reduction goals and retirement security. Identity often fused with sense of 'earned' pension stability.
constraint_indexing:constraint_classification(fossil_fuel_divestment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING ECONOMY (TANGLED ROPE) — Nations relying on coal/oil exports for government revenue and development financing face genuine dilemma. Divestment reduces demand and prices, cutting export revenue needed for healthcare, education, infrastructure. Yet genuine coordination function exists: transition finance, technology transfer, and managed declining pathways offer win-win. High suppression from capital market leverage but constrained exit — agents retain some negotiating power.
constraint_indexing:constraint_classification(fossil_fuel_divestment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: RENEWABLE SECTOR (ROPE) — Direct beneficiary of divestment capital reallocation. Experiences constraint as pure coordination mechanism — capital flows toward renewables, enabling deployment at scale. Low experienced extraction; high arbitrage capability. Benefits from investor flight risk across multiple institutional contexts simultaneously.
constraint_indexing:constraint_classification(fossil_fuel_divestment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UNIVERSITY ENDOWMENT (TANGLED ROPE) — Faces dual constraints: fiduciary duty to maximize returns AND institutional pressure (students, faculty, public legitimacy) to divest. Cannot arbitrage fully (fiduciary lock-in). Constrained exit. Genuine coordination (institutional exemplarity drives norm shifts) coexists with extraction (reputation cost if funds underperform, career risk for fund managers who resist pressure).
constraint_indexing:constraint_classification(fossil_fuel_divestment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DIVESTMENT RITUAL (PITON) — Institutional divestment ceremonies (announcements, celebrations, reporting) have become substantially performative. Theater ratio driven by: (1) widespread continued indirect fossil fuel exposure through supply chains, derivatives, and index funds; (2) investor flight causing reallocation among existing investors rather than reducing capital supply to fossil fuel industry; (3) reputational benefit to institution exceeding climate impact of actual capital reallocation. Constraint persists through institutional inertia and theater maintenance despite degraded functional impact.
constraint_indexing:constraint_classification(fossil_fuel_divestment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: CLIMATE FINANCE COALITION (SCAFFOLD) — Organized actors (international development banks, green funds, climate funds) see divestment as temporary coordination mechanism during energy transition. Sunset logic: as renewable capacity reaches cost parity and deployment scale, fossil fuel capital starvation becomes moot — market forces (not institutional pressure) eliminate fossil fuel expansion. Coordinating transition finance toward developing economies softens the snare for stranded workers and resource-dependent nations. Mobile exit available through transition participation.
constraint_indexing:constraint_classification(fossil_fuel_divestment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From civilizational/universal scope, cumulative carbon emissions are path-dependent: every molecule emitted since industrialization stays in atmosphere for centuries. The constraint (capital lock-in to fossil fuels) reflects an immutable feature of decarbonization physics — historically accumulated capital stock in fossil infrastructure must be retired before it depreciates naturally. However, this naturalizes what is partly institutional: the carbon lock-in is structural (physical assets) but the *extraction during transition* is policy contingent.
constraint_indexing:constraint_classification(fossil_fuel_divestment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fossil_fuel_divestment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fossil_fuel_divestment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fossil_fuel_divestment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fossil_fuel_divestment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fossil_fuel_divestment, TR),
    TR >= 0.70.

:- end_tests(fossil_fuel_divestment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint creates asymmetric cost distribution during energy transition. Stranded workers face genuine career destruction; pensioners face benefit uncertainty; carbon-export economies face revenue collapse. These are real extraction costs, not coordination overhead. However, extractiveness is not extreme (0.75+) because: (1) renewable sector genuinely benefits from coordination; (2) global climate coordination does generate positive externalities; (3) some actors have constrained but non-zero exit options through reskilling programs, portfolio rebalancing. Suppression (0.62): Moderate-high. Significant barriers to exit and alternatives include: geographic immobility of fossil fuel infrastructure and workers; political economy of pension systems (beneficiaries cannot easily switch to alternative retirement income); coordination barriers to developing economy diversification; asymmetric information (climate urgency narrative suppresses visibility of transition costs). But suppression is not total (0.85+) because: labor organizing, pension fund activism, and developing economy coalition-building provide counter-pressure. Theater ratio (0.65): Moderate-high. Institutional divestment has become substantially performative. Measurable gap between institutional reporting (percent of portfolio divested from direct fossil holdings) and actual climate impact (indirect exposure persistence, supply chain lock-in, investor reallocation without capital reduction). Theater increases over interval as divestment moves from activist fringe to mainstream institutional practice; later adoption is driven by reputational conformity rather than independent climate assessment.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is unusually wide because beneficiary and victim populations are structurally separated by power level and geography. Renewable sector is concentrated, organized, and globally mobile; workers are dispersed, politically fragmented (nationalist/regional identities dominate class solidarity), and geographically trapped. This power asymmetry means beneficiary can orchestrate constraints that victim cannot escape — characteristic Snare signature. Yet institutional coordination (norm-setting, capital reallocation) is genuine, distinguishing this from pure extraction. The constraint demonstrates how coordination can be distributionally asymmetric: Rope for beneficiaries coexists with Snare for victims through differential exit capacity and power concentration.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status plus exit options. Stranded workers (trapped + victim) experience maximum d (~0.95) yielding highest f(d) and maximum experienced extraction chi. Renewable sector (arbitrage + beneficiary) experiences minimum d (~0.05) yielding negative f(d) and received benefits. Pension beneficiaries (trapped + victim identity-locked) experience high d (~0.92) — retirement identity fusion creates suppression beyond material barriers. Developing economies (constrained + victim but organized) experience moderate-high d (~0.65) — have negotiating power but asymmetric information and capital flows limit exit. Institutional endowments (constrained + both beneficiary and victim depending on time horizon) experience mid-range d (~0.50) — experience Tangled Rope through different time splits. Analytical observer (analytical + no beneficiary/victim relationship) experiences moderate d (~0.73) deriving from information asymmetry and measurement limitations.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mandatrophy through honest acknowledgment of mixed dynamics. The claimed type (Tangled Rope) reflects that both genuine coordination (renewable capital mobilization, climate norm diffusion) and asymmetric extraction (worker/pensioner cost concentration) are simultaneously present at different structural levels. Analytical observer's Mountain classification is revealed as false summit — naturalizing divestment-driven carbon lock-in as inherent rather than recognizing it as contingent policy choice. The Snare classification for stranded workers is not overruled by climate-good framing; rather, it is contextualized as extraction justified by coordination goals elsewhere. The Scaffold classification reflects real sunset possibilities (renewable cost parity + market displacement of fossil fuels) but requires explicit transition finance structures to prevent workers and pensioners from remaining in permanent Snare. Mandatrophy is resolved by distinguishing (1) what the constraint genuinely coordinates (capital reallocation to renewables), (2) what extraction costs it generates (worker/pensioner income loss), and (3) what redistribution would prevent perverse morphing into pure predation on powerless agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indirect_exposure_persistence,
    'Does divestment from direct fossil fuel holdings actually reduce capital availability to the industry, or does it primarily shuffle ownership among less ethically-constrained investors while leaving financing infrastructure intact?',
    'Empirical analysis of fossil fuel capital costs pre- and post-divestment campaigns. Direct measure: has cost of debt/equity financing for fossil fuel companies changed? Indirect measure: has capital investment in fossil fuel extraction changed after major divestment announcements?',
    'If actual reduction: divestment functions as coordination mechanism with real climate impact (Rope/Scaffold confirmed). If pure shuffle: divestment is primarily symbolic (Piton/Theater confirmed); extraction redirected toward workers and pensioners without impact on industry capital access.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indirect_exposure_persistence, empirical, 'Whether divestment reduces capital availability or shuffles ownership').

omega_variable(
    transition_finance_sufficiency,
    'Is the volume of capital redirected toward climate solutions sufficient to fund just transition for fossil fuel workers, pension obligations, and developing economy diversification, or does divestment create pressure without providing exit ramps?',
    'Cost analysis: (a) retraining and relocation costs for stranded workers in coal-dependent regions; (b) pension fund actuarial impacts from portfolio reallocation; (c) development finance requirements for transition in carbon-exporting nations. Compare to actual climate finance flows and divestment-driven capital reallocation.',
    'If sufficient: Tangled Rope classification holds — genuine coordination with cost-sharing. If insufficient: Snare classification dominates — extraction falls on powerless agents without exit support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_finance_sufficiency, empirical, 'Whether transition finance suffices for just transition').

omega_variable(
    renewable_cost_parity_timeline,
    'At what renewable energy cost point does market competition eliminate fossil fuel expansion regardless of divestment pressure? Does this create a definite sunset date for scaffold structures?',
    'Projection modeling: LCOE (levelized cost of energy) for renewables vs fossil alternatives; capital investment trends in absence of activist pressure; sensitivity analysis on carbon pricing and technology cost curves.',
    'If sunset is 10-15 years: scaffold perspective is realistic. If sunset is 30+ years: coordinate transition finance structures must persist much longer than scaffold design assumes, degrading into Tangled Rope/Snare for resource-dependent economies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_cost_parity_timeline, empirical, 'Timeline to market-driven fossil fuel elimination').

omega_variable(
    worker_political_capture_risk,
    'Does extraction-without-exit-support for fossil fuel workers create political backlash that strengthens fossil fuel industry coalition and delays climate policy overall? Does the snare create its own perpetuation?',
    'Political economy analysis: correlate divestment campaign intensity with fossil fuel lobbying expenditure; track political support for climate measures vs fossil fuel worker displacement in affected regions; measure union political alignment shifts post-divestment campaigns.',
    'If strong backlash: Snare classification indicates instability — extraction creates political pressure that defeats the coordination goal (Scaffold sunset becomes unreachable). If weak backlash: Snare is stable but morally problematic; coordination for others achieved through sacrifice of powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_political_capture_risk, empirical, 'Whether worker extraction creates political backlash defeating climate goals').

omega_variable(
    theater_measurement_fragility,
    'Is the high theater_ratio (0.65) driven by actual gap between institutional performance and climate impact, or by measurement methodology privileging easily-reported symbolic acts over hard-to-measure supply chain decarbonization?',
    'Disaggregate divestment impact into: (1) direct fossil fuel equity elimination; (2) derivative and indirect exposure persistence; (3) supply chain fossil fuel dependence; (4) investor reallocation patterns. Measure climate impact per institutional reporting vs climate impact by independent carbon accounting.',
    'If measurement artifact: constraint may be lower-theater (higher function) than assessed — Rope may be more accurate than Tangled Rope. If real gap: Piton/Theater dominates institutional experience despite coordinated climate framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_measurement_fragility, empirical, 'Whether high theater ratio reflects real impact gap or measurement bias').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fossil_fuel_divestment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffd_tr_t0, fossil_fuel_divestment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ffd_tr_t5, fossil_fuel_divestment, theater_ratio, 5, 0.58).
narrative_ontology:measurement(ffd_tr_t10, fossil_fuel_divestment, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(ffd_be_t0, fossil_fuel_divestment, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ffd_be_t5, fossil_fuel_divestment, base_extractiveness, 5, 0.49).
narrative_ontology:measurement(ffd_be_t10, fossil_fuel_divestment, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fossil_fuel_divestment, resource_allocation).
narrative_ontology:affects_constraint(fossil_fuel_divestment, stranded_fossil_fuel_assets).
narrative_ontology:affects_constraint(fossil_fuel_divestment, just_transition_financing).
narrative_ontology:affects_constraint(fossil_fuel_divestment, carbon_lock_in_infrastructure).
narrative_ontology:affects_constraint(fossil_fuel_divestment, pension_fund_decarbonization).

% DUAL FORMULATION NOTE:
% Fossil fuel divestment is an institutional coordination mechanism that generates multiple downstream constraints. The divestment flow (capital reallocation from fossil to renewable) is the primary story. Stranded asset dynamics, just transition finance architecture, and pension system decarbonization are distinct constraints with different ε values. The stranded asset constraint reflects that physical capital cannot be instantly retired; just transition reflects that worker/community exit capacity requires active support; pension constraint reflects long-term beneficiary income security during portfolio transition. All are linked: divestment-driven capital reallocation creates pressure on stranded assets; stranded worker extraction creates just transition demands; pension rebalancing creates pensioner extraction. The upstream divestment story influences all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fossil_fuel_divestment, powerless, 0.95).
constraint_indexing:directionality_override(fossil_fuel_divestment, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
