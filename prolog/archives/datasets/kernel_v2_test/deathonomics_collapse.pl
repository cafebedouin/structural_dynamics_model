% ============================================================================
% CONSTRAINT STORY: deathonomics_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deathonomics_collapse, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deathonomics_collapse
 *   human_readable: Deathonomics Collapse: Exhaustion of Volunteer Compensation Model
 *   domain: political_economy/regime_stability/military_conflict
 *
 * SUMMARY:
 *   The 'deathonomics' model emerged as a regime solution to the manpower
 *   exhaustion trap: recruit volunteers from economically depressed regions
 *   with promises of substantial compensation to families if killed in
 *   combat. For approximately two years (2024-2026), this model coordinated
 *   regime survival by converting economic resources into military manpower
 *   while avoiding politically costly conscription. The constraint is now
 *   collapsing as its fiscal foundation erodes: $198.8B annual military
 *   spending against depleting reserves (nearly exhausted April 2026), halved
 *   oil revenues (Q1 2026), collapsing business profits (-33.1% Jan-Feb
 *   2026), and a 2.5M worker labor deficit. The regime faces an acute
 *   trilemma: maintain compensation payments and exhaust reserves, default on
 *   families and trigger political backlash, or shift to conscription and
 *   face broader resistance. Deposit freeze preparations signal the regime's
 *   likely path: capture domestic savings to fund continued military
 *   operations, shifting extraction from volunteer families to the general
 *   population. The constraint exhibits the classic snare structure from the
 *   perspective of those trapped within it (volunteer families, general
 *   population) while appearing as functional coordination to the regime
 *   leadership. The analytical perspective sees a tangled rope: genuine
 *   coordination function (the model did solve the manpower problem) layered
 *   with severe extraction (fiscal unsustainability, population immiseration)
 *   that is now collapsing.
 *
 * KEY AGENTS:
 *   - Volunteer Families: Primary victims (powerless/trapped) — promised compensation increasingly uncertain; irreversible losses as payment mechanism collapses
 *   - General Population: Primary victims (powerless/constrained) — bears fiscal extraction through halved oil revenues, business profit collapse, labor shortage, and impending deposit freeze
 *   - Business Sector: Mixed victim-beneficiary (moderate/constrained) — coordination through state contracts and resource access; extraction through profit collapse, labor shortage, capital controls
 *   - Regime Leadership: Primary beneficiary (institutional/arbitrage) — the model coordinates regime survival; offshore assets and coercive capacity provide exit options
 *   - Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — captures military spending ($198.8B annually); benefits from sustained conflict
 *   - Security Apparatus: Primary beneficiary (institutional/arbitrage) — enforces capital controls, suppresses dissent, maintains regime stability
 *   - Compensation Bureaucracy: Institutional actor (institutional/constrained) — processes payments and claims; function atrophying as reserves deplete (piton perspective)
 *   - Reform Coalition (Hypothetical): Organized opposition (organized/constrained) — views collapse as transitional crisis with structural sunset
 *   - Analytical Observer: External perspective (analytical/analytical) — sees hybrid coordination-extraction structure collapsing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deathonomics_collapse, 0.78).
domain_priors:suppression_score(deathonomics_collapse, 0.82).
domain_priors:theater_ratio(deathonomics_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deathonomics_collapse, extractiveness, 0.78).
narrative_ontology:constraint_metric(deathonomics_collapse, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(deathonomics_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deathonomics_collapse, snare).
narrative_ontology:human_readable(deathonomics_collapse, "Deathonomics Collapse: Exhaustion of Volunteer Compensation Model").
narrative_ontology:topic_domain(deathonomics_collapse, "political_economy/regime_stability/military_conflict").

domain_priors:requires_active_enforcement(deathonomics_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deathonomics_collapse, regime_leadership).
narrative_ontology:constraint_beneficiary(deathonomics_collapse, military_industrial_complex).
narrative_ontology:constraint_beneficiary(deathonomics_collapse, security_apparatus).
narrative_ontology:constraint_victim(deathonomics_collapse, volunteer_families).
narrative_ontology:constraint_victim(deathonomics_collapse, general_population).
narrative_ontology:constraint_victim(deathonomics_collapse, regional_economies).
narrative_ontology:constraint_victim(deathonomics_collapse, business_sector).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VOLUNTEER FAMILIES (SNARE) — Trapped by economic necessity and sunk costs (family member already deployed or killed). Promised compensation increasingly uncertain as reserves deplete. Cannot exit the contract structure; face maximum extraction as the payment mechanism collapses beneath them while their losses are irreversible.
constraint_indexing:constraint_classification(deathonomics_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GENERAL POPULATION (SNARE) — Constrained by capital controls, deposit freeze preparations, and labor market collapse (2.5M worker deficit). Bears the fiscal extraction (halved oil revenues, -33.1% business profits) while regime maintains military spending. Exit options exist (emigration, capital flight) but at severe cost and under increasing restriction.
constraint_indexing:constraint_classification(deathonomics_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS SECTOR (TANGLED ROPE) — Experiences both coordination (state contracts, resource access, regulatory protection) and extraction (profit collapse -33.1%, labor shortage 2.5M, impending deposit freeze). Constrained by capital controls and regime dependency but retains some negotiating power through economic importance. Mixed beneficiary-victim status.
constraint_indexing:constraint_classification(deathonomics_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIME LEADERSHIP (ROPE) — Primary beneficiary. The deathonomics model coordinates regime survival: volunteers absorb casualties, compensation payments buy political stability, military spending sustains power. Arbitrage-level exit (offshore assets, alternative revenue streams, coercive capacity). Experiences the constraint as functional coordination despite its extractive impact on the population.
constraint_indexing:constraint_classification(deathonomics_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Hypothetical organized opposition viewing the deathonomics collapse as a transitional crisis with a structural sunset: the model is financially unsustainable (reserves depleting April 2026) and must either transform or collapse. Sees the constraint as temporary extraction that will force systemic change. Constrained by suppression but organized around the collapse timeline.
constraint_indexing:constraint_classification(deathonomics_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPENSATION BUREAUCRACY (PITON) — The administrative apparatus processing volunteer payments and family compensation. Function has atrophied (payments delayed, amounts reduced, eligibility tightened) but the bureaucratic ritual persists. Theater ratio high: forms are filed, claims are processed, but the economic substance has degraded. Maintained through institutional inertia as reserves deplete.
constraint_indexing:constraint_classification(deathonomics_collapse, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination function (the model did solve the regime's manpower problem for 2+ years) and severe extraction (fiscal unsustainability, population immiseration, irreversible human capital loss). The constraint coordinated regime survival while extracting from the population; now the coordination function is collapsing but the extraction continues. Tangled rope classification reflects the hybrid structure.
constraint_indexing:constraint_classification(deathonomics_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deathonomics_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deathonomics_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deathonomics_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deathonomics_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deathonomics_collapse, TR),
    TR >= 0.70.

:- end_tests(deathonomics_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The model extracts from volunteer families (irreversible human capital loss, uncertain compensation), general population (fiscal burden, labor shortage, capital controls), and business sector (profit collapse, resource diversion). The regime captures the extracted value through sustained military operations and political stability. Extraction has increased over the 24-month interval as the fiscal foundation eroded: initial extractiveness (0.45) reflected the model's genuine coordination function (volunteers received compensation, families were paid, recruitment succeeded); current extractiveness (0.78) reflects the collapse of that function while extraction continues (payments delayed/reduced, reserves depleting, deposit freeze imminent). Suppression (0.82): High. Capital controls prevent exit; deposit freeze preparations trap domestic savings; labor market collapse (2.5M deficit) limits economic alternatives; regime coercive capacity suppresses dissent. Suppression has intensified over the interval (0.55 → 0.82) as the regime tightened controls to prevent capital flight and maintain the model despite fiscal stress. Theater ratio (0.68): Moderate-high. The compensation bureaucracy maintains the ritual of processing claims and issuing payments, but the substance has degraded: payments are delayed, amounts are reduced, eligibility is tightened, and the fiscal backing is nearly exhausted. Theater has increased over the interval (0.35 → 0.68) as the gap between promised compensation and actual payment capacity widened. The regime maintains the performance to sustain volunteer recruitment and family acquiescence even as the economic foundation collapses.
 *
 * PERSPECTIVAL GAP:
 *   The deathonomics constraint demonstrates extreme perspectival divergence driven by structural position. Volunteer families and the general population experience a snare: trapped by economic necessity, sunk costs, and capital controls, bearing maximum extraction as the payment mechanism collapses. The business sector experiences a tangled rope: genuine coordination through state contracts and resource access, but severe extraction through profit collapse and labor shortage. The regime leadership experiences a rope: the model coordinates regime survival, and their arbitrage-level exit options (offshore assets, coercive capacity) insulate them from the extraction they impose on others. The hypothetical reform coalition sees a scaffold: the model's fiscal unsustainability creates a structural sunset forcing systemic change. The compensation bureaucracy sees a piton: the administrative ritual persists as the function atrophies. The analytical observer sees a tangled rope: the model did provide genuine coordination (solved the manpower problem for 2+ years) but layered severe extraction on top, and the coordination function is now collapsing while extraction continues. The perspectival gap is not a difference of opinion about the same experience — it reflects fundamentally different structural relationships to the constraint. Those trapped within it bear irreversible losses; those who designed it capture the benefits and retain exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Volunteer families are victims with trapped exit → high d → high effective extraction (maximum). General population are victims with constrained exit → high d → high effective extraction. Business sector are mixed beneficiary-victims with constrained exit → moderate d → moderate effective extraction (tangled rope). Regime leadership, military-industrial complex, and security apparatus are beneficiaries with arbitrage exit → low d → low or negative effective extraction (they experience the constraint as coordination). The compensation bureaucracy is neither clear beneficiary nor victim but has constrained exit → moderate d. The analytical observer has analytical exit → d derived from structural analysis rather than personal experience. The directionality derivation captures the structural asymmetry: extraction flows from those with no exit (families, population) toward those with arbitrage-level exit (regime, military-industrial complex). The business sector sits in the middle: they benefit from state contracts but bear the fiscal collapse. No directionality overrides are needed — the structural declarations produce the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The deathonomics constraint resolves the mandatrophy by demonstrating that the same structural phenomenon — a volunteer compensation model funded by depleting reserves — is simultaneously a snare (from the perspective of those trapped within it), a rope (from the perspective of the regime benefiting from it), a tangled rope (from the analytical perspective seeing both coordination and extraction), a scaffold (from the perspective of those viewing the collapse as transitional), and a piton (from the perspective of the bureaucracy maintaining the degraded ritual). The mandate (coordinate regime survival through volunteer recruitment) has not outlived its function from the regime's perspective — the model is still serving its purpose, just at unsustainable cost. But from the population's perspective, the mandate has become pure extraction: the promised compensation is collapsing, the fiscal burden is unbearable, and the human capital loss is irreversible. The mandatrophy is resolved not by choosing one classification but by recognizing that all classifications are valid from their respective structural positions. The constraint is a snare if you are trapped in it, a rope if you designed it, and a tangled rope if you can see both perspectives. The classification is indexical — it depends on where you sit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_depletion_timeline,
    'What is the actual timeline to reserve fund exhaustion, and what fiscal measures can extend it?',
    'Monthly reserve fund reporting; oil price trajectory; alternative revenue mobilization (asset sales, domestic borrowing, external credit); expenditure cuts outside military spending',
    'If reserves last >12 months: regime has time to restructure the model (reduce payments, shift to conscription, negotiate settlement). If reserves exhaust <6 months: acute crisis forces immediate collapse or dramatic policy shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_depletion_timeline, empirical, 'Timeline to reserve exhaustion and fiscal collapse').

omega_variable(
    compensation_default_threshold,
    'At what point does the regime default on compensation promises, and what are the political consequences?',
    'Payment delay patterns; reduction in compensation amounts; eligibility tightening; family protest activity; volunteer recruitment rates',
    'If default occurs before reserves fully deplete: signals regime prioritizing other expenditures over volunteer families, potentially triggering political backlash. If regime maintains payments until reserves gone: maximizes extraction from general population to sustain the model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compensation_default_threshold, empirical, 'Threshold and consequences of compensation default').

omega_variable(
    alternative_manpower_model,
    'Can the regime transition from volunteer compensation to conscription or other manpower models before fiscal collapse?',
    'Policy announcements; conscription legislation; mobilization orders; public response; effectiveness of alternative models in sustaining military operations',
    'If transition succeeds: deathonomics constraint is replaced by a different extraction mechanism (conscription snare). If transition fails: manpower exhaustion forces operational collapse or settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_manpower_model, empirical, 'Feasibility of transitioning to alternative manpower models').

omega_variable(
    deposit_freeze_trigger,
    'What conditions trigger the deposit freeze, and how does it redistribute extraction?',
    'Capital flight indicators; reserve levels; bank run risk; regime announcements; implementation of capital controls',
    'If freeze occurs: shifts extraction from volunteer families to general depositors; regime captures domestic savings to fund military spending. If freeze avoided: regime must find alternative revenue or cut spending.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deposit_freeze_trigger, empirical, 'Deposit freeze trigger conditions and extraction redistribution').

omega_variable(
    oil_revenue_recovery,
    'Can oil revenues recover from Q1 2026 halving, and what factors drive recovery?',
    'Global oil price trajectory; production capacity; sanctions impact; alternative export routes; demand from major importers',
    'If revenues recover to 75%+ of pre-halving levels: extends reserve depletion timeline, reduces fiscal pressure. If revenues remain halved or decline further: accelerates collapse of deathonomics model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oil_revenue_recovery, empirical, 'Oil revenue recovery potential and timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deathonomics_collapse, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(death_econ_theater_t0, deathonomics_collapse, theater_ratio, 0, 0.35).
narrative_ontology:measurement(death_econ_theater_t6, deathonomics_collapse, theater_ratio, 6, 0.42).
narrative_ontology:measurement(death_econ_theater_t12, deathonomics_collapse, theater_ratio, 12, 0.52).
narrative_ontology:measurement(death_econ_theater_t18, deathonomics_collapse, theater_ratio, 18, 0.61).
narrative_ontology:measurement(death_econ_theater_t24, deathonomics_collapse, theater_ratio, 24, 0.68).

% Extraction over time
narrative_ontology:measurement(death_econ_extract_t0, deathonomics_collapse, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(death_econ_extract_t6, deathonomics_collapse, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(death_econ_extract_t12, deathonomics_collapse, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(death_econ_extract_t18, deathonomics_collapse, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(death_econ_extract_t24, deathonomics_collapse, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(death_econ_suppress_t0, deathonomics_collapse, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(death_econ_suppress_t6, deathonomics_collapse, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(death_econ_suppress_t12, deathonomics_collapse, suppression_requirement, 12, 0.71).
narrative_ontology:measurement(death_econ_suppress_t18, deathonomics_collapse, suppression_requirement, 18, 0.78).
narrative_ontology:measurement(death_econ_suppress_t24, deathonomics_collapse, suppression_requirement, 24, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deathonomics_collapse, resource_allocation).

% DUAL FORMULATION NOTE:
% The deathonomics collapse is downstream of the manpower exhaustion trap (manpower_exhaustion_trap). The upstream constraint (manpower exhaustion) is a mountain from most perspectives: the regime faces an irreducible demographic and economic limit on available military manpower. The deathonomics model was the regime's constructed solution to that mountain — a resource allocation mechanism that converts economic reserves into military manpower through volunteer compensation. The collapse of deathonomics does not resolve the upstream manpower exhaustion; it reveals that the attempted solution was fiscally unsustainable. The two constraints have different ε values: manpower exhaustion has low ε (it is a genuine limit), while deathonomics collapse has high ε (it is an extractive mechanism layered on top of the limit).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
