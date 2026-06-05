% ============================================================================
% CONSTRAINT STORY: viral_emergence_covid19_exemplar
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_viral_emergence_covid19_exemplar, []).

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
 *   constraint_id: viral_emergence_covid19_exemplar
 *   human_readable: Societal Response to SARS-CoV-2 Emergence
 *   domain: social/political/health
 *
 * SUMMARY:
 *   The societal response to SARS-CoV-2 emergence represents a constraint
 *   exhibiting extraction, coordination, and governance theater across
 *   multiple populations and time horizons. The nominal objective — limiting
 *   pathogen transmission and preventing healthcare collapse — is genuine
 *   coordination work. However, the implementation created asymmetric costs
 *   and benefits: emergency powers concentrated in state/institutional
 *   actors; pharmaceutical manufacturers gained monopoly pricing latitude and
 *   liability shields; essential workers faced impossible tradeoffs between
 *   infection risk and economic survival; small businesses faced mandated
 *   closures while larger competitors consolidated market share. The
 *   constraint evolved over time from a legitimate crisis response (immediate
 *   coordination need) toward institutional inertia (emergency declarations
 *   persisting beyond their epidemiological rationale). The theater ratio
 *   increased as government protocols became performative displays
 *   disconnected from actual epidemiological conditions. This exemplar
 *   demonstrates how a single structural phenomenon — novel pathogen
 *   emergence — classifies differently depending on observer position:
 *   powerless workers experience pure extraction (snare); institutional
 *   beneficiaries experience coordination with embedded extraction (tangled
 *   rope); public health authorities experience coordination (rope);
 *   preparedness infrastructure experiences a temporary constraint with
 *   sunset logic (scaffold); and the civilizational view risks naturalizing
 *   what is partly a contingent governance choice (mountain false summit).
 *   The core mandatrophy question: Is this outbreak response coordination
 *   that necessarily requires some asymmetric extraction, or is it extraction
 *   legitimized through coordination framing?
 *
 * KEY AGENTS:
 *   - Frontline Essential Workers: Primary victim (powerless/trapped) — forced to expose themselves to infection to maintain economic survival, no exit options, no political power to negotiate terms
 *   - Low-Income Service Sector: Primary victim (moderate/constrained) — face job loss through closures, reduced hours, or health-driven unemployment; constrained exit options due to lack of remote work capacity
 *   - Small Business Owners: Secondary victim (powerful/constrained) — face mandated closures or capacity restrictions; cannot escape supply-chain disruptions; see large competitors consolidate market share
 *   - State Emergency Power Apparatus: Primary beneficiary (institutional/arbitrage) — expands surveillance, emergency declarations, and executive authority with legal cover; can exit emergency state at will through policy decision
 *   - Pharmaceutical Manufacturers: Secondary beneficiary (powerful/mobile) — access to government purchase guarantees, liability shields, patent protections, and monopoly pricing latitude; can arbitrage between markets
 *   - Digital Surveillance Infrastructure: Beneficiary (institutional/arbitrage) — contact tracing systems, vaccine passports, and health data collection expand institutional capacity permanently
 *   - Public Health Authority: Tertiary beneficiary (institutional/arbitrage) — gains resources, political salience, and expanded enforcement authority during emergency
 *   - Mental Health and Social Services: Mixed victim/participant (organized/constrained) — experiences both coordination (integrated crisis response) and extraction (reduced funding, staff redeployment, constrained capacity)
 *   - Analytical Observer: Neutral observer (analytical/analytical) — sees the constraint as both biological inevitability and institutional choice; must avoid naturalizing governance structures as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, 0.58).
domain_priors:suppression_score(viral_emergence_covid19_exemplar, 0.68).
domain_priors:theater_ratio(viral_emergence_covid19_exemplar, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, extractiveness, 0.58).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(viral_emergence_covid19_exemplar, tangled_rope).
narrative_ontology:human_readable(viral_emergence_covid19_exemplar, "Societal Response to SARS-CoV-2 Emergence").
narrative_ontology:topic_domain(viral_emergence_covid19_exemplar, "social/political/health").

domain_priors:requires_active_enforcement(viral_emergence_covid19_exemplar).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, state_emergency_powers).
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, digital_surveillance_infrastructure).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, low_income_workers).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, supply_chain_resilience).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, mental_health_populations).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, small_business_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE ESSENTIAL WORKER (SNARE) — Trapped between economic survival and infection risk. Lacks negotiating power, cannot work remotely, cannot exit labor market without destitution. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Pure extraction with no exit option.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (SNARE) — Faces mandated closures or capacity restrictions. Cannot relocate business, locked into lease obligations, constrained by supply chain disruption. Larger competitors benefit from consolidation. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.77.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE PHARMACEUTICAL MANUFACTURER (TANGLED ROPE) — Experiences genuine coordination function (vaccine development, supply-chain stabilization) AND asymmetric extraction (patent protection, liability shields, government purchase guarantees, monopoly pricing latitude). d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17. Low effective extraction because of mobile exit options, but structural extraction is real.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (ROPE) — Experiences the constraint as coordination problem: communicating risk, distributing resources, standardizing protocols. Benefits from emergency powers that expand agency. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary through institutional arbitrage.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MENTAL HEALTH AND SOCIAL SERVICES SECTOR (TANGLED ROPE) — Experiences both coordination (integrated crisis response, resource pooling) AND extraction (reduced funding during emergency, staff redeployment, increased demand but constrained capacity). d≈0.62, f(d)≈0.90, σ=1.2 → χ≈0.52. Moderate extraction within a coordination framework.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EMERGENCY GOVERNANCE THEATER (PITON) — Emergency declarations and pandemic protocols persist through institutional inertia long after evidence-base justifies them. theater_ratio=0.64 captures the gap between proclaimed epidemiological rationale and actual operational variance. Mask mandates, isolation protocols, and capacity restrictions become performative displays of governmental action. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PANDEMIC PREPAREDNESS INFRASTRUCTURE (SCAFFOLD) — Temporary emergency public health architecture (vaccination campaigns, supply chains, surveillance systems) with explicit sunset logic: as immunity spreads and variants emerge at lower severity, the constraint decays. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.35. Moderate extraction that declines as population immunity matures.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: BIOLOGICAL NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, viral emergence is an immutable feature of zoonotic ecology: novel respiratory pathogens will continue to emerge from animal reservoirs. No society can eliminate this constraint entirely. However, the structural data (ε=0.58, suppression=0.68) contradicts pure mountain classification. The engine will identify this as a false summit: the institutional choices (centralized governance, economic shutdowns, surveillance expansion) are not inherent to pathogen control.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_emergence_covid19_exemplar_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(viral_emergence_covid19_exemplar, TR),
    TR >= 0.70.

:- end_tests(viral_emergence_covid19_exemplar_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from essential workers and small businesses through emergency powers and supply disruption, but the extraction is not maximal because: (1) large portions of high-income populations received income support or remote work flexibility, (2) vaccination campaigns distributed doses widely rather than hoarding, (3) no single actor captured all extraction benefits. The 0.58 value reflects that the constraint enables significant asymmetric harm but does not achieve total extraction. Suppression (0.68): High. Substantial barriers to exit and alternatives include: government-mandated closures, labor-market collapse in non-essential sectors, international travel restrictions, supply-chain fragmentation, and social enforcement of isolation norms. Individuals and businesses cannot exit the constraint without accepting severe costs. Theater ratio (0.64): Moderate-high. The ratio captures several performative elements: (1) capacity restrictions that vary arbitrarily by business type, (2) mask mandates that persist after evidence-base weakens, (3) isolation protocols that continue in absence of active transmission, (4) daily case counts as ritual status reports rather than epidemiological drivers of policy. However, theater is not dominant (≥0.70) because genuine coordination work (vaccine distribution, hospital surge capacity) remains functionally important throughout the interval. The increasing theater ratio (0.35→0.64 over 12 months) reflects the progression from crisis response to ritualized governance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival variation across the eight perspectives. The frontline worker experiences pure extraction with no escape (snare); the institutional authority experiences coordination with embedded benefits (rope/tangled rope); the small business owner experiences extraction with constrained exit (snare); the pharmaceutical manufacturer experiences coordination with highly favorable terms (tangled rope); the mental health sector experiences mixed coordination and extraction (tangled rope); the governance theater perspective reveals institutional inertia (piton); the scaffold perspective sees a genuine sunset (pandemic immunity) that will decay the constraint; and the civilizational view risks seeing immutable biology (mountain false summit) when the extraction mechanisms are institutional choices. No single perspective captures the full structure — the presheaf of all perspectives together reveals that the constraint is simultaneously coordination, extraction, theater, and temporary scaffolding.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline essential workers: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Small business: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction. State emergency powers: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary. Pharmaceutical manufacturers: Beneficiary + mobile but constrained → d≈0.35, f(d)≈0.30. Low effective extraction despite absolute benefits, because they retain mobile exit options. Mental health sector: Victim + constrained but organized → d≈0.62, f(d)≈0.90. Moderate extraction within coordination. Digital surveillance: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary through infrastructure expansion. Public health authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary through authority expansion. Analytical observer: d≈0.72, f(d)≈1.15. Moderate-high effective extraction because observation position is constrained by dependence on institutional data.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION MECHANISM: The mandatrophy resolves through disaggregation by population. The constraint is simultaneously genuine coordination (preventing healthcare collapse, distributing vaccines) AND genuine extraction (enforcing unequal sacrifice, enabling market consolidation). The false dichotomy is 'pure coordination' vs 'pure extraction.' The tangled_rope classification captures both: the beneficiaries (state, pharmaceutical, surveillance infrastructure) experience coordination that enables their own expansion; the victims (essential workers, small business) experience extraction; the scaffolding (pandemic preparedness) is temporary but the institutional expansion (digital surveillance, emergency powers) may persist as piton. The mandatrophy prevents conflation by requiring explicit beneficiary/victim/enforcement declaration. Without the beneficiaries declaration, the constraint appears to be pure emergency response (coordination). Without the victims declaration, it appears to be successful public health (rope). The tangled_rope classification forces the uncomfortable truth: the response is BOTH coordination AND extraction, with different populations experiencing different aspects of the same structure. The theater ratio (0.64, increasing over time) indicates growing performative content — protocols persist beyond their epidemiological rationale, suggesting institutional inertia (piton degradation risk) rather than pure temporary scaffolding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    asymmetric_harm_quantification,
    'What measurement framework captures the total harm distribution across job loss, supply disruption, mental health decline, and lives saved by intervention?',
    'Longitudinal cohort analysis tracking employment, income, mental health, and excess mortality across populations stratified by economic class and occupation',
    'If benefits concentrate in high-income populations and harms in low-income: classification shifts toward pure snare. If harms are genuinely distributed: classification remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_harm_quantification, empirical, 'Whether intervention benefits justify costs across populations').

omega_variable(
    enforcement_necessity_boundary,
    'At what threshold of population compliance does enforcement shift from coordination mechanism (rope) to coercive extraction (snare)?',
    'Cross-national comparative analysis of voluntary adoption rates vs mandated compliance; identification of threshold populations where voluntary uptake was sufficient',
    'If most populations exceeded voluntary threshold: constraint was coordination with optional enforcement (rope). If enforcement was essential: constraint was extraction (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_necessity_boundary, empirical, 'Whether enforcement was necessary or voluntariness would have sufficed').

omega_variable(
    institutional_inertia_decay,
    'Will emergency powers and protocols persist indefinitely (piton degradation) or sunset as intended (scaffold maturation)?',
    'Longitudinal tracking of emergency declarations, surveillance systems, and supply-chain controls through post-endemic phase; measurement of policy rollback rates',
    'If sunset occurs within 5 years: scaffold classification confirmed. If powers persist >10 years: piton classification confirmed — theater has become the primary function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_inertia_decay, preference, 'Whether emergency governance persists as institutional theater').

omega_variable(
    supply_chain_concentration_permanence,
    'Did the constraint permanently shift supply chains toward concentration in fewer manufacturing hubs, or did geographic diversification occur post-disruption?',
    'Network analysis of manufacturing hubs, supplier concentration ratios, and supply-chain complexity before, during, and 5+ years post-peak',
    'If concentration increased: constraint enabled permanent rent extraction by consolidated manufacturers. If diversification occurred: short-term snare became temporary scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_concentration_permanence, empirical, 'Whether supply disruption enabled permanent market consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_emergence_covid19_exemplar, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(covid_tr_t0, viral_emergence_covid19_exemplar, theater_ratio, 0, 0.35).
narrative_ontology:measurement(covid_tr_t6, viral_emergence_covid19_exemplar, theater_ratio, 6, 0.55).
narrative_ontology:measurement(covid_tr_t12, viral_emergence_covid19_exemplar, theater_ratio, 12, 0.64).

% Extraction over time
narrative_ontology:measurement(covid_be_t0, viral_emergence_covid19_exemplar, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(covid_be_t6, viral_emergence_covid19_exemplar, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(covid_be_t12, viral_emergence_covid19_exemplar, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_emergence_covid19_exemplar, enforcement_mechanism).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, vaccine_distribution_equity).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, supply_chain_concentration_covid).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, digital_surveillance_permanence).
narrative_ontology:affects_constraint(viral_emergence_covid19_exemplar, emergency_power_persistence).

% DUAL FORMULATION NOTE:
% The viral emergence constraint decomposes into downstream constraints: vaccine equity (how coordination benefits distribute), supply concentration (how market consolidation persists), surveillance permanence (whether temporary systems become piton), and emergency power persistence (whether emergency governance outlives the emergency). Each downstream constraint has its own ε reflecting the specific extraction or coordination mechanism. The parent constraint (viral_emergence_covid19_exemplar) operates at the level of societal response; the children operate at the level of specific governance and economic mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(viral_emergence_covid19_exemplar, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
