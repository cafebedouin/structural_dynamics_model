% ============================================================================
% CONSTRAINT STORY: labor_export_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_export_dependency, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: labor_export_dependency
 *   human_readable: Labor Export Dependency Constraint
 *   domain: economic/labor/migration
 *
 * SUMMARY:
 *   Labor export dependency is a structural constraint operating across
 *   source and destination countries, binding migrant workers, source country
 *   economies, destination country employers, and international labor
 *   standards regimes into a coordinated but asymmetric extraction system.
 *   The constraint exhibits genuine coordination functions — it solves real
 *   labor market mismatches and enables economic sectors in destination
 *   countries while providing foreign exchange and employment relief in
 *   source countries — but layers extractive mechanisms onto these
 *   coordination functions: wage suppression, debt bondage, visa sponsorship,
 *   suppression of alternatives, and brain drain. The constraint is sustained
 *   by institutional arrangements (visa sponsorship, contract binding,
 *   remittance systems) and structural factors (wage differentials,
 *   demographic imbalances, lack of domestic alternatives), making it
 *   simultaneously easier to reform (institutional barriers can be removed)
 *   and harder to escape (structural factors persist). The theater_ratio
 *   (0.48) reflects moderate performative content: international labor
 *   standards are widely proclaimed but unenforced; source countries
 *   celebrate emigration as a development strategy while experiencing
 *   dependency; destination countries frame migration as economic necessity
 *   while suppressing wages through migrant access.
 *
 * KEY AGENTS:
 *   - Migrant Workers: Primary victims (powerless/trapped) — experience maximum extraction through wage suppression, debt bondage, visa sponsorship, unsafe conditions, and forced family separation
 *   - Source Country Economies: Secondary victims (powerless/trapped) — trapped by remittance dependency and foreign exchange reliance; experience brain drain and reduced domestic investment incentives
 *   - Destination Country Employers: Primary beneficiaries (institutional/arbitrage) — capture wage arbitrage and flexible workforce; solve labor shortage without wage increases; can switch source countries if labor becomes unavailable
 *   - Destination Country Labor Markets: Mixed agent (powerful/constrained) — benefit from labor supply filling sectors (care, agriculture, construction) but experience wage suppression and reduced bargaining power; cannot easily restructure to use only domestic labor
 *   - Source Country Governments: Mixed institutional actor (organized/constrained) — benefit from foreign exchange and unemployment reduction; experience extraction through brain drain and loss of tax base; constrained from blocking emigration without political instability
 *   - International Labor Standards Regime: Institutional observer (institutional/constrained) — maintains symbolic commitment to worker protection while the actual constraint persists unimpeded; trapped in performative rather than functional enforcement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_export_dependency, 0.58).
domain_priors:suppression_score(labor_export_dependency, 0.72).
domain_priors:theater_ratio(labor_export_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_export_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_export_dependency, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(labor_export_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_export_dependency, tangled_rope).
narrative_ontology:human_readable(labor_export_dependency, "Labor Export Dependency Constraint").
narrative_ontology:topic_domain(labor_export_dependency, "economic/labor/migration").

domain_priors:requires_active_enforcement(labor_export_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_export_dependency, destination_countries).
narrative_ontology:constraint_beneficiary(labor_export_dependency, labor_importing_employers).
narrative_ontology:constraint_beneficiary(labor_export_dependency, remittance_financial_intermediaries).
narrative_ontology:constraint_victim(labor_export_dependency, source_country_workers).
narrative_ontology:constraint_victim(labor_export_dependency, source_country_economies).
narrative_ontology:constraint_victim(labor_export_dependency, domestic_labor_markets_destination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRANT WORKER (SNARE) — Trapped by debt bondage, visa sponsorship tied to single employer, lack of local labor alternatives, and family financial dependence on remittances. Maximum experienced extraction through wage suppression, unsafe conditions, forced savings schemes, and inability to change employers or return home without catastrophic family impact. No exit option.
constraint_indexing:constraint_classification(labor_export_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOURCE COUNTRY DOMESTIC ECONOMY (SNARE) — Trapped by remittance dependency: government revenues come from taxes on remittances, foreign exchange reserves rely on migrant inflows, domestic employment has atrophied due to brain drain and wage-pull dynamics. Cannot exit without immediate fiscal collapse. Experiences extraction through loss of productive labor, reduced domestic investment, and institutional weakening as decision-makers emigrate.
constraint_indexing:constraint_classification(labor_export_dependency, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DESTINATION COUNTRY EMPLOYER (ROPE) — Benefits from coordinated labor supply at below-market wages, flexible workforce with limited legal recourse, reduced training and retention costs. Experiences the constraint as pure coordination mechanism — solving the labor shortage problem without requiring wage increases or working condition improvements. Arbitrage exit: can shift recruitment to other source countries if labor becomes unavailable.
constraint_indexing:constraint_classification(labor_export_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DESTINATION COUNTRY LABOR MARKET (TANGLED ROPE) — Experiences mixed coordination and extraction. Genuine coordination benefit: migrant labor fills genuine labor shortages and enables economic sectors (agriculture, healthcare, construction) to function. But also experiences wage suppression across lower-skill jobs, reduced bargaining power for domestic workers, and skill mismatch in STEM sectors. Constrained exit: cannot easily shift to domestic labor without sector restructuring; cannot easily block migration without political blowback and labor shortage crises.
constraint_indexing:constraint_classification(labor_export_dependency, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOURCE COUNTRY GOVERNMENT (TANGLED ROPE) — Genuine coordination function: labor export generates foreign exchange, reduces domestic unemployment pressure, and brings in tax revenue through remittance channels and emigrant tax obligations. But also experiences active extraction: brain drain of skilled workers, loss of tax base from emigrants, political pressure to enable emigration as solution to joblessness rather than investing in domestic employment. Constrained exit: cannot block emigration without political instability; cannot reduce emigration incentives without addressing root economic dysfunction.
constraint_indexing:constraint_classification(labor_export_dependency, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LABOR STANDARDS REGIME (PITON) — The International Labour Organization conventions on migrant worker protections (C97, C143, C189) exist largely as theater. Ratification without enforcement, reporting without verification, and sanctionless non-compliance characterize the regime. The constraint persists through institutional inertia — maintaining the symbolic framework of worker protection while the actual extraction mechanism operates unimpeded. High theater_ratio reflects the gap between proclaimed standards and structural reality.
constraint_indexing:constraint_classification(labor_export_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the perspective that labor migration is an immutable economic law driven by wage differentials and demographic imbalances across regions, the constraint appears inevitable and unchangeable. Wage gaps between regions will always drive migration; aging populations will always require younger workers; development disparities will always create incentives to relocate. This perspective risks naturalizing what is actually a contingent institutional arrangement (visa sponsorship systems, debt bondage, remittance taxation, brain-drain acceptance) as inherent economic necessity.
constraint_indexing:constraint_classification(labor_export_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_export_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_export_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_export_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_export_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_export_dependency, TR),
    TR >= 0.70.

:- end_tests(labor_export_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly through wage suppression, asymmetric access to legal protections, and debt mechanisms, but not as severely as pure labor coercion (slavery, 0.80+) because workers retain some choice (exit is possible at high cost, not prohibited). The extractiveness has increased over the measurement interval (0.38 → 0.58) as visa sponsorship systems have tightened, debt bondage has formalized (contract binding), and destination country wage stagnation has widened wage differentials. Suppression (0.72): High. Structural barriers (wage differentials, lack of domestic alternatives) combine with institutional barriers (visa sponsorship, contract binding, debt, travel document confiscation) to severely limit worker alternatives. Workers cannot easily exit the constraint without catastrophic family impact (remittance dependency) or legal complications (visa status). Theater ratio (0.48): Moderate. International labor standards (ILO conventions) are widely announced but minimally enforced; source countries celebrate emigration as development strategy while experiencing dependency; destination countries frame migration as economic necessity while suppressing wages. The moderate theater reflects that some accountability mechanisms exist (ILO reporting, some national labor inspections) but enforcement is weak.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications depending on the observer's structural position. Migrant workers see a snare (trapped, high extraction, no escape). Source country economies see a snare (trapped by remittance dependency, brain drain, no exit without fiscal crisis). Destination employers see a rope (pure coordination solving labor shortage without institutional cost). Destination labor markets see tangled_rope (genuine labor shortage solved, but wages suppressed). Source governments see tangled_rope (foreign exchange benefit, employment relief, but brain drain and reduced investment). International standards regime sees piton (symbolically committed but functionally degraded). The analytical observer risks seeing a mountain (wage differentials and demographic imbalances make the constraint appear immutable), but structural analysis reveals this as false naturalization — the extraction mechanisms are contingent institutional designs (visa sponsorship, debt bondage, suppression of domestic alternatives).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations and exit options create directional asymmetry. Destination employers benefit without constraint (arbitrage exit); source workers suffer with no exit (trapped). This asymmetry is crystallized in the visa sponsorship system: the worker's legal status is tied to a single employer, making exit legally impossible without becoming undocumented. The institutional embedding of this asymmetry (contract binding, debt, travel document confiscation) is what distinguishes this from a pure wage-differential flow. If workers could freely exit and seek alternate employment, the constraint would weaken toward rope (pure coordination in a functioning labor market). The fact that wages remain suppressed despite stated labor shortages suggests that institutional barriers (suppression mechanisms) are doing work — preventing workers from exercising market power even when labor is scarce.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is present but not fully resolved. The constraint appears as tangled_rope (claiming both coordination and extraction), but the evidence for genuine coordination is mixed. Is the constraint genuinely coordinating labor supply for sectors that would otherwise malfunction, or is it simply enabling wage suppression? The answer differs by sector: healthcare and care work may have genuine coordination functions (domestic workers won't accept these wages/conditions at any price); agriculture and construction may be pure extraction (employers could pay higher wages and recruit domestically). The constraint family should decompose into separate stories per sector, each with its own ε and coordination assessment. Until this decomposition occurs, the mandatrophy remains: the claim that both coordination and extraction occur at the same structural level is asserted but not fully justified. Recommended resolution: write separate constraint stories for migrant workers in healthcare/care (likely rope or tangled_rope with genuine coordination function) vs construction/agriculture (likely snare with minimal genuine coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_structural_suppression,
    'Is the suppression primarily institutional (visa sponsorship, contract binding, legal barriers) or structural (wage differentials, lack of domestic alternatives, family dependency)?',
    'Comparative analysis of migrant mobility in systems with vs without visa sponsorship; measurement of exit costs when institutional barriers are removed but structural barriers remain',
    'If institutional: removal of visa sponsorship and debt bondage would substantially increase worker agency (move classification from snare toward constrained). If structural: workers remain trapped even without legal barriers due to economic necessity and family obligations (suppression persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_vs_structural_suppression, empirical, 'Whether suppression is institutional or structural').

omega_variable(
    remittance_efficacy_paradox,
    'Do remittances represent genuine economic benefit to source countries or do they substitute for domestic investment and create dependency traps?',
    'Longitudinal economic analysis comparing remittance-dependent vs export-manufacturing-dependent economies; correlation between remittance concentration and domestic capital formation; measurement of multiplier effects',
    'If beneficial: source country experiences rope perspective (genuine coordination). If trap: source country experiences snare perspective (extraction). Current evidence suggests bifurcation by sector and timing — short-term benefit, long-term dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_efficacy_paradox, empirical, 'Whether remittances create economic benefit or dependency').

omega_variable(
    demographic_necessity_claim,
    'Is destination country labor shortage genuinely structural (aging population, fertility decline) or are wage/working condition improvements sufficient to enable domestic recruitment?',
    'Experimental policy analysis: regional wage increases in care/agricultural sectors and measurement of domestic labor supply elasticity; comparison of countries that restricted migration vs those that expanded it',
    'If structural necessity: mountain classification is partially justified (constraints are immutable). If wage-elastic: the constraint is contingent institutional design (destination country could increase wages but chooses extraction instead).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_necessity_claim, empirical, 'Whether labor shortage is demographically necessary or wage-responsive').

omega_variable(
    circular_vs_permanent_migration_dynamics,
    'Does the constraint enable circular migration (temporary, reversible) or does it lock workers into permanent emigration trajectories?',
    'Longitudinal tracking of worker migration patterns; measurement of return rates, settlement rates, and intergenerational lock-in; comparison across source countries with different return-migration policies',
    'If circular: workers maintain home-country ties and can exit (constrained rather than trapped). If permanent: workers become locked out of source country labor markets and cannot return (trapped). Current evidence shows extreme variation by sector and country policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(circular_vs_permanent_migration_dynamics, empirical, 'Whether migration is circular or permanent lock-in').

omega_variable(
    destination_labor_market_identity_lock,
    'Have destination country workers internalized the narrative that migrant labor is necessary and natural, making them identity-locked rather than constrained advocates for immigration restrictions?',
    'Survey data on stated attitudes about wage bargaining; measurement of correlation between labor market position and immigration stance; analysis of whether opposition persists when actual wage/employment data is presented',
    'If identity-locked: destination country working class opposes their own economic interest not from rational constraint but from identity commitment to nationalist or meritocratic frames. This makes political resistance to the constraint asymmetric and difficult to organize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(destination_labor_market_identity_lock, conceptual, 'Whether destination workers are identity-locked to labor export framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_export_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labexp_tr_t0, labor_export_dependency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(labexp_tr_t5, labor_export_dependency, theater_ratio, 5, 0.42).
narrative_ontology:measurement(labexp_tr_t10, labor_export_dependency, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(labexp_be_t0, labor_export_dependency, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(labexp_be_t5, labor_export_dependency, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(labexp_be_t10, labor_export_dependency, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_export_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(labor_export_dependency, 0.18).
narrative_ontology:affects_constraint(labor_export_dependency, wage_suppression_mechanisms).
narrative_ontology:affects_constraint(labor_export_dependency, remittance_financial_extraction).
narrative_ontology:affects_constraint(labor_export_dependency, brain_drain_structural_dependency).
narrative_ontology:affects_constraint(labor_export_dependency, visa_sponsorship_debt_bondage).

% DUAL FORMULATION NOTE:
% Labor export dependency decomposes into four structurally distinct constraints: (1) wage suppression mechanisms in destination labor markets (economic extraction, direct impact on wages); (2) remittance financial extraction (financial services extracting from worker savings); (3) brain drain dependency (institutional extraction from source countries); (4) visa sponsorship debt bondage (legal/contractual extraction from individual workers). Each constraint has a different ε, different beneficiary/victim declarations, and different measurement trajectory. The labor_export_dependency story is the aggregate or family story; the four decomposed stories are the atomic constraints. This story should be understood as the coordinating frame for the family rather than the final analytical unit.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_export_dependency, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
