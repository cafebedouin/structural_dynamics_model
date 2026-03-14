% ============================================================================
% CONSTRAINT STORY: household_registry_system
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_household_registry_system, []).

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
 *   constraint_id: household_registry_system
 *   human_readable: Household Registry System (Hukou) as Extractive Constraint on Mobility and Social Access
 *   domain: political_economy/social_stratification
 *
 * SUMMARY:
 *   The household registry (hukou) system originated in 1953 as a population
 *   tracking mechanism to coordinate rural land tenure and urban resource
 *   allocation in the early PRC. It evolved into the primary institutional
 *   mechanism for restricting urban social access (education, healthcare,
 *   pensions, housing assistance) to migrants without local registration. The
 *   constraint operates across multiple levels: rural-to-urban migration is
 *   legally permitted, but social access is not. Enforcement occurs through
 *   police authority (detention of unregistered migrants), employer
 *   enforcement (workplace inspections), and housing discrimination
 *   (landlords verify hukou status). Over 300 million 'floating population'
 *   (migrants with rural hukou working in cities) live under this constraint.
 *   The system demonstrates all six DR types from different structural
 *   positions: snare from the powerless rural migrant's perspective (no exit,
 *   maximum extraction), tangled rope from the moderate migrant family's view
 *   (some coordination benefits mixed with extraction), rope from employers'
 *   perspective (pure coordination benefit), tangled rope from urban
 *   governments (genuine coordination mixed with enforcement costs), scaffold
 *   from reform advocates (temporary problem with fiscal solutions), piton
 *   from rural governments (degraded institution maintained by inertia), and
 *   mountain from the naturalizing analytical view (risking false summit).
 *   Base extractiveness has increased from 0.42 to 0.58 over the 20-year
 *   interval as labor market tightening and migration acceleration have made
 *   the discrimination more visible and enforcement more costly, while
 *   alternatives (portable insurance, school decoupling pilots) have emerged
 *   that reduce the apparent necessity of hukou restrictions.
 *
 * KEY AGENTS:
 *   - Rural Migrant Workers: Primary victims (powerless/trapped) — cannot legally access urban education, healthcare, housing assistance, pensions; face police enforcement and workplace discrimination; bear maximum extraction cost
 *   - Floating Population Families: Secondary victims (moderate/constrained) — face barriers to children's education and family healthcare; some exit capacity through informal networks; experience mixed coordination and extraction
 *   - Urban Employers: Primary beneficiaries (institutional/arbitrage) — benefit from labor control and cost avoidance (migrants have limited claim on employer-provided benefits); high exit capacity
 *   - Urban Local Governments: Secondary beneficiaries (institutional/constrained) — coordinate service provision and limit welfare costs; require active enforcement; constrained exit due to fiscal coupling
 *   - Reform Coalition: Organized agents (organized/mobile) — NGOs, policy reformers, younger urban residents; drive hukou liberalization in second-tier cities; see sunset path through fiscal decoupling
 *   - Rural Local Governments: Institutional observers (institutional/arbitrage) — maintain registration through inertia; theater-dominant position reflects degradation of original function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(household_registry_system, 0.58).
domain_priors:suppression_score(household_registry_system, 0.72).
domain_priors:theater_ratio(household_registry_system, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(household_registry_system, extractiveness, 0.58).
narrative_ontology:constraint_metric(household_registry_system, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(household_registry_system, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(household_registry_system, snare).
narrative_ontology:human_readable(household_registry_system, "Household Registry System (Hukou) as Extractive Constraint on Mobility and Social Access").
narrative_ontology:topic_domain(household_registry_system, "political_economy/social_stratification").

domain_priors:requires_active_enforcement(household_registry_system).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(household_registry_system, urban_local_governments).
narrative_ontology:constraint_beneficiary(household_registry_system, established_urban_residents).
narrative_ontology:constraint_victim(household_registry_system, rural_migrants).
narrative_ontology:constraint_victim(household_registry_system, floating_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL MIGRANT WORKER (SNARE) — Trapped by hukou status in origin county. Cannot legally access urban education, healthcare, housing assistance, pension benefits, or childcare services. Cannot change hukou classification without extraordinary bureaucratic process (typically 5-10+ years, significant fees, proof of local employment/property ownership). Faces active enforcement through police checks, workplace raids, and housing discrimination. Bears full extraction cost with zero coordination benefit. Maximal experience of constraint as coercive mechanism.
constraint_indexing:constraint_classification(household_registry_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FLOATING POPULATION FAMILY (TANGLED ROPE) — Migrant family with some coordination function: access to informal employment networks, shared housing with co-ethnics, mutual aid within diaspora community. But also subject to extraction: children cannot attend urban public schools (forcing private tutoring costs or separation from family), healthcare access restricted to emergency care, housing vulnerable to arbitrary evictions. Suppression significant but not absolute — some exit capacity through internal networks and informal economy. Mixed experience reflects both coordination benefits (diaspora cohesion) and asymmetric extraction (benefits accrue to urban employers and local governments, costs borne by migrants).
constraint_indexing:constraint_classification(household_registry_system, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: URBAN EMPLOYER (ROPE) — Coordinates workforce allocation through hukou system: legal framework guarantees that migrant workers have limited claim on city services (avoiding employer cost-shifting for healthcare, pensions, childcare). Experiences constraint as pure coordination benefit with minimal enforcement cost. Can arbitrage between migrant and urban-registered labor pools. High exit capacity — can lobby for hukou reform if labor shortage demands it.
constraint_indexing:constraint_classification(household_registry_system, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: URBAN LOCAL GOVERNMENT (TANGLED ROPE) — Coordinates urban service provision through hukou classification: legitimate coordination benefit of predictable welfare costs and population planning. But also requires active enforcement (police checks, housing control, school enrollment restrictions) that consumes bureaucratic resources and creates corruption risks. Faces generational pressure to reform (children of migrants aging into voting-age population, labor shortages in low-skill sectors). Constrained exit because hukou is entangled with property taxes, school funding, and healthcare financing — dismantling one mechanism requires reforming entire fiscal system. Benefits flow to city (property values, tax base, labor control); costs borne by migrants.
constraint_indexing:constraint_classification(household_registry_system, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized civil society (NGOs, lawyers, journalists), reform-oriented policy makers, and younger urban residents see hukou as temporary coordination failure being solved through targeted reforms. Point-of-need hukou relaxation in second-tier cities (Chongqing, Chengdu) and integration pilots in coastal zones create exit pathways. High agency to drive change. Sees constraint as having sunset clause: as labor market tightens and urbanization matures, fiscal decoupling and social insurance reform make hukou enforcement increasingly expensive. Suppression declining as alternative coordination mechanisms (portable social insurance, school funding decoupling) mature. Theater ratio low (enforcement is real, not performative; costs are material).
constraint_indexing:constraint_classification(household_registry_system, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: RURAL LOCAL GOVERNMENT (PITON) — Experiences hukou as degraded institution. System originally coordinated rural land tenure and commune organization during collective agriculture era. In post-reform market economy, rural hukou is largely performative: no longer controls land access (households hold nominal rights), no longer organizes production or welfare, primarily restricts urban access without providing rural services. Maintains hukou registration through inertia and because loss of hukou authority would reduce county government administrative importance. Theater ratio high — hukou enforcement exists as institutional ritual rather than functional necessity.
constraint_indexing:constraint_classification(household_registry_system, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing hukou as an immutable feature of large-state organization: 'All nations must track population location; registration systems are inherent to modern governance.' This perspective risks treating a contingent institutional arrangement (the specific legal discrimination embedded in hukou) as a natural law. However, comparative evidence (other large nations use registration without the mobility restrictions; hukou itself has undergone major reforms in some provinces) contradicts the mountain classification. The engine will identify this as a false summit, revealing that the 'inherent to governance' framing obscures policy choices.
constraint_indexing:constraint_classification(household_registry_system, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(household_registry_system_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(household_registry_system, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(household_registry_system, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(household_registry_system, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(household_registry_system, TR),
    TR >= 0.70.

:- end_tests(household_registry_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The rural migrant experiences direct deprivation in education access, healthcare, housing security, and pension eligibility while working in urban labor markets. These are not trivial costs — education and healthcare differentials compound across generations. The value reflects that extraction is structural and severe but not absolute: informal networks provide some services, enforcement has gaps, and some cities have initiated reforms. The upward trajectory over 20 years (0.42 to 0.58) reflects that as migration has accelerated and the floating population has aged into childcare-requiring and elder-care-needing cohorts, the actual extraction has intensified even as the policy language has softened. Suppression (0.72): High. Barriers are material (legal status restrictions), institutional (police authority, employer enforcement, housing discrimination), and psychological (identity-lock to origin hukou, shame, internalized inferiority). Removal of legal barriers is slow (government has initiated reform pilots but faces fiscal and political resistance), and psychological internalization means that legal reform alone may not produce rapid behavioral change. Theater ratio (0.48): Moderate-low. Hukou enforcement is substantially real and material — police conduct checks, employers lose workers to deportation, families are separated. This is not purely performative, distinguishing it from vestigial regulations that are theater-heavy. The moderate theater ratio reflects that enforcement has eroded in some city zones (coastal regions with labor shortages have relaxed hukou checks) while remaining intense in others (interior cities with surplus labor), and that the gap between official restriction and actual practice has widened. Suppression is doing more work than theater in maintaining the constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a four-way perspectival gap. (1) The powerless/trapped agent and institutional/arbitrage agent experience opposite-sign extraction: migrants bear 0.78 extracted; employers receive -0.05 (subsidy). This is the archetypal snare gap. (2) The institutional/constrained agent (urban government) and institutional/arbitrage agent (employer) both classify as beneficiaries but experience different extraction: government sees tangled_rope (genuine coordination mixed with enforcement cost); employer sees rope (pure coordination benefit). The difference is that government must actively enforce while employer can free-ride. (3) The organized/mobile agent (reform coalition) and powerless/trapped agent see fundamentally different timelines: reform coalition sees generational/scaffold (sunset within 15 years); trapped agent sees biographical/snare (no exit within lifetime). (4) The analytical observer risks collapsing the perspectival structure into a mountain (inherent to governance), which false summit detection will reject. The perspectival gaps reveal that hukou is not a natural law but a contingent institutional arrangement with clear beneficiaries, clear victims, and clear pathways for reform.
 *
 * DIRECTIONALITY LOGIC:
 *   The powerless/trapped agent (rural migrant) derives d ≈ 0.92 from their structural position: victimhood (high d), trapped exit (d ≈ 1.0), zero beneficiary status, and no arbitrage options. This produces f(d) ≈ 1.35, so their effective extraction χ ≈ 0.58 × 1.35 × 1.0 (national scope) ≈ 0.78 experienced extractiveness. The institutional/arbitrage agent (urban employer) derives d ≈ 0.08 from their structural position: beneficiary status (low d), arbitrage exit options, and zero victim status. This produces f(d) ≈ -0.10, so their effective extraction χ ≈ 0.58 × (-0.10) × 0.8 (local scope) ≈ -0.05 — they experience the constraint as providing subsidy, not extraction. The institutional/constrained agent (urban government) derives d ≈ 0.45 from mixed beneficiary (administrative convenience) and enforcement cost (requires active bureaucratic labor). This produces f(d) ≈ 0.58, so their effective extraction χ ≈ 0.58 × 0.58 × 1.0 ≈ 0.34 — mixed experience of coordination and cost. The moderate/constrained agent (migrant family) derives d ≈ 0.65 from victim status mixed with some coordination benefit (diaspora networks). This produces f(d) ≈ 1.00, matching the legacy moderate power atom. The organized/mobile agent (reform coalition) derives d ≈ 0.35 from their structural position: partial victim status (affected communities) mixed with substantial agency and exit paths. This produces f(d) ≈ 0.40, so their effective extraction χ ≈ 0.58 × 0.40 × 1.0 ≈ 0.23 experienced extractiveness — low enough that they see the constraint as solvable rather than fundamental.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that hukou is a genuine snare that has been partially reframed as coordination through institutional rhetoric ('population planning,' 'urban management,' 'fiscal sustainability'). The coordination function (matching welfare provision to fiscal capacity) is real but not sufficient to justify the discrimination. The separability test: Can the coordination function (population tracking, service cost control) be preserved without the extractive access restrictions? Yes — portable national insurance, school funding decoupling, and property-based taxation separate the coordination functions from the discrimination mechanism. Therefore, the snare classification is correct for the baseline perspective (powerless/trapped victim). The tangled_rope perspectives (moderate family, institutional government) capture secondary coordination functions that exist within the snare but do not redeem it. The scaffold perspective (reform coalition) reflects real structural changes (fiscal decoupling, labor shortage pressure) that are creating genuine sunset paths. The mountain perspective is a false summit and must be rejected. The piton perspective (rural government) captures the genuine degradation of the original coordination function while the enforcement mechanism persists through inertia. The comprehensive classification is: snare for trapped agents, tangled_rope for agents with mixed benefit/burden and constrained exit, rope for agents with arbitrage escape, scaffold for organized actors with generational exit paths, and piton for institutions maintaining the constraint despite functional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_stability,
    'Is hukou suppression primarily maintained by active enforcement (police authority, workplace inspections) or by internalized belief in legitimacy and identity-locking to origin place?',
    'Comparative enforcement data during policy relaxation periods; survey data on migrant exit behavior when enforcement pressure eases; analysis of enforcement costs vs. actual compliance without enforcement',
    'If primarily enforced: suppression declines rapidly when enforcement pauses (snare mechanism fails). If internalized: suppression persists even after enforcement eases (identity-locked mechanism); true exit requires identity frame shift, not just barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_stability, empirical, 'Whether hukou suppression is active enforcement or internalized identity lock').

omega_variable(
    reform_sunset_timeline,
    'Is fiscal decoupling of social services from hukou status technically feasible and politically achievable within 15 years, or is the political commitment insufficient?',
    'Analysis of existing pilot programs (Chongqing hukou reform, portable pension programs); fiscal impact modeling of full decoupling; tracking of actual hukou usage changes in liberalized zones vs. intent to liberalize nationally',
    'If technically feasible and politically committed: scaffold perspective is correct — constraint has real sunset. If technically feasible but politically stalled: constraint reclassifies from Scaffold to Piton (maintained by political inertia despite declining function). If infeasible: constraint returns to Snare (enforcement justification shifts from administrative convenience to structural necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sunset_timeline, empirical, 'Whether hukou can be fiscally decoupled from service access').

omega_variable(
    coordination_necessity_residual,
    'After removing the discriminatory access restrictions, what residual coordination function does hukou registration provide that cannot be replicated by portable ID systems, national health insurance portability, or mobile-friendly school enrollment mechanisms?',
    'Decompose hukou functions into: (A) population tracking (solved by national ID), (B) property tax coordination (solved by property registration), (C) school funding allocation (solved by per-child funding formulas), (D) health insurance pooling (solved by portable insurance). Identify genuine residual coordination that cannot be decoupled.',
    'If residual is substantial: tangled_rope perspective is more accurate than snare (genuine coordination function exists alongside extraction). If residual is minimal or zero: snare and piton perspectives are accurate (constraint is pure extraction or pure theater, with negligible coordination benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_residual, conceptual, 'What coordination function survives after discriminatory restrictions are removed').

omega_variable(
    identity_lock_rural_attachment,
    'To what extent are rural migrants identity-locked to origin hukou through lineage, ancestral property, burial rights, and kinship obligations that make exit psychologically impossible despite legal and economic mobility?',
    'Ethnographic study of hukou change behavior: migrants with legal opportunity to change hukou but choose not to; analysis of return migration patterns; comparison of barriers experienced (material cost, legal time) vs. reported psychological blocks (identity, belonging, shame)',
    'If identity-lock is significant: many migrants are trapped not by legal barriers but by internalized frames of belonging and obligation. Even legal hukou reform would not produce exit. Suppression mechanism is more psychological than material. If identity-lock is weak: legal barriers are primary mechanism; reform that removes legal restrictions produces rapid behavioral change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_rural_attachment, empirical, 'Whether rural migrants are identity-locked to origin hukou').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(household_registry_system, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hrs_tr_t0, household_registry_system, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hrs_tr_t10, household_registry_system, theater_ratio, 10, 0.42).
narrative_ontology:measurement(hrs_tr_t20, household_registry_system, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(hrs_be_t0, household_registry_system, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hrs_be_t10, household_registry_system, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(hrs_be_t20, household_registry_system, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(household_registry_system, resource_allocation).
narrative_ontology:affects_constraint(household_registry_system, urban_school_funding_stratification).
narrative_ontology:affects_constraint(household_registry_system, migrant_healthcare_access_rationing).
narrative_ontology:affects_constraint(household_registry_system, pension_system_portability).

% DUAL FORMULATION NOTE:
% The hukou system decomposes into three structurally distinct constraints: (1) school access coordination (education rationing by hukou), (2) healthcare access coordination (insurance pooling by hukou), and (3) pension access coordination (retirement entitlement by hukou registration location). Each has distinct ε values reflecting whether the coordination function is genuine and whether alternatives exist. The household registry system story treats hukou as a meta-constraint linking these three, with base extractiveness reflecting the average suppression across all three domains. See sister stories for domain-specific analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(household_registry_system, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
