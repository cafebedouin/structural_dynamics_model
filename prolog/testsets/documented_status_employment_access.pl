% ============================================================================
% CONSTRAINT STORY: documented_status_employment_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_documented_status_employment_access, []).

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
 *   constraint_id: documented_status_employment_access
 *   human_readable: Documented Status Employment Access Restriction
 *   domain: labor/immigration/economic
 *
 * SUMMARY:
 *   Documented status employment access restrictions create a legal
 *   segmentation of labor markets in which workers without documentation of
 *   citizenship or authorized work status are excluded from formal employment
 *   sectors. The constraint nominally addresses labor law enforcement and
 *   border control while functionally enabling systematic wage suppression
 *   and labor exploitation. This is a high-extraction, high-suppression
 *   mechanism that affects millions of workers globally and exhibits stark
 *   perspectival divergence: the undocumented worker experiences absolute
 *   confinement (Snare); the formal employer experiences efficient labor cost
 *   reduction through legal exclusion (Rope); the enforcement apparatus
 *   experiences both coordination (standardized eligibility verification) and
 *   expansion (institutional growth and discretionary power, Tangled Rope);
 *   and the analytical observer sees the constraint as a hybrid
 *   coordination-extraction mechanism with extractive dominance. The
 *   increasing extractiveness over the measurement interval (0.52 to 0.68)
 *   reflects intensifying enforcement and shrinking informal sector wages as
 *   digital tracking and employer verification systems improve. Theater ratio
 *   remains low to moderate (0.45) because the constraint operates through
 *   explicit legal mechanisms with clear rules, not through performative
 *   ritual — this distinguishes it from constraints whose extraction is
 *   hidden behind formal claims about their coordination function.
 *
 * KEY AGENTS:
 *   - Undocumented Migrants: Primary victims (powerless/trapped) — cannot legally access formal employment; forced into informal sector at depressed wages and under coercive conditions; no legal recourse for wage theft or unsafe conditions; estimated 164 million globally (IOM 2024)
 *   - Precarious Workers: Secondary victims (powerless/trapped) — documented workers on temporary status whose immigration status is tied to employment; employer holds visa hostage; cannot exit without deportation risk
 *   - Formal Sector Employers: Primary beneficiaries (institutional/arbitrage) — access labor pool willing to accept below-market wages due to legal exclusion; reduce hiring liability through documented status verification; benefit from depressed wage levels across labor market as informal sector competition is suppressed
 *   - Documented Citizen Workers: Secondary beneficiaries (institutional/arbitrage) — pool into protected wage group; cartelized against undocumented competition; gain framing of constraint as 'labor protection' despite actual function as wage depression
 *   - Enforcement Apparatus: Institutional beneficiary with complexity (powerful/mobile to constrained depending on political context) — administers documented status verification; expands through budget growth and specialized workforce; operates through selective enforcement creating arbitrage
 *   - Labor Rights Coalition: Organized constraint critics (organized/constrained) — see degraded protection mechanism; have constrained exit (lobbying power but not structural override); perceive high theater in the constraint's legitimating narratives
 *   - Informal Sector: Structural consequence rather than agent — grows as documented status enforcement tightens; wages decline as formal sector competition is legally eliminated; conditions worsen as workers accept exploitation to avoid detection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(documented_status_employment_access, 0.68).
domain_priors:suppression_score(documented_status_employment_access, 0.72).
domain_priors:theater_ratio(documented_status_employment_access, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(documented_status_employment_access, extractiveness, 0.68).
narrative_ontology:constraint_metric(documented_status_employment_access, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(documented_status_employment_access, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(documented_status_employment_access, snare).
narrative_ontology:human_readable(documented_status_employment_access, "Documented Status Employment Access Restriction").
narrative_ontology:topic_domain(documented_status_employment_access, "labor/immigration/economic").

domain_priors:requires_active_enforcement(documented_status_employment_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(documented_status_employment_access, formal_sector_employers).
narrative_ontology:constraint_beneficiary(documented_status_employment_access, documented_citizen_workers).
narrative_ontology:constraint_beneficiary(documented_status_employment_access, enforcement_apparatus).
narrative_ontology:constraint_victim(documented_status_employment_access, undocumented_migrants).
narrative_ontology:constraint_victim(documented_status_employment_access, precarious_workers).
narrative_ontology:constraint_victim(documented_status_employment_access, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED MIGRANT (SNARE) — Faces absolute barrier to legal employment without documented status. Cannot exit through legitimate labor market access; confinement is structural and enforced. Must accept informal sector wages, unsafe conditions, and exploitation with no legal recourse. Zero degrees of freedom within formal economy.
constraint_indexing:constraint_classification(documented_status_employment_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS WORKER (SNARE) — Even documented workers on temporary visas face confinement: losing work visa triggers deportation. Employer holds documentation hostage; worker cannot negotiate or exit without losing immigration status. High suppression through legal entanglement — exit costs are existential.
constraint_indexing:constraint_classification(documented_status_employment_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FORMAL SECTOR EMPLOYER (ROPE) — Experiences documented status requirement as coordination mechanism: verification of workers' legal eligibility reduces hiring liability. The constraint solves a collective action problem (avoiding legal penalties) while providing preferential access to desperate workers willing to accept depressed wages. Net beneficiary with arbitrage — can hire at below-market rates by excluding undocumented workers from competing.
constraint_indexing:constraint_classification(documented_status_employment_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DOCUMENTED CITIZEN WORKER (ROPE) — Experiences the constraint as coordination and cartelization: documented status requirement pools them into a higher-wage group by legally excluding undocumented competitors. Also provides moral and civic framing ('protecting domestic workers'). Benefits from extraction directed at undocumented workers; sees constraint as fair labor market regulation.
constraint_indexing:constraint_classification(documented_status_employment_access, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ENFORCEMENT APPARATUS (TANGLED ROPE) — Administers the documented status requirement through workplace raids, document verification, employer penalties. Genuinely solves coordination problem (standardizes eligibility verification across employers) while simultaneously extracting through selective enforcement, corruption, and institutional expansion (budget growth, personnel). Powerful actor with mobility — could exit if political will changed, but has structural incentive to perpetuate the constraint.
constraint_indexing:constraint_classification(documented_status_employment_access, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR RIGHTS COALITION (PITON) — Organized actors see documented status as a degraded protection mechanism: it was designed to safeguard worker rights but now functions primarily as a selection mechanism for employer exploitation of undocumented workers. Coalition has constrained exit (can lobby but not overturn the constraint unilaterally) and sees high theater — the constraint is maintained through rhetoric about 'rule of law' and 'border security' while its primary function (enabling labor market segmentation) remains unacknowledged. Theater ratio reflects how much political legitimacy derives from stated purposes versus actual outcomes.
constraint_indexing:constraint_classification(documented_status_employment_access, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint contains genuine coordination elements (verification of eligibility, standardization of hiring practices, risk allocation between employers and state) alongside severe asymmetric extraction (segmentation of labor market into protected and disposable tiers, wage suppression through exclusion, institutional expansion of enforcement). The classification depends on relative weight assigned to coordination vs extraction; the data justifies tangled_rope as the analytically stable position.
constraint_indexing:constraint_classification(documented_status_employment_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(documented_status_employment_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(documented_status_employment_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(documented_status_employment_access, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(documented_status_employment_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(documented_status_employment_access, TR),
    TR >= 0.70.

:- end_tests(documented_status_employment_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The documented status requirement directly enables wage depression through legal exclusion — undocumented workers in formal economy would earn 20-30% more in comparable roles (World Bank studies). The extraction flows from undocumented workers (through suppressed informal sector wages, which respond to formal sector closure) to documented employers and workers (through artificially elevated formal sector wages). Suppression (0.72): Very high. The barrier is legal and enforced through state machinery. Undocumented workers cannot formally work, cannot change immigration status without leaving, and face deportation if discovered. Some de facto mobility exists (working informally, geographic arbitrage between countries), but these options are costly and dangerous. Theater ratio (0.45): Moderate-low. The constraint operates through explicit legal requirements with objective documentation standards — minimal performative content. However, rhetoric around 'labor protection,' 'rule of law,' and 'national security' provides narrative cover for what functions as labor market segmentation. The theater ratio is lower than constraints like traditional peer review because the documented status rule is transparent and administered through clear bureaucratic processes, not through claimed expertise or scientific judgment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism can classify as pure extraction (Snare) from the victim's perspective and as beneficial coordination (Rope) from the beneficiary's perspective. The documented status requirement solves a real coordination problem for employers (standardized verification of legal eligibility) while simultaneously enabling systematic extraction from undocumented workers. The beneficiary experiences the constraint as coordination because it reduces their hiring liability and provides access to desperate workers. The victim experiences it as extraction because it forecloses their only escape path — they cannot enter the formal market even at reduced wages, cannot compete for better conditions, and cannot legally advocate for themselves. The enforcement apparatus's Tangled Rope classification reflects that it both administers coordination (clear rules, standardized processes) and extracts (selective enforcement, institutional expansion, corruption opportunities). The analytical observer's Tangled Rope classification confirms that extraction is the dominant function even though coordination elements exist — the constraint's persistence depends on its extractive benefits for employers and documented workers, not on the coordination function alone (which could be replicated through alternative verification mechanisms).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derivation for each perspective flows from structural position. Undocumented migrants as victims with trapped exit: d ≈ 0.95 → f(d) ≈ 1.42, amplifying experienced extraction. Formal employers as beneficiaries with arbitrage: d ≈ 0.05 → f(d) ≈ -0.12, reducing or inverting experienced extraction (they perceive gain from the constraint). Enforcement apparatus as powerful institutional actor with mobile to constrained exit: d varies (0.35-0.50 depending on political context) → f(d) ≈ 0.4-0.65, producing moderate extraction for them (they both administer and benefit). The calculated χ values follow: χ = 0.68 × 1.42 × 1.0 ≈ 0.96 for victims (Snare threshold χ ≥ 0.66 clearly met), χ = 0.68 × -0.12 × 1.0 ≈ -0.08 for beneficiaries (negative χ characteristic of Rope perception), χ = 0.68 × 0.5 × 1.0 ≈ 0.34 for enforcement apparatus (Tangled Rope range). Spatial scope (national, σ = 1.0) applies uniformly because documented status is administered at the nation-state level.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by clarifying that beneficiaries and victims have inverting assessments of the same coordination function. The formal employer sees 'standardized eligibility verification' as beneficial coordination that reduces their legal risk. The undocumented worker sees the same mechanism as purely extractive because they are legally excluded from the formal market entirely — there is no way to pass the documented status gate at any wage. The coordination function is real but asymmetric: it coordinates among employers and documented workers to maintain cartelization, not coordination across the full labor market. This is why the constraint persists despite economic inefficiency — it concentrates benefits (employers, documented workers, enforcement apparatus) while dispersing costs (undocumented workers, informal sector, overall labor market efficiency). The theater ratio of 0.45 (lower than constraints like peer review) reflects that the mechanism is administratively transparent — the extraction does not hide behind claims about the coordination function but operates through explicit legal rules. The mandatrophy is resolved by recognizing that 'is documented status a coordination mechanism or extraction?' has an answer that depends on scope: within-formal-sector, it is coordination; for the full labor market including undocumented workers, it is extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'How much of the documented status requirement''s function is genuine coordination (standardizing eligibility verification) versus how much is extractive segmentation (creating a legal underclass for wage suppression)?',
    'Counterfactual analysis: what would labor markets look like if documented status were dropped but alternative verification mechanisms (e.g., secure biometric ID, portable credential systems) were maintained? Do wage effects persist?',
    'If primarily coordination: reclassify toward Rope; strengthen policy arguments for alternatives that preserve coordination without segmentation. If primarily extraction: confirm Snare for primary victims; focus policy on dismantling the legal barrier rather than replacing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Boundary between coordination function and extractive segmentation').

omega_variable(
    informal_sector_sustainability,
    'Is the informal sector a genuine alternative exit path for undocumented workers, or does documented status segmentation make informal work the only option while simultaneously making it more exploitative?',
    'Comparative wage and condition analysis: undocumented workers in informal sectors with and without documented status legal barriers; elasticity of informal sector expansion in response to documented status enforcement',
    'If informal sector is viable alternative: suppression metric should be lowered (exit option improves from trapped toward constrained). If informal sector expands precisely because documented status closes formal doors, and those informal jobs are more exploitative: suppression metric confirmed or raised; the constraint creates the informality it claims to address.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_sector_sustainability, empirical, 'Whether informal sector provides genuine or degraded exit').

omega_variable(
    enforcement_consistency_and_arbitrage,
    'Is enforcement of documented status requirements applied uniformly across employers, sectors, and geographies, or does selective enforcement create arbitrage opportunities that benefit some actors while punishing others?',
    'Audit studies; inspection frequency analysis by sector and geography; correlation between enforcement activity and employer political connections or union density',
    'If enforcement is consistent: constraint functions more like coordination (clear rules). If enforcement is selective: arbitrage opportunities confirm extractive classification and suggest enforcement apparatus itself is a beneficiary (high d for institutional actors, higher chi). Selective enforcement also explains how the constraint persists despite economic inefficiency — differential punishment preserves political coalition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_consistency_and_arbitrage, empirical, 'Uniformity vs selectivity of documented status enforcement').

omega_variable(
    documented_status_as_identity_lock,
    'For long-term undocumented migrants, does the documented status barrier function as an external trap (material barrier) or have workers internalized the constraint as identity (undocumented identity as unchangeable, self-concept as outsider)?',
    'Ethnographic study of identity frames; survey of undocumented workers on whether exit feels possible in principle vs impossible in practice; behavior change following legalization (do workers'' employment and career aspirations shift?)',
    'If primarily external trap: classification confirmed as Snare; focus on removing the barrier. If identity-locked: some proportion of the suppression is internalized (cognitive rather than structural); reclassify exit_options from trapped toward identity_locked; note that exit requires identity reframing, not just legal status change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_status_as_identity_lock, empirical, 'External material trap versus internalized identity lock').

omega_variable(
    documented_status_global_heterogeneity,
    'Does the constraint operate identically across all documented status regimes, or are there structural variations (e.g., EU citizenship free movement, bilateral labor agreements, humanitarian exceptions) that create different extraction profiles?',
    'Comparative analysis across jurisdictions with different documented status frameworks; ε measurement for each variant',
    'If heterogeneous: may require decomposition into multiple constraint stories with different ε values (EU internal mobility vs third-country workers; skilled vs unskilled; seasonal vs permanent). Each story would have different perspectives and different classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documented_status_global_heterogeneity, empirical, 'Global heterogeneity in documented status extraction profiles').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(documented_status_employment_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(docstat_tr_t0, documented_status_employment_access, theater_ratio, 0, 0.38).
narrative_ontology:measurement(docstat_tr_t5, documented_status_employment_access, theater_ratio, 5, 0.42).
narrative_ontology:measurement(docstat_tr_t10, documented_status_employment_access, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(docstat_be_t0, documented_status_employment_access, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(docstat_be_t5, documented_status_employment_access, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(docstat_be_t10, documented_status_employment_access, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(documented_status_employment_access, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(documented_status_employment_access, 0.12).
narrative_ontology:affects_constraint(documented_status_employment_access, informal_sector_wage_suppression).
narrative_ontology:affects_constraint(documented_status_employment_access, employer_monopsony_power).
narrative_ontology:affects_constraint(documented_status_employment_access, migrant_debt_bondage).

% DUAL FORMULATION NOTE:
% Documented status employment access is upstream of informal sector dynamics: as formal employment becomes legally inaccessible, workers shift to informal markets where wages are depressed and conditions deteriorate. The three downstream constraints (informal wage suppression, employer monopsony, migrant debt) are causally dependent on documented status segmentation. Each should be analyzed with ε values reflecting their position in the causal chain; the upstream constraint's classification influences but does not determine the downstream constraints' types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(documented_status_employment_access, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
