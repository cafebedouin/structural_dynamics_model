% ============================================================================
% CONSTRAINT STORY: uk_minimum_wage_coverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_minimum_wage_coverage, []).

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
 *   constraint_id: uk_minimum_wage_coverage
 *   human_readable: UK Minimum Wage Coverage and Enforcement Exclusions
 *   domain: economic/labor_regulation
 *
 * SUMMARY:
 *   The UK minimum wage system exhibits structural coordination function
 *   (establishing a wage floor to prevent destructive competition) combined
 *   with systematic extraction through exemption categories and enforcement
 *   gaps. The constraint operates through selective coverage: apprentices,
 *   care home sleep-in attendants, trainee teachers, and some agency workers
 *   are legally excluded from minimum wage protection. Simultaneously,
 *   enforcement capacity has remained roughly constant while the scope of
 *   work and employment forms has expanded, creating a theater dynamic where
 *   formal regulations exist but practical enforcement is selective. This
 *   produces a perspectival landscape where the same structural phenomenon
 *   appears as pure extraction (snare) to excluded workers, mixed
 *   coordination-extraction (tangled rope) to covered low-wage workers and
 *   the enforcement agency, coordination (rope) to large compliant employers,
 *   degraded ritual (piton) to non-compliant employers, and a scaffolding
 *   solution (scaffold) to organized reform coalitions building alternative
 *   verification pathways through living wage standards and union organizing.
 *
 * KEY AGENTS:
 *   - Excluded Worker Categories: Primary victim (powerless/trapped) — care home attendants, apprentices, trainee teachers, disabled workers, some agency workers with no legal wage floor
 *   - Covered Low-Wage Workers: Secondary victim (moderate/constrained) — protected in theory but face enforcement asymmetry and employment precarity
 *   - Large Compliant Employers: Primary beneficiary (institutional/arbitrage) — gain coordination benefit from wage floor eliminating predatory undercutting
 *   - Small Non-Compliant Employers: Secondary actor (institutional/constrained) — rational response to enforcement bottleneck is informal payment and exemption exploitation
 *   - Enforcement Agency (HMRC/BEIS): Mixed actor (institutional/constrained) — mandated for universal enforcement with partial resources; benefits from depoliticized wage-setting, harms from unrealistic scope
 *   - Living Wage Foundation, Unions, Reform Coalition: Organized agents (organized/mobile) — building alternative verification through accreditation and collective bargaining with sunset logic for exemptions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political exemption choices as inherent labor market asymmetries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_minimum_wage_coverage, 0.52).
domain_priors:suppression_score(uk_minimum_wage_coverage, 0.65).
domain_priors:theater_ratio(uk_minimum_wage_coverage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_minimum_wage_coverage, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_minimum_wage_coverage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uk_minimum_wage_coverage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_minimum_wage_coverage, tangled_rope).
narrative_ontology:human_readable(uk_minimum_wage_coverage, "UK Minimum Wage Coverage and Enforcement Exclusions").
narrative_ontology:topic_domain(uk_minimum_wage_coverage, "economic/labor_regulation").

domain_priors:requires_active_enforcement(uk_minimum_wage_coverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_minimum_wage_coverage, large_employers).
narrative_ontology:constraint_beneficiary(uk_minimum_wage_coverage, compliant_low_wage_sector).
narrative_ontology:constraint_victim(uk_minimum_wage_coverage, excluded_worker_categories).
narrative_ontology:constraint_victim(uk_minimum_wage_coverage, enforcement_resource_constraints).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED WORKER (SNARE) — Trapped by legal exemptions from minimum wage protection. Care home sleep-in attendants, apprentices under 21, trainee teachers, some agency workers face no wage floor. Exit options are severely constrained: changing sectors requires skills retraining; formal complaint risks employment termination in weak-enforcement environments. The constraint extracts labor below statutory minimum without remedy.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COVERED LOW-WAGE WORKER (TANGLED ROPE) — Protected by minimum wage floor but faces enforcement asymmetry. Formal coverage exists; exit is costly but possible (switching employers, relocation, union organization). Benefits from wage floor coordination; bears costs of compliance burden that shifts to employment reduction. Mixed experience: protected in theory, precarious in practice.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE COMPLIANT EMPLOYER (ROPE) — Experiences minimum wage as coordination mechanism. Legal certainty enables payroll planning; wage floor eliminates predatory undercutting by competitors. Can arbitrage compliance costs against market benefits: reputation advantage, reduced turnover, simplified HR administration. Net beneficiary through coordination.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SMALL EMPLOYER IN UNDERGROUND ECONOMY (PITON) — Faces enforcement bottleneck as routine operating context. Legally required to comply but rational response is informal payment, cash-in-hand arrangements, or exemption exploitation. Theater ratio is high: formal minimum wage regulations exist but enforcement capacity has degraded relative to scope of informal economy. Constraint persists through institutional inertia despite functional exhaustion.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENFORCEMENT REFORM COALITION (SCAFFOLD) — Organized agents see minimum wage as incomplete coordination requiring sunset of exemptions and enforcement scaling. Living Wage movement, unions, and workplace rights advocates are building parallel verification pathways: accreditation standards, union organizing, public procurement living-wage requirements. Sunset logic: formal exemptions (apprentice rate, care worker exclusion) should phase out; enforcement should transition from state inspection to sectoral standards and collective bargaining.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ENFORCEMENT AGENCY (TANGLED ROPE) — Constrained by budget allocation; enforcement capacity has remained roughly constant while informal economy and worker categories have expanded. Benefits from coordination mandate (avoids politicization of wage-setting); bears costs of structural under-resourcing. Mixed position: tasked with universal enforcement, equipped for partial coverage. The constraint benefits the agency through reducing political controversy over wage levels but harms through unrealistic scope expectations.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, minimum wage exemptions reflect inherent structural asymmetries in labor markets: atypical employment (apprenticeships, care work) creates verification problems that are inherent to labor's heterogeneity. This perspective risks naturalizing what is actually a political choice about whose work counts for statutory protection. The engine's false summit detector should flag this as contingent institutional architecture, not immutable law.
constraint_indexing:constraint_classification(uk_minimum_wage_coverage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_minimum_wage_coverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_minimum_wage_coverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_minimum_wage_coverage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_minimum_wage_coverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_minimum_wage_coverage, TR),
    TR >= 0.70.

:- end_tests(uk_minimum_wage_coverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant value from excluded workers (care attendants earning 30-40% below covered rate, apprentices at substantially reduced rates) and from enforcement asymmetry benefiting non-compliant employers. But extractiveness is not extreme because: (1) the formal minimum wage for covered workers does provide genuine coordination function reducing predatory competition, (2) many excluded workers operate in sectors where they have some collective power (care sector union organizing, apprenticeship completion leverage), and (3) enforcement, though imperfect, does achieve 70-80% compliance in covered sectors. The increase from 0.38 to 0.52 over 15 years reflects: expanding care sector as proportion of economy (more workers in exempted care roles), growth of non-standard employment forms (zero-hours, agency work) where enforcement is difficult, and erosion of enforcement capacity relative to scope. Suppression (0.65): High. Barriers to exit or remedy include: legal exemption status (no remedy available in law), power asymmetry in care work (high employer specificity of care knowledge, emotional labor lock-in), apprenticeship structure (training credential locked to exploitative period), and enforcement gaps reducing credible threat of sanction. Excluded workers have constrained exit options; covered workers face job loss risk if they challenge exploitation. Theater ratio (0.58): Moderate-high. The formal minimum wage regulations are real and enforce 70-80% compliance in covered sectors, so theater is not extreme. But exemptions and enforcement gaps create performative elements: regulations exist for apprentices and care workers but are not legally enforceable, enforcement agency's compliance messaging exceeds its actual capacity, and non-compliant employer behavior (off-books arrangements) persists despite formal legal prohibition.
 *
 * PERSPECTIVAL GAP:
 *   Sharp divergence between excluded workers' experience (snare) and large employers' experience (rope). Both groups are constrained by the same formal regulations, but the exemptions and enforcement structure create opposite extraction flows. Excluded workers experience pure extraction with no remedy. Large employers experience coordination benefit (stabilized labor costs, reduced predatory competition) with arbitrage capacity to shift compliance burden. The covered low-wage worker (tangled rope) and enforcement agency (tangled rope) occupy intermediate positions: both benefit from coordination function while bearing costs of asymmetric enforcement and precarity. The reform coalition (scaffold) sees a real exit path through living wage standards and union organizing that would sunset the exemptions and improve enforcement. The natural law perspective (mountain) risks conflating the exemptions' political origins with economic inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   The extraction flow runs from excluded workers (powerless/trapped, d ≈ 0.92) and non-compliant employers toward large compliant employers (institutional/arbitrage, d ≈ 0.08) and enforcement agencies. Excluded workers are systematically undercompensated with no legal remedy, creating pure extraction. Covered workers benefit from the wage floor (coordination) but pay through employment precarity (extraction). Large employers benefit twice: from the wage floor that prevents undercutting and from selective enforcement that punishes their competitors more than themselves. The enforcement agency benefits from depoliticized wage-setting (removed from each election cycle) but harms through perpetual underfunding relative to mandate scope. The directionality is complex because the constraint contains both genuine coordination (the wage floor prevents destructive competition) and systematic extraction (exemptions and enforcement gaps concentrate benefits on employers). This mixed character makes tangled rope the appropriate primary classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing coordination (wage floor that prevents destructive competition) from extraction (exemptions and enforcement gaps that concentrate benefits on employers). The tangled rope classification captures both functions simultaneously: genuine coordination reducing predatory undercutting coexists with systematic extraction via exemption categories. The mandatrophy risk is that the coordination function (legitimate) gets used to justify the extraction function (illegitimate). Analysis shows: (1) the wage floor coordination is real and beneficial (reduces employment volatility, prevents downward spirals), (2) the exemptions are political choices without coordination rationale (apprentice exemption reduces training costs but doesn't serve coordination function; care worker exemption exploits sectoral power imbalance, not coordination necessity), (3) enforcement gaps reflect resource scarcity, not design necessity. The scaffold perspective validates that the exemptions can and should sunset as alternative verification mechanisms (living wage accreditation, sectoral bargaining) mature. The constraint is not mandatrophy-resolved at this time because the mix of coordination and extraction remains unresolved politically — no threshold clarity exists between legitimate wage-setting and illegitimate exemption exploitation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apprentice_exemption_necessity,
    'Is the apprentice minimum wage exemption (lower rate for under-21) necessary for training investment or does it primarily enable exploitation of young workers?',
    'Comparative analysis: apprenticeship completion rates and wage progression in UK vs jurisdictions with unified minimum wage; employer recruitment response to hypothetical unified rate; correlation between lower apprentice rate and training quality',
    'If necessary: exemption is coordination cost, supports genuine training function, reduces suppression assessment. If exploitative: exemption is pure extraction, classification shifts toward snare for apprentices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apprentice_exemption_necessity, empirical, 'Whether apprentice wage exemption serves training function or enables exploitation').

omega_variable(
    care_home_sleep_in_boundary,
    'Are care home sleep-in attendants genuinely uncompensable (no marginal labor during sleep hours) or does the exemption permit systematic undercompensation for on-call availability and risk?',
    'Work-time analysis: actual on-call response requirements, incident frequency, interruption patterns; comparative compensation in regulated sleep-in arrangements (hospitals, security); post-exemption removal regulatory impact analysis if implemented',
    'If genuinely non-compensable: sleep-in exclusion reflects labor market reality, reduces suppression. If compensable: exemption is extractive, classification strengthens toward snare for care workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(care_home_sleep_in_boundary, empirical, 'Whether care home sleep-in work is compensable or inherently non-marginal').

omega_variable(
    enforcement_budget_sufficiency_threshold,
    'What enforcement budget share of total labor enforcement spending is necessary to achieve 80%+ compliance in covered sectors, and what share for uncovered/informal economy?',
    'Budget impact analysis: current enforcement spending vs compliance rate trajectory; pilot enforcement scaling studies; international benchmark spending as % of wage bill or GDP',
    'If current budget at threshold: constraint is coordination bottleneck (resource-constrained enforcement, not design). If below threshold: structural underfunding makes enforcement theater, shifts classification toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_budget_sufficiency_threshold, empirical, 'Enforcement budget sufficiency threshold for minimum wage compliance').

omega_variable(
    informal_economy_substitution_elasticity,
    'When minimum wage is enforced in formal sectors, do non-compliant employers systematically shift to informal economy (cash-in-hand, zero-hours evasion) or do they absorb costs through other channels?',
    'Sector-level analysis pre/post enforcement scaling: wage bill distribution across formal/informal, employment displacement, underground economy size; worker survey on off-books arrangement prevalence',
    'If high substitution elasticity: enforcement creates piton dynamics (formal regulation drives informal evasion). If low: enforcement effectiveness confirms coordination logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_economy_substitution_elasticity, empirical, 'Substitution elasticity between formal compliance and informal evasion').

omega_variable(
    excluded_category_political_coalescence,
    'Will excluded worker categories (apprentices, care workers, disabled workers) build organized political power to eliminate exemptions, or will exemptions persist through diffuse beneficiary interests?',
    'Organizational trend analysis: union density in care sector, apprentice council strength, disabled workers advocacy coalition size; political economy of future minimum wage legislation; public opinion tracking on exemptions',
    'If coalescence: scaffold perspective validated, sunset path is real. If persistent fragmentation: exclusions may be structural, classification shifts toward piton for enforcement agency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_category_political_coalescence, preference, 'Whether excluded workers will organize to eliminate exemptions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_minimum_wage_coverage, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukminwage_tr_t0, uk_minimum_wage_coverage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ukminwage_tr_t8, uk_minimum_wage_coverage, theater_ratio, 8, 0.54).
narrative_ontology:measurement(ukminwage_tr_t15, uk_minimum_wage_coverage, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(ukminwage_be_t0, uk_minimum_wage_coverage, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ukminwage_be_t8, uk_minimum_wage_coverage, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(ukminwage_be_t15, uk_minimum_wage_coverage, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_minimum_wage_coverage, resource_allocation).
narrative_ontology:affects_constraint(uk_minimum_wage_coverage, care_sector_labor_extraction).
narrative_ontology:affects_constraint(uk_minimum_wage_coverage, apprenticeship_funding_model).
narrative_ontology:affects_constraint(uk_minimum_wage_coverage, enforcement_agency_resource_constraint).

% DUAL FORMULATION NOTE:
% UK minimum wage coverage decomposes into three structurally distinct constraints: (1) care_sector_labor_extraction (ε≈0.68, snare) — covers care workers specifically, focusing on the sleep-in exemption abuse; (2) apprenticeship_funding_model (ε≈0.45, tangled rope) — covers training subsidy extraction; (3) enforcement_agency_resource_constraint (ε≈0.52, piton) — covers the performance theater of enforcement. The present story models the system-level constraint; the three decomposed stories model domain-specific mechanisms. All linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_minimum_wage_coverage, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
