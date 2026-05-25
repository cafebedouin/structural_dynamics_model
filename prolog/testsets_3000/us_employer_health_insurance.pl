% ============================================================================
% CONSTRAINT STORY: us_employer_health_insurance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_employer_health_insurance, []).

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
 *   constraint_id: us_employer_health_insurance
 *   human_readable: US Employer-Sponsored Insurance (ESI) System
 *   domain: economic/social
 *
 * SUMMARY:
 *   The US employer-sponsored insurance (ESI) system is a path-dependent
 *   institutional artifact that emerged from WWII-era wage freezes and was
 *   crystallized by tax code incentives in the 1954 Internal Revenue Code.
 *   Initially a temporary wartime expediency, ESI became structural through
 *   policy lock-in, network effects, and regulatory inertia. The system ties
 *   essential healthcare access to specific employment, creating asymmetric
 *   bargaining power that suppresses worker mobility and wages. Large
 *   employers and health insurers benefit from the tax-advantaged structure
 *   and employer lock-in of workforce; workers, especially those in
 *   precarious employment or self-employed, bear extraction costs through
 *   limited mobility, gap-coverage risk, and wage suppression. The constraint
 *   exhibits all six DR types depending on observer perspective: for
 *   dependent workers, it is a Snare with maximum extraction; for large
 *   employers, a Rope with coordination gains; for organized labor, a Tangled
 *   Rope mixing coordination and extraction; for insurers, an increasingly
 *   performative Piton maintained by administrative theater; for
 *   policymakers, a Scaffold with a slow sunset as individual market
 *   alternatives mature; for the analytical observer, a false mountain that
 *   naturalizes contingent institutional choices as immutable economic laws.
 *
 * KEY AGENTS:
 *   - Dependent Workers: Primary victims (powerless/trapped) — tied to current employment due to healthcare coverage dependency, suppressed bargaining power, face catastrophic risk if employment ends
 *   - Self-Employed and Gig Workers: Secondary victims (powerless/trapped) — excluded from ESI system, face unaffordable individual market coverage, bear full healthcare cost burden
 *   - Large Employers (especially Fortune 500): Primary beneficiaries (institutional/arbitrage) — capture tax deduction value, obtain workforce lock-in, negotiate advantageous group rates, reduce voluntary turnover
 *   - Health Insurers (major national carriers): Secondary beneficiaries (institutional/constrained) — extract administrative rents from employer intermediary role, benefit from employer captive purchasing, maintain market position through path dependency
 *   - Pharmaceutical Manufacturers: Tertiary beneficiaries (powerful/mobile) — benefit from employer-distance pricing mechanism, insulated from end-user price resistance through intermediary structure
 *   - Organized Labor Unions: Mixed actors (organized/constrained) — negotiate ESI as major benefit (coordination function) but locked into defending system that suppresses worker mobility and diverts bargaining capital from wages
 *   - Public Policy Coalition: Reform advocates (organized/constrained) — push for individual market alternatives, subsidized coverage expansion, and structural decoupling of healthcare from employment, view ESI as temporary problem with sunset
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing path-dependent artifact as universal economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_employer_health_insurance, 0.58).
domain_priors:suppression_score(us_employer_health_insurance, 0.72).
domain_priors:theater_ratio(us_employer_health_insurance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_employer_health_insurance, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_employer_health_insurance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_employer_health_insurance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_employer_health_insurance, snare).
narrative_ontology:human_readable(us_employer_health_insurance, "US Employer-Sponsored Insurance (ESI) System").
narrative_ontology:topic_domain(us_employer_health_insurance, "economic/social").

domain_priors:requires_active_enforcement(us_employer_health_insurance).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, large_employers).
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, health_insurers).
narrative_ontology:constraint_beneficiary(us_employer_health_insurance, pharmaceutical_manufacturers).
narrative_ontology:constraint_victim(us_employer_health_insurance, workers_tied_to_jobs).
narrative_ontology:constraint_victim(us_employer_health_insurance, self_employed_individuals).
narrative_ontology:constraint_victim(us_employer_health_insurance, gig_workers).
narrative_ontology:constraint_victim(us_employer_health_insurance, unemployed_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEPENDENT WORKER (SNARE) — Trapped in current employment due to healthcare coverage dependency. Loss of job means loss of health insurance (absent COBRA continuation, which is unaffordable for most). Cannot exit without incurring catastrophic health risk. Experiences maximum extraction: labor bargaining power is suppressed by existential healthcare needs, employer captures wage concessions, worker bears full risk of gaps and pre-existing condition exclusions.
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE UNINSURED AND SELF-EMPLOYED (SNARE) — Cannot access affordable individual market coverage. The ESI system has crowded out individual market development, leaving non-employed populations to face unaffordable premiums, high deductibles, and exclusions. Trapped by cost and coverage gaps. Maximum extraction and maximum suppression — no alternative pathways exist at affordable cost.
constraint_indexing:constraint_classification(us_employer_health_insurance, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LARGE EMPLOYER (ROPE) — Experiences ESI as a coordination mechanism and tax arbitrage tool. Employer benefits: tax deduction for ESI premiums (employees see benefit as non-taxable income), recruiting advantage via health coverage, lock-in of workforce (reduced turnover). The coordination function is real — ESI solves the collective action problem of financing health coverage at scale. Exit options exist: can negotiate better rates, self-insure, or exit the system. Benefits exceed costs for institutional actors with scale.
constraint_indexing:constraint_classification(us_employer_health_insurance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED LABOR (TANGLED ROPE) — Unions negotiate ESI as a key benefit and have achieved coordination gains (better coverage, lower employee contributions). However, ESI negotiations also lock unions into defending a system that suppresses individual choice and ties workers to particular employers, reducing lateral mobility. Coordination function exists (collective bargaining for coverage), but significant extraction occurs: unions must spend bargaining capital on health coverage that could otherwise be reinvested in wages. Suppression is moderate — organized workers have some exit capacity through negotiation, but not full mobility.
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE HEALTHCARE INSURER (PITON) — The ESI system is largely performative for insurers from a civilizational view. The insurer's core function — pooling risk and managing claims — could operate through any mechanism (individual market, public system, direct employer contracting). Instead, ESI maintains an elaborate administrative theater: enrollment periods, benefits specialists, plan tiers, utilization review, network management. Most of this apparatus exists to preserve the employer-insurer intermediary relationship despite its inefficiencies. Theater ratio is high because the insurer's market position depends on institutional inertia (path dependency, tax code incentives) rather than functional superiority. The system persists through policy lock-in, not because it delivers better outcomes.
constraint_indexing:constraint_classification(us_employer_health_insurance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE PHARMACEUTICAL MANUFACTURER (TANGLED ROPE) — Benefits from ESI structure: employer-based insurance creates captive market with employers (purchasers) distant from end-users (patients), enabling higher pricing; list prices set without consumer price resistance. Also provides coordination function: employer group purchasing enables scale and standardization. However, recent threats (PBM negotiation, direct contracting, policy pressure on drug pricing) expose the extraction mechanism. Suppression is moderate — manufacturers face organized buyer pressure but retain market power through patent exclusivity and information asymmetry.
constraint_indexing:constraint_classification(us_employer_health_insurance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: THE PUBLIC POLICY COALITION (SCAFFOLD) — Progressive policy advocates, patient organizations, and healthcare reformers see ESI as a temporary coordination failure with a structural sunset. The Affordable Care Act (2010) and subsequent expansions of subsidized individual market coverage are building alternative pathways. As subsidized public options mature and individual market regulations tighten, the ESI lock-in mechanism loses force. The sunset is slow (decades) but structural — employers can offload workers to public exchanges as individual coverage becomes affordable. Theater is moderate because ESI's sunset is contested (many advocates argue for single-payer rather than ACA-model reform).
constraint_indexing:constraint_classification(us_employer_health_insurance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — The ESI system is often presented as an inevitable outcome of employer-employee relationships and tax incentives ('this is just how employment works'). The analytical observer risks naturalizing path dependency as structural law. However, the structural data contradicts this: ESI is contingent on specific policy choices (tax code treatment, wage freeze regulations, lack of portable individual market), not on immutable constraints. The engine will compute this as a false summit, revealing that naturalizing contingent institutional arrangements as universal laws is a common mechanism for legitimating extraction.
constraint_indexing:constraint_classification(us_employer_health_insurance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_employer_health_insurance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_employer_health_insurance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_employer_health_insurance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_employer_health_insurance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_employer_health_insurance, TR),
    TR >= 0.70.

:- end_tests(us_employer_health_insurance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The ESI system extracts significant value from dependent workers through suppressed bargaining power, reduced job mobility (lock-in), and shifting health risk to individuals. However, extraction is not total (0.70+) because: (1) legitimate coordination gains exist (pooling, scale economies, employer contributions do reduce individual burden relative to full-cost individual market), (2) some workers benefit from employer subsidies, (3) alternative exit pathways exist for some (switching employers, individual market for young/healthy). The 0.58 value reflects net extraction: benefits to employers/insurers exceed net harm to workers when accounting for coordination gains, but harm to trapped/uninsured populations is substantial. Suppression (0.72): High. Barriers to exit are severe: loss of job means loss of healthcare (absent COBRA), individual market coverage is expensive and excludes pre-existing conditions (pre-ACA era) or charges more (post-ACA), self-employed coverage is unaffordable, gap coverage during job transition is costly. However, not maximum (0.80+) because: (1) employer switching exists (though costly), (2) public programs (Medicaid, Medicare) provide partial alternative, (3) ACA individual market expansion reduces suppression for some demographics. Theater ratio (0.65): Moderate-high. Significant performative elements in ESI: enrollment periods and benefits counseling serve marketing function; plan tiers and network restrictions manage cream-skimming; utilization review provides cost-control theater without substantively improving outcomes; benefits administration overhead (30-40% of premiums in small groups) exceeds value delivery. However, theater is not dominant because coordination function is genuine — pooling and scale economies do reduce per-capita costs relative to individual full-price market. The theater has increased over time as administrative complexity has grown and actual insurance functions have been outsourced to PBMs and TPAs.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between dependent worker (Snare) and large employer (Rope) is maximal, reflecting fundamental structural asymmetry. The dependent worker experiences ESI as job-lock extraction with minimal alternatives. The large employer experiences ESI as advantageous coordination mechanism that reduces overall compensation costs and improves workforce retention. Both perspectives are structurally accurate from their positions; they disagree because their structural relationships to the constraint are opposite. The gap widens for workers in declining industries (fewer job-switching options) and narrows for workers in high-demand tech (more bargaining power). The piton perspective (insurer) reveals that much of ESI's persistence comes from administrative inertia rather than functional superiority — the insurance industry maintains the employer-intermediary relationship because it is entrenched, not because it delivers better outcomes than direct contracting or individual market alternatives. The scaffold perspective (policy coalition) correctly identifies that individual market expansion (ACA subsidies, regulatory protections) is eroding ESI's necessity by building alternatives at comparable or lower cost. The false mountain perspective reveals the rationalization mechanism: ESI is naturalized as inevitable ('this is just how employment works') despite being contingent on specific tax code provisions and policy choices. The analytical observer's job is to expose that naturalization and clarify the contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from agent power, exit options, and beneficiary/victim status. Dependent workers (powerless/trapped): d ≈ 0.95 (nearly full target of extraction, minimal alternatives). Self-employed (powerless/trapped but different market segment): d ≈ 0.90 (full target, but outside ESI system proper, so separated perspective). Large employers (institutional/arbitrage): d ≈ 0.05 (beneficiary with full exit options — can negotiate rates, self-insure, or migrate to other benefits structures). Insurers (institutional/constrained): d ≈ 0.25 (benefit from system but somewhat constrained by regulatory environment and employer bargaining power). Pharma manufacturers (powerful/mobile): d ≈ 0.10 (benefit from pricing power, but subject to downstream regulatory pressure). Organized labor (organized/constrained): d ≈ 0.45 (mixed — they negotiate benefits but are locked into defending system that constrains worker mobility). The analytical observer (analytical/analytical): d ≈ 0.72 (sees full structure, neither purely benefits nor purely bears costs, but risks false naturalization). The sigmoid f(d) transforms these d values into effective power modifiers that scale extractiveness χ = ε × f(d) × σ(S). For dependent workers, f(d) ≈ 1.42 (powerless multiplier), so experienced χ ≈ 0.58 × 1.42 × 1.0 ≈ 0.82 (high extraction). For large employers, f(d) ≈ -0.12 (institutional beneficiary), so experienced χ ≈ 0.58 × (-0.12) × 1.0 ≈ -0.07 (negative — they benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness 0.58, > 0.46): This constraint avoids the mandatrophy trap (misidentifying Snare as Rope) through explicit decomposition of coordination function versus extraction mechanism. Coordination genuine: ESI does solve collective action problems of healthcare financing at scale; employers can negotiate better terms than individuals, and pooling reduces per-capita administrative cost. Extraction genuine: worker mobility is suppressed, bargaining power is reduced, and health risk is shifted to individuals in ways that would not occur in alternative market structures (Swiss-style individual market with auto-enrollment, public option, single-payer). The mandatrophy is resolved by recognizing that both are real: ESI = coordination + extraction, not 'merely coordination' (Rope) and not 'merely extraction' (Snare alone). Classification as Snare from the dependent worker perspective and Rope from the employer perspective is correct; both perspectives are structurally accurate. The system persists because coordination gains accrue to large actors (employers, insurers) while extraction falls on smaller/weaker actors (individual workers, self-employed). If all actors experienced both coordination and extraction symmetrically, the system would be pure Rope. The asymmetry makes it Snare from the victim perspective. The analytical perspective must acknowledge both without collapsing into either false summitry (naturalizing as 'just how markets work') or false reductionism (treating as pure extraction with no coordination value).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_market_viability,
    'Can a subsidized individual insurance market with robust regulatory protections (ACA model) eventually crowd out ESI, or does ESI''s tax advantage constitute an irreducible structural lock-in?',
    'Longitudinal analysis of ESI enrollment trends as individual market subsidies expand; comparative analysis of countries that transitioned away from employment-based systems; modeling of equilibrium employer contribution incentives under various subsidy regimes',
    'If individual market can replace ESI: scaffold perspective correct, sunset is real, extractiveness declines over time. If tax advantages create irreducible lock-in: snare perspective dominates, structural change is blocked, extractiveness persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_market_viability, empirical, 'Whether individual market subsidies can replace ESI or if tax lock-in is structural').

omega_variable(
    extraction_vs_coordination_decomposition,
    'What fraction of ESI''s extractiveness (0.58) derives from legitimate coordination gains (pooling, scale economies) versus illegitimate job-lock mechanisms (loss-of-coverage threat, employer bargaining suppression)?',
    'Decomposition analysis: compare ESI coordination value to alternative delivery models (Swiss-style individual market with employer auto-enrollment, German multi-payer model, single-payer benchmarks); measure coordination gains against administrative overhead; analyze wage suppression attributable to health benefit dependency',
    'If coordination >> extraction: ESI classification shifts toward Rope/Tangled Rope with lower concern. If extraction >> coordination: confirms Snare classification and justifies policy reform prioritizing alternative pathways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, empirical, 'Decomposition of coordination gains from extraction mechanisms in ESI').

omega_variable(
    path_dependency_lock_mechanism,
    'Is ESI''s persistence primarily due to policy path dependency (sunk costs in tax code, regulatory infrastructure, employer practices) or due to genuine employer/insurer preference for ESI over alternatives?',
    'Counterfactual analysis: survey employer preferences in scenarios without tax deduction for ESI; analyze employer behavior when forced to internalize full ESI costs; compare to outcomes in countries without ESI tax incentives; measure switching costs for employers attempting to migrate to individual market contributions',
    'If path dependency dominates: policy reform (tax code change, portable benefits mandate) could shift equilibrium rapidly. If genuine preference: reform requires addressing underlying coordination needs, not just removing tax incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(path_dependency_lock_mechanism, empirical, 'Whether ESI persistence is driven by path dependency or genuine preference').

omega_variable(
    gig_economy_exit_dynamics,
    'Does growth of gig work and self-employment represent an exit from ESI-based extraction or a degradation into Snare territory (uninsured/underinsured populations bearing greater health risk)?',
    'Longitudinal analysis of insurance coverage rates for gig workers vs traditional employees; measurement of healthcare cost burden (as % of income) for self-employed vs ESI-covered; analysis of whether gig workers experience this as escape or as loss of coverage protection',
    'If exit: gig economy represents margin of Rope sustainability degrading. If degradation: gig economy expands Snare victim population without reducing ESI extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gig_economy_exit_dynamics, empirical, 'Whether gig economy growth represents exit from ESI or expansion of uninsured Snare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_employer_health_insurance, 1945, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esi_tr_t0, us_employer_health_insurance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(esi_tr_t35, us_employer_health_insurance, theater_ratio, 35, 0.58).
narrative_ontology:measurement(esi_tr_t70, us_employer_health_insurance, theater_ratio, 70, 0.65).

% Extraction over time
narrative_ontology:measurement(esi_be_t0, us_employer_health_insurance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(esi_be_t35, us_employer_health_insurance, base_extractiveness, 35, 0.52).
narrative_ontology:measurement(esi_be_t70, us_employer_health_insurance, base_extractiveness, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_employer_health_insurance, resource_allocation).
narrative_ontology:boltzmann_floor_override(us_employer_health_insurance, 0.35).
narrative_ontology:affects_constraint(us_employer_health_insurance, healthcare_cost_inflation).
narrative_ontology:affects_constraint(us_employer_health_insurance, job_mobility_suppression).
narrative_ontology:affects_constraint(us_employer_health_insurance, small_business_coverage_gaps).
narrative_ontology:affects_constraint(us_employer_health_insurance, occupational_licensing_lock_in).

% DUAL FORMULATION NOTE:
% The ESI system decomposes into multiple structurally distinct constraints in different domains. Healthcare cost inflation (ε ≈ 0.42) is partly downstream of ESI's insulation from price signals. Job mobility suppression (ε ≈ 0.65) is a direct consequence of health coverage lock-in. Small business coverage gaps (ε ≈ 0.72) emerge from ESI's scale economies that disadvantage small employers. Each story has its own extractiveness value reflecting domain-specific dynamics. The network links them through institutional coupling: ESI structure shapes healthcare markets, labor markets, and small business economics simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_employer_health_insurance, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
