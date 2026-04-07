% ============================================================================
% CONSTRAINT STORY: information_asymmetry_labor_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_labor_markets, []).

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
 *   constraint_id: information_asymmetry_labor_markets
 *   human_readable: Information Asymmetry in Labor Markets
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Information asymmetry in labor markets creates a structural tension
 *   between the legitimate coordination problem (employers cannot directly
 *   observe worker quality) and extractive credential gatekeeping
 *   (institutional actors capture rents by controlling the signals employers
 *   rely on). The constraint has intensified over recent decades as education
 *   costs have risen faster than wage growth, while simultaneously
 *   alternative credentialing pathways (bootcamps, online certifications,
 *   skills-based hiring) have emerged to challenge the traditional
 *   university-degree monopoly. This constraint exhibits all six DR types
 *   depending on the observer's structural position: it appears as pure
 *   extraction (snare) to workers without credentials, as mixed
 *   coordination-extraction (tangled rope) to credentialed workers and
 *   credential institutions, as pure coordination (rope) to employers who
 *   benefit from screening mechanisms, as a solvable temporary problem
 *   (scaffold) to alternative credentialing movements, as a degraded ritual
 *   (piton) to hiring managers who maintain degree requirements despite
 *   evidence they predict performance poorly, and as an immutable natural law
 *   (mountain) to analysts who treat information asymmetry as inherent to
 *   labor markets. The rising extractiveness trajectory (0.35 → 0.52)
 *   reflects credential inflation: the threshold credential requirement for
 *   entry-level positions has escalated, increasing extraction costs for
 *   workers. The theater ratio rise (0.48 → 0.64) indicates that credential
 *   screening has become increasingly performative as degree holders
 *   proliferate but actual job-relevant skills diverge from curriculum.
 *
 * KEY AGENTS:
 *   - Workers without credentials: Primary victims (powerless/trapped) — face wage suppression, job exclusion, and forced acceptance of lower-quality employment
 *   - Credentialed workers: Secondary victims (moderate/constrained) — benefit from signal value but bear extraction through escalating education costs and credential inflation
 *   - Employers: Primary beneficiaries (institutional/arbitrage) — solve screening problem cost-effectively through credential filtering; capture wage surplus from information advantage
 *   - Universities and professional bodies: Secondary beneficiaries (institutional/constrained) — extract tuition rents while providing genuine credentialing signal; constrained by accreditation requirements
 *   - Alternative credentialing providers: Organized challengers (organized/mobile) — building parallel verification systems with sunset logic; not yet fully replacing traditional credentials
 *   - Hiring managers and HR departments: Institutional actors maintaining piton (powerful/mobile) — persist with degree requirements through institutional inertia and risk aversion despite low predictive power
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing institutional credential arrangement as immutable feature of labor market functioning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_labor_markets, 0.52).
domain_priors:suppression_score(information_asymmetry_labor_markets, 0.58).
domain_priors:theater_ratio(information_asymmetry_labor_markets, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_labor_markets, extractiveness, 0.52).
narrative_ontology:constraint_metric(information_asymmetry_labor_markets, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(information_asymmetry_labor_markets, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_labor_markets, tangled_rope).
narrative_ontology:human_readable(information_asymmetry_labor_markets, "Information Asymmetry in Labor Markets").
narrative_ontology:topic_domain(information_asymmetry_labor_markets, "economic/labor").

domain_priors:requires_active_enforcement(information_asymmetry_labor_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_labor_markets, employers).
narrative_ontology:constraint_beneficiary(information_asymmetry_labor_markets, credential_gatekeepers).
narrative_ontology:constraint_victim(information_asymmetry_labor_markets, job_seekers).
narrative_ontology:constraint_victim(information_asymmetry_labor_markets, workers_without_signals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNSCREENED WORKER (SNARE) — Workers without elite credentials or social networks face systematic extraction through wage suppression and exclusion from high-opportunity positions. No exit option: alternative labor markets (gig economy, informal sector) offer lower pay and worse conditions. Employers capture information advantage completely. Maximum experienced extraction.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CREDENTIALED WORKER (TANGLED ROPE) — Workers with recognized credentials experience the constraint as mixed: it coordinates efficient matching (employers can trust degree signals) AND extracts through credential cost and credential inflation requirements. Some agency through credential switching costs but meaningful extraction through necessary education expense. Both coordination and asymmetric extraction.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Experiences the constraint as pure coordination: information asymmetry is solved by credential screening, reference checks, and interview protocols. These mechanisms enable matching of worker skills to job requirements with manageable screening costs. Net beneficiary with optionality to adjust screening intensity. Low experienced extraction.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIAL INSTITUTION (TANGLED ROPE) — Universities and professional certifying bodies benefit from information asymmetry (extraction rent through tuition and credentialing fees) while providing genuine coordination (signal of competence). Constrained by regulatory requirements and accreditation standards. Active enforcement through credential gatekeeping. Mixed coordination and extraction.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE CREDENTIALING COALITION (SCAFFOLD) — Bootcamps, online certifications (Coursera, edX), skills-based hiring, and portfolio assessment represent organized attempts to bypass traditional credential gatekeeping. These create parallel verification pathways with lower cost. Sunset logic: as employer norms shift toward skills assessment (GitHub portfolios, project work, demonstrated output), the traditional credential monopoly weakens. Estimated sunset: 15-25 years for complete norm shift.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DEGREE-OBSESSED HIRING MANAGER (PITON) — Many employers continue credential-only screening (degree required even for roles where skills matter more) despite evidence that credentials poorly predict actual job performance. The ritual persists through institutional inertia: HR departments maintain degree requirements because alternatives require policy change and risk management. Theater ratio high because screening theater (interview panels, reference checks) substitutes for actual skill assessment. The constraint is maintained by performative institutional behavior rather than functional necessity.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry in labor markets appears as an immutable feature: employers can never perfectly observe worker quality, so screening mechanisms are inherently necessary. Verification lag between credential and actual performance is fundamental to how labor markets function. However, the structural data contradicts this naturalization — information asymmetry is amplified by credential gatekeeping policies and hiring norms, not determined by physics. The mountain classification is a false summit revealing how institutional arrangements masquerade as natural limits.
constraint_indexing:constraint_classification(information_asymmetry_labor_markets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_labor_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_labor_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_labor_markets, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_labor_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_labor_markets, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_labor_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Information asymmetry creates genuine coordination value (employers need screening mechanisms) but extraction is substantial and rising. The base value reflects: (1) wage suppression from unequal information (employers capture surplus), (2) credential inflation (workers must overeducate to meet arbitrary thresholds), (3) credential gatekeeping rents (universities extract tuition revenue), and (4) exclusion of non-credentialed workers from opportunity. The rising trajectory indicates escalating credential requirements beyond actual job-relevant skill levels. Suppression (0.58): Moderate-high. Workers face multiple barriers: education costs create financial dependency on credential institutions, social networks concentrate credential access by class/race, alternative labor markets offer worse conditions (wage lower, hours longer, benefits absent), career path penalties for credential gaps, and institutional inertia locks employers into degree-only screening. Suppression is not total because some alternative credentialing exists and some employers are shifting to skills-based hiring. Theater ratio (0.64): Moderate-high. Screening mechanisms have significant performative content: interview panels assess personality/cultural fit more than job-relevant skills, reference checks are predictable rather than informative, degree requirements persist for roles where on-the-job training would be equal/better, and hiring criteria diverge from actual job performance metrics. Theater has increased as credential inflation widened gap between degree signals and demonstrated skills.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence across power positions. The unscreened worker sees pure extraction (snare) — they are systematically disadvantaged with no exit. The credentialed worker sees mixed extraction-coordination (tangled rope) — credentials solved their information problem but at high cost. The employer sees pure coordination (rope) — credential screening is their cost-effective solution to a real problem. The credential institution sees mixed (tangled rope) — they coordinate educational signaling while extracting tuition rents. The alternative credentialing coalition sees a temporary problem with a solution path (scaffold) — employer norms are shifting toward skills-based hiring. The hiring manager who insists on degrees despite irrelevance sees only ritual (piton) — the degree requirement persists through institutional habit. The analytical observer who believes information asymmetry is inherent risks naturalizing a contingent institutional arrangement (false summit mountain). The gap is sharpest between the powerless worker (snare) and the employer (rope): identical structural phenomenon experienced as maximum extraction vs minimal extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to information extraction flow. Unscreened workers face maximum directionality (d ≈ 0.95): trapped exit + full victim status → f(d) ≈ 1.42 → high experienced χ. Credentialed workers face intermediate directionality (d ≈ 0.55): constrained exit + partial victim status (also benefit from credential signal) → f(d) ≈ 0.75 → moderate χ. Employers face low directionality (d ≈ 0.15): arbitrage exit + full beneficiary status → f(d) ≈ -0.01 → negative/minimal χ (they experience the constraint as coordination solution, not extraction). Credential institutions face low-intermediate directionality (d ≈ 0.35): constrained exit (regulatory requirements) + mixed beneficiary/victim (extract tuition but provide genuine signal) → f(d) ≈ 0.30 → moderate χ. Alternative credentialing coalition faces intermediate directionality (d ≈ 0.50): mobile exit + neither pure beneficiary nor victim (external challengers) → f(d) ≈ 0.65 → moderate χ. The piton perspective derives from theater_ratio (0.64) exceeding the gate threshold (0.50), not from high χ. The mountain perspective is classified as false summit by the engine because accessibility_collapse will be low (credential monopoly is not inescapable; alternative pathways exist) and resistance will be moderate (institutional forces defend credentials, not laws of nature).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint's mandatrophy is resolved by recognizing that information asymmetry has both genuine coordination function (screening heterogeneous worker quality) and genuine extraction function (credential gatekeeping rents, wage suppression through information monopoly). The classification as tangled rope (not pure rope or pure snare) captures this hybrid. The mandatrophy would arise if an analyst tried to classify this as pure rope (all coordination, no extraction) or pure snare (all extraction, no coordination). The data prevents this misclassification: beneficiaries exist (employers, credential institutions) showing coordination function exists; victims exist (unscreened workers, credentialed workers bearing education costs) showing extraction exists; active enforcement is required (credential gatekeeping must be continuously defended against alternative pathways). The perspectival gap between employer (rope) and unscreened worker (snare) is not an error — it reflects real structural difference: employers genuinely benefit from the coordination solution, while excluded workers genuinely suffer the extraction. Both perspectives are locally accurate from their structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_signal_quality,
    'What proportion of wage differences attributed to information asymmetry actually reflects credential signal quality versus credential gatekeeping extraction?',
    'Comparative analysis of wage premiums for credentialed vs non-credentialed workers in roles with measurable output (sales commissions, project management); correlation between credential specificity and actual job performance metrics',
    'If signal quality dominates: credentialing is primarily coordination, extractiveness lower. If gatekeeping dominates: credentialing is primarily extraction, extractiveness higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signal_quality, empirical, 'Proportion of wage differences from signal quality vs gatekeeping extraction').

omega_variable(
    alternative_screening_effectiveness,
    'Do skills-based hiring, portfolio assessment, and work trials successfully identify high-performing workers at rates comparable to credential screening?',
    'Longitudinal performance tracking for workers hired via credential screening vs alternative pathways; retention rates, productivity metrics, promotion velocity; error rates (false positives and false negatives) across screening mechanisms',
    'If alternative screening equally effective: scaffold sunset is achievable, constraint can shift toward lower extractiveness. If significantly worse: credential gatekeeping has genuine functional value despite high theater ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_screening_effectiveness, empirical, 'Effectiveness of alternative screening mechanisms compared to credentials').

omega_variable(
    network_effects_credential_monopoly,
    'To what extent does credential value persist due to network effects (everyone wants degrees because employers want degrees) versus genuine signal quality?',
    'Controlled experiments in hiring: identical candidates presented with/without degrees; comparison of employer decision patterns across industries with different credential norms; analysis of credential demand elasticity to signals of actual skills',
    'If primarily network effects: credential requirement is coordination failure (all-pay auction dynamic), easily disrupted by norm shift. If partly genuine signal: alternative credentials will struggle to fully displace traditional ones.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_credential_monopoly, empirical, 'Network effects versus genuine signal quality in credential value').

omega_variable(
    information_asymmetry_measurement,
    'How is information asymmetry magnitude estimated? Is it measured as wage variance attributable to unobserved worker quality, or as the cost of screening mechanisms, or as credential inflation premium?',
    'Meta-analysis of labor economics literature; mapping of different measurement methodologies to extractiveness values; sensitivity analysis of base_properties.extractiveness to measurement choice',
    'If extractiveness value depends heavily on measurement methodology, this is a false constraint (observable-dependent) and should be decomposed into multiple stories per ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_measurement, conceptual, 'Measurement methodology dependence of information asymmetry magnitude').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_labor_markets, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infoasym_tr_t0, information_asymmetry_labor_markets, theater_ratio, 0, 0.48).
narrative_ontology:measurement(infoasym_tr_t10, information_asymmetry_labor_markets, theater_ratio, 10, 0.58).
narrative_ontology:measurement(infoasym_tr_t20, information_asymmetry_labor_markets, theater_ratio, 20, 0.64).
narrative_ontology:measurement(infoasym_tr_t30, information_asymmetry_labor_markets, theater_ratio, 30, 0.59).

% Extraction over time
narrative_ontology:measurement(infoasym_be_t0, information_asymmetry_labor_markets, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(infoasym_be_t10, information_asymmetry_labor_markets, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(infoasym_be_t20, information_asymmetry_labor_markets, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(infoasym_be_t30, information_asymmetry_labor_markets, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_labor_markets, information_standard).
narrative_ontology:boltzmann_floor_override(information_asymmetry_labor_markets, 0.18).
narrative_ontology:affects_constraint(information_asymmetry_labor_markets, credential_inflation).
narrative_ontology:affects_constraint(information_asymmetry_labor_markets, social_capital_concentration).
narrative_ontology:affects_constraint(information_asymmetry_labor_markets, wage_inequality_structural).

% DUAL FORMULATION NOTE:
% Information asymmetry in labor markets decomposes into multiple structurally distinct constraints. This story covers the screening mechanism constraint (extractiveness primarily from credential gatekeeping and information monopoly). Upstream constraint credential_inflation (extractiveness from escalating education cost requirements) and social_capital_concentration (extractiveness from unequal access to networks and signaling opportunities) are separate stories with their own ε values. All three are linked by affects_constraints to show institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_asymmetry_labor_markets, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
