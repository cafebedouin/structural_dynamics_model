% ============================================================================
% CONSTRAINT STORY: data_asymmetry_labor_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_asymmetry_labor_markets, []).

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
 *   constraint_id: data_asymmetry_labor_markets
 *   human_readable: Information Asymmetry in Labor Markets
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Information asymmetry in labor markets creates a structural tension
 *   between the legitimate coordination problem of matching workers to roles
 *   and the extractive incentive of employers to suppress wage information
 *   and control offer timing. Workers lack access to internal salary bands,
 *   performance evaluation criteria, promotion pathways, and competing offers
 *   until late in hiring processes. Employers control information disclosure
 *   timing and content, creating systematic wage suppression and power
 *   imbalance. The constraint exhibits characteristics of both pure
 *   extraction (snare) for powerless workers and mixed
 *   coordination-extraction (tangled rope) for organized labor and mobile
 *   workers. The emergence of transparency mandates (California, NYC, EU
 *   salary disclosure rules) represents an organized challenge to the
 *   constraint's suppression mechanism, creating a scaffold perspective with
 *   explicit sunset logic. The deep cultural norm against discussing
 *   compensation (piton perspective) persists despite being organizationally
 *   inertial, maintained through enforcement ritual rather than functional
 *   necessity. From a civilizational scale, information asymmetry appears
 *   inherent to labor matching (mountain perspective), but empirical
 *   variation across jurisdictions reveals this as false summit — the
 *   constraint is structurally contingent, not natural law.
 *
 * KEY AGENTS:
 *   - Job Seekers: Primary victim (powerless/trapped) — lack access to salary data, internal metrics, and competing offers. Cannot exit labor market without existential risk.
 *   - Employers: Primary beneficiary (institutional/arbitrage) — control information disclosure and extract wage suppression. Can arbitrage between jurisdictions and labor markets.
 *   - Recruitment Platforms: Secondary beneficiary (institutional/arbitrage) — extract value through access fees and data asymmetry. Control candidate/employer visibility.
 *   - Labor Organizing Coalition: Organized victim (organized/constrained) — push for transparency mandates; constrained by employer resistance and legal barriers.
 *   - High-Skill Workers: Mobile partial victim (powerful/mobile) — have alternative information channels and exit options; still operate within asymmetry but less trapped.
 *   - Transparency Mandate Coalition: Organized solver (organized/constrained) — legislatures, labor departments imposing salary disclosure requirements with explicit sunset horizon.
 *   - Confidentiality Norm: Institutional enforcement mechanism (institutional/arbitrage) — cultural rule against discussing compensation persists through enforcement ritual.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent to labor economics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_asymmetry_labor_markets, 0.58).
domain_priors:suppression_score(data_asymmetry_labor_markets, 0.62).
domain_priors:theater_ratio(data_asymmetry_labor_markets, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_asymmetry_labor_markets, extractiveness, 0.58).
narrative_ontology:constraint_metric(data_asymmetry_labor_markets, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(data_asymmetry_labor_markets, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_asymmetry_labor_markets, tangled_rope).
narrative_ontology:human_readable(data_asymmetry_labor_markets, "Information Asymmetry in Labor Markets").
narrative_ontology:topic_domain(data_asymmetry_labor_markets, "economic/labor").

domain_priors:requires_active_enforcement(data_asymmetry_labor_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_asymmetry_labor_markets, employers).
narrative_ontology:constraint_beneficiary(data_asymmetry_labor_markets, recruitment_platforms).
narrative_ontology:constraint_victim(data_asymmetry_labor_markets, job_seekers).
narrative_ontology:constraint_victim(data_asymmetry_labor_markets, wage_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOB SEEKER (SNARE) — Trapped in asymmetric information environment. Lacks access to internal salary data, performance metrics, promotion pathways, and competing offers until late in hiring process. Cannot walk away from labor market entirely without existential risk. Experiences maximum extraction through wage suppression and information disadvantage.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR COALITION (TANGLED ROPE) — Organized agents (unions, worker advocacy groups) perceive genuine coordination function: matching workers to roles requires some information flow. But also perceive asymmetric extraction: employers control what information flows and when. Can organize to demand salary transparency, but face employer resistance and legal/contractual barriers to information sharing.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Net beneficiary of information asymmetry. Controls information disclosure timing and content. Experiences the constraint as coordination: job descriptions, interviews, and reference checking serve legitimate matching function. Can arbitrage between labor markets (hiring remotely, relocating positions) if transparency pressures rise in one jurisdiction.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HIGH-SKILL WORKER (TANGLED ROPE) — Mobile labor with access to internal networks, headhunters, and competing offers. Still operates within information asymmetry but has exit options and alternative information channels. Experiences constraint as less extractive than powerless workers but still present — cannot fully observe internal salary bands or cultural fit metrics until embedded.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RECRUITMENT PLATFORM (ROPE) — Coordinates matching between workers and employers. Extracts value through access fees, featured listing, and data analytics. Benefits from information asymmetry (platforms control what candidate and employer data is visible). Can arbitrage between labor markets and shift platform rules if transparency pressures rise.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TRANSPARENCY MANDATE COALITION (SCAFFOLD) — Organized agents (labor departments, legislatures, worker advocates) imposing salary transparency requirements (NYC, California, EU models). See constraint as temporary and solvable: legal mandates to disclose salary ranges in job postings create alternative information pathways. Extract low chi because sunset is explicit — regulations creating sunset with 5-10 year compliance horizon.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CONFIDENTIALITY NORM (PITON) — Deep institutional norm that salary discussion is private/taboo persists despite being organizationally inertial. The norm served coordination function when labor markets were local and repeated transactions created incentive alignment. Now largely performative: confidentiality persists through cultural enforcement despite worker mobility and impersonal hiring. Theater ratio high because enforcement ritual ("don't discuss compensation") exceeds functional benefit.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, information asymmetry appears inherent to all market matching: one party always has more information about their own quality/constraints/alternatives. Hiring is fundamentally about incomplete information. This view naturalizes the contingent institutional arrangements (nondisclosure, timing control) as laws of labor economics. However, structural data contradicts mountain classification — empirical variation across jurisdictions and time periods reveals this as false summit.
constraint_indexing:constraint_classification(data_asymmetry_labor_markets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_asymmetry_labor_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_asymmetry_labor_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_asymmetry_labor_markets, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_asymmetry_labor_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_asymmetry_labor_markets, TR),
    TR >= 0.70.

:- end_tests(data_asymmetry_labor_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated. Wage suppression through information control is systematic and affects most labor market participants. However, value is below 0.70 (snare threshold) because: (1) employers derive real coordination benefits from some information asymmetry (assessing job fit requires information workers cannot provide about themselves), and (2) empirical variation shows asymmetry can be reduced through policy without destroying labor matching. The measurement trajectory shows increasing extractiveness over the interval (0.42→0.58) reflecting intensification of information concentration through platform dominance and globalization, even as transparency mandates emerge. Suppression (0.62): Moderate-high. Significant barriers to reducing asymmetry include: cultural norms against salary discussion, legal restrictions on information sharing, contractual confidentiality clauses, and employer collective action against disclosure. But suppression is not total — worker networks, glassdoor reviews, legislation, and union organizing provide alternative information channels and create exit pressure. Theater ratio (0.55): Moderate. The hiring interview ritual includes genuine coordination (assessing fit, communication, cultural alignment) but also performative gatekeeping. Reference checking, behavioral assessments, and certification requirements serve both coordination and suppression functions. Theater increased slightly (0.48→0.55) as companies developed more elaborate interview processes that exceed what job performance prediction studies justify.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence across power positions. Powerless job seekers experience pure extraction (snare) — the constraint offers them no exit and maximum harm. Organized labor experiences mixed coordination and extraction (tangled rope) — they recognize the legitimate matching function while identifying systematic extraction. Employers experience coordination (rope) — their perspective centers on legitimate hiring needs while suppressing awareness of their information advantage. High-skill workers experience moderate extraction (tangled rope) — they have some information channels and alternatives but still operate within the asymmetry. Transparency mandate advocates experience a solvable problem (scaffold) — the constraint is temporary and has an explicit sunset path through legislation. The confidentiality norm enforcement sees the constraint as natural and necessary (piton with high theater). The civilizational analyst risks seeing immutable law (mountain) — 'information asymmetry is inherent to hiring' — but the structural data reveals this as false summit: the specific institutional arrangements (wage secrecy, timing control, reference asymmetry) are contingent, not natural.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent structural position. Job seekers are trapped victims: d ≈ 0.95, experiencing maximum f(d) ≈ 1.42, amplified by scope σ(national=1.0). Employers are arbitrage beneficiaries: d ≈ 0.05, experiencing negative f(d) ≈ -0.12, extraction runs toward them. Organized labor at constrained exits: d ≈ 0.60 (between victim and beneficiary, asymmetrically burdened), f(d) ≈ 0.85. High-skill mobile workers: d ≈ 0.45 (more balanced than powerless), f(d) ≈ 0.50. Platforms at institutional/arbitrage: d ≈ 0.10 (beneficiaries but less central than employers), f(d) ≈ -0.05. The transparency mandate coalition operates from outside the direct extraction flow, seeking to reset the constraint structure — d ≈ 0.50 (neutrally positioned relative to the extraction, but constrained by employer resistance). The directional asymmetry is sharp: the constraint concentrates extraction on the most powerless and least mobile agents while distributing benefits to the most institutional and most mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through the scaffold perspective and temporal measurement trajectory. The constraint exhibits genuine coordination function (job matching requires information flow — employers need to assess fit, workers need to assess opportunities). But it also exhibits asymmetric extraction (information control suppresses wages and worker agency). The tangled rope classification at the base level correctly identifies both functions. The snare classification from the powerless worker perspective reveals the extraction dominates their experience. The scaffold classification from the transparency mandate perspective reveals the constraint is solvable through institutional means (salary disclosure laws, union organizing, platform transparency requirements). The temporal data shows extractiveness increasing (0.42→0.58) despite transparency mandate emergence, suggesting the constraint is adapting rather than dissolving — information control shifts from wages to benefits, performance metrics, and algorithmic ranking. The scaffold has a real sunset if transparency mandates achieve sufficient adoption and enforce-ability; but the unresolved omega variable (whether mandates simply shift extraction to alternative channels) indicates the sunset may be partial. The piton classification of the confidentiality norm reveals the constraint is partly maintained through institutional inertia — the norm serves enforcement function but has largely lost coordination necessity. False mountain detection at the analytical level confirms the constraint is contingent institutional arrangement, not natural law of labor economics — empirical variation and legislative solutions prove alternatives exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_transparency_counterfactual,
    'Would complete wage transparency reduce or increase overall wage inequality and labor market efficiency?',
    'Comparative analysis of jurisdictions with salary disclosure mandates (California, NYC, EU) vs without; measurement of wage compression, hiring velocity, and labor market participation rates pre/post transparency legislation',
    'If reduces inequality: constraint is primarily extractive (snare). If increases inequality or reduces efficiency: constraint includes genuine coordination function (tangled_rope). If neutral: constraint is pure coordination with theatrical suppression (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_transparency_counterfactual, empirical, 'Whether wage transparency reduces or increases inequality').

omega_variable(
    employer_information_necessity,
    'How much information asymmetry is structurally necessary for employers to make hiring decisions versus how much is extractive rent-seeking?',
    'Analysis of interview process: (a) information employers require to assess job fit vs (b) information employers extract to reduce competing offers and suppress negotiations. Measurement of correlation between interview depth and job performance prediction.',
    'High ratio of necessary/extractive: constraint is tangled rope (coordination + extraction). Low ratio: constraint is snare with theatrical coordination claims. If dominated by performance prediction: reveals coordination function is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_information_necessity, empirical, 'Proportion of information asymmetry that is structurally necessary').

omega_variable(
    platform_aggregation_effects,
    'Do recruitment platforms reduce overall information asymmetry (by aggregating job postings and candidate profiles) or amplify it (by controlling information visibility and ranking)?',
    'Measurement of information accessibility: time to job match, offer acceptance rates, wage compression, hiring velocity with vs without platforms. Analysis of platform algorithmic ranking and recommendation bias.',
    'If platforms reduce asymmetry: classification shifts toward Rope for platform perspective. If amplify: platform becomes primary extractor (snare victim becomes recruitment platform victim). If neutral: platform is piton (theatrical matching with degraded function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_aggregation_effects, empirical, 'Whether platforms reduce or amplify information asymmetry').

omega_variable(
    reference_checking_asymmetry,
    'How much of employers'' information advantage derives from asymmetric reference checking (they check worker history; workers rarely check employer history) versus wage nondisclosure?',
    'Decomposition of information asymmetry sources: survey of worker access to employer reviews/glassdoor vs employer access to background checks/references. Measurement of wage offer variance explained by unobservable employer quality vs worker negotiation suppression.',
    'If high proportion from reference asymmetry: constraint partially resolves through worker access to employer reputation (shifts piton/snare to rope). If dominated by wage secrecy: suppression mechanism is primary and requires explicit mandate for resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reference_checking_asymmetry, empirical, 'Proportion of asymmetry from reference checking vs wage secrecy').

omega_variable(
    mandatory_disclosure_unintended_effects,
    'Do salary transparency mandates produce unintended extraction through alternative channels (non-salary benefits, relocation requirements, performance metrics as proxies)?',
    'Post-mandate analysis: measure whether wage suppression is replaced by benefits suppression, increased geographic requirements, higher performance uncertainty, or other compensating mechanisms. Longitudinal tracking of total compensation inequality before/after mandate.',
    'If extraction simply shifts: mandatrophy fails and constraint remains tangled rope at lower ε but same χ. If extraction genuinely reduced: mandate is successful scaffold with real sunset. If extraction increases: unintended consequences reveal structure (constraint moves toward snare with more sophisticated extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatory_disclosure_unintended_effects, empirical, 'Whether transparency mandates shift extraction to alternative channels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_asymmetry_labor_markets, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(datalm_tr_t0, data_asymmetry_labor_markets, theater_ratio, 0, 0.48).
narrative_ontology:measurement(datalm_tr_t5, data_asymmetry_labor_markets, theater_ratio, 5, 0.52).
narrative_ontology:measurement(datalm_tr_t10, data_asymmetry_labor_markets, theater_ratio, 10, 0.55).
narrative_ontology:measurement(datalm_tr_t15, data_asymmetry_labor_markets, theater_ratio, 15, 0.54).

% Extraction over time
narrative_ontology:measurement(datalm_be_t0, data_asymmetry_labor_markets, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(datalm_be_t5, data_asymmetry_labor_markets, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(datalm_be_t10, data_asymmetry_labor_markets, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(datalm_be_t15, data_asymmetry_labor_markets, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_asymmetry_labor_markets, resource_allocation).
narrative_ontology:affects_constraint(data_asymmetry_labor_markets, wage_suppression_mechanisms).
narrative_ontology:affects_constraint(data_asymmetry_labor_markets, worker_mobility_barriers).
narrative_ontology:affects_constraint(data_asymmetry_labor_markets, employment_contract_lock_in).

% DUAL FORMULATION NOTE:
% Data asymmetry in labor markets is upstream of wage suppression mechanisms and worker mobility constraints. Separate stories should decompose: (1) wage secrecy-specific extraction (ε≈0.52, snare/tangled rope), (2) performance metric opacity (ε≈0.48, tangled rope), (3) internal mobility barriers from hidden promotion criteria (ε≈0.55, tangled rope). Each story has different ε and different exit options for affected agents. The family is linked through platform aggregation (recruitment platforms amplify all three).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_asymmetry_labor_markets, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
