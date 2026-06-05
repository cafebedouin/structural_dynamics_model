% ============================================================================
% CONSTRAINT STORY: educational_credential_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_educational_credential_inflation, []).

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
 *   constraint_id: educational_credential_inflation
 *   human_readable: Educational Credential Inflation
 *   domain: economic/labor_market/social
 *
 * SUMMARY:
 *   Educational credential inflation is a labor market constraint where
 *   rising entry-level educational requirements systematically exceed
 *   improvements in job-related competency demands. The mechanism operates as
 *   follows: competing employers raise credential thresholds to signal
 *   quality without coordinating on outcomes; educational institutions expand
 *   credentialing programs in response to demand; established workers benefit
 *   from credential scarcity; new entrants bear compounding costs (tuition,
 *   time, opportunity cost) to access positions that once required fewer
 *   credentials; credentialing bodies profit from expanded enrollment. The
 *   constraint exhibits partial coordination function (credentials do signal
 *   trainability and baseline capability) mixed with substantial extraction
 *   (much credentialing is performative, credentials depreciate as they
 *   inflate, and costs concentrate on entrants with fewest alternatives).
 *   Theater ratio (0.68) reflects that employers often demand credentials not
 *   because they predict performance but because competitors demand them,
 *   creating a ritual that persists despite contestable utility. Over the
 *   20-year interval measured (roughly 2005-2025), extractiveness increased
 *   from 0.32 to 0.58 as credential requirements proliferated and alternative
 *   pathways remained marginal. Theater ratio increased from 0.42 to 0.68 as
 *   educational institutions optimized for enrollment metrics rather than
 *   learning outcomes, and as employer credential demands diverged visibly
 *   from actual job requirements. Alternative credentialing pathways
 *   (bootcamps, competency-based hiring, apprenticeships) represent a sunset
 *   mechanism, but adoption remains constrained by incumbent institutional
 *   resistance and reputational risk aversion.
 *
 * KEY AGENTS:
 *   - New Labor Market Entrants: Primary victims (powerless/trapped) — forced to acquire credentials not required by task requirements; no alternative pathways; bear full opportunity cost
 *   - Mid-Career Incumbents: Secondary victims and partial beneficiaries (moderate/constrained) — benefit from established credentials' continued relevance but face risk of credential devaluation requiring additional education to maintain position
 *   - Educational Institutions: Primary beneficiaries (institutional/arbitrage) — expand enrollment in response to credential demand without improving educational outcomes; extract revenue through tuition inflation
 *   - Elite Credential Gatekeepers: Power beneficiaries (powerful/mobile) — maintain credential scarcity and prestige concentration; extract disproportionate tuition and market value as competition intensifies
 *   - Employers (Credentialing Demand Signal): Distributed institutional actors (institutional/constrained) — maintain credential requirements largely for signaling purposes; locked into escalation by competitive dynamics and reputational risk aversion
 *   - Alternative Credentialing Movement: Organized challengers (organized/constrained) — building substitutes (bootcamps, portfolios, apprenticeships) with sunset logic; constrained by employer skepticism and incumbent resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing escalation as inherent to competitive markets rather than recognizing it as a coordination failure sustained by institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(educational_credential_inflation, 0.58).
domain_priors:suppression_score(educational_credential_inflation, 0.65).
domain_priors:theater_ratio(educational_credential_inflation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(educational_credential_inflation, extractiveness, 0.58).
narrative_ontology:constraint_metric(educational_credential_inflation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(educational_credential_inflation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(educational_credential_inflation, tangled_rope).
narrative_ontology:human_readable(educational_credential_inflation, "Educational Credential Inflation").
narrative_ontology:topic_domain(educational_credential_inflation, "economic/labor_market/social").

domain_priors:requires_active_enforcement(educational_credential_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(educational_credential_inflation, educational_institutions).
narrative_ontology:constraint_beneficiary(educational_credential_inflation, credentialing_bodies).
narrative_ontology:constraint_beneficiary(educational_credential_inflation, high_credential_holders).
narrative_ontology:constraint_victim(educational_credential_inflation, new_labor_market_entrants).
narrative_ontology:constraint_victim(educational_credential_inflation, credential_devaluation_cohorts).
narrative_ontology:constraint_victim(educational_credential_inflation, opportunity_cost_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW LABOR MARKET ENTRANT (SNARE) — Trapped by the credential arms race. Entry into competitive fields requires credentials that did not exist a generation ago. Cannot exit: refusing credentialing forecloses entire occupational pathways. Bears full cost of credential inflation through time, tuition, and opportunity cost while experiencing no coordination benefit. No alternatives; no recourse.
constraint_indexing:constraint_classification(educational_credential_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER INCUMBENT (TANGLED ROPE) — Constrained by credential lock-in. Holds degrees that were sufficient for current role but sees the field raising entry requirements continuously. Benefits from their own established credential's durability but bears extraction through: (a) risk that their legacy credential becomes devalued, requiring additional credentials to defend position; (b) having to manage or mentor credential-inflated new hires who require more training despite higher credentials. Genuine coordination function (credential signaling) mixed with asymmetric extraction (established workers extract value from their credential scarcity, new workers bear cost of credential abundance).
constraint_indexing:constraint_classification(educational_credential_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTIONS (ROPE) — Primary beneficiary (institutional/arbitrage). Experiences credential inflation as pure coordination: students need credentials, institutions provide them. Revenue expands with enrollment demand without requiring institutions to improve service quality or educational outcomes. Institutions have arbitrage options: they can exit (close programs, reduce enrollment) but face no pressure to do so because demand remains high despite diminishing marginal returns to credentials. The constraint serves genuine coordination function (credentialing) from this perspective.
constraint_indexing:constraint_classification(educational_credential_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ELITE CREDENTIAL GATEKEEPERS (TANGLED ROPE) — Powerful actors (prestigious universities, elite professional certifications) see the constraint as coordination with extraction. They coordinate the market for signals (prestigious credentials remain scarce and valuable) while extracting disproportionate returns from credential inflation. As competition intensifies, prestige concentration deepens — elite institutions extract premium tuition and enrollment while mass-market institutions see their credentials depreciate. Mobile at global scope (can relocate recruiting, move resources) but constrained by institutional identity and reputation lock-in at national/regional scope.
constraint_indexing:constraint_classification(educational_credential_inflation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE CREDENTIALING RITUAL (PITON) — The constraint's theater component is exceptionally high. Much credentialing is performative: employers demand credentials largely because competitors demand them, not because credentials predict job performance. Educational institutions teach toward assessment metrics rather than toward skill acquisition. The ritual persists through inertia — removing credential requirements would signal weakness to competitors and investors, even though credentials' actual predictive validity for most roles is contested. Theater ratio (0.68) reflects this performative maintenance.
constraint_indexing:constraint_classification(educational_credential_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALING MOVEMENT (SCAFFOLD) — Organized agents (bootcamps, competency-based certifications, apprenticeship programs, portfolio-based hiring) are building alternative verification pathways that bypass traditional degree requirements. These alternatives represent a sunset mechanism: as employers verify hiring outcomes through direct assessment rather than degree requirements, the traditional credential's extraction mechanism weakens. Constrained by employer skepticism and existing credential inertia, but have exit paths (niche market dominance, sector-specific adoption). Sunset clause rationale: competency-based hiring and portfolio assessment are gradually replacing credentialing as labor market signals in tech, trades, and creative industries. Estimated sunset: 15-25 years for alternatives to establish legitimacy across sectors.
constraint_indexing:constraint_classification(educational_credential_inflation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, credential inflation may appear as an immutable property of competitive labor markets: if all competitors raise the bar, none can unilaterally lower it without disadvantage. This perspective naturalizes the constraint as a prisoners' dilemma inherent to signaling dynamics. However, the structural data contradicts the mountain classification — the constraint requires active enforcement (institutions maintaining credential requirements despite declining marginal value) and exhibits high theater (performative credentialing). The mountain framing disguises what is actually a contingent institutional arrangement sustained by coordination failure and reputational risk aversion.
constraint_indexing:constraint_classification(educational_credential_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(educational_credential_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(educational_credential_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(educational_credential_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(educational_credential_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(educational_credential_inflation, TR),
    TR >= 0.70.

:- end_tests(educational_credential_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits genuine coordination function (credentials signal trainability, employers can assess baseline capability through formal qualifications, educational institutions serve a real social role). However, this is substantially overwhelmed by extraction mechanisms: (1) Credential inflation outpaces task requirement changes — many positions now demand degrees not demonstrably required for competent performance; (2) Employers demand credentials for signaling and competitive reasons, not because credentials predict performance; (3) Educational institutions expand programs driven by enrollment metrics, not learning outcomes; (4) Costs concentrate on new entrants with fewest alternatives and highest time sensitivity; (5) Credential depreciation is systematic — as inflation continues, the same credential captures less labor market advantage. The value 0.58 reflects that a majority of the credentialing activity is extractive rent-seeking rather than genuine skill development. Suppression (0.65): High. Multiple barriers prevent exit: (1) Structural barriers — occupational licensing, employer requirements, educational pathway norms; (2) Timing barriers — young workers have limited time to defer education without career penalty; (3) Information barriers — alternative credentials remain unknown or signaled as lower-quality; (4) Economic barriers — educational debt and opportunity costs are severe; (5) Identity barriers — credentialing is fused with middle-class status and parental expectations. Suppression is not total (some alternative pathways exist, some employers hire without degrees, some individuals succeed despite credential gaps), but barriers are substantial. Theater ratio (0.68): High. Much credentialing activity is performative: (1) Employers demand credentials they cannot verify as job-relevant; (2) Educational institutions teach toward assessment metrics rather than skill acquisition; (3) Students and families perform the ritual (credential pursuit) because competitors do, not because they believe in optimal skill development; (4) Credentialing bodies maintain reputation and enrollment by perpetuating the ritual; (5) The entire system continues despite widespread recognition that credential inflation exceeds utility. Theater increased over the measurement interval as the gap between credential requirements and actual job demands widened.
 *
 * PERSPECTIVAL GAP:
 *   The gap between new entrant and institutional perspectives is maximal because the directionality derivation produces d values at opposite extremes: entrants derive d near 1.0 (full target), institutions derive d near 0.0 (full beneficiary). This drives their classification divergence: Snare (high extraction experienced) vs Rope (coordination experienced). The mid-career incumbent perspective (d ≈ 0.55) produces Tangled Rope — genuinely mixed coordination and extraction — because this agent has both benefits (credential scarcity) and costs (credential devaluation risk). The analytical perspective risks Mountain because civilizational-scale observation tempts naturalizing the constraint as inherent to competitive signaling. The perspectival gap reveals the true structure: the constraint exists in the directionality distribution itself — it extracts from agents positioned as targets (entrants) and benefits agents positioned as beneficiaries (institutions). The Tangled Rope classification from the analytical perspective (if the engine assigns it) correctly identifies that both coordination and extraction are present, but the distribution is asymmetric (concentration of extraction on powerless entrants, concentration of coordination benefits on institutional actors).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from each agent's structural position. New entrants are trapped victims with no exit options — they derive d ≈ 0.92 (near 1.0), producing maximum f(d) ≈ 1.40, maximum experienced extraction chi. Educational institutions are beneficiaries with arbitrage options — they derive d ≈ 0.08, producing f(d) ≈ -0.12, negative chi (extraction flows toward them). Mid-career incumbents are mixed — they benefit from credential scarcity but bear extraction from credential devaluation; they are constrained (higher exit cost than institutional actors but lower than trapped agents) — they derive d ≈ 0.55, producing f(d) ≈ 0.75, moderate chi. Elite gatekeepers are powerful beneficiaries — d ≈ 0.35, f(d) ≈ 0.30 (low extraction because they control the system). The scope modifier σ(S) is national (1.0) for most perspectives because credential inflation is primarily a labor market phenomenon operating at national scale (credential markets are nationally bounded despite global capital flows). The directional divergence — entrants at d ≈ 0.92 vs institutions at d ≈ 0.08 — explains why the constraint appears as Snare from the powerless perspective and Rope from the institutional perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not cleanly resolve the mandatrophy between coordination and extraction because the same institutional mechanisms (credentialing requirements) do both. The genuine coordination function is real: credentials signal trainability, reduce employer information costs, and enable educational institutions to maintain social roles in skill development. But the extraction is equally real and grows over time: credential inflation outpaces task requirement growth, educational institutions optimize for enrollment rather than learning, employers maintain requirements for competitive signaling rather than competency assessment, and costs concentrate on entrants with fewest alternatives. The mandatrophy resolves by recognizing that this is genuinely a Tangled Rope — not because the constraint is ambiguous, but because it is structurally hybrid. Some portion of extractiveness (perhaps 0.20-0.25) represents genuine coordination cost (legitimate signaling, educational overhead). The remainder (0.33-0.38) represents extraction: rent-seeking by institutions, competitive escalation by employers, and opportunity cost imposed on entrants. As theater ratio increased from 0.42 to 0.68, the ratio of extraction to coordination shifted — more of the credentialing activity became performative. If theater continues rising and extractiveness remains high, the classification would trend toward Snare. If alternative credentialing pathways achieve sufficient adoption and employer acceptance, extractiveness would decline and the constraint would trend toward Scaffold or degraded Rope (Piton). The current classification as Tangled Rope is stable for the measured interval but represents a dynamic equilibrium between coordination and extraction functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_predictive_validity,
    'What fraction of actual job performance variation is explained by holding a specific credential?',
    'Longitudinal matched-pair analysis: workers with vs without credentials in same role; correlation between credential-predicted competency and actual performance metrics',
    'If validity < 15%: credential inflation is primarily extraction (Snare for entrants). If validity > 40%: genuine coordination function preserved (Rope/Tangled Rope). If validity intermediate: mandatrophy unresolved — both extraction and coordination present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_predictive_validity, empirical, 'Predictive validity of credentials for job performance').

omega_variable(
    employer_credential_demand_elasticity,
    'If one major employer signals that they no longer require a specific credential, do competitors follow or maintain the requirement to signal quality?',
    'Natural experiments: firms that remove credential requirements and track hiring outcomes and market position; competitor response timing and justifications',
    'If high follow-through: credential requirements are information signals (Rope). If low follow-through/active resistance: requirements are coordination enforcement (Snare/Tangled Rope) — firms maintain demands to preserve scarcity value and avoid reputational damage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_credential_demand_elasticity, empirical, 'Whether employer credential demands respond to market signals').

omega_variable(
    alternative_pathway_substitutability,
    'Are bootcamp/portfolio/apprenticeship credentials effective substitutes for traditional degrees in opening employment opportunities?',
    'Comparative hiring rates and wage trajectories: alternative-credentialed workers vs degree-holders in same occupations; sector-specific adoption rates; employer preference revelation',
    'If highly substitutable: scaffold sunset is viable, constraint degrades toward Rope. If poor substitutes: alternative credentials remain niche (Scaffold classification confirmed but sunset delayed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_substitutability, empirical, 'Substitutability of alternative credentials for traditional degrees').

omega_variable(
    identity_lock_in_credentialing,
    'Do employers and credentialing bodies continue inflating credential requirements despite recognizing diminishing returns, because their institutional identity is fused with credentialing gatekeeping?',
    'Documentary analysis: employer justifications for credential requirements (public statements, job descriptions, survey data); compare stated reasons (competency requirements) with implicit reasons (market differentiation, institutional legitimacy)',
    'If identity-locked: institutional actors experience identity_locked exit options; perspectival gap widens between institutional and powerless observers. Indicates that credential inflation is sustained by cognitive capture rather than structural necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_in_credentialing, conceptual, 'Whether institutional actors are identity-locked into credentialing gatekeeping').

omega_variable(
    opportunity_cost_concentration,
    'Do credential inflation costs fall uniformly on new entrants or concentrate on specific demographic groups?',
    'Demographic analysis of educational debt, time-to-first-employment, credential-to-job-match by socioeconomic background, race, gender; identify if inflation acts as implicit barrier to specific populations',
    'If highly concentrated: credential inflation functions as occupational closure mechanism (Snare with high suppression). If uniform: extraction is less asymmetric (moves toward Tangled Rope/Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opportunity_cost_concentration, empirical, 'Distribution of credential inflation costs across demographic groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(educational_credential_inflation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edcred_tr_t0, educational_credential_inflation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(edcred_tr_t10, educational_credential_inflation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(edcred_tr_t20, educational_credential_inflation, theater_ratio, 20, 0.68).
narrative_ontology:measurement(edcred_tr_t5, educational_credential_inflation, theater_ratio, 5, 0.5).

% Extraction over time
narrative_ontology:measurement(edcred_be_t0, educational_credential_inflation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(edcred_be_t10, educational_credential_inflation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(edcred_be_t20, educational_credential_inflation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(edcred_be_t5, educational_credential_inflation, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(educational_credential_inflation, information_standard).
narrative_ontology:affects_constraint(educational_credential_inflation, occupational_licensing_gatekeeping).
narrative_ontology:affects_constraint(educational_credential_inflation, educational_debt_entrapment).
narrative_ontology:affects_constraint(educational_credential_inflation, labor_market_sorting_failure).

% DUAL FORMULATION NOTE:
% Credential inflation decomposes into three structurally distinct constraints: (1) the credentialing system itself (information_standard coordination), (2) the debt accumulation mechanism (resource_allocation extraction), and (3) the labor market sorting failure (enforcement_mechanism breakdown). Each has distinct ε: credentialing system ε ≈ 0.55 (mixed coordination/extraction), debt mechanism ε ≈ 0.72 (primarily extractive), labor market sorting ε ≈ 0.48 (mixed). The stories are linked because the credentialing system drives enrollment demand → debt accumulation, and both impair labor market sorting. The present story focuses on the credential requirement inflation mechanism; related stories address debt dynamics and sorting breakdown separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(educational_credential_inflation, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
