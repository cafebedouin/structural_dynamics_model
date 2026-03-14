% ============================================================================
% CONSTRAINT STORY: developer_training_capital
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developer_training_capital, []).

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
 *   constraint_id: developer_training_capital
 *   human_readable: Developer Training Capital Lock
 *   domain: labor_economics/technology_sector
 *
 * SUMMARY:
 *   Developer training capital creates a structural asymmetry in the labor
 *   market between employers who invest in junior developer onboarding and
 *   the junior developers themselves who accumulate firm-specific knowledge.
 *   The constraint exhibits tension between genuine coordination (firms must
 *   invest in training; developers must invest time to learn complex systems)
 *   and extractive lock-in (accumulated capital is non-transferable;
 *   departure resets career progression). The extractiveness trajectory (0.35
 *   → 0.58 over 10 years) reflects increasing lock-in as junior developers
 *   accumulate domain-specific knowledge in proprietary codebases, internal
 *   frameworks, and firm-specific architectural patterns. Theater ratio
 *   growth (0.42 → 0.55) indicates that formal training programs and
 *   credential systems have become increasingly performative — many
 *   onboarding rituals signal investment in developer growth while
 *   reinforcing non-compete enforcement and proprietary lock-in. Open-source
 *   pathways (GitHub, public portfolios, community projects) represent a
 *   structurally distinct exit mechanism with sunset logic: as hiring
 *   standards shift from firm-specific credentials to demonstrable portfolio
 *   skills, firm-specific training capital becomes less binding.
 *
 * KEY AGENTS:
 *   - Junior Developers: Primary victims (powerless/trapped) — accumulate firm-specific capital with no alternative labor market; departure resets career progression
 *   - Mid-Career Engineers: Secondary victims (moderate/constrained) — face high exit costs but develop transferable skills; some agency and alternative options
 *   - Employer Firms: Primary beneficiaries (institutional/arbitrage) — capture ROI through retention; coordinate knowledge transfer; have multiple alternative talent strategies
 *   - Open-Source Community: Organized agents (organized/mobile) — create alternative skill pathways that reduce firm-specific lock-in; sunset mechanism through portfolio-driven hiring
 *   - Corporate Training Programs: Institutional actor (institutional/arbitrage) — maintain performative onboarding and credentialing systems; see own function as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent lock-in mechanisms as inherent to software engineering expertise development
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developer_training_capital, 0.58).
domain_priors:suppression_score(developer_training_capital, 0.62).
domain_priors:theater_ratio(developer_training_capital, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developer_training_capital, extractiveness, 0.58).
narrative_ontology:constraint_metric(developer_training_capital, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(developer_training_capital, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developer_training_capital, tangled_rope).
narrative_ontology:human_readable(developer_training_capital, "Developer Training Capital Lock").
narrative_ontology:topic_domain(developer_training_capital, "labor_economics/technology_sector").

domain_priors:requires_active_enforcement(developer_training_capital).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developer_training_capital, employer_firms).
narrative_ontology:constraint_beneficiary(developer_training_capital, incumbent_platforms).
narrative_ontology:constraint_victim(developer_training_capital, junior_developers).
narrative_ontology:constraint_victim(developer_training_capital, skill_transferability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR DEVELOPER (SNARE) — Trapped by firm-specific skill accumulation. Departure requires abandoning months/years of accumulated domain knowledge in proprietary systems, codebases, and internal frameworks. Career progression depends on staying; exit costs are maximal. Zero arbitrage — the skills do not transfer to alternative employers.
constraint_indexing:constraint_classification(developer_training_capital, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER ENGINEER (TANGLED ROPE) — Faces high exit costs (lost seniority, relocation, rebuilding reputation) but has developed sufficient transferable skills to eventually switch firms. The constraint genuinely coordinates knowledge transfer and mentorship while extracting disproportionate loyalty and below-market compensation during the lock-in period. Mixed function and extraction.
constraint_indexing:constraint_classification(developer_training_capital, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER FIRM (ROPE) — Experiences training capital constraint as pure coordination: the firm invests in developer training, creating shared knowledge that increases collective productivity. The firm captures ROI through developer retention and performance. Net beneficiary with multiple exit options (recruit differently, outsource, acquire expertise).
constraint_indexing:constraint_classification(developer_training_capital, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE COMMUNITY (SCAFFOLD) — Creates alternative skill pathways that reduce firm-specific lock-in. As open-source contribution becomes a recognized credential (GitHub profiles, portfolio projects), developers gain portable capital that bypasses the firm-specific accumulation trap. Temporary support with sunset: once portfolio-driven hiring replaces credential-based hiring, firm-specific training becomes less extractive.
constraint_indexing:constraint_classification(developer_training_capital, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE TRAINING PROGRAM (PITON) — Many formal corporate training programs (onboarding, internal certifications, mentorship cycles) are substantially performative: they signal investment in developer growth while reinforcing proprietary lock-in. The theater persists through institutional inertia — replaced partially by informal knowledge transfer and YouTube tutorials that deliver equivalent learning at lower cost.
constraint_indexing:constraint_classification(developer_training_capital, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some firm-specific skill accumulation is inherent to complex software engineering: domain knowledge in large codebases takes time to build, and early-career developers necessarily depend on firms to provide that training. This framing naturalizes the lock-in as an immutable feature of how expertise develops. However, the structural data contradicts this — the extractiveness value indicates contingent institutional choice (platform lock-in, non-compete agreements, credential gatekeeping), not natural law.
constraint_indexing:constraint_classification(developer_training_capital, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developer_training_capital_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developer_training_capital, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developer_training_capital, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developer_training_capital, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developer_training_capital, TR),
    TR >= 0.70.

:- end_tests(developer_training_capital_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from junior developers through firm-specific skill lock-in, but the extraction is not maximal (would be 0.70+) because some training is genuinely valuable and transferable. Career progression asymmetry is real — junior developers in high-extraction firms experience depressed wage growth relative to open-market alternatives during lock-in period, typically 2-4 years. The trajectory increase (0.35 → 0.58) reflects accumulation of lock-in mechanisms over time: non-compete agreements, internal credential systems, and relational bonds deepen. Suppression (0.62): High. Multiple barriers reduce exit: (1) Legal/contractual — non-compete clauses (jurisdictionally variant), non-solicitation agreements, restricted stock vesting. (2) Structural — firm-specific skills have limited external market value; retraining elsewhere requires months of ramping. (3) Reputational — internal networks and mentor relationships are firm-specific; departure risks damaging relationships that contribute to career trajectory. (4) Psychological — many junior developers internalize firm culture and see departure as disloyalty. Suppression remains high even in jurisdictions with weak non-compete enforcement (like California) because structural and psychological barriers persist. Theater ratio (0.55): Moderate-high. Corporate training programs (formal onboarding, internal certifications, mentorship frameworks) are substantially performative: they signal firm investment while creating lock-in. Many onboarding processes could be replaced by well-documented open-source codebases and YouTube tutorials at lower cost. The rise of theater is driven by firms' need to justify internal lock-in as 'investment in growth' rather than 'extraction through mobility barriers.' However, theater is not dominant (would be 0.70+) because some genuine knowledge transfer occurs through mentorship and project experience.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. The employer firm sees Rope — pure coordination of training and knowledge transfer with clear ROI. The junior developer sees Snare — irreversible capital accumulation with no exit. The mid-career engineer sees Tangled Rope — genuine learning benefits mixed with extraction through suppressed mobility. Open-source community sees Scaffold — a temporary coordination failure with sunset mechanisms. The corporate training program sees itself as Piton — performative ritual that persists through inertia as alternatives (self-taught, online learning, GitHub portfolios) provide equivalent value at lower lock-in cost. The civilizational analytical observer risks seeing Mountain — expertise development in complex systems naturally requires firm-based apprenticeship — but the structural data reveals this as a false summit naturalizing contingent institutional arrangements (non-competes, credential gatekeeping, mentor relationship tying).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values differ sharply across perspectives based on structural relationship to the extraction flow. Junior developers (powerless/trapped) experience maximum directionality toward being targets (d ≈ 0.95): they are structurally dependent on the firm for capital accumulation with no alternative labor market. Employer firms (institutional/arbitrage) experience minimum directionality as targets (d ≈ 0.05): they are beneficiaries with multiple exit options (recruit differently, promote from within, outsource, acquire). Mid-career engineers (moderate/constrained) experience intermediate directionality (d ≈ 0.65): they have developed some transferable capital and face surmountable (though costly) exit barriers. Open-source community (organized/mobile) experiences d ≈ 0.35 because they have genuine exit options (skill development outside firm context) and are creating coordination benefits (reducing lock-in for future developers). The constraint's effective extractiveness (χ) scales asymmetrically across these perspectives due to f(d) — the same underlying constraint produces high experienced extraction for trapped junior developers and negative experienced extraction (subsidy) for beneficiary firms.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The classification as Tangled Rope resolves the mandatrophy by demonstrating that developer training capital genuinely contains both coordination and extraction. The coordination function is real: (1) Firms must invest in training because complex systems require apprenticeship; (2) Junior developers must invest time in domain-specific learning; (3) Mentorship and knowledge transfer are genuine collective goods that increase overall productivity. The extraction function is equally real: (1) Accumulated capital is disproportionately firm-specific, reducing external labor market value; (2) This lock-in depresses wages for trapped junior developers (comparative disadvantage relative to what they could earn if fully mobile); (3) Non-compete enforcement, internal credentialing, and relational bonds create active suppression. The constraint is NOT reducible to pure coordination (Rope) because the extractive mechanisms are not incidental — they are actively maintained (non-compete clauses, credential gatekeeping, mentor relationship tying) to prevent alternatives. The constraint is NOT reducible to pure extraction (Snare) because genuine training and knowledge transfer occur, creating real productivity gains that all parties benefit from. The Tangled Rope classification forces explicit recognition that beneficial coordination is being weaponized to extract loyalty and mobility costs from junior developers. The scaffold perspective's sunset mechanism (open-source portfolios reducing firm-specific credential value) is consistent with Tangled Rope — it identifies a pathway to reduce extraction while maintaining coordination benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_threshold_ambiguity,
    'What percentage of a junior developer''s training capital is genuinely firm-specific (non-transferable) versus platform-general (transferable to competitors)?',
    'Empirical measurement: tracking developer job transitions and success rates; correlating hiring manager evaluations of prior-firm experience across different firms; analyzing GitHub skill signaling against prior employment',
    'If < 30% is truly firm-specific: constraint should reclassify to Rope (coordination dominates). If > 60% is firm-specific: constraint approaches pure Snare (extraction dominates). Current estimate ~50% drives Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portability_threshold_ambiguity, empirical, 'Firm-specific vs transferable skill composition in developer training').

omega_variable(
    non_compete_enforceability_variance,
    'How much of the suppression (0.62) derives from legally enforceable non-compete clauses versus informal reputation/network effects that deter departure?',
    'Jurisdictional analysis of non-compete enforceability (varies dramatically: California void, other states strict); cross-border hiring surveys; measurement of developer departure rates pre/post non-compete enforcement changes',
    'If legal enforcement is weak: structural suppression is lower than measured; constraint moves toward Rope. If network effects dominate: even without legal enforcement, suppression persists through informal mechanisms (reputational damage, network exclusion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_compete_enforceability_variance, empirical, 'Legal vs informal suppression mechanisms in non-compete enforcement').

omega_variable(
    remote_work_exit_option_shift,
    'Has the rise of remote work created arbitrage opportunities that convert trapped developers to constrained or mobile developers?',
    'Longitudinal developer job transition data: mobility rates pre-2019 vs post-2021; geographic salary arbitrage analysis; non-compete enforcement success rates in remote contexts where enforcement is geographically weak',
    'If remote work significantly increased arbitrage: exit_options shift from trapped → constrained for many developers; perspectival classification of junior developers moves from Snare toward Tangled Rope. If remote work had minimal impact: suppression persists despite geographic mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_exit_option_shift, empirical, 'Whether remote work enables exit arbitrage for trapped developers').

omega_variable(
    open_source_credentialing_effectiveness,
    'Do GitHub portfolios and open-source contributions actually reduce hiring preference for firm-specific credentials, or do they supplement rather than replace firm-based training capital?',
    'Hiring decision analysis: comparing interview outcomes and job offers for candidates with strong GitHub profiles but weak firm credentials vs those with strong firm credentials but weak GitHub; longitudinal tracking of hiring standards across tech firms',
    'If GitHub strongly replaces firm credentials: scaffold sunset is real — open-source pathways genuinely reduce firm-specific lock-in, validating generational timescale. If GitHub supplements: scaffold is aspirational, not structural; lock-in persists and sunset is delayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_credentialing_effectiveness, empirical, 'Whether open-source portfolios substitute for firm training credentials').

omega_variable(
    training_capital_vs_relationship_lock,
    'Is the primary binding mechanism the accumulated technical capital (hard to transfer) or the relational/identity lock (developer sees self as member of firm culture)?',
    'Qualitative analysis of departure narratives; measurement of whether developers leaving for external opportunity cite skill transferability concerns vs cultural/relational concerns; analysis of internal mobility (transfer to different team/project within firm) as substitute for external departure',
    'If primarily technical: constraint is structural (Snare/Tangled Rope). If primarily relational: constraint exhibits identity_locked exit pattern; some developers are structurally mobile but identity-fused; perspectival gap widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_capital_vs_relationship_lock, empirical, 'Whether binding mechanism is technical capital or relational lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developer_training_capital, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devtrain_tr_t0, developer_training_capital, theater_ratio, 0, 0.42).
narrative_ontology:measurement(devtrain_tr_t5, developer_training_capital, theater_ratio, 5, 0.48).
narrative_ontology:measurement(devtrain_tr_t10, developer_training_capital, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(devtrain_be_t0, developer_training_capital, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(devtrain_be_t5, developer_training_capital, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(devtrain_be_t10, developer_training_capital, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developer_training_capital, resource_allocation).
narrative_ontology:affects_constraint(developer_training_capital, geographic_wage_arbitrage).
narrative_ontology:affects_constraint(developer_training_capital, tech_worker_visa_gatekeeping).

% DUAL FORMULATION NOTE:
% Developer training capital is part of a broader constraint family around labor mobility in technology sector. Upstream constraints include geographic wage arbitrage (causes firm competition for junior talent) and tech worker visa gatekeeping (creates external supply constraints). Downstream constraints include tech worker burnout cycles (high extraction compounds over career arc) and knowledge silos in critical infrastructure (firm-specific lock-in creates systemic risk when experienced developers leave).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developer_training_capital, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
