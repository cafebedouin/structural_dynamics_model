% ============================================================================
% CONSTRAINT STORY: labor_market_signaling_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_signaling_asymmetry, []).

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
 *   constraint_id: labor_market_signaling_asymmetry
 *   human_readable: Labor Market Signaling Asymmetry
 *   domain: labor_economics/institutional_coordination
 *
 * SUMMARY:
 *   The labor market signaling asymmetry represents a structural mechanism
 *   where formal educational credentials function as gateways to employment
 *   and earnings, creating a coordination mechanism for employers to assess
 *   worker competence while simultaneously enabling extraction from workers
 *   lacking credentials. The constraint exhibits tension between genuine
 *   information coordination (reducing employer uncertainty about job fit)
 *   and rent extraction (credential holders capturing wage premiums partly
 *   due to artificial scarcity rather than competence). Over the 40-year
 *   interval, both extractiveness and theater ratio have increased,
 *   suggesting credential inflation and growing performativity. Base
 *   extractiveness rose from 0.35 to 0.52, indicating that the rent component
 *   has grown relative to the coordination component. Theater ratio increased
 *   from 0.52 to 0.64, reflecting that credential validation has become more
 *   performative (degree ownership verified) while actual job-readiness
 *   assessment has become less reliable (skills mismatch persists despite
 *   widespread credential holding). The constraint's six perspectives reveal
 *   fundamentally different structural experiences: powerless unschooled
 *   workers see pure extraction (Snare), lower-income seekers see mixed
 *   coordination-extraction (Tangled Rope), credentialed workers see pure
 *   coordination benefit (Rope), organized alternative-pathway advocates see
 *   a temporary problem with addressable sunset (Scaffold), the educational
 *   system itself sees its own degraded ritual (Piton), and the analytical
 *   observer risks naturalizing a contingent institutional arrangement as a
 *   law of markets (Mountain).
 *
 * KEY AGENTS:
 *   - Unschooled Competent Workers: Primary victims (powerless/trapped) — possess job-relevant skills but cannot signal without formal credentials; face insurmountable barriers to credential acquisition
 *   - Lower-Income Job Seekers: Secondary victims (moderate/constrained) — can acquire credentials but at high cost relative to income; face earnings extraction during school-to-work transition
 *   - Credentialed Workers: Primary beneficiaries (institutional/arbitrage) — capture wage premiums from signaling capacity; can arbitrage credentials across markets and career transitions
 *   - Educational Institutions: Secondary beneficiaries (institutional/arbitrage) — capture tuition revenue; maintain institutional legitimacy through credential-granting monopoly
 *   - Alternative Credentialing Coalition: Organized agents (organized/constrained) — bootcamps, apprenticeships, skills-based hiring initiatives creating parallel signaling mechanisms
 *   - Employers: Institutional actors (institutional/arbitrage) — benefit from credential screening reducing hiring uncertainty; locked into path-dependent hiring practices
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing credential requirements as market necessity rather than institutional design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_signaling_asymmetry, 0.52).
domain_priors:suppression_score(labor_market_signaling_asymmetry, 0.58).
domain_priors:theater_ratio(labor_market_signaling_asymmetry, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_signaling_asymmetry, extractiveness, 0.52).
narrative_ontology:constraint_metric(labor_market_signaling_asymmetry, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(labor_market_signaling_asymmetry, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_signaling_asymmetry, tangled_rope).
narrative_ontology:human_readable(labor_market_signaling_asymmetry, "Labor Market Signaling Asymmetry").
narrative_ontology:topic_domain(labor_market_signaling_asymmetry, "labor_economics/institutional_coordination").

domain_priors:requires_active_enforcement(labor_market_signaling_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_signaling_asymmetry, credentialed_workers).
narrative_ontology:constraint_beneficiary(labor_market_signaling_asymmetry, educational_institutions).
narrative_ontology:constraint_victim(labor_market_signaling_asymmetry, unschooled_competent_workers).
narrative_ontology:constraint_victim(labor_market_signaling_asymmetry, lower_income_job_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNSCHOOLED COMPETENT WORKER (SNARE) — Trapped by credential requirements regardless of actual job-relevant competence. Cannot signal capability without formal qualification. Faces structural barriers: credential costs are prohibitive, employer screening is credential-based, alternative pathways are blocked by institutional design. Maximum extraction relative to agent's structural position — cannot exercise exit despite possessing actual skills.
constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOWER-INCOME JOB SEEKER (TANGLED ROPE) — Faces high credential costs relative to income. Some coordination benefit exists: formal credentials do enable job access and earnings mobility, creating genuine value. However, extraction exists: must pay signal costs (tuition, time) that wealthier competitors can absorb more easily, creating earnings asymmetry independent of competence. Exit is possible but costly — retrain, migrate, network-build — hence constrained rather than trapped.
constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALED WORKER COHORT (ROPE) — Experiences constraint as pure coordination mechanism: their credentials efficiently signal competence to employers, solving the information asymmetry problem. Earns wage premium from signaling capacity. Can arbitrage across labor markets due to portable credentials. The coordination function creates genuine value; extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE CREDENTIALING COALITION (SCAFFOLD) — Organized agents (bootcamps, apprenticeships, competency-based hiring) see the credential barrier as a temporary coordination failure with addressable sunset logic. Apprenticeship systems, skills-based hiring, and project portfolios create alternative signaling mechanisms. As these mature and gain employer acceptance, the traditional educational credential's extraction mechanism weakens. The sunset clause is conditional: if alternative signals gain sufficient institutional legitimacy (5-10 year horizon), wage asymmetry decouples from credential status.
constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EDUCATIONAL CREDENTIALING SYSTEM (PITON) — The institutional system maintains credential gates through inertia despite degraded signaling function. Credentials signal educational attainment more reliably than job readiness; employers use them as screening proxies anyway. The system persists through path dependence (employers trust what they trust) and institutional coupling (HR departments built around credential-based screening). Theater ratio (0.64) reflects that credential validation is substantially performative: diploma ownership is verified, but actual competence matching to job requirements is often poor.
constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical position, information asymmetry between employers and job seekers is inherent to labor markets: employers cannot directly observe competence before hiring, necessitating some signaling mechanism. This perspective sees credential barriers as natural, immutable, and functional. However, the structural data reveals this as false naturalization: the credential requirement is contingent on institutional design choices (licensing, hiring norms, wage-setting conventions), not inherent to the information problem. Alternative signaling mechanisms (apprenticeships, portfolios, competency assessment) solve the same information problem with different distributional consequences.
constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_signaling_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_signaling_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_signaling_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_signaling_asymmetry, TR),
    TR >= 0.70.

:- end_tests(labor_market_signaling_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The credential system exhibits genuine coordination value (reduces employer information asymmetry, enables job-fit matching) AND significant extraction (credential holders capture wage premiums ~40% larger than non-credentialed peers controlling for task performance; credential acquisition costs fall disproportionately on lower-income workers). The measurement trajectory shows extractiveness rising from 0.35 to 0.52 over 40 years, indicating credential inflation — degree requirements increasing faster than job skill demands, suggesting growing rent component relative to coordination component. Suppression (0.58): Moderate-high. Barriers to credential alternative include: (1) employer path-dependence — hiring screens locked into credential verification, (2) network effects — credentials' value depends on others accepting them, (3) legal/liability structures — some professions legally require credentials, (4) opportunity costs — time-to-credential delays labor market entry and compounded earnings. Suppression is not total because alternatives exist (apprenticeships, portfolio-based hiring, skills certification) and can be adopted with effort. Theater ratio (0.64): Moderate-high. Credential screening is substantially performative: employers verify degree ownership as a proxy for competence but this proxy has weakened as credential attainment has increased. Diploma ownership is objectively verifiable; actual job-readiness matching is poor (skills mismatch persists even among credentialed workers). The performative component has increased as credential proliferation has made degrees less reliable signals.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across institutional contexts. The credentialed worker and the unschooled competent worker perceive the same institutional structure as fundamentally opposite: one sees pure coordination (Rope), the other sees pure extraction (Snare). The lower-income seeker occupies the middle ground, experiencing both coordination value (credentials do enable job access) and extraction (relative cost is higher). The educational system's self-perception (Piton) — that its credential function is degraded but persistent — is structurally accurate; employers continue credential-based screening not because it works reliably but because alternatives haven't fully displaced it. The scaffold perspective (alternative credentialing) is real and growing but currently captures <20% of labor market transitions in most sectors, making its sunset timeline uncertain. The mountain perspective (natural law of markets) is a false summit: information asymmetry between employers and workers is real and requires some signaling mechanism, but the specific form (formal educational credentials) is contingent, not necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from their structural relationship to the extraction flow. Credentialed workers and educational institutions are beneficiaries with arbitrage-level exit capacity: they can switch between labor markets, credential types, and educational providers with relatively low cost (institutional and powerful agents with high mobility). Their d values are low (~0.15-0.25), producing negative effective extraction from their perspective — they benefit from the constraint. Unschooled competent workers are victims with trapped exit: they possess job skills but cannot credibly signal them without credentials, and credential acquisition is prohibitively costly or time-consuming. Their d value is high (~0.90-0.95), producing maximum effective extraction. Lower-income seekers are victims with constrained exit: they can acquire credentials but at significant financial and opportunity cost. Their d value is moderate-high (~0.70), producing high effective extraction but not maximum. The alternative credentialing coalition consists of organized agents with constrained exit (they can build alternative pathways but face network-effect barriers and institutional friction). Their d value is moderate (~0.55), producing moderate effective extraction appropriate to their mixed agency and real but surmountable obstacles.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that all six types are legitimate perspectival readings arising from the same base properties. The resolved question is not 'is this coordination or extraction?' but 'from which agent's structural position?' The extractiveness (0.52) is high enough to trigger Snare classification from powerless trapped agents and Tangled Rope from moderate constrained agents, while the same base properties produce Rope classification from institutional beneficiaries. The theater ratio (0.64) justifies the Piton classification from the educational system's perspective: credential screening persists through inertia despite its degraded function. The scaffold perspective is empirically real (alternative credentialing is growing) but not yet dominant (sunset timeline is uncertain, adoption <20%). The mountain perspective is a false summit that the structural data reveals: credential requirements are not inherent to labor market coordination but rather a specific institutional choice that naturalizes itself through employer path-dependence and educational legitimacy narratives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_reliability_vs_cost_tradeoff,
    'What portion of the credential wage premium reflects genuine signal reliability (employer information reduction) versus rent extraction (artificial scarcity)?',
    'Comparative wage analysis: credential holders with low job-relevant skills vs non-credentialed workers with demonstrated high competence; employer hiring data showing credential-competence mismatch; earnings correlations with task-specific assessment scores vs educational attainment',
    'If signal reliability dominates (>70% of premium): constraint is primarily coordination (Rope dominates). If extraction dominates (>40% of premium is credential rent): constraint is primarily extraction (Snare/Tangled Rope dominates). Current estimates suggest 35-50% extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_reliability_vs_cost_tradeoff, empirical, 'Wage premium decomposition: signal reliability vs rent extraction').

omega_variable(
    alternative_signal_adoption_rate,
    'Are alternative signaling mechanisms (bootcamps, portfolios, apprenticeships) achieving genuine employer acceptance at rates that would justify scaffold classification?',
    'Hiring data: proportion of new hires from alternative pathways; wage parity studies comparing alternative-credentialed workers to traditional degree holders controlling for task performance; employer survey data on signal trust.',
    'If adoption rates exceed 30% in key sectors and wage gaps close to <10% controlled for performance: scaffold sunset is real and timeline is 5-10 years. If adoption stalls <15%: scaffold is aspirational, not structural — constraint persists as Tangled Rope/Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_signal_adoption_rate, empirical, 'Market adoption rate of alternative signaling mechanisms').

omega_variable(
    credential_inflation_dynamics,
    'Is the extractiveness of the credential system increasing over time due to credential inflation (degree requirements rising faster than job demands)?',
    'Historical analysis: degree requirements for job categories over 20-year periods; skill-level requirements vs education-level requirements in job postings; comparison of credential attainment vs task complexity growth.',
    'If credential inflation is documented: extractiveness is increasing (measurements should show rising base_extractiveness over time). If task complexity is rising faster than credential requirements: extractiveness is constant or declining. Inflation suggests Snare dominates for newer labor market entrants (biographical horizon) while older workers see Rope (locked in with outdated credentials accepted).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_inflation_dynamics, empirical, 'Whether credential requirements are inflating faster than job skill demands').

omega_variable(
    identity_locked_credentialism,
    'To what degree are employers and educational institutions bound by identity-fused credentialism (their institutional identity constituted through credential-based hiring/granting) versus external structural barriers?',
    'Case studies of credential-abandoning organizations; analysis of hiring changes when credential requirements are explicitly removed; survey data on employer beliefs about credential necessity vs empirical hiring outcomes.',
    'If identity-locked dominates: institutional actors could shift to alternative signals if their identity frames shifted, even without external incentives. If structural barriers dominate: changing requires systemic coordination (network effects, liability structures, HR infrastructure redesign). Identity-locked suggests faster policy change; structural barriers suggest longer timelines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_credentialism, empirical, 'Whether credential system persistence is identity-locked or structurally constrained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_signaling_asymmetry, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmsa_tr_t0, labor_market_signaling_asymmetry, theater_ratio, 0, 0.52).
narrative_ontology:measurement(lmsa_tr_t20, labor_market_signaling_asymmetry, theater_ratio, 20, 0.58).
narrative_ontology:measurement(lmsa_tr_t40, labor_market_signaling_asymmetry, theater_ratio, 40, 0.64).
narrative_ontology:measurement(lmsa_tr_t10, labor_market_signaling_asymmetry, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(lmsa_be_t0, labor_market_signaling_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lmsa_be_t20, labor_market_signaling_asymmetry, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(lmsa_be_t40, labor_market_signaling_asymmetry, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(lmsa_be_t10, labor_market_signaling_asymmetry, base_extractiveness, 10, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_signaling_asymmetry, information_standard).
narrative_ontology:affects_constraint(labor_market_signaling_asymmetry, earnings_inequality_structural).
narrative_ontology:affects_constraint(labor_market_signaling_asymmetry, social_mobility_constraints).
narrative_ontology:affects_constraint(labor_market_signaling_asymmetry, educational_system_gatekeeping).

% DUAL FORMULATION NOTE:
% Labor market signaling asymmetry is upstream of earnings inequality and social mobility constraints but represents a distinct structural mechanism. The credential system coordinates information matching AND extracts value through artificial scarcity. Decomposition into separate constraint stories is not required by ε-invariance (all reading the same physical phenomenon) but ε-invariance DOES require recognizing that alternative signaling mechanisms (apprenticeships, portfolios) would have different ε values and should be modeled as separate constraints if their empirical frequency grows sufficiently (threshold ~20% market adoption).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_signaling_asymmetry, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
