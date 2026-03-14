% ============================================================================
% CONSTRAINT STORY: credentialism_wage_premium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credentialism_wage_premium, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: credentialism_wage_premium
 *   human_readable: Credentialism Wage Premium
 *   domain: labor_markets/education/economic_inequality
 *
 * SUMMARY:
 *   Credentialism wage premium describes the structural constraint where
 *   formal educational credentials (degrees, licenses, certifications) become
 *   gatekeeping mechanisms that suppress wages for non-credentialed workers
 *   and restrict labor market entry, even when credentials do not correlate
 *   with job task requirements. The constraint exhibits genuine coordination
 *   function (employers use credentials as quality signals; professional
 *   standards maintain competence) coupled with extractive scarcity rents
 *   (artificial restriction of supply maintains wage premium; educational
 *   institutions capture tuition rents; incumbent credentialed workers
 *   restrict competition). The theater ratio (0.65) reflects that many
 *   credential requirements are increasingly performative rather than
 *   functional — employers demand degrees for roles where competency-based
 *   assessment would be superior, perpetuating credential inflation divorced
 *   from actual skill requirements. Non-credentialed workers experience
 *   maximum extraction (snare) through systematic wage suppression (15-40%
 *   lifetime earnings gap) with trapped exit options; credentialing
 *   institutions experience pure coordination (rope) and capture rents;
 *   alternative credentials (bootcamps, apprenticeships, portfolio
 *   assessment) represent organized exit pathways (scaffold) with
 *   identifiable sunset as labor market verification infrastructure matures.
 *
 * KEY AGENTS:
 *   - Non-Credentialed Workers: Primary victims (powerless/trapped) — face systematic wage penalties and hiring discrimination; no alternatives to credential acquisition despite opportunity costs and debt burden
 *   - Degree-Holding Workers: Primary beneficiaries (institutional/arbitrage) — capture wage premium from credential scarcity; experience constraint as coordination mechanism
 *   - Educational Institutions: Secondary beneficiary (institutional/arbitrage) — extract tuition rents; maintain market power through degree gatekeeping
 *   - Professional Licensing Bodies: Secondary beneficiary (institutional/arbitrage) — restrict credential supply; maintain professional wage premiums
 *   - Skilled Tradespeople: Mixed position (moderate/constrained) — bear extraction through wage suppression but benefit from professional standards and quality assurance
 *   - Organized Labor Coalitions: Mixed position (organized/constrained) — maintain both genuine wage standards and extractive gatekeeping of apprenticeship credentials
 *   - Alternative Credential Providers: Organized coalition (organized/mobile) — bootcamps, apprenticeships, competency-based platforms building exit pathways
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes simultaneous genuine coordination (quality signaling) and extractive scarcity rents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialism_wage_premium, 0.52).
domain_priors:suppression_score(credentialism_wage_premium, 0.58).
domain_priors:theater_ratio(credentialism_wage_premium, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialism_wage_premium, extractiveness, 0.52).
narrative_ontology:constraint_metric(credentialism_wage_premium, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(credentialism_wage_premium, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credentialism_wage_premium, tangled_rope).
narrative_ontology:human_readable(credentialism_wage_premium, "Credentialism Wage Premium").
narrative_ontology:topic_domain(credentialism_wage_premium, "labor_markets/education/economic_inequality").

domain_priors:requires_active_enforcement(credentialism_wage_premium).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credentialism_wage_premium, degree_holders).
narrative_ontology:constraint_beneficiary(credentialism_wage_premium, educational_institutions).
narrative_ontology:constraint_beneficiary(credentialism_wage_premium, credentialing_bodies).
narrative_ontology:constraint_victim(credentialism_wage_premium, non_credentialed_workers).
narrative_ontology:constraint_victim(credentialism_wage_premium, lower_income_cohorts).
narrative_ontology:constraint_victim(credentialism_wage_premium, skill_labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CREDENTIALED WORKER (SNARE) — Faces systematic wage penalties (15-40% lifetime earnings gap) with minimal exit pathways. Credential requirement is structurally enforced through hiring gatekeeping and licensing restrictions. No alternatives; trapped by economic necessity into accepting lower compensation for equivalent work. Maximum extraction, minimum coordination benefit.
constraint_indexing:constraint_classification(credentialism_wage_premium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED TRADESPERSON (TANGLED ROPE) — Bears extraction through wage suppression but also benefits from credentialing coordination (professional standards, peer quality assurance, consumer confidence in certified practitioners). High cost of credential acquisition creates constraint; coordination function is genuine but asymmetric extraction persists through licensing restrictions that limit supply.
constraint_indexing:constraint_classification(credentialism_wage_premium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Benefits from credential system through tuition revenue, prestige, and labor market power. Experiences constraint as pure coordination: institutional role is to certify competence and maintain standards. Arbitrage exit (can shift between credentials, adjust pricing, expand markets). Extraction runs toward this agent; perceives system as legitimate coordination mechanism.
constraint_indexing:constraint_classification(credentialism_wage_premium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED LABOR COALITION (TANGLED ROPE) — Union/guild structures maintain both genuine coordination (wage standards, safety protocols, apprenticeship pathways) and extractive gatekeeping (restrict credential supply, maintain wage premium). Mixed function: legitimate professional standards coupled with artificial supply restriction. Constraining because membership costs and credential rents benefit incumbent members at expense of entrants.
constraint_indexing:constraint_classification(credentialism_wage_premium, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY CREDENTIALISM SYSTEM (PITON) — Original coordination function (consumer protection, quality assurance) has atrophied as credentials became pure economic gatekeeping signals divorced from actual job task requirements. Theater ratio high (65%): credential displays, degree mills, inflated credentialism in non-technical roles). System persists through institutional inertia despite degraded functional value — employers demand credentials not for quality assurance but because signaling convention has replaced actual verification.
constraint_indexing:constraint_classification(credentialism_wage_premium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALING MOVEMENT (SCAFFOLD) — Bootcamps, competency-based assessments, portfolio credentials, apprenticeship reforms building parallel verification pathways. Organized agents (tech industry, progressive educators) see credentialism as temporary coordination failure with identifiable sunset: as employers develop competency-based hiring and portfolio assessment matures, traditional degree requirements lose enforcement power. Mobile exit through alternative pathways. Sunset horizon: 15-25 years as labor market verification infrastructure matures.
constraint_indexing:constraint_classification(credentialism_wage_premium, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Credentialism functions simultaneously as information standard (employers need signals of worker quality; certifications provide verifiable signal) and as rent extraction mechanism (credential holders restrict supply to maintain wage premium; educational institutions capture rents through tuition). Both functions are structurally real; neither is reducible to the other. System exhibits high suppression because workers lack alternative exit pathways; high coordination value because certification genuinely reduces information asymmetry in hiring. Perspectival unity: the same constraint generates both coordination and extraction across different agent positions.
constraint_indexing:constraint_classification(credentialism_wage_premium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialism_wage_premium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credentialism_wage_premium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credentialism_wage_premium, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credentialism_wage_premium, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credentialism_wage_premium, TR),
    TR >= 0.70.

:- end_tests(credentialism_wage_premium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Wage gap between degree-holders and non-credentialed workers performing equivalent tasks averages 15-40% depending on field. However, not all of this gap is pure extraction — some reflects genuine quality signaling value. The 0.52 estimate reflects that a substantial portion is scarcity rent rather than productivity difference. Suppression (0.58): Moderate-high. Non-credentialed workers face systematic gatekeeping through licensing requirements, employer hiring preferences, and credential signaling norms. Exit barriers are high: cost of credential acquisition (tuition, time, opportunity cost) prevents many lower-income workers from entering credentialed pathways. However, suppression is not total — some workers find alternative pathways (apprenticeships, bootcamps, portfolio building), and labor shortages can override credential requirements. Theater ratio (0.65): Moderate-high. Many credential requirements are increasingly disconnected from actual job task requirements. Examples: bachelor's degree requirement for administrative roles (minimal college-level task content); MBA for management roles (generalist credential used as pure signaling); occupational licensing in non-safety-critical fields (cosmetology, security guard). The theater has increased over time as credential inflation has outpaced actual task complexity growth.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival gap between beneficiaries and victims. Degree-holders see rope — they experience the constraint as legitimate coordination (employers need quality signals; they provide them and are rewarded fairly). Credentialing institutions see rope — their core function is quality assurance; extraction is secondary to their experience. Non-credentialed workers see snare — they face systematic suppression and extraction with no coordination benefit; the constraint functions as pure wage suppression. Skilled tradespeople see tangled rope — they benefit from professional standards and quality assurance but bear extraction through restricted supply and wage compression. Organized labor sees tangled rope — they maintain genuine wage standards but also restrict apprenticeship access to maintain scarcity rents. The alternative credential movement sees scaffold — they view credentialism as temporary coordination failure solvable through technology (competency platforms) and market maturation (employer acceptance of alternatives). The gap between snare (victims), rope (beneficiaries), and tangled rope (mixed) perspectives reveals the core structural tension: genuine coordination function is real, but is distributed asymmetrically, creating extractive dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to extraction flow. Non-credentialed workers are pure targets of extraction (d ≈ 0.92) — trapped exit and victim status produce maximum f(d). Degree-holders are net beneficiaries (d ≈ 0.15) — arbitrage exit and beneficiary status produce negative effective extraction. Credentialing institutions are arbitrage beneficiaries (d ≈ 0.10). Skilled tradespeople occupy mixed position: constrained exit + both beneficiary and victim status yields moderate d (≈ 0.50) reflecting genuine mixed experience. Alternative credential providers have mobile exit (d ≈ 0.40), reducing their experienced extraction. The analytical observer (analytical exit) derives d from the symmetric mixed function: genuine coordination + genuine extraction yields d ≈ 0.50, producing moderate effective extraction chi that reflects the tangled nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through structural decomposition: credentialism simultaneously performs coordination (quality signaling) and extraction (scarcity rent, gatekeeping). The coordinate claim 'credentials are just quality signals' is true and false — true for the coordination function, false as a complete description of the constraint. The extraction claim 'credentialism is pure wage suppression' is true and false — true for non-credentialed workers, false for the coordination role. The tangled rope classification acknowledges both: genuine coordination function (beneficiaries array: degree_holders, educational_institutions, credentialing_bodies), asymmetric extraction (victims array: non_credentialed_workers, lower_income_cohorts, skill_labor_market_efficiency), active enforcement (hiring gatekeeping, licensing restrictions). The theater ratio (0.65) shows that the performative component has grown — credential requirements for non-technical roles are increasingly signals of conformity rather than competence, indicating mandate drift. The constraint exhibits classic mandate corruption: original coordination function (ensure worker competence) has been layered with extraction mechanism (restrict supply, maintain wage premium). Alternative credentials (scaffold perspective) represent genuine exit pathways that lower suppression, suggesting the system is not a mountain but a contingent institutional arrangement vulnerable to substitution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_signal_decomposition,
    'How much of the wage premium reflects genuine quality signaling vs. pure credential scarcity rent?',
    'Regression analysis: wage premium controlling for task-specific skill match; comparison of wage gaps for identical-task roles across credentialed vs non-credentialed contexts (apprenticeship-licensed vs competency-based); longitudinal tracking of wage convergence as alternative credentials scale',
    'If premium is 70%+ scarcity rent: snare classification confirmed for non-credentialed workers. If premium is 40%+ genuine signal: rope perspective gains credibility; coordination function is substantial. Impact affects whether extraction is excessive or justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signal_decomposition, empirical, 'Decomposition of wage premium into signaling vs. rent extraction').

omega_variable(
    alternative_credential_effectiveness,
    'Do bootcamp graduates and portfolio-credentialed workers achieve equivalent long-term earnings and job stability compared to degree-holders in comparable roles?',
    '5-10 year longitudinal tracking of alternative credential holders vs traditional degree holders; employer satisfaction ratings; career progression parity; wage convergence timelines',
    'If parity achieved: scaffold perspective confirmed — alternative credentials provide genuine exit pathway, reducing suppression. Classification shifts toward rope for alternative-track workers. If persistent 10-20% discount: alternatives are partial exit only; suppression remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credential_effectiveness, empirical, 'Whether alternative credentials provide equivalent labor market access').

omega_variable(
    licensing_necessity_threshold,
    'For which occupational categories does credential requirement serve genuine safety/competence assurance vs. pure economic gatekeeping?',
    'Consumer harm analysis by occupation: complication rates for licensed vs unlicensed providers in comparable services (medical, construction, legal); regulator effectiveness audits; jurisdictional variance in licensing requirements for same task',
    'If majority of licensing is gatekeeping rather than safety: extractive classification strengthens. If significant genuinely protective licensing exists: coordination function is real and substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_necessity_threshold, empirical, 'Necessity of licensing vs. gatekeeping across occupations').

omega_variable(
    institutional_credential_inflation,
    'Is the wage premium for credentials growing faster than the productivity gap between credentialed and non-credentialed workers would justify?',
    'Time-series decomposition: credential wage premium growth rates vs. task-specific productivity differential growth; credential inflation metrics (proportion of jobs requiring degrees despite stable task requirements)',
    'If premium growth outpaces productivity gap: credential inflation is occurring — extraction is accelerating beyond coordination function. Suppression may be rising over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_credential_inflation, empirical, 'Whether credential wage premium exceeds productivity justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credentialism_wage_premium, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credentialism_wage_premium, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cred_tr_t10, credentialism_wage_premium, theater_ratio, 10, 0.58).
narrative_ontology:measurement(cred_tr_t20, credentialism_wage_premium, theater_ratio, 20, 0.65).
narrative_ontology:measurement(cred_tr_t5, credentialism_wage_premium, theater_ratio, 5, 0.53).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credentialism_wage_premium, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cred_be_t10, credentialism_wage_premium, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cred_be_t20, credentialism_wage_premium, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(cred_be_t5, credentialism_wage_premium, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credentialism_wage_premium, information_standard).
narrative_ontology:affects_constraint(credentialism_wage_premium, wage_inequality_persistence).
narrative_ontology:affects_constraint(credentialism_wage_premium, educational_access_barriers).
narrative_ontology:affects_constraint(credentialism_wage_premium, occupational_licensing_rent_extraction).

% DUAL FORMULATION NOTE:
% Credentialism operates at the intersection of labor market signaling (information standard coordination) and professional gatekeeping (extraction mechanism). The constraint family includes related structures: educational access barriers (who can acquire credentials in first place), occupational licensing rent extraction (specific licensing-based wage premiums), and wage inequality persistence (aggregate effect across labor market). Each shares the extractive mechanism but operates at different structural scales. This story focuses on the credentialism constraint itself; the upstream stories (educational access) determine who can enter the extraction system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
