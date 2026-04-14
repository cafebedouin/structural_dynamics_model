% ============================================================================
% CONSTRAINT STORY: credential_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_inflation, []).

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
 *   constraint_id: credential_inflation
 *   human_readable: Credential Inflation as Coordination-Extraction Hybrid
 *   domain: labor_market/education/organizational_gatekeeping
 *
 * SUMMARY:
 *   Credential inflation represents a structural tension between the genuine
 *   coordination function of standardized qualifications and the extractive
 *   use of credential requirements as labor supply gatekeeping. Entry-level
 *   job seekers face escalating degree requirements (bachelor's, then
 *   master's, then specialized certifications) that employers explicitly
 *   justify as 'quality assurance' but that function primarily to restrict
 *   the candidate pool and suppress wage pressure on incumbents. The
 *   constraint exhibits Tangled Rope characteristics: it coordinates labor
 *   market information (standardized signals of training completion) while
 *   simultaneously extracting from job seekers through unnecessary
 *   credentialing requirements. Over the 20-year interval measured,
 *   extractiveness has nearly doubled (0.32 to 0.58) and theater ratio has
 *   increased 62% (0.42 to 0.68), indicating credential inflation is a
 *   worsening problem driven by supply-side hoarding rather than genuine
 *   skill requirements. The tech sector's portfolio-based hiring and skilled
 *   trade's apprenticeship models demonstrate that the coordination function
 *   of credentials can be achieved at lower extraction cost, but
 *   institutional inertia and incumbent status protection maintain the
 *   credential requirement equilibrium across most sectors.
 *
 * KEY AGENTS:
 *   - Entry-level Job Seekers: Primary victims (powerless/trapped) — face escalating credential requirements with no alternative pathway; experience full extraction as increasing time and financial burden
 *   - Credentialing Institutions: Primary beneficiaries (institutional/arbitrage) — capture expanded enrollment and tuition revenue from credential inflation; see themselves as coordinating labor market information
 *   - Incumbent Degree Holders: Secondary beneficiaries (powerful/mobile) — benefit from credential gatekeeping that restricts labor supply and suppresses wage competition; protected by status quo
 *   - Large Employers in Credential-Dependent Sectors: Mixed actors (powerful/mobile) — use credentials for labor supply restriction but locked into credential dependency by sector norms and competitive dynamics
 *   - Skilled Workers Without Degrees: Primary victims (moderate/constrained) — face career ceiling despite demonstrated competence; constrained by cost and family obligation barriers to credential pursuit
 *   - Tech Sector and Portfolio-Based Employers: Early adopters (powerful/arbitrage) — escaped credential trap through alternative signaling mechanisms; represent proof-of-concept that coordination can occur at lower extraction cost
 *   - HR Departments: Institutional actors (institutional/arbitrage) — maintain credential requirements as defensible gatekeeping and hiring ritual despite weak job performance correlation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_inflation, 0.58).
domain_priors:suppression_score(credential_inflation, 0.65).
domain_priors:theater_ratio(credential_inflation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_inflation, extractiveness, 0.58).
narrative_ontology:constraint_metric(credential_inflation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credential_inflation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_inflation, tangled_rope).
narrative_ontology:human_readable(credential_inflation, "Credential Inflation as Coordination-Extraction Hybrid").
narrative_ontology:topic_domain(credential_inflation, "labor_market/education/organizational_gatekeeping").

domain_priors:requires_active_enforcement(credential_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_inflation, credentialing_institutions).
narrative_ontology:constraint_beneficiary(credential_inflation, incumbent_degree_holders).
narrative_ontology:constraint_victim(credential_inflation, labor_market_entrants).
narrative_ontology:constraint_victim(credential_inflation, skill_based_workers_without_degrees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL JOB SEEKER (SNARE) — Trapped by escalating degree requirements that bear no relationship to actual job performance. Must pursue credentials at increasing financial and time cost despite structural evidence that skill matters more than paper. No exit: cannot bypass credential gatekeeping without accepting permanent labor market exclusion. Experiences maximum extraction masked as meritocratic requirement.
constraint_indexing:constraint_classification(credential_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED WORKER WITHOUT DEGREE (TANGLED ROPE) — Constrained by career ceiling imposed by credential requirements despite demonstrated competence. Credentials are presented as coordination mechanism (standardization of qualifications) but function primarily as extraction (restricting supply of qualified labor). Can exit by pursuing credentials at significant cost, but constrained by family obligations, geography, or prior economic decisions. Mixed burden and benefit — some roles benefit from skill-based networks; most require paper.
constraint_indexing:constraint_classification(credential_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Experiences credential inflation as pure coordination: standardizing qualifications enables employers to assess candidates efficiently, enables workers to signal competence, enables labor market matching. Institutions benefit from expanded enrollment (extraction) but see the constraint primarily as coordination function. Arbitrage exit: can shift credential value interpretation or create new credentials without structural friction.
constraint_indexing:constraint_classification(credential_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE EMPLOYER IN CREDENTIAL-DEPENDENT SECTOR (TANGLED ROPE) — Uses degree requirements for gatekeeping and labor supply restriction, reducing wage pressure. But also locked into credential dependency: abandoning degree requirements would trigger competitive unraveling where competitors still use credentials, making this employer appear reckless or dismissive of 'standards.' Mobile exit exists (can relocate to markets with skill-based hiring) but constrained by sector norms and shareholder pressure. Benefits from extraction; coordinated by the credential standard.
constraint_indexing:constraint_classification(credential_inflation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TECH SECTOR EARLY ADOPTER (ROPE) — Experienced credential inflation as pure coordination problem, inverted it through demonstrable outcomes (portfolio-based hiring, live coding assessments). Has arbitrage exit: can hire from non-credentialed pools at wage discount, capturing talent excluded from credential-inflated markets. Sees credential inflation as a market inefficiency they can exploit, not a constraint they must follow. Zero effective extraction from their vantage point.
constraint_indexing:constraint_classification(credential_inflation, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HR DEPARTMENT INSTITUTIONAL RITUAL (PITON) — Degree requirements persist in hiring rubrics despite mounting evidence that they predict neither job performance nor retention. The credential serves as screening theater: HR departments use degrees as defensible exclusion criteria (they can show they required degree X) rather than as genuine competence filters. The ritual persists through institutional inertia and legal defensibility, not functional verification. Theater ratio: 0.68 reflects that approximately two-thirds of credential requirements in many roles serve gatekeeping rather than quality assurance.
constraint_indexing:constraint_classification(credential_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risks seeing credential inflation as an inevitable feature of complex labor markets: 'As complexity increases, employers must filter candidates somehow, and credentials are the most efficient mechanism.' This naturalizes what is actually a contingent institutional arrangement driven by regulatory path dependency, status signaling, and coordination failure. The constraint is not a law of labor economics — it is a specific equilibrium maintained by extraction incentives and collective action barriers.
constraint_indexing:constraint_classification(credential_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_inflation, TR),
    TR >= 0.70.

:- end_tests(credential_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Credential inflation functions as extraction primarily through unnecessary escalation (jobs that previously required bachelor's degrees now require master's degrees or specialized certifications) imposed without demonstrated performance benefit. The escalation is not coordination failure — it is intentional labor supply restriction. The value has increased from 0.32 to 0.58 over 20 years, indicating systematic credential-hoarding by employers and incumbent degree-holders. Suppression (0.65): High. Barriers to exit are substantial: job seekers have limited alternative labor market pathways, face financial obstacles to credential pursuit (student debt), lack geographic mobility for apprenticeship or portfolio-based opportunities, and face time barriers (family obligations, existing employment). The suppression is particularly severe for low-income job seekers in regions without alternative hiring ecosystems. Theater ratio (0.68): High and rising. Much of the stated justification for credential requirements (quality assurance, skill verification) serves as cover story for labor gatekeeping. Actual job performance prediction from credentials is weak in most sectors (meta-analysis of credential-performance correlation averages 0.15-0.25 across sectors), yet requirements continue escalating. The theater has increased over time as the disconnect between stated and actual function has grown.
 *
 * PERSPECTIVAL GAP:
 *   Entry-level job seekers (powerless/trapped) see snare: pure extraction with no exit. Skilled workers without degrees (moderate/constrained) see tangled rope: the system both enables some opportunities (credentials do open doors) and extracts heavily (but unnecessarily). Credentialing institutions (institutional/arbitrage) see rope: pure coordination with no extraction cost. Employers in credential-dependent sectors (powerful/mobile) see tangled rope: they benefit from labor supply restriction but are locked in by competitive dynamics. Tech sector employers (powerful/arbitrage) have escaped the constraint entirely through portfolio-based hiring. The piton perspective (HR ritual) reveals that credential requirements persist despite empirical evidence that they don't predict performance — they are maintained through hiring theater. The false mountain perspective warns against naturalizing credential inflation as an inevitable feature of complex labor markets — it is a specific institutional equilibrium.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural flow of extraction. Credentialing institutions and incumbent degree holders are net beneficiaries (low d): they benefit from credential gatekeeping without bearing significant costs. Their arbitrage exit means they can shift credential interpretation or create new credentials without friction. Entry-level job seekers are net victims (high d): they bear the full cost of credential escalation through time and financial investment, while their job performance is not actually dependent on the credentials. Skilled workers without degrees are partially trapped victims (moderately high d): they face career ceilings despite competence. Large employers benefit from labor supply restriction but are constrained by sector norms (moderate d). Tech sector employers have escaped (low d as beneficiaries, but they've already exited). The HR department's institutional position makes them arbitrage actors (low d) — they can shift hiring criteria without personal cost. The directionality formula derives chi values that increase extraction for trapped agents and decrease it for beneficiaries with exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution for credential inflation is structural: the constraint is genuinely hybrid (tangled rope), not misclassified as pure coordination. Credentialing institutions do coordinate labor market information (genuine coordination function) AND extract from job seekers through unnecessary escalation (genuine asymmetric extraction). Both functions are real. The engine correctly prevents mislabeling this as rope (pure coordination) because the extraction is substantial and systematic. The engine also prevents mislabeling it as snare (pure extraction) because the coordination function is real — credentials do provide useful labor market signals. The perspectival gap shows WHY this hybrid classification is essential: credentialing institutions genuinely see coordination (their function is to signal qualifications); job seekers genuinely see extraction (they are forced to pursue unnecessary credentials). The tech sector's escape proves that the coordination function can be achieved at lower extraction cost, confirming that the current equilibrium is not a natural law but a specific institutional arrangement maintained by incumbent interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_requirement_necessity,
    'For any given job category, what proportion of stated credential requirements are genuinely predictive of performance versus serving as gatekeeping proxies?',
    'Longitudinal employer studies comparing performance of credentialed vs non-credentialed workers in same roles; meta-analysis of credential-job performance correlations by sector',
    'If > 70% predictive: credential inflation is minimal coordination problem with acceptable extraction overhead. If < 40% predictive: credential inflation is primarily extraction mechanism masked as meritocracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_requirement_necessity, empirical, 'Proportion of credential requirements that predict job performance').

omega_variable(
    coordination_alternative_sufficiency,
    'Do portfolio-based, skills-based, or outcomes-based hiring mechanisms provide equivalent information to employers as credential screening?',
    'Comparison of hiring costs (time-to-hire, assessment expense, error rates) between credential-based and alternative screening mechanisms; long-term performance tracking across hiring methods',
    'If alternatives are equally efficient: credential inflation is pure extraction with viable exit path (employers can unilaterally shift). If credential-based screening is substantively cheaper: extraction is coupled to genuine coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_alternative_sufficiency, empirical, 'Whether alternative hiring mechanisms provide equivalent information to credentials').

omega_variable(
    credential_inflation_equilibrium_stability,
    'Is credential inflation an unstable equilibrium (self-correcting through market discovery) or a stable trap (self-reinforcing through coordination failure)?',
    'Sectoral comparison: do sectors with lower credential inflation (tech, skilled trades, creative industries) show different innovation or performance outcomes? Do regional labor markets show credential inflation drift over time (increasing or stabilizing)?',
    'If unstable: credential inflation will correct without intervention as sectors discover alternatives; extraction is temporary. If stable trap: coordination failure prevents exit despite mutual benefit from abandoning requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_equilibrium_stability, empirical, 'Whether credential inflation is self-correcting or self-reinforcing').

omega_variable(
    intergenerational_identity_lock,
    'Among degree-holding parents, how much credential advocacy is driven by identity protection (the parent''s status depends on credentials remaining valuable) versus genuine belief in meritocratic signaling?',
    'Survey and interview studies comparing credential advocacy across cohorts; analysis of credential inflation in sectors where incumbent degree-holders have high status concentration',
    'If high identity lock: credential inflation is reinforced by generational status preservation, making it resistant to evidence and policy intervention. If low: credential inflation is responsive to information about alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_identity_lock, preference, 'Identity protection motivation in credential advocacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_inflation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credential_inflation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cred_tr_t10, credential_inflation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cred_tr_t20, credential_inflation, theater_ratio, 20, 0.68).
narrative_ontology:measurement(cred_tr_t5, credential_inflation, theater_ratio, 5, 0.48).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credential_inflation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cred_be_t10, credential_inflation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cred_be_t20, credential_inflation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cred_be_t5, credential_inflation, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_inflation, information_standard).
narrative_ontology:affects_constraint(credential_inflation, student_debt_trap).
narrative_ontology:affects_constraint(credential_inflation, skill_based_labor_market_segmentation).
narrative_ontology:affects_constraint(credential_inflation, intergenerational_economic_mobility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credential_inflation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
