% ============================================================================
% CONSTRAINT STORY: job_market_search_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_job_market_search_friction, []).

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
 *   constraint_id: job_market_search_friction
 *   human_readable: Job Market Search Friction and Asymmetric Information Extraction
 *   domain: economic/labor_markets
 *
 * SUMMARY:
 *   Job market search friction emerges from information asymmetry between
 *   employers and workers: employers cannot directly observe worker
 *   competence, and workers cannot efficiently signal capability. The
 *   constraint solves a coordination problem (matching) while simultaneously
 *   enabling extraction (credential gatekeeping, intermediary rents, network
 *   lock-in). The friction's growth trajectory (theater rising from 0.48 to
 *   0.61, extractiveness rising from 0.38 to 0.52) reflects increasing
 *   reliance on credentials and gatekeeping relative to genuine skill
 *   verification. Early-career workers and underrepresented groups bear
 *   disproportionate costs—they lack inherited networks and must invest in
 *   credentials despite uncertain returns. Employers and recruitment
 *   intermediaries benefit from the friction through non-portable wage
 *   advantages and placement fees. The constraint exhibits all the hallmarks
 *   of Tangled Rope: genuine coordination function (matching), active
 *   enforcement (credential requirements, hiring screening), multiple
 *   beneficiaries, clear victims, and substantial extraction alongside
 *   coordination.
 *
 * KEY AGENTS:
 *   - Early Career Job Seekers: Primary victims (powerless/trapped) — face information disadvantage, credential barriers, high search costs with no exit
 *   - Established Workers Seeking Transition: Secondary victims (moderate/constrained) — face switching costs from credential debt and network lock-in
 *   - Employers: Primary beneficiaries (institutional/arbitrage) — use friction as coordination signal; benefit from non-portable wage rents
 *   - Recruitment Intermediaries: Extractors (institutional/arbitrage) — extract rent through placement fees and screening services; also coordinate matching
 *   - Credentialing Institutions: Gatekeepers (institutional/arbitrage) — maintain credential requirements through institutional inertia; benefit from positional value
 *   - Organized Job Seeker Coalition: Reformers (organized/constrained) — building alternative pathways (bootcamps, skills platforms) with sunset logic
 *   - Analytical Observer: Sees mixed coordination-extraction (analytical/analytical) — recognizes both genuine matching function and contingent gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(job_market_search_friction, 0.52).
domain_priors:suppression_score(job_market_search_friction, 0.58).
domain_priors:theater_ratio(job_market_search_friction, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(job_market_search_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(job_market_search_friction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(job_market_search_friction, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(job_market_search_friction, tangled_rope).
narrative_ontology:human_readable(job_market_search_friction, "Job Market Search Friction and Asymmetric Information Extraction").
narrative_ontology:topic_domain(job_market_search_friction, "economic/labor_markets").

domain_priors:requires_active_enforcement(job_market_search_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(job_market_search_friction, employers).
narrative_ontology:constraint_beneficiary(job_market_search_friction, recruitment_intermediaries).
narrative_ontology:constraint_beneficiary(job_market_search_friction, credentialing_institutions).
narrative_ontology:constraint_victim(job_market_search_friction, job_seekers).
narrative_ontology:constraint_victim(job_market_search_friction, early_career_workers).
narrative_ontology:constraint_victim(job_market_search_friction, underrepresented_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER JOB SEEKER (SNARE) — Trapped by information asymmetry, resume screening bottlenecks, and credential requirements they cannot readily escape. Must invest thousands of hours in applications, certifications, and networking with no guarantee of advancement. High suppression: switching costs (relocation, credential debt, time investment) are immense. No meaningful exit path short of leaving the labor market entirely.
constraint_indexing:constraint_classification(job_market_search_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ESTABLISHED WORKER (TANGLED ROPE) — Constrained by switching costs (credential debt, loss of seniority, relocation burden, age discrimination signals) but also benefits from network effects and institutional reputation that job search frictions paradoxically protect. Some agency through referral networks, but high friction discourages career exploration. Mixed extraction and coordination.
constraint_indexing:constraint_classification(job_market_search_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Experiences job market friction as pure coordination problem. High search costs incentivize standardized credential signals, network hiring (which reduces screening burden), and retention mechanisms. Benefits from friction: it raises switching costs for employees, creating non-portable rents. But the primary function is solving the information asymmetry problem — employers need reliable signals of competence, and the friction provides those signals.
constraint_indexing:constraint_classification(job_market_search_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RECRUITMENT INTERMEDIARY (ROPE) — Pure beneficiary with arbitrage options. Extracts rent by sitting between employers and workers: charging placement fees, resume-screening services, credentialing programs. But also genuinely coordinates matching — reduces job seeker time investment and helps employers filter candidates. Low suppression from intermediary perspective (they can exit by switching to different markets or sectors).
constraint_indexing:constraint_classification(job_market_search_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIALING INSTITUTION (PITON) — University degree requirements, professional certifications, and licensing boards maintain their gatekeeping role largely through institutional inertia and employer coordination. The credential's signaling function (filtering for reliability) is genuine coordination. But theater is high (0.61): much of the credential's value is positional/signaling rather than functional skill. Many employers require degrees not because they use degree-specific knowledge but because degrees are coordination devices that all other employers require. The credential system is partially degraded — employers know it's imperfect screening, but switching to alternative signals requires collective coordination, so the system persists.
constraint_indexing:constraint_classification(job_market_search_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED JOB SEEKER COALITION (SCAFFOLD) — Skills bootcamps, online platforms (LinkedIn, AngelList), and alternative credential systems (skills-based hiring, portfolio assessment) are creating bypass pathways around traditional friction. These have a sunset logic: as employers increasingly verify skills directly rather than relying on degrees/credentials, the traditional friction-based gatekeeping loses force. Organized actors have agency and see an exit path. Theater is rising in this space but extraction is declining as verification becomes more direct.
constraint_indexing:constraint_classification(job_market_search_friction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the job market as exhibiting both genuine coordination (matching workers to jobs, signaling competence) and extractive mechanisms (credential gatekeeping, rent extraction by intermediaries, information advantage for established networks). The friction is not inevitable — it's a contingent institutional arrangement that locks in beneficiaries. This perspective drives the tangled rope classification: ε=0.52 reflects that extraction and coordination coexist structurally.
constraint_indexing:constraint_classification(job_market_search_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(job_market_search_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(job_market_search_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_market_search_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(job_market_search_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(job_market_search_friction, TR),
    TR >= 0.70.

:- end_tests(job_market_search_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting that friction provides genuine coordination (employers need reliable signals, workers need to communicate competence) alongside extraction (credential gatekeeping, intermediate rents, network advantages). The 0.52 value is justified by the balance: credential requirements do screen for reliability, but they also lock in benefits for incumbents and intermediaries. Suppression (0.58): Moderate-high. Job seekers face substantial barriers to exit: credential debt (average student loan debt ~$37k in US), time investment (4-year degrees, 6-12 month job search cycles), geographic immobility (relocation costs), and age discrimination signals. But suppression is not total—some alternative pathways exist (bootcamps, referral hiring), and suppression is lowest for workers with inherited advantages (networks, family support). Theater ratio (0.61): Moderate-high and rising. Credential requirements and screening processes include substantial performative elements: resume optimization, cover letters, interview preparation are largely about signaling rather than revealing true competence. Degree requirements in many roles reflect positional value (everyone else requires it) rather than job-specific skills. The rising trajectory reflects increasing credential inflation—employers raising requirements as labor supply increases, not because task requirements changed.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless (Snare) and institutional (Rope) perspectives reveals that friction is experienced radically differently depending on structural position. An early-career worker sees pure extraction—they bear costs, face barriers, and gain no offsetting benefit beyond eventual (maybe) employment. An employer sees pure coordination—they are solving the problem of identifying reliable workers. A credentialing institution sees partial degradation (Piton)—they know their screening is imperfect and increasingly theater, but they can't unilaterally switch without coordinating with other employers. The analytical observer (Tangled Rope) is the only perspective that integrates both functions: yes, friction does coordinate; yes, it also extracts. The constraint is Tangled Rope because both must be true simultaneously from a structural standpoint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are employers, recruitment intermediaries, and credentialing institutions. Victims are job seekers (especially early-career and underrepresented groups) who face information disadvantage and high search costs. The beneficiary-victim distinction is sharp: employers set hiring requirements and benefit from the resulting screening; workers face requirements and bear search costs. Recruitment intermediaries explicitly extract by charging placement fees and screening services, but they also coordinate matching. Credentialing institutions extract by maintaining positional value (degree requirement regardless of job-specific relevance) while providing genuine but imperfect signaling. The directionality derivation will compute high d for trapped early-career workers (powerless + trapped + victim) and low d for employers (institutional + arbitrage + beneficiary), producing the perspectival gap between Snare and Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely both coordination (matching workers to jobs) and extraction (rent extraction through gatekeeping). The false dichotomy is whether the friction is 'necessary' (pure coordination) vs. 'exploitative' (pure extraction). The Tangled Rope classification shows it is both. The extraction exists *because* the coordination problem exists—the solution (credentials, hiring standards) is what enables the extraction (gatekeeping, lock-in). Critically, the rising trajectory of theater (0.48 → 0.61) reveals that the coordination function is being degraded over time: credentials are increasingly positional (required because everyone else requires them) rather than informative (predicting actual job performance). The analytics path out is: (1) separate the genuine matching function from the gatekeeping rent; (2) design alternative matching mechanisms (skills-based hiring, portfolio assessment) that provide the coordination benefit without the extraction; (3) the Scaffold perspective (organized agents building alternatives) represents this path. The constraint can remain Tangled Rope only if both coordination and extraction remain significant. If alternatives succeed, the classification shifts toward Rope (coordination only) or Piton (degraded theater persists) depending on whether the old friction is eliminated or just supplemented.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_quality_vs_gatekeeping_ratio,
    'What proportion of job market friction is legitimate information asymmetry reduction vs. pure gatekeeping rent extraction?',
    'Empirical measurement: correlation between credential requirements and actual job performance; analysis of hiring outcomes with vs. without credential screening; cross-country comparison of credential dependence and labor market efficiency',
    'If primarily signal quality (>70%): constraint is closer to Rope. If primarily gatekeeping (<30%): constraint is closer to Snare. Current evidence suggests 40-50% split, supporting Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_quality_vs_gatekeeping_ratio, empirical, 'Proportion of friction serving information reduction vs. gatekeeping').

omega_variable(
    skills_based_hiring_viability,
    'Can alternative verification mechanisms (portfolio assessment, skills tests, apprenticeships, work trials) replace credential-based screening at scale without new forms of gatekeeping?',
    'Pilot outcomes from skills-based hiring programs; comparative analysis of hiring reliability and diversity outcomes; long-term follow of alternative credentialing systems (bootcamp graduates, portfolio-screened workers)',
    'If viable: scaffold sunset is real and friction extraction will decline. If new gatekeeping emerges (platform algorithms, certification costs): friction persists in different form. Constraint would shift to Piton (degraded ritual persists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(skills_based_hiring_viability, empirical, 'Whether alternative hiring mechanisms can replace credentials at scale').

omega_variable(
    network_effect_irreversibility,
    'Is the value of established professional networks (LinkedIn, alumni connections, referral hiring) structurally irreversible, or would it collapse if hiring protocols switched to decentralized skills verification?',
    'Analysis of network switching costs: labor flow studies comparing cohorts that switch to alternative hiring vs. those staying in credential-based networks; measurement of portable vs. network-embedded wage premiums',
    'If irreversible: workers cannot exit friction even with new alternatives (lock-in to network investment). If reversible: new entrants avoid friction costs, creating generational delta in extraction. Affects sustainability of scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_irreversibility, empirical, 'Whether professional network value is irreversibly locked in').

omega_variable(
    identity_lock_mechanism_in_credential_commitment,
    'Are job seekers identity-locked to credential pathways (self-concept as ''degree-seeking'' or ''professional''), or are they simply facing high material switching costs?',
    'Qualitative analysis of career-changer narratives; measurement of cognitive frames in bootstrap and non-traditional entrant cohorts; comparison of exit patterns when material barriers are removed (e.g., bootcamp completion vs. traditional degree)',
    'If identity-locked: credential constraint persists even when material barriers fall. If purely constrained: alternative pathways with lower switching costs will attract participants. Affects classification stability under policy intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_credential_commitment, conceptual, 'Whether credential pathways lock in identity or material cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(job_market_search_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jobsearch_tr_t0, job_market_search_friction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jobsearch_tr_t5, job_market_search_friction, theater_ratio, 5, 0.57).
narrative_ontology:measurement(jobsearch_tr_t10, job_market_search_friction, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(jobsearch_be_t0, job_market_search_friction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jobsearch_be_t5, job_market_search_friction, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(jobsearch_be_t10, job_market_search_friction, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(job_market_search_friction, resource_allocation).
narrative_ontology:affects_constraint(job_market_search_friction, credential_inflation).
narrative_ontology:affects_constraint(job_market_search_friction, network_gatekeeping).
narrative_ontology:affects_constraint(job_market_search_friction, student_debt_lock_in).

% DUAL FORMULATION NOTE:
% Job market search friction decomposes into at least three structurally distinct constraints: (1) information asymmetry in hiring (genuine coordination problem, low ε); (2) credential gatekeeping and positional value extraction (higher ε); (3) network lock-in and inherited advantage mechanisms (high ε, identity_coordination type). This story covers the aggregate phenomenon. Downstream constraints address the specific mechanisms with independent ε values and measurement data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(job_market_search_friction, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
