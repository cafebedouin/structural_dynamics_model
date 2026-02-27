% ============================================================================
% CONSTRAINT STORY: skills_based_hiring
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_skills_based_hiring, []).

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
 *   constraint_id: skills_based_hiring
 *   human_readable: Skills-Based Hiring (De-credentialing)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   Skills-based hiring represents a structural shift in how labor markets
 *   match workers to roles, de-emphasizing traditional educational
 *   credentials in favor of demonstrated competence. This constraint exhibits
 *   the hallmark tension of a tangled rope: it solves a genuine coordination
 *   problem (reducing barriers for non-traditional talent, improving
 *   role-to-capability matching) while embedding new extractive mechanisms
 *   (platform gatekeeping, algorithmic opacity, verification cost
 *   concentration). The constraint's theater ratio (0.58) reflects that while
 *   skills-based hiring does reduce performative credentialing, it has
 *   introduced new performative elements: maintaining competitive portfolios,
 *   showcasing skills on platforms, narrative construction of non-traditional
 *   pathways. The base extractiveness has grown from 0.15 to 0.38 over the
 *   interval, indicating that as skills-based hiring has scaled, platform
 *   consolidation and assessment cost concentration have increased
 *   extraction. The constraint is particularly sensitive to platform
 *   architecture: open standards and transparent algorithms sustain
 *   coordination benefits; proprietary systems and opaque filtering recreate
 *   credential gatekeeping at a new layer.
 *
 * KEY AGENTS:
 *   - Non-traditional talent pools (powerless/trapped): Cannot afford premium skill verification platforms or portfolio maintenance; face circular dependency. Primary victims of extraction.
 *   - Self-taught developers with portfolios (moderate/constrained): Can demonstrate competence but face algorithmic filtering and continuous re-assessment burden.
 *   - Technology platforms (organized/mobile): LinkedIn, HackerRank, Coursera, Talent.com. Intermediating assessment, capturing data value. Primary beneficiaries of coordination.
 *   - Large technology employers (powerful/arbitrage): Access broader talent pools, reduce hiring costs, use skills assessment to gate hiring at scale. Primary beneficiaries.
 *   - Traditional education institutions (institutional/constrained): Forced to adapt through competency mapping and bootcamps; experience extraction (diploma value erosion) and coordination (forced innovation).
 *   - Credential licensing systems (institutional/trapped): Formal licensure persists through legal mandate; inertial structure for regulated professions.
 *   - Open-credentials movement (organized/constrained): Mozilla Open Badges, free micro-credential standards. Building alternative pathways to reduce platform lock-in.
 *   - Analytical observer (analytical/analytical): Sees skills-based hiring as coordination bundled with extractive platform gatekeeping. Neutral assessment of structural hybrid.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(skills_based_hiring, 0.38).
domain_priors:suppression_score(skills_based_hiring, 0.42).
domain_priors:theater_ratio(skills_based_hiring, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skills_based_hiring, extractiveness, 0.38).
narrative_ontology:constraint_metric(skills_based_hiring, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(skills_based_hiring, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(skills_based_hiring, tangled_rope).
narrative_ontology:human_readable(skills_based_hiring, "Skills-Based Hiring (De-credentialing)").
narrative_ontology:topic_domain(skills_based_hiring, "economic/technological/social").

domain_priors:requires_active_enforcement(skills_based_hiring).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(skills_based_hiring, non_traditional_talent_pools).
narrative_ontology:constraint_beneficiary(skills_based_hiring, employers_seeking_flexibility).
narrative_ontology:constraint_beneficiary(skills_based_hiring, technology_platforms_intermediating_assessment).
narrative_ontology:constraint_victim(skills_based_hiring, traditional_education_providers).
narrative_ontology:constraint_victim(skills_based_hiring, credential_gatekeepers).
narrative_ontology:constraint_victim(skills_based_hiring, workers_without_access_to_skill_certification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNACCREDITED WORKER WITHOUT VERIFICATION ACCESS (SNARE) — Cannot afford proprietary skill assessment platforms (LinkedIn, HackerRank, Coursera certificates). Lacks institutional backing to demonstrate competence. Even if skilled, trapped in circular dependency: must demonstrate skills to access jobs, but assessment access requires credentials or payment. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(skills_based_hiring, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SELF-TAUGHT DEVELOPER WITH PORTFOLIO (TANGLED ROPE) — Experiences both coordination benefit (can demonstrate competence directly) and extraction (must maintain expensive portfolio/GitHub, navigate opaque algorithmic hiring filters, repeat skill proofs for each employer). Skills-based hiring enabled access but also created new verification burden. d≈0.58, f(d)≈0.73, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY PLATFORM PROVIDER (ROPE) — Benefits from coordination function: solving the matching problem between employers seeking skills and workers with non-traditional pathways. Platform captures value through assessment data. Has mobile exit option — can pivot to new assessment modalities. Low extraction relative to value created. d≈0.35, f(d)≈0.31, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE TECHNOLOGY EMPLOYER (ROPE) — Primary beneficiary of skills-based hiring shift. Can access broader talent pools and reduce hiring costs. Uses skills assessment to gate hiring at scale. Has arbitrage option — can move assessment systems, switch platforms, or revert to credential-based hiring. Pure coordination value: matching talent to roles efficiently. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL EDUCATION INSTITUTION (TANGLED ROPE) — Victim of de-credentialing but also forced to adapt through coordination. Universities develop bootcamps, competency mapping, skills-aligned curricula. Constrained: must respond to labor market signals but has high exit costs (infrastructure, accreditation). Experiences both extraction (reduced diploma value) and coordination benefit (forced innovation). d≈0.62, f(d)≈0.81, σ=1.0 → χ≈0.31.
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CREDENTIAL LICENSING SYSTEM (PITON) — Formal licensure for regulated professions (medicine, law, engineering) persists despite skills-based hiring pressure because legal mandate, not labor market signal. Theater ratio ≈0.65: licensing exams test some competence but also gate access to lucrative professions. Inertial structure maintained by regulatory capture and professional associations. Cannot exit without statutory change. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(skills_based_hiring, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN-CREDENTIALS MOVEMENT (SCAFFOLD) — Organized agents (Mozilla Open Badges, micro-credentials standards, free skill verification platforms) are building alternative verification pathways to reduce platform lock-in. Low coordination extraction (shared infrastructure), high functional value. Sunset clause: as open standards mature, proprietary skill assessment gatekeeping weakens. d≈0.32, f(d)≈0.25, σ=1.0 → χ≈0.10.
constraint_indexing:constraint_classification(skills_based_hiring, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Skills-based hiring is a genuine coordination mechanism (solves matching problem) bundled with extractive elements (verification rent-seeking, data concentration, algorithmic opacity). From civilizational view: shifts gating mechanism from educational credentials to assessment platforms, creating new asymmetries. Base extraction ε=0.38, suppression 0.42, theater 0.58 indicate hybrid nature: real coordination value but also real extraction embedded in assessment infrastructure. d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(skills_based_hiring, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skills_based_hiring_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(skills_based_hiring, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(skills_based_hiring, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(skills_based_hiring, TR),
    TR >= 0.70.

:- end_tests(skills_based_hiring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. Skills-based hiring has genuine coordination function (solves matching problem, reduces artificial barriers), but assessment infrastructure has become increasingly concentrated and costly. Platform intermediaries capture significant value through data and filtering. Growth from 0.15 to 0.38 over the interval reflects platform consolidation and rent-seeking layered onto coordination. Suppression (0.42): Moderate. Workers can theoretically demonstrate skills without credentials, but significant barriers exist: access to quality skill assessment tools (payment, internet connectivity, prior knowledge), algorithmic opacity preventing appeal/revision, narrative disadvantage for workers without polished portfolio presentation skills. Theater ratio (0.58): Moderate-high and rising. Skills-based hiring reduces some theatrical elements of traditional credentialing (GPA performance, institutional prestige signaling), but introduces new performance requirements: maintaining competitive GitHub profiles, articulating non-traditional pathways convincingly, gaming skill assessment algorithms, continuous skill currency. The growth from 0.35 to 0.58 reflects the increasing prominence of portfolio performance as a gating mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces sharp perspectival divergence. The large employer and platform see pure coordination (Rope) — they are solving matching problems efficiently. The self-taught developer sees mixed coordination and extraction (Tangled Rope) — genuine opportunity but also assessment burden. The unaccredited worker without platform access sees pure extraction (Snare) — circular dependency unbroken. The traditional education institution sees extraction bundled with forced adaptation (Tangled Rope) — diploma value erodes but innovation is required. The licensing system sees inertial extraction (Piton) — legally mandated but increasingly anachronistic theater. The open-credentials movement sees a temporary coordination failure being solved (Scaffold) — open standards and transparent assessment will sunset proprietary gatekeeping. The analytical observer sees a genuine tangled rope — coordination mechanism with embedded asymmetric extraction. The perspectival gap reflects real structural differences in who can exit the constraint and who is locked into it.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-traditional talent without platform access: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; cannot exit and cannot demonstrate competence. Self-taught developer with portfolio: Victim + constrained → d≈0.58, f(d)≈0.73. Significant extraction (continuous reassessment, algorithm opacity) but can iterate and improve. Technology platform: Beneficiary + mobile → d≈0.35, f(d)≈0.31. Can shift assessment modality; low extraction from their perspective. Large employer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Can exit entirely; pure coordination value. Traditional education institution: Victim + constrained → d≈0.62, f(d)≈0.81. Diploma value erodes but cannot exit education sector. Licensing system: Victim + trapped → d≈0.75, f(d)≈1.10. Cannot exit due to statutory mandate. Open-credentials movement: Organized + constrained → d≈0.32, f(d)≈0.25. Low extraction; coalition has agency and clear path forward. Analytical observer: d≈0.68, f(d)≈1.03. Balanced view of coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Skills-based hiring resolves mandatrophy by being genuinely hybrid: it is NOT mislabeling pure coordination as extraction (the snare trap) nor pure extraction as coordination (the rope trap). The constraint has real coordination value (matches workers to roles, reduces artificial barriers, enables non-traditional pathways). It also has real extraction (platform gatekeeping, assessment cost concentration, algorithmic opacity, verification rents). The tangled rope classification is the accurate assessment. The mandatrophy resolution hinges on distinguishing: (1) who benefits from the coordination (employers, platforms, workers with platform access) vs (2) who bears extraction costs (workers without platform access, traditional educators, workers underrepresented in algorithmic training data). The growth of extractiveness over the interval (0.15 → 0.38) reflects real market dynamics: as skills-based hiring has scaled, platforms have consolidated and assessment costs have increased. This is not a mislabeling error; it is a legitimate shift in the constraint's character as the market structure evolved. The open-credentials perspective suggests a genuine sunset path: as interoperable standards mature, platform lock-in decreases, and the tangled rope should gradually shift toward rope + scaffold dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_verification_cost_distribution,
    'Who bears the cost of skill verification — employer, worker, or platform — and does this allocation change who benefits from the constraint?',
    'Longitudinal cost accounting: employer assessment platform fees vs worker certification/portfolio maintenance costs vs platform operational costs; correlation with hiring outcome equity across income groups',
    'If workers bear most costs: extraction dominates, snare classification strengthens. If employers bear costs: coordination dominates, rope classification strengthens. If costs shift over time: constraint transitions between types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_verification_cost_distribution, empirical, 'Cost distribution among workers, employers, and platforms in skills-based hiring').

omega_variable(
    skill_transferability_decay,
    'How quickly do demonstrated skills become obsolete relative to credential refresh cycles, and does this create new credential-like extraction structures?',
    'Skill half-life analysis: time to 50% replacement of top 20 technical skills; frequency of reassessment required; correlation with career continuity for workers who don''t continuously re-skill',
    'If decay is rapid (< 2 years): skills-based hiring creates perpetual re-assessment treadmill (snare with high theater). If slow (> 10 years): genuine de-credentialing benefit (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transferability_decay, empirical, 'Obsolescence rate of skills relative to credential refresh cycles').

omega_variable(
    algorithmic_opacity_in_hiring,
    'Are the filtering algorithms used by skills-based hiring platforms transparent to workers, and does opacity recreate the gatekeeping asymmetry of traditional credentials?',
    'Audit study comparing worker feedback loops (visibility into why application rejected) between traditional resume screening vs algorithmic skills filtering; analysis of proprietary vs open assessment systems',
    'If opaque: skills-based hiring merely relocates gatekeeping to algorithmic layer (tangled rope, high theater). If transparent: genuine reduction in extraction (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_in_hiring, empirical, 'Transparency of filtering algorithms in skills-based hiring platforms').

omega_variable(
    access_equity_across_demographics,
    'Do non-traditional workers (racial minorities, first-generation, low-income) gain disproportionate access from skills-based hiring, or do they face new barriers in skill demonstration?',
    'Cohort analysis: hiring outcomes for non-traditional workers under skills-based vs credential-based hiring; cost-adjusted analysis of who can afford high-quality portfolio maintenance; linguistic and cultural bias in skill assessment prompts',
    'If equity gains: skills-based hiring delivers on de-credentialing promise (rope/scaffold). If equity stagnates or reverses: extraction mechanism has shifted but access barriers remain (tangled rope/snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_equity_across_demographics, empirical, 'Whether skills-based hiring reduces or maintains barriers for non-traditional workers').

omega_variable(
    platform_consolidation_dynamics,
    'Do skills-based hiring platforms consolidate into oligopolistic gatekeepers, recreating traditional credential power structures at a new layer?',
    'Market concentration analysis: HHI of skill assessment platforms; switching costs for workers and employers; interoperability of skill credentials across platforms; regulatory capture of platform governance by major employers',
    'If consolidation occurs: skills-based hiring is a temporary scaffold that collapses into snare (high platform extraction). If fragmented/interoperable: sustained rope/scaffold dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_consolidation_dynamics, empirical, 'Platform consolidation and oligopoly formation in skills assessment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(skills_based_hiring, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sbh_tr_t0, skills_based_hiring, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sbh_tr_t5, skills_based_hiring, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sbh_tr_t10, skills_based_hiring, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(sbh_be_t0, skills_based_hiring, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sbh_be_t5, skills_based_hiring, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(sbh_be_t10, skills_based_hiring, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(skills_based_hiring, resource_allocation).
narrative_ontology:affects_constraint(skills_based_hiring, educational_credentialing_signaling).
narrative_ontology:affects_constraint(skills_based_hiring, platform_labor_intermediation).
narrative_ontology:affects_constraint(skills_based_hiring, algorithmic_hiring_bias).

% DUAL FORMULATION NOTE:
% Skills-based hiring decomposes into multiple structural constraints: (1) the matching coordination problem (solved by skills-based hiring) — candidate constraint: educational_credentialing_signaling; (2) the platform intermediation extraction layer — candidate constraint: platform_labor_intermediation; (3) algorithmic bias in assessment filtering — candidate constraint: algorithmic_hiring_bias. Each upstream constraint has different ε values and different beneficiary/victim structures. The skills_based_hiring story integrates these perspectives but acknowledges that different measurement approaches (platform revenue concentration vs worker access equity vs algorithmic transparency) would yield different ε values and potentially different classifications for specialized sub-constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(skills_based_hiring, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
