% ============================================================================
% CONSTRAINT STORY: labor_market_skill_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_skill_arbitrage, []).

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
 *   constraint_id: labor_market_skill_arbitrage
 *   human_readable: Labor Market Skill Arbitrage
 *   domain: economic/labor
 *
 * SUMMARY:
 *   Labor market skill arbitrage is the structural extraction mechanism by
 *   which credentialing institutions and employer gatekeepers capture rents
 *   through the control of formal credentials, despite the existence of
 *   alternative verification mechanisms that would efficiently signal worker
 *   competence. The constraint exhibits mixed coordination (credentials do
 *   provide genuine quality signals) and extraction (credential requirements
 *   exceed true skill thresholds and are actively maintained despite
 *   declining functional necessity). Over the 30-year interval (roughly
 *   1995–2025), the extractiveness has increased from 0.35 to 0.52 as
 *   credential inflation accelerated, while simultaneously the theater
 *   component (performative aspects of credentialing) has remained relatively
 *   stable at 0.35–0.48, indicating that the constraint operates as a hybrid
 *   coordination-extraction mechanism rather than pure performance theater.
 *   The suppression level (0.65) reflects significant structural barriers:
 *   legal licensing requirements in regulated professions, employer hiring
 *   system entrenchment, educational pathway costs, and opportunity costs of
 *   credential accumulation. The constraint decomposition reveals six
 *   distinct institutional perspectives and two asymptotic perspectives
 *   (scaffold/piton, mountain), each with measurable directionality and
 *   empirical markers for resolution.
 *
 * KEY AGENTS:
 *   - Credential-Constrained Workers: Primary victims (powerless/trapped) — face wage suppression and employment discrimination; cannot exit without credential acquisition
 *   - Self-Taught Skilled Workers: Secondary victims (moderate/constrained) — possess demonstrable skills but face high barriers to formal credential pathways; experience constrained exit options
 *   - Professional Associations: Coordination actors (organized/mobile) — establish standards and maintain quality signals; genuinely serve coordination but benefit from credential scarcity
 *   - Credentialing Institutions: Primary beneficiaries (institutional/arbitrage) — capture tuition revenue and prestige from credential scarcity; actively enforce credential requirements
 *   - Employer Gatekeepers: Secondary beneficiaries (institutional/constrained) — use credentials as risk-reduction signal even when skills alone would suffice; cannot unilaterally drop requirements without competitive disadvantage
 *   - Skills-Based Hiring Coalition: Emerging alternative (organized/mobile) — tech companies and forward-thinking employers building alternative verification mechanisms with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional credential premium as inevitable information asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_skill_arbitrage, 0.52).
domain_priors:suppression_score(labor_market_skill_arbitrage, 0.65).
domain_priors:theater_ratio(labor_market_skill_arbitrage, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_skill_arbitrage, extractiveness, 0.52).
narrative_ontology:constraint_metric(labor_market_skill_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_market_skill_arbitrage, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_skill_arbitrage, tangled_rope).
narrative_ontology:human_readable(labor_market_skill_arbitrage, "Labor Market Skill Arbitrage").
narrative_ontology:topic_domain(labor_market_skill_arbitrage, "economic/labor").

domain_priors:requires_active_enforcement(labor_market_skill_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_skill_arbitrage, credentialing_institutions).
narrative_ontology:constraint_beneficiary(labor_market_skill_arbitrage, employer_gatekeepers).
narrative_ontology:constraint_beneficiary(labor_market_skill_arbitrage, information_asymmetry_exploiters).
narrative_ontology:constraint_victim(labor_market_skill_arbitrage, skill_abundant_workers).
narrative_ontology:constraint_victim(labor_market_skill_arbitrage, credential_constrained_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREDENTIAL-CONSTRAINED WORKER (SNARE) — Trapped by institutional requirements for formal credentials despite possessing requisite skills. No exit option: skills alone are insufficient; credentials are legally or de facto required. Bears maximum extraction through wage suppression, employment discrimination, and forced credential accumulation.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SELF-TAUGHT SKILLED WORKER (TANGLED ROPE) — Possesses demonstrable skills but faces high barriers to credential attainment (time, cost, opportunity cost). Experiences mixed benefits (access to opportunities through skill development) and costs (credential tax, employment discrimination for non-credentialed roles). Constrained exit: can develop skills informally but credential bypass remains partial.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROFESSIONAL ASSOCIATION (ROPE) — Organized credentialing bodies experience the constraint as pure coordination: establishing professional standards, maintaining quality signals, and enabling labor market matching. Genuinely serves coordination function (employers need reliable signals; workers benefit from quality thresholds). Mobile exit: associations can evolve standards without collapsing.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIALING INSTITUTION (TANGLED ROPE) — Benefits from credential scarcity (tuition revenue, prestige, enrollment stability). Coordination function exists (delivers genuine skill development) but coexists with extraction (maintains credential premium through supply restriction). Active enforcement: accreditation gatekeeping, curriculum control, degree exclusivity. Arbitrage exit: can adjust standards but benefits from existing scarcity.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYER GATEKEEPER (TANGLED ROPE) — Uses credentials as a signal of reliability and quality even when skills alone would suffice (coordination benefit: reduces hiring risk). But enforcement of credential requirements beyond skill thresholds extracts value through wage depression and labor market screening costs. Constrained exit: cannot unilaterally drop credential requirements without competitive disadvantage (signaling collapse).
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SKILLS-BASED HIRING COALITION (SCAFFOLD) — Tech companies, forward-thinking employers, and competency assessment platforms are building alternative verification mechanisms (skills tests, portfolio reviews, work samples, apprenticeships). These create sunset logic: as skills-based hiring matures, the credential premium declines. Low extraction experienced by this coalition because they have agency and visible exit pathway through alternative verification.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY DEGREE (PITON) — Four-year degree requirements persist in many sectors (finance, law, management) despite degraded functional requirement: job tasks could be performed with 18-month certification or apprenticeship. Theater ratio high (0.65): hiring processes maintain degree filters as ritual quality control even when actual job performance shows weak correlation with degree attainment. Maintained through institutional inertia and coordinated signaling equilibrium.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / INFORMATION ASYMMETRY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some credential premium is inherent to labor markets: employers cannot perfectly observe worker productivity before hiring, so signals are necessary. Information asymmetry is an immutable structural feature of the employment relationship. However, the base extractiveness (0.52) contradicts the mountain classification—revealing that much of the 'necessary' credential premium is institutional rather than inherent.
constraint_indexing:constraint_classification(labor_market_skill_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_skill_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_skill_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_skill_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_skill_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_skill_arbitrage, TR),
    TR >= 0.70.

:- end_tests(labor_market_skill_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflecting significant but not maximal extraction. The credential premium is real—workers with formal credentials earn 20–40% wage premiums in most fields—but much of this reflects legitimate skill development rather than pure rent. The increase from 0.35 to 0.52 over the interval reflects credential inflation: degree requirements have expanded into fields (management, data analysis, skilled trades) where alternative verification would suffice. Suppression (0.65): High. Barriers to credential-free labor market entry include legal requirements (law, medicine, accounting), employer hiring system entrenchment (degree filters automated into resume screening), cost barriers (tuition averaging $100k for four-year degrees), and opportunity cost (4 years forgone wages). These are substantial but not total—skilled trades and tech have shown partial escape routes. Theater ratio (0.48): Moderate. Unlike pure performance rituals, credentialing does deliver genuine educational content and quality screening. But the theater component has grown: employers increasingly use degree credentials as a proxy signal in hiring even when job tasks could be learned through 6–12 month apprenticeships, and educational institutions have incentivized credential breadth (MBA requirement for middle management) beyond marginal productivity contribution.
 *
 * PERSPECTIVAL GAP:
 *   The credential-constrained worker sees pure extraction (snare: no coordination benefit, maximum extraction). The professional association sees pure coordination (rope: quality maintenance, labor market matching). The credentialing institution sees tangled rope: genuine educational function mixed with credential premium capture. The employer gatekeeper also sees tangled rope: real signaling need mixed with credential filtering that exceeds necessity. The skills-based hiring coalition sees scaffold: a temporary constraint with visible sunset as alternatives mature. The piton perspective reveals theater accumulation: degree requirements persist through signaling equilibrium and hiring ritual despite declining functional correlation with job performance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from agents' structural position relative to the extraction flow. Credentialed workers (beneficiaries with arbitrage/mobile exit) experience low or negative d—they have mobility and benefit from the system. Credential-constrained workers (victims with trapped exit) experience high d close to 1.0—they bear extraction costs with minimal escape. Employers face moderate d around 0.55: they are partial extractors (use credentials for gatekeeping beyond necessity) but also partial victims (credential filtering imposes hiring cost and reduces talent pool). Professional associations face d around 0.40: genuinely provide coordination but also benefit from credential scarcity. The engine's derivation computes chi from d via the sigmoid f(d), producing experienced extractiveness that varies by perspective while keeping base ε constant at 0.52. This perspectival variance is the diagnostic signature of tangled rope: the same constraint produces snare from the trapped perspective but rope from the coordination actor's perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED TANGLED ROPE: The constraint exhibits genuine dual function (coordination and extraction) that prevents collapse into either pure rope (coordination only) or pure snare (extraction only). The empirical markers are: (1) beneficiaries exist with genuine coordination interest (professional associations maintain standards, employers need signals), (2) victims exist experiencing genuine extraction costs (wage suppression, employment discrimination), (3) enforcer exists (credentialing institutions actively police credential requirements and suppress alternatives), and (4) measurable asymmetry (beneficiaries capture 60–70% of credential premium while victims bear 100% of credential acquisition cost). The mandatrophy is resolved by showing that all six perspectives are legitimate readings of the same structural data. The snare classification from the trapped worker perspective is empirically accurate—that agent experiences maximum extraction with no exit. The rope classification from the professional association perspective is also accurate—that actor genuinely solves a coordination problem. Neither is 'the' answer; both are correct descriptions of different positions in the extraction architecture. The scaffold and piton perspectives show temporal dynamics: the skills-based hiring coalition is building an exit path (sunset), while legacy degree requirements persist through institutional inertia (degradation).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_skill_correlation_magnitude,
    'What proportion of wage differences between credentialed and non-credentialed workers reflects true skill differences versus institutional credential premium (rent)?',
    'Longitudinal studies comparing job performance of credentialed vs. skills-verified non-credentialed workers; within-job wage analysis controlling for actual task performance; sector-specific studies (tech, skilled trades, healthcare)',
    'High correlation (> 0.7): credential requirement is primarily coordination function, snare classification overstated. Low correlation (< 0.4): credential requirement is primarily extraction mechanism, snare and tangled rope classifications confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_skill_correlation_magnitude, empirical, 'Magnitude of credential premium attributable to pure skill signaling vs. institutional gatekeeping').

omega_variable(
    alternative_verification_effectiveness,
    'Do skills-based hiring mechanisms (work samples, portfolio reviews, apprenticeships, competency assessments) successfully replace credential signals without quality degradation?',
    'Comparison of hiring quality metrics (retention, performance, promotion) between credential-filtered and skills-filtered hires; long-term employment outcomes; error rate differential',
    'If effective: scaffold sunset is real, extraction mechanism will decline. If ineffective: skills-based hiring remains marginal, credential requirement persists, snare classification remains primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_verification_effectiveness, empirical, 'Whether skills-based hiring provides equivalent quality signal to credentials').

omega_variable(
    credential_atrophy_in_fields,
    'In fields where credential requirements have been relaxed or eliminated (e.g., tech, skilled trades), does labor quality decline, remain stable, or improve?',
    'Sector-level analysis of fields that dropped degree requirements; comparison of work quality metrics before/after; wage trajectories and mobility',
    'Quality decline or wages crash: credential requirement was functional, mountain perspective gains strength. Quality stability or improvement: credential requirement was primarily extractive, snare/tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_atrophy_in_fields, empirical, 'Quality and wage effects of credential requirement relaxation').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.65) primarily structural (legal requirements, hiring system barriers) or internalized (workers believe credentials are necessary even when gatekeeping does not)?',
    'Post-exit trajectory analysis: when workers acquire alternative credentials or switch to skills-based employers, does suppression persist? Survey analysis of worker beliefs about credential necessity vs. actual gating outcomes',
    'If structural: suppression declines post-exit. If internalized: suppression persists in belief even after barrier removal—constraint is psychologically self-enforcing. Informs whether escape from credential system genuinely liberates or requires identity reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in credential gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_skill_arbitrage, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmsa_tr_t0, labor_market_skill_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lmsa_tr_t15, labor_market_skill_arbitrage, theater_ratio, 15, 0.42).
narrative_ontology:measurement(lmsa_tr_t30, labor_market_skill_arbitrage, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(lmsa_be_t0, labor_market_skill_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(lmsa_be_t15, labor_market_skill_arbitrage, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(lmsa_be_t30, labor_market_skill_arbitrage, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_skill_arbitrage, information_standard).
narrative_ontology:affects_constraint(labor_market_skill_arbitrage, occupational_licensing_gatekeeping).
narrative_ontology:affects_constraint(labor_market_skill_arbitrage, degree_inflation_educational_cost).
narrative_ontology:affects_constraint(labor_market_skill_arbitrage, employer_hiring_discrimination).

% DUAL FORMULATION NOTE:
% Labor market skill arbitrage decomposes into three structurally distinct constraints: (1) occupational_licensing_gatekeeping (legal credential requirements, ε ≈ 0.68, snare), (2) degree_inflation_educational_cost (unnecessary credential proliferation, ε ≈ 0.45, tangled rope), and (3) employer_hiring_discrimination (credential filters in hiring systems, ε ≈ 0.48, tangled rope). The current story aggregates these three constraints. For finer analysis, separate stories per constraint would enable tracking which sector contributes most to overall extractiveness and which intervention (licensing reform, degree unbundling, hiring system redesign) has highest impact on extraction reduction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_skill_arbitrage, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
