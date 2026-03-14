% ============================================================================
% CONSTRAINT STORY: hiring_bias_perpetuation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiring_bias_perpetuation, []).

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
 *   constraint_id: hiring_bias_perpetuation
 *   human_readable: Hiring Bias Perpetuation in Professional Labor Markets
 *   domain: labor_economics/organizational_behavior
 *
 * SUMMARY:
 *   Hiring bias perpetuation is a structural constraint that simultaneously
 *   functions as coordination mechanism, extraction system, degraded ritual,
 *   and temporary problem depending on the observer's institutional position.
 *   The constraint arises from information asymmetry in labor markets (how
 *   can firms identify competent candidates without knowing them?) but has
 *   evolved into a system that perpetuates demographic advantage through
 *   credential gatekeeping, network homophily, and interview heuristics.
 *   Extractiveness (0.58) reflects genuine benefit capture by incumbent
 *   groups during hiring — they face reduced competition, can leverage
 *   cultural capital and networks to signal competence, and maintain
 *   occupational segregation. Suppression (0.68) reflects significant
 *   barriers to exit: excluded candidates face systematic screening, must
 *   accumulate excess credentials to overcome bias, face geographic mobility
 *   requirements, and experience identity-locked internalization of biased
 *   narratives. Theater ratio (0.65) reflects that formal hiring procedures
 *   (structured interviews, competency assessments, credential verification)
 *   are partly performative: they create an appearance of objective
 *   evaluation while reproducing the same demographic patterns. The
 *   extractiveness trend shows initial growth (0.42 → 0.58 over 20 years) as
 *   credential inflation and hiring ritual increase, with some moderation
 *   (0.58 → 0.55 at year 30) as diversity initiatives begin showing effects.
 *   Theater ratio drifts upward (0.55 → 0.68) as hiring procedures accumulate
 *   more performative elements — the more elaborate the evaluation ritual,
 *   the less its predictive validity.
 *
 * KEY AGENTS:
 *   - Excluded Job Seekers: Primary victim (powerless/trapped) — face systematic screening, credential inflation, network gatekeeping, and career foreclosure with no exit options
 *   - Marginalized Professionals: Secondary victim (moderate/constrained) — experience mixed coordination (transparent hiring rules reduce some subjective bias) and extraction (overqualification requirements, repeated merit-proving)
 *   - Incumbent Demographic Groups: Primary beneficiary (institutional/arbitrage) — capture competitive advantage through network effects, cultural capital, and credential prestige; can arbitrage to different industries and maintain advantage
 *   - Hiring Managers and Organizational Gatekeepers: Beneficiary agents (institutional/arbitrage) — exercise discretion in filtering and selection; experience bias as natural heuristic and comfortable culture fit
 *   - Diversity Initiative Coalition: Organized agents (organized/constrained) — HR departments, regulatory bodies, advocacy coalitions building structural alternatives (blind resume review, diverse panels, pipeline development) with generational sunset logic
 *   - Legacy Credentialing System: Institutional actor (institutional/arbitrage) — maintains performative signaling function; persists through inertia and network effects despite reduced predictive validity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent information economics limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiring_bias_perpetuation, 0.58).
domain_priors:suppression_score(hiring_bias_perpetuation, 0.68).
domain_priors:theater_ratio(hiring_bias_perpetuation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiring_bias_perpetuation, extractiveness, 0.58).
narrative_ontology:constraint_metric(hiring_bias_perpetuation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hiring_bias_perpetuation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiring_bias_perpetuation, tangled_rope).
narrative_ontology:human_readable(hiring_bias_perpetuation, "Hiring Bias Perpetuation in Professional Labor Markets").
narrative_ontology:topic_domain(hiring_bias_perpetuation, "labor_economics/organizational_behavior").

domain_priors:requires_active_enforcement(hiring_bias_perpetuation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiring_bias_perpetuation, incumbent_demographic_groups).
narrative_ontology:constraint_beneficiary(hiring_bias_perpetuation, hiring_managers).
narrative_ontology:constraint_beneficiary(hiring_bias_perpetuation, organizational_gatekeepers).
narrative_ontology:constraint_victim(hiring_bias_perpetuation, excluded_demographic_groups).
narrative_ontology:constraint_victim(hiring_bias_perpetuation, labor_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED JOB SEEKER (SNARE) — Trapped within biased hiring systems with no meaningful exit. Faces systematic screening out at resume phase, unconscious bias in interviews, network gatekeeping, and credential inflation. Bears full extraction: denied opportunities, wage suppression, career foreclosure. No organization or collective power; individual exit to self-employment faces capital and legitimacy barriers.
constraint_indexing:constraint_classification(hiring_bias_perpetuation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINALIZED PROFESSIONAL (TANGLED ROPE) — Constrained by cost of credential accumulation, geographic mobility requirements, and network dependence. Experiences genuine coordination benefit: formalized hiring reduces some subjective discrimination if mechanisms are transparent. Also experiences extraction: must overqualify, navigate microaggressions, prove merit repeatedly. Has some agency through advocacy and occupational switching, but costs are substantial.
constraint_indexing:constraint_classification(hiring_bias_perpetuation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT BENEFICIARY GROUP (ROPE) — Institutional actors (majority demographic groups with cultural capital and network advantage) see hiring bias as pure coordination: their resume filters match their own background, their networks produce qualified candidates, their interview comfort signals 'culture fit.' Net beneficiary through reduced competition and maintained status advantage. Arbitrage option: can switch industries and maintain advantage; can relocate and find similar-demographic networks elsewhere.
constraint_indexing:constraint_classification(hiring_bias_perpetuation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIVERSITY INITIATIVE COALITION (SCAFFOLD) — Organized agents (HR departments, diversity committees, regulatory compliance frameworks, activist coalitions) see hiring bias as a temporary market failure with architectural solutions: structured interviews, blind resume review, diverse hiring panels, affirmative action policies. Low effective extraction because this coalition has agency and sees concrete sunset mechanisms (generational talent acquisition, network diversification, regulatory compliance). Theater ratio moderate: diversity initiatives themselves have performative elements (DEI theater) but produce measurable change when coupled with structural mechanisms.
constraint_indexing:constraint_classification(hiring_bias_perpetuation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CREDENTIALING SYSTEM (PITON) — Formalized education and certification (MBA, Ivy League degree, professional licensure) were originally designed to coordinate labor market information asymmetry and signal competence. Now substantially performative: credential inflation (advanced degrees for entry-level roles), prestige signaling divorced from functional competence, network-based credential access. System persists through institutional inertia despite low functional verification of competence. Theater ratio high (0.65–0.75): much hiring ritual focuses on credential display rather than performance prediction.
constraint_indexing:constraint_classification(hiring_bias_perpetuation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information asymmetry in hiring is inherent to the employment relationship: firms cannot perfectly predict worker productivity, and workers cannot perfectly signal competence. Statistical discrimination and in-group preference are immutable features of information scarcity. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that what is naturalizable as information economics is actually contingent on specific institutional mechanisms (credential gatekeeping, network homophily, resume screening rules) that can be redesigned.
constraint_indexing:constraint_classification(hiring_bias_perpetuation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiring_bias_perpetuation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiring_bias_perpetuation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiring_bias_perpetuation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hiring_bias_perpetuation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiring_bias_perpetuation, TR),
    TR >= 0.70.

:- end_tests(hiring_bias_perpetuation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not maximal. The original research group in this domain (economists studying hiring discrimination) estimated bias costs to excluded candidates at 10–40% wage penalty plus career foreclosure, reflecting moderate-to-high extraction. The value of 0.58 reflects that some of the competitive advantage is legitimate first-mover reward (networks genuinely reduce search costs for firms) but significant portion is extractive rent (credential inflation, demographic filtering, unconscious bias). The trend from 0.42 to 0.58 over 20 years reflects credential inflation (hiring requirements have risen faster than job complexity) and ritual elaboration (interview procedures have accumulated performative elements). Suppression (0.68): High. Barriers to exit include: credential requirements (2–4 additional years for many excluded groups), network gatekeeping (must overcome referral bias to be considered), geographic mobility (must relocate to access diverse networks), legal and legitimate discrimination (allowed to discriminate on many correlated dimensions), and psychological internalization (many candidates accept biased narrative and reduce effort). Some candidates exit through self-employment, occupational switching, or labor force withdrawal — these are exits but at substantial cost. Theater ratio (0.65): Moderate-high. Formal hiring procedures (structured interviews, rubrics, assessments) create appearance of objective evaluation while hiring outcomes show persistent demographic patterns. The ritual has increased over time (more elaborate procedures, more documentation) without corresponding improvement in demographic outcomes, indicating increasing performative content.
 *
 * PERSPECTIVAL GAP:
 *   The excluded job seeker perceives maximized extraction (snare) because they face systematic barriers and no coordination benefit — hiring bias is pure cost. The incumbent beneficiary perceives pure coordination (rope) because their networks and credentials genuinely signal quality *within their own cultural frame* — they experience the constraint as solving a real problem (finding qualified candidates). The diversity coalition perceives a solvable temporary problem (scaffold) because they have agency and see concrete mechanisms (blind resume review, structured interviews, diverse panels) that reduce bias while accommodating legitimate competence concerns. The hiring manager perceives natural heuristic (rope or piton depending on awareness of bias) because in-group preference feels like good judgment. The analytical observer risks naturalizing the constraint as inherent information economics limitation (mountain), but the structural data reveals it as contingent institutional design: different hiring mechanisms (skill tests, performance trials, transparent criteria) produce very different bias outcomes, showing that bias perpetuation is not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by the agent's structural position relative to the bias constraint. Incumbent demographic groups have low d (near 0.0) because the constraint subsidizes them — they benefit from reduced competition and can easily signal competence through existing networks. Excluded job seekers have high d (near 0.95) because the constraint extracts from them — they face systematic barriers and bear the cost. The sigmoid f(d) translates these to experienced extractiveness: beneficiaries experience negative chi (they gain), targets experience high chi (they lose). Hiring managers occupy a middling position (d ≈ 0.50) — they benefit from biased outcomes but also face organizational pressure to diversify and legal exposure from discrimination lawsuits. The diversity coalition has constrained exit (d ≈ 0.60) — they fund and build alternatives to bias, but encounter organizational resistance and network inertia. Each perspective's directionality feeds into chi through the formula, producing the classification pattern: beneficiaries see rope (low chi), targets see snare (high chi), organized challengers see scaffold (moderate chi with sunset).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing that hiring bias is genuinely a tangled rope — it contains BOTH a coordination function (reducing information asymmetry in hiring) AND asymmetric extraction (benefiting incumbents while extracting from excluded groups). The equilibrium is not pure extraction (snare) masked as coordination, nor pure coordination (rope) with incidental side effects. Rather, the constraint *requires* the coordination function to sustain the extraction: the bias is economically rational (firms use heuristics to screen candidates) but socially harmful (those heuristics reproduce demographics). The mandatrophy fails if the classification collapses to pure snare (ignoring that some hiring does coordinate useful information) or to pure rope (ignoring that bias extracts significant rent). The true classification — tangled rope — shows that solving hiring bias requires addressing both functions: provide better coordination mechanisms (structured interviews, skills assessments) that don't depend on demographic proxies, while simultaneously enforcing against extractive gatekeeping (credential inflation, network monopolization). The diversity initiatives succeed to the extent they decouple coordination from extraction, allowing the coordination function to persist while removing the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statistical_discrimination_vs_taste_discrimination,
    'Is observed hiring bias driven by rational statistical inference (group-based competence beliefs) or by preference-based animus (taste-based discrimination)?',
    'Controlled experiments: resume audits with identical qualifications across demographic groups; gap analysis between performance-based and demographic-based selection; cognitive testing of hiring manager beliefs about group productivity',
    'If statistical: bias may self-correct as selection outcomes reveal true productivity; structural solution is better information flow. If taste-based: bias persists despite evidence; structural solution requires enforcement and incentive realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statistical_discrimination_vs_taste_discrimination, empirical, 'Whether hiring bias is statistical inference or taste-based preference').

omega_variable(
    network_effect_inertia_vs_discrimination,
    'How much of observed bias perpetuation is driven by discriminatory preferences vs. by network homophily and information asymmetry inherent to referral hiring?',
    'Decomposition analysis: comparison of bias rates in referral vs. open-application hiring; measurement of information quality in referred vs. unreferred candidates; structural analysis of hiring outcomes by recruitment channel',
    'If network-driven: diversity initiatives targeting recruitment pipelines (community recruiting, blind referrals, diverse interview panels) can reduce bias without requiring preference change. If preference-driven: requires organizational culture work or regulatory enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_inertia_vs_discrimination, empirical, 'Relative contribution of network effects vs. preference-based discrimination').

omega_variable(
    credential_signaling_sufficiency,
    'Do educational credentials and past employment history accurately predict future job performance, or do they primarily serve as demographic filtering mechanisms?',
    'Correlation analysis: relationship between credential prestige and actual job performance; measurement of predictive validity of hiring criteria vs. demographic composition of hired cohorts; longitudinal tracking of performance variance explained by credentials vs. by demographics',
    'If credentials are sufficient predictors: credential-based screening is legitimate coordination (reduces information asymmetry). If primarily demographic filters: credential inflation is the extraction mechanism; structural solution requires performance-based rather than credential-based selection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signaling_sufficiency, empirical, 'Whether credentials accurately predict job performance or serve as demographic filters').

omega_variable(
    identity_locked_internalization,
    'To what extent do excluded candidates internalize the biased narrative (lacking competence, poor fit, insufficient qualification) rather than attributing rejection to discrimination?',
    'Exit trajectory analysis: labor force participation, wage paths, and occupational segregation for candidates who exit after rejection; comparative analysis of persistence in job search by demographic group; cognitive frame testing of rejected candidates'' interpretations',
    'If high internalization: identity-locked exit (candidates accept narrative and reduce effort); constraint perpetuates through psychological mechanism even without continued enforcement. If low internalization: candidates seek alternative pathways (self-employment, different industries); constraint requires active enforcement to sustain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_internalization, empirical, 'Degree of identity-locked internalization among excluded job seekers').

omega_variable(
    diversity_initiative_effectiveness_plateau,
    'Do diversity hiring initiatives durably shift hiring patterns, or do they produce temporary improvement followed by regression once enforcement attention relaxes?',
    'Longitudinal cohort tracking: hiring outcomes before/during/after diversity initiative implementation; measurement of demographic composition stability across hiring cycles; analysis of promotion and retention patterns for diversely hired cohorts vs. self-selection bias in attrition',
    'If durable: scaffold sunset mechanisms (hiring norms shift, network diversity compounds) are real; constraint improves over generational horizon. If temporary: initiatives are performative theater; underlying bias perpetuation mechanism reasserts itself; requires continuous enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_initiative_effectiveness_plateau, empirical, 'Whether diversity initiatives produce durable or temporary hiring pattern change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiring_bias_perpetuation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hire_bias_tr_t0, hiring_bias_perpetuation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hire_bias_tr_t10, hiring_bias_perpetuation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(hire_bias_tr_t20, hiring_bias_perpetuation, theater_ratio, 20, 0.65).
narrative_ontology:measurement(hire_bias_tr_t30, hiring_bias_perpetuation, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(hire_bias_be_t0, hiring_bias_perpetuation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hire_bias_be_t10, hiring_bias_perpetuation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(hire_bias_be_t20, hiring_bias_perpetuation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hire_bias_be_t30, hiring_bias_perpetuation, base_extractiveness, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiring_bias_perpetuation, resource_allocation).
narrative_ontology:affects_constraint(hiring_bias_perpetuation, wage_gap_perpetuation).
narrative_ontology:affects_constraint(hiring_bias_perpetuation, occupational_segregation).
narrative_ontology:affects_constraint(hiring_bias_perpetuation, intergenerational_inequality).

% DUAL FORMULATION NOTE:
% Hiring bias perpetuation is the upstream constraint enabling wage gap perpetuation (downstream) and occupational segregation (downstream). The three constraints form a family: hiring bias determines who enters specific occupations and firms; that initial allocation determines lifetime wage trajectory and segregation patterns. Each has its own extractiveness value but they are causally coupled — hiring bias is the initial sorting mechanism that makes downstream inequality stable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiring_bias_perpetuation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
