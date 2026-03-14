% ============================================================================
% CONSTRAINT STORY: credential_inflation_employment_gate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_inflation_employment_gate, []).

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
 *   constraint_id: credential_inflation_employment_gate
 *   human_readable: Credential Inflation as Employment Gating Mechanism
 *   domain: labor_economics/education_policy
 *
 * SUMMARY:
 *   Credential inflation represents a structural constraint where job market
 *   signaling and employer screening mechanisms have become decoupled from
 *   actual task requirements. Entry-level positions that once required high
 *   school diplomas or apprenticeship now mandate bachelor's degrees;
 *   mid-level roles require master's degrees; technical positions require
 *   certifications beyond job necessity. This constraint exhibits
 *   coordinating and extracting functions simultaneously: genuine labor
 *   market signaling exists (degrees do correlate with general capability),
 *   but credential inflation has outpaced functional necessity. The
 *   suppression is high (0.68) because alternatives to degree-gated hiring —
 *   apprenticeship, portfolio assessment, work sample testing — have been
 *   systematically displaced by credential requirements that reduce employer
 *   hiring friction. Theater ratio (0.65) reflects that credential
 *   verification is largely performative: employers rarely verify degree
 *   completion or course content relevance; the credential functions as a
 *   filter that signals 'this applicant passed a sunk-cost threshold,' not
 *   'this applicant has specific job-relevant skills.' The extractiveness
 *   trajectory shows steady inflation over thirty years: credential
 *   requirements have ratcheted upward as educational attainment increased,
 *   creating a coordination failure where employer credential-setting is
 *   independent and results in collective credential inflation that benefits
 *   no one except credential issuers.
 *
 * KEY AGENTS:
 *   - Entry-Level Job Seekers: Primary victims (powerless/trapped) — face credential barriers without viable alternatives; bear costs of tuition, time, debt, and opportunity cost
 *   - Non-Degree Pathway Workers: Secondary victims (moderate/constrained) — skilled trade workers, apprentices, portfolio-builders blocked from credential-required roles despite competence
 *   - Credential Issuers (Universities, Certifiers): Primary beneficiaries (institutional/arbitrage) — extract tuition, enrollment growth, and labor market gatekeeping power from credential inflation
 *   - Incumbent Credentialed Workers: Secondary beneficiaries (powerful/mobile) — benefit from market protection; credentials shield them from lower-credentialed competition
 *   - Employers (via HR Departments): Mixed (institutional/arbitrage) — benefit from credential filtering reducing screening costs; see constraint as coordination, not extraction
 *   - Labor Market Coordination Mechanisms: Victim (abstract/trapped) — credential inflation degrades signal quality; true ability-job fit becomes harder to assess as credentials proliferate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_inflation_employment_gate, 0.58).
domain_priors:suppression_score(credential_inflation_employment_gate, 0.68).
domain_priors:theater_ratio(credential_inflation_employment_gate, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_inflation_employment_gate, extractiveness, 0.58).
narrative_ontology:constraint_metric(credential_inflation_employment_gate, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(credential_inflation_employment_gate, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_inflation_employment_gate, tangled_rope).
narrative_ontology:human_readable(credential_inflation_employment_gate, "Credential Inflation as Employment Gating Mechanism").
narrative_ontology:topic_domain(credential_inflation_employment_gate, "labor_economics/education_policy").

domain_priors:requires_active_enforcement(credential_inflation_employment_gate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_inflation_employment_gate, credential_issuers).
narrative_ontology:constraint_beneficiary(credential_inflation_employment_gate, incumbent_credentialed_workers).
narrative_ontology:constraint_victim(credential_inflation_employment_gate, entry_level_job_seekers).
narrative_ontology:constraint_victim(credential_inflation_employment_gate, non_degree_pathway_workers).
narrative_ontology:constraint_victim(credential_inflation_employment_gate, low_income_education_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL JOB SEEKER (SNARE) — Faces credential requirements that have inflated far beyond functional job necessity. No viable exit: cannot access employment without credentials; cannot afford credentials without employment or family support. Trapped in a requirement cycle with no alternative pathways. Bears maximum extraction — time, financial debt, and opportunity cost while credential value erodes.
constraint_indexing:constraint_classification(credential_inflation_employment_gate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED TRADE WORKER (TANGLED ROPE) — Experiences gating in sectors where traditional apprenticeship once functioned. Some genuine coordination benefit (shared credential standards enable labor mobility and quality assurance). But also experiences extraction: credential requirements filter out competitors and protect incumbent wage premiums. High suppression due to credential gatekeeping but mixed coordination/extraction dynamic.
constraint_indexing:constraint_classification(credential_inflation_employment_gate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIAL ISSUER (ROPE) — Universities and credentialing bodies experience the constraint as coordination mechanism: standardizing credentials enables labor market signaling and employer coordination. Can exit by reforming credential requirements or by being displaced by alternative credentialing systems (bootcamps, portfolio assessment). Arbitrage exit enables flexible disengagement. Net beneficiary but sees constraint as functional coordination.
constraint_indexing:constraint_classification(credential_inflation_employment_gate, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT CREDENTIALED WORKER (TANGLED ROPE) — Experiences genuine coordination benefit from credential standardization (ensures job market portability and wage stability). Also benefits from extraction: credential inflation protects their market position from lower-credentialed competition. Mixed position — net beneficiary but also participating in the extraction mechanism. Mobile exit options (credential remains portable across employers) but constrained by switching costs.
constraint_indexing:constraint_classification(credential_inflation_employment_gate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMAN RESOURCES DEPARTMENT (PITON) — Uses credential requirements as a filtering heuristic despite knowing that many credential-required roles don't actually require the credential level. The theater ratio is high: credential filtering persists because it reduces HR review load and provides legal cover ('we have objective criteria'), not because degrees are functionally necessary. Performative credential checking persists through institutional inertia. Alternative assessment mechanisms (skill tests, portfolio review) could replace credential theater but would require higher HR scrutiny.
constraint_indexing:constraint_classification(credential_inflation_employment_gate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk perspective: the analyst may see credential inflation as an immutable feature of modern labor markets, a natural consequence of educational expansion and signaling theory. From this view, credential requirements are inherent to how labor markets solve information asymmetries. However, the structural data contradicts this — the constraint exhibits suppression (0.68) and theater (0.65) that reveal contingent institutional choices rather than natural law. Historical periods with lower credential inflation despite comparable labor market uncertainty reveal that the current arrangement is not immutable.
constraint_indexing:constraint_classification(credential_inflation_employment_gate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_inflation_employment_gate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_inflation_employment_gate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_inflation_employment_gate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_inflation_employment_gate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_inflation_employment_gate, TR),
    TR >= 0.70.

:- end_tests(credential_inflation_employment_gate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts from entry-level job seekers (forced credential purchases) and non-degree workers (exclusion from credential-required roles). But the extraction is not maximal because: (1) some genuine signaling function remains — degrees do correlate with general capability; (2) some alternative pathways exist (bootcamps, self-teaching, freelance); (3) credential value degrades as inflation increases, reducing extraction magnitude. The trajectory shows steady increase from 0.32 to 0.68, indicating layering of requirements over time. Suppression (0.68): High. Barriers to non-credential pathways include: (1) employer credential-only filtering; (2) weak alternative credentialing infrastructure; (3) career narrative bias (credentialed career progression is publicly visible; alternative pathways are less visible); (4) legal liability concerns for employers (credentials provide defensibility). Theater ratio (0.65): Moderately high. Credential verification is largely performative — employers rarely verify GPA, check coursework relevance, or assess retained knowledge. The credential functions as a costly signal of 'willingness to comply with institutional requirements,' not as verification of job-relevant capability. Theater has increased over the interval as credential inflation has continued despite evidence that requirements outpace functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   The credential inflation constraint shows how the same structural mechanism appears as coordination to beneficiaries and extraction to victims. Credential issuers and incumbent workers genuinely experience the constraint as coordination: it solves the signaling problem and protects against quality degradation (or so the framing goes). Entry-level seekers experience it as pure snare: they cannot access jobs without credentials, cannot afford credentials without jobs or family support, and cannot prove competence without credentials even if they have the capability. The gap is not about different measurements of the same thing — it's about whether the constraint actually solves a coordination problem (genuine signaling necessity) or whether it manufactures a filtering mechanism that extracts tuition and excludes competitors. The theater ratio (0.65) suggests the latter: if credentials were genuinely necessary signals, employers would verify them more carefully.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in credential inflation flows from beneficiary to victim: credential issuers extract value from job seekers through required credential purchases; incumbent credentialed workers extract value through market protection. The derivation chain produces: beneficiary (credential issuer) + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.08 (negative effective extraction, i.e., benefit accrues to this agent); victim (entry-level seeker) + trapped exit → d ≈ 0.92 → f(d) ≈ 1.35 (maximum experienced extraction). Incumbent workers occupy an intermediate position: they are secondary beneficiaries (protected by credential filtering) but also experience suppression costs (time investment in credential maintenance, credential devaluation as inflation continues). Their directionality is mixed: d ≈ 0.40 (both benefit and bear some cost).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between coordination and extraction by showing that credential inflation is Tangled Rope: it solves a real coordination problem (labor market signaling) while simultaneously enabling extraction (credential requirements filter out non-credentialed competitors). The mandatrophy would arise if the constraint were pure rope (pure coordination with no extraction) or pure snare (pure extraction with no coordination). But the data shows both: genuine coordination benefit (credentials do enable labor market matching) AND genuine extraction (non-degree pathways are systematically excluded despite competence). The theater ratio (0.65) indicates that much of what appears as coordination is actually performative — the credential serves as a costly signal of institutional compliance, not as verification of job-relevant capability. The alternative classification (mountain: 'credential inflation is a natural law of labor market signaling') is false. Historical periods (1960s-1980s) show comparable labor market uncertainty with much lower credential inflation, revealing that the current arrangement is contingent, not immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_credential_necessity,
    'What proportion of credential-required jobs actually require the specified credential level for task performance?',
    'Job task analysis: correlation between credential requirements in job postings and actual skill requirements measured by work sample testing, cognitive assessment, or task decomposition',
    'If < 30% of requirements are functionally necessary: credential inflation is primarily extraction (suppress theater, raise extraction classification). If > 70%: credential inflation reflects genuine signal problem (maintain tangled_rope). If 30-70%: mixed (confirms tangled_rope with high theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_credential_necessity, empirical, 'Proportion of credential requirements that are functionally necessary').

omega_variable(
    alternative_credentialing_scalability,
    'Can portfolio-based assessment, skill testing, or apprenticeship pathways scale to replace degree-gated hiring without sacrificing labor market coordination?',
    'Pilot programs (apprenticeship acceleration, tech bootcamp hiring, portfolio-based entry) compared against degree-gated hiring on retention, performance, and wage trajectory outcomes; network effects analysis for alternative credentialing systems',
    'If scalable: scaffold sunset is real (alternative pathways can replace credential gating). Classification shifts from snare/tangled_rope to scaffold for new entrants. If not scalable: credential inflation will persist despite suppression; reclassify as pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credentialing_scalability, empirical, 'Whether alternative credentialing pathways can scale and replace degree gating').

omega_variable(
    employer_credential_demand_signaling,
    'Do employers require higher credentials as a genuine signal of unobservable ability, or as a screening mechanism to reduce applicant volume and HR processing cost?',
    'Decompose credential requirements by: (a) whether jobs explicitly justify credential level in posting, (b) survey of hiring managers on why credentials required, (c) correlation between credential inflation and HR staffing ratios, (d) A/B testing of credential-blind hiring with equivalent screening mechanisms',
    'If signal necessity dominates: credential requirements reflect genuine information asymmetry, tangled_rope justified. If screening cost dominates: requirements are theater (performative filtering), piton and snare classifications dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_credential_demand_signaling, empirical, 'Whether credential requirements reflect signal necessity or HR screening cost reduction').

omega_variable(
    generational_suppression_internalization,
    'Do job seekers internalize credential inflation as a legitimate, inherent requirement, or perceive it as an extractive barrier?',
    'Cohort comparison: survey/interview data on perceived necessity and legitimacy of credential requirements across generations (pre-credential-inflation cohorts vs current entrants); analysis of career narrative framing in applicant materials',
    'If internalized as legitimate: suppression is lower (agent accepts the requirement as necessary). If perceived as extractive: suppression is structural (external barriers remain high). Mixed internalization indicates identity-locking dynamics (agent is identity-fused with the credentialed pathway).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_suppression_internalization, empirical, 'Whether credential requirements are internalized as legitimate or perceived as extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_inflation_employment_gate, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_infl_tr_t0, credential_inflation_employment_gate, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cred_infl_tr_t10, credential_inflation_employment_gate, theater_ratio, 10, 0.52).
narrative_ontology:measurement(cred_infl_tr_t20, credential_inflation_employment_gate, theater_ratio, 20, 0.65).
narrative_ontology:measurement(cred_infl_tr_t30, credential_inflation_employment_gate, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(cred_infl_be_t0, credential_inflation_employment_gate, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cred_infl_be_t10, credential_inflation_employment_gate, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(cred_infl_be_t20, credential_inflation_employment_gate, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cred_infl_be_t30, credential_inflation_employment_gate, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_inflation_employment_gate, information_standard).
narrative_ontology:affects_constraint(credential_inflation_employment_gate, student_debt_trap).
narrative_ontology:affects_constraint(credential_inflation_employment_gate, apprenticeship_pathway_displacement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credential_inflation_employment_gate, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
