% ============================================================================
% CONSTRAINT STORY: credential_inflation_labor_markets
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_inflation_labor_markets, []).

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
 *   constraint_id: credential_inflation_labor_markets
 *   human_readable: Credential Inflation in Labor Markets
 *   domain: labor_economics/educational_policy
 *
 * SUMMARY:
 *   Credential inflation in labor markets represents a structural constraint
 *   where employers require educational credentials (degrees, certificates,
 *   licenses) for job access in ways that exceed the actual cognitive or
 *   technical demands of the work. The constraint exhibits tangled rope
 *   structure: it genuinely solves a coordination problem (employers need
 *   signals of worker reliability) while simultaneously extracting rents
 *   through credential gatekeeping and imposing barriers that exceed the real
 *   skill requirements. The constraint's extractiveness has risen from 0.28
 *   (1990s labor markets with more skills-based hiring) to 0.58 (2020s labor
 *   markets where bachelor's degrees are standard entry requirements for
 *   clerical and administrative roles that historically required high school
 *   completion). The theater ratio has correspondingly increased: many
 *   credentials persist not because they reliably predict job performance but
 *   because they serve as filtering devices and because the institutions that
 *   grant credentials have institutional interest in maintaining
 *   requirements. The spectrum of perspectives reveals the constraint's
 *   complexity: entry-level workers experience pure extraction (snare),
 *   organizations see genuine coordination benefits alongside extracted rents
 *   (tangled rope), universities see unambiguous benefit (rope), employers
 *   see a coordination problem they cannot exit (tangled rope), credentialing
 *   bodies see degraded-but-maintained rituals (piton), and the analytical
 *   observer risks naturalizing a contingent arrangement as inevitable.
 *
 * KEY AGENTS:
 *   - Entry-Level Workers: Primary victim (powerless/trapped) — blocked from job access without credential investment; face debt burden and time barriers
 *   - Low-Income Job Seekers: Primary victim (powerless/trapped) — lack access to credential investment due to family financial constraints and care obligations
 *   - Educational Institutions: Primary beneficiary (institutional/arbitrage) — capture tuition revenue and enrollment growth from credential inflation
 *   - Credential Gatekeepers: Beneficiary (institutional/arbitrage) — licensing bodies, professional associations benefit from requirement maintenance
 *   - Mid-Career Workers: Secondary victim (moderate/constrained) — face retraining barriers if switching careers; options constrained by education cost
 *   - Employers: Organized agent (organized/constrained) — benefit from screening signal but constrained by shrinking talent pipeline and wage pressure
 *   - Policy Reform Advocates: Analytical agent (analytical/analytical) — attempt to reduce credential inflation through alternative pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_inflation_labor_markets, 0.58).
domain_priors:suppression_score(credential_inflation_labor_markets, 0.65).
domain_priors:theater_ratio(credential_inflation_labor_markets, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_inflation_labor_markets, extractiveness, 0.58).
narrative_ontology:constraint_metric(credential_inflation_labor_markets, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credential_inflation_labor_markets, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_inflation_labor_markets, tangled_rope).
narrative_ontology:human_readable(credential_inflation_labor_markets, "Credential Inflation in Labor Markets").
narrative_ontology:topic_domain(credential_inflation_labor_markets, "labor_economics/educational_policy").

domain_priors:requires_active_enforcement(credential_inflation_labor_markets).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_inflation_labor_markets, educational_institutions).
narrative_ontology:constraint_beneficiary(credential_inflation_labor_markets, credential_gatekeepers).
narrative_ontology:constraint_victim(credential_inflation_labor_markets, entry_level_workers).
narrative_ontology:constraint_victim(credential_inflation_labor_markets, low_income_job_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL WORKER (SNARE) — Trapped in a credential requirement spiral. Without a bachelor's degree, access to jobs that once required only high school completion is now blocked. Cannot exit the constraint without investing 4+ years and significant debt. The suppression is structural: barriers include time cost, financial burden, and family care obligations. Maximum experienced extraction — this agent faces a gate that raises continuously as credential inflation spreads.
constraint_indexing:constraint_classification(credential_inflation_labor_markets, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAREER-SWITCHER (TANGLED ROPE) — Constrained by credential requirements but benefits from labor market coordination mechanisms (job boards, professional networks, upskilling programs). Has structural mobility (can invest in education) but faces high cost. Experiences both the coordination function (credentials do signal reliability) and the extractive overlay (credential requirements exceed actual job task complexity). Moderate extraction with meaningful agency.
constraint_indexing:constraint_classification(credential_inflation_labor_markets, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITY SYSTEM (ROPE) — Experiences credential inflation as beneficial coordination. Universities benefit from enrollment increases and revenue stabilization as credential requirements expand. Genuinely provides coordination function: credentials do certify domain knowledge and work-readiness (real benefit). The constraint appears as pure coordination from this perspective — solving the employer's information problem about worker reliability. Net beneficiary with significant revenue capture.
constraint_indexing:constraint_classification(credential_inflation_labor_markets, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPLOYERS (TANGLED ROPE) — Face a coordination dilemma: credentials genuinely solve information problems (reliability signal) but credential inflation creates matching inefficiency. Employers increasingly demand degrees not because job tasks require them but because signal-to-noise ratio has degraded. Organized agents see both benefit (reliable screening) and cost (reduced talent pipeline depth, wage pressure). Can lobby for policy changes (alternative credentials) but constrained by collective action problems. Moderate-to-significant extraction.
constraint_indexing:constraint_classification(credential_inflation_labor_markets, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LICENSING APPARATUS (PITON) — Professional licensing bodies (bar associations, medical boards, engineering societies) maintain credential requirements partly for genuine quality assurance and partly through institutional inertia. Theater ratio is high: many licensing requirements persist long after technology or practice changes would permit lower barriers. The apparatus maintains itself through prestige and gatekeeping even as the functional verification value has degraded. Sees its own process as bureaucratic burden but continues it because alternatives haven't fully emerged.
constraint_indexing:constraint_classification(credential_inflation_labor_markets, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, information asymmetries in labor markets create structural limits on trust between employers and workers. Some credential requirement is inherent to matching high-skill jobs with capable workers. But the structural data reveals this as naturalizing a contingent institutional arrangement: the scope and specificity of credential inflation exceed what information economics would require. The false summit detector will flag this perspective as misapplying the natural law frame.
constraint_indexing:constraint_classification(credential_inflation_labor_markets, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_inflation_labor_markets_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_inflation_labor_markets, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_inflation_labor_markets, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_inflation_labor_markets, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_inflation_labor_markets, TR),
    TR >= 0.70.

:- end_tests(credential_inflation_labor_markets_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through three mechanisms: (1) time cost (4+ years per credential for entry-level workers), (2) financial cost (average ~$30k-100k total debt per degree), and (3) opportunity cost (foregone wages and career development time). The extractiveness value reflects that these costs are not proportional to the actual skills required for many jobs. The value has increased over the 20-year interval as credential inflation has accelerated — jobs that required high school completion in 1995 now require bachelor's degrees in 2015. Suppression (0.65): High. Multiple barriers prevent exit: (a) structural — educational access, family care obligations, geographic barriers to quality education; (b) informational — workers lack signals that alternative credentials are accepted; (c) institutional — employers continue demanding credentials even when alternatives would suffice. The suppression is maintained by institutional inertia (credentialing bodies maintain old requirements) and by genuine information asymmetries (employers cannot easily assess worker quality without credentials). Theater ratio (0.68): High and rising. Many credential requirements persist through institutional ritual rather than functional necessity. Licensing requirements for jobs with stable, non-technical duties are iconic examples: barber licenses, real estate licenses, and other professional credentials often test knowledge that does not predict job performance. The theater has increased as credential requirements have expanded into domains (administrative roles, entry-level technical support) where the credential genuinely adds little information value beyond filtering cost.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates multiple legitimate perspectives on identical structural data. The university system sees pure coordination and genuine benefit (rope) — they are solving the real problem of certifying worker reliability. Employers see mixed benefit and burden (tangled rope) — they use credentials for screening but recognize the inefficiency from credential inflation. Entry-level workers see extraction (snare) — they face barriers that exceed the real skill requirements and have no alternative exit. Mid-career workers see constrained options (tangled rope) — they could in principle switch careers but face retraining costs that exceed what career structure would support. The credentialing apparatus sees degraded ritual (piton) — peer review of licensing requirements would show many exceed functional necessity, yet institutional inertia maintains them. The perspectival gap reveals that credential inflation is not a question of 'is this good?' but 'from whose structural position?' The answer depends on whether you have capital (time and money) to invest in credentials or whether you lack it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural relationship to the constraint. Entry-level workers are pure victims — they pay costs and receive no benefits from the credential requirement (0.95 d value, high f(d), maximum experienced extraction). Universities are beneficiaries — they capture tuition revenue and enrollment increases without bearing credential inflation costs (0.08 d value, negative f(d), negative/minimal experienced extraction). Employers are mixed — they benefit from the screening signal (lower d) but are constrained by the shrinking talent pipeline (higher d) relative to pure beneficiary position. The directionality chain reveals why the constraint persists: beneficiaries (universities, credentialing bodies) experience low or negative extraction and thus perceive the constraint as beneficial coordination, while victims (entry-level workers) experience maximum extraction and perceive it as pure barrier. The asymmetry in directionality explains the perspectival gap and the stability of the constraint despite harm to powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   Credential inflation resolves mandatrophy by disaggregating the constraint across perspectives. The constraint is simultaneously genuine coordination (credentials do solve information problems) and genuine extraction (credential requirements exceed information needs). The mandatrophy is resolved not by choosing one classification but by recognizing that the presheaf of perspectives across the indexed lattice shows: (1) how different agents experience the same constraint differently, and (2) that the constraint's design benefits beneficiaries (universities, credentialing bodies) while harming victims (entry-level workers) in ways that cannot be fully justified by the coordination benefits. The analytical observer's mountain perspective is a false summit: credential requirements are not immutable laws of labor markets but contingent institutional arrangements that reflect power asymmetries between beneficiaries and victims. The tangled rope classification emerges as the constraint's true structure: it coordinates matching between employers and workers while extracting rents through credential gatekeeping.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_skill_mismatch_magnitude,
    'How much of the credential requirement gap between job posting and actual task complexity is genuine information asymmetry vs. institutional inertia?',
    'Task complexity audits: measure actual job duties and required cognitive skills; compare against credential prerequisites. Historical analysis of when specific credentials were first required vs. when job tasks changed.',
    'If >60% is inertia: credential inflation is primarily extractive (Snare from worker perspective). If <30% is inertia: credential inflation solves real information problems (Rope from institutional perspective is closer to accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_skill_mismatch_magnitude, empirical, 'Credential requirement gap attributable to institutional inertia vs. genuine skill requirements').

omega_variable(
    alternative_certification_viability,
    'Do alternative credentials (bootcamps, portfolios, apprenticeships, competency-based assessments) provide equivalent signals to traditional degrees for employer hiring decisions?',
    'Cohort comparison: track hiring outcomes, wage trajectories, and job retention for workers with alternative credentials vs. traditional degrees in same roles. Employer stated preferences vs. revealed preferences analysis.',
    'If viable: the credential inflation constraint has a real exit pathway (scaffold dynamics). If not viable: pathway to exit is blocked even for organized agents, making suppression higher and extraction deeper (snare-like for all but top-tier alternative credentials).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_certification_viability, empirical, 'Whether alternative credentials provide equivalent signals for labor market access').

omega_variable(
    debt_trap_mechanism_boundary,
    'At what credential cost threshold does education debt create material lock-in that transforms ''constrained'' agents into effectively ''trapped'' ones?',
    'Debt-to-income analysis: measure at what debt/income ratio agents'' exit options narrow (reduced geographic mobility, delayed family formation, inability to change careers). Longitudinal tracking of repayment burden vs. labor market mobility.',
    'If threshold is low (~1.5× annual income): credential debt creates trap-like conditions for most workers (snare perspective more accurate). If threshold is high (~3.5× annual income): debt is onerous but mobility persists (tangled rope more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_trap_mechanism_boundary, empirical, 'Debt threshold at which education financing transforms exit options from constrained to trapped').

omega_variable(
    institutional_capture_of_policy_reform,
    'Do educational institutions and credential gatekeepers actively resist policy reforms that would reduce credential inflation, and if so, how effectively?',
    'Policy analysis: track proposed reform bills and track opposition positions. Lobbying expenditure analysis. Comparison of credential requirement changes in jurisdictions with vs. without institutional regulatory capture.',
    'If capture is effective: constraint is maintained by organized beneficiaries against victim interests (Snare dynamics dominate). If capture is weak: reform pathways are open and suppression is lower (Scaffold dynamics more viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_policy_reform, empirical, 'Degree of institutional capture preventing credential inflation reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_inflation_labor_markets, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(credinf_tr_t0, credential_inflation_labor_markets, theater_ratio, 0, 0.45).
narrative_ontology:measurement(credinf_tr_t10, credential_inflation_labor_markets, theater_ratio, 10, 0.58).
narrative_ontology:measurement(credinf_tr_t20, credential_inflation_labor_markets, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(credinf_be_t0, credential_inflation_labor_markets, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(credinf_be_t10, credential_inflation_labor_markets, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(credinf_be_t20, credential_inflation_labor_markets, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_inflation_labor_markets, resource_allocation).
narrative_ontology:affects_constraint(credential_inflation_labor_markets, student_debt_trap).
narrative_ontology:affects_constraint(credential_inflation_labor_markets, skills_gap_signaling).
narrative_ontology:affects_constraint(credential_inflation_labor_markets, labor_market_segmentation).

% DUAL FORMULATION NOTE:
% Credential inflation decomposes into multiple structurally distinct constraints: (1) the coordination problem of employer uncertainty about worker quality (genuine information asymmetry), (2) the extraction mechanism of credential gatekeeping (institutional inertia and rent-seeking), and (3) the labor market segmentation effect of credential barriers. These are linked stories with different ε values and different primary mechanisms. The primary story focuses on the tangled rope structure combining both coordination and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credential_inflation_labor_markets, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
