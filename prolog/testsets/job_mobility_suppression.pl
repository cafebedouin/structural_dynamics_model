% ============================================================================
% CONSTRAINT STORY: job_mobility_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_job_mobility_suppression, []).

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
 *   constraint_id: job_mobility_suppression
 *   human_readable: Job Mobility Suppression
 *   domain: labor_economics/employment_relations
 *
 * SUMMARY:
 *   Job mobility suppression encompasses mechanisms that restrict workers'
 *   ability to exit current employment or change career paths without
 *   significant cost. These mechanisms include non-compete clauses, licensing
 *   requirements, vesting schedules tied to tenure, firm-specific skill
 *   capital, geographic lock-in through housing and family ties, credential
 *   gatekeeping, and psychological identification with professional role. The
 *   constraint exhibits the full spectrum of DR classifications depending on
 *   observer position: powerless workers see it as an inescapable trap
 *   (Snare), skilled workers experience it as a mix of coordination benefits
 *   and extraction costs (Tangled Rope), employers see it as pure
 *   coordination problem (Rope), reform coalitions see it as temporary
 *   problem with policy solutions (Scaffold), institutional HR apparatus
 *   maintains it as degraded ritual (Piton), and the analytical observer
 *   risks naturalizing institutional contingency as inherent labor economics
 *   friction (Mountain). Over the 20-year interval, extractiveness has
 *   increased from 0.35 to 0.58, and theater ratio from 0.45 to 0.65,
 *   reflecting both intensification of suppression mechanisms and expansion
 *   of performative justifications. The constraint's suppression metric
 *   (0.72) reflects multiple barriers: material (equipment costs, geographic
 *   relocation, lost tenure benefits), informational (credential discounting,
 *   tacit knowledge transfer), legal (non-compete enforcement, licensing
 *   reciprocity), and psychological (identity fusion, opportunity cost
 *   internalization).
 *
 * KEY AGENTS:
 *   - Trapped Worker: Primary victim (powerless/trapped) — bears full cost of immobility, experiences maximum extraction through wage suppression and career constraint
 *   - Skilled Mobile Worker: Secondary victim (moderate/constrained) — faces significant but surmountable barriers, experiences mixed coordination benefits and extraction
 *   - Employer Firm: Primary beneficiary (institutional/arbitrage) — benefits from reduced poaching risk, wage suppression, and ability to recoup training investment
 *   - Capital Concentration: Secondary beneficiary (institutional/arbitrage) — benefits from reduced labor negotiating power and increased firm valuation through reduced turnover costs
 *   - Labor-Market Reform Coalition: Organized agent (organized/constrained) — seeks to reduce suppression through non-compete bans, portable benefits, credential standardization with estimated 5-10 year sunset
 *   - HR Theater Apparatus: Institutional actor (institutional/arbitrage) — maintains performative talent development and retention strategies; sees own function as degraded
 *   - Elite Mobile Professional: Powerful agent (powerful/mobile) — retains mobility options but experiences extraction through executive recruitment mechanisms and talent concentration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional suppression as inherent labor friction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(job_mobility_suppression, 0.58).
domain_priors:suppression_score(job_mobility_suppression, 0.72).
domain_priors:theater_ratio(job_mobility_suppression, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(job_mobility_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(job_mobility_suppression, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(job_mobility_suppression, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(job_mobility_suppression, tangled_rope).
narrative_ontology:human_readable(job_mobility_suppression, "Job Mobility Suppression").
narrative_ontology:topic_domain(job_mobility_suppression, "labor_economics/employment_relations").

domain_priors:requires_active_enforcement(job_mobility_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(job_mobility_suppression, employer_firms).
narrative_ontology:constraint_beneficiary(job_mobility_suppression, capital_concentration).
narrative_ontology:constraint_victim(job_mobility_suppression, workers).
narrative_ontology:constraint_victim(job_mobility_suppression, labor_market_dynamism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED WORKER (SNARE) — Worker faces non-compete clauses, licensing requirements, geographic lock-in, and skill-specific capital that transfers poorly. Career capital is firm-specific. Exit would mean abandoning accumulated knowledge, social networks, and resume credentials tied to current employer. Maximum extraction experienced: no alternatives visible, high cost to exit, suppression fully internalized through opportunity cost calculation.
constraint_indexing:constraint_classification(job_mobility_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED MOBILE WORKER (TANGLED ROPE) — High-skill worker (software engineer, manager) retains some mobility but faces significant costs: non-compete enforcement, vesting cliffs, equity compensation loss, reputation risk, credential portability barriers. Experiences genuine coordination benefits (firm-internal development, skill-matching) alongside extraction (wage suppression through immobility, equity lock-in). Significant but not total extraction.
constraint_indexing:constraint_classification(job_mobility_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYER FIRM (ROPE) — Firm benefits from worker immobility through wage suppression, reduced poaching risk, and ability to recoup training investments. Experiences constraint as pure coordination problem: how to retain talent and invest in development in competitive labor market. Non-compete clauses and golden handcuffs solve the retention coordination problem with minimal coercive overhead from the firm's perspective. Beneficiary position with arbitrage exit (can replace workers, relocate operations).
constraint_indexing:constraint_classification(job_mobility_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR-MARKET REFORM COALITION (SCAFFOLD) — Organized agents (antitrust enforcement, labor unions, policy advocates) see suppression as a temporary coordination failure being addressed through non-compete ban legislation, portable benefits reform, and credential standardization. Low effective extraction from this perspective because organized agents have clear exit pathway: regulatory reform with estimated sunset of 5-10 years as state-level non-compete bans propagate and federal harmonization advances.
constraint_indexing:constraint_classification(job_mobility_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HR THEATER APPARATUS (PITON) — Human resources 'talent development' and 'retention strategies' are substantially performative: career pathing discussions without real mobility, development plans locked to firm-specific goals, mandatory loyalty signaling. The machinery persists through institutional inertia (inherited from mid-20th century corporate culture) despite low actual function. The theater has been partially replaced by gig economy and job-hopping norms among top talent, but institutional HR apparatus maintains the performance for compliance and legitimacy. Theater ratio remains high (0.65) because mobility suppression still operates through formal mechanisms even as functional reality shifts.
constraint_indexing:constraint_classification(job_mobility_suppression, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ELITE MOBILE PROFESSIONAL (TANGLED ROPE) — Top 5% of workers (elite technologists, executives, specialized experts) have genuine mobility options and arbitrage capacity. But they experience the constraint differently: their mobility is coordinated through recruitment ecosystems, executive search, and specialized networks that extract through high placement fees, equity concentration, and talent lock-in during acquisition cycles. Even with high power and mobility, they experience asymmetric extraction in the form of talent concentration mechanisms and golden handcuffs. The constraint is tangled for them — it coordinates talent matching while extracting rents through scarcity.
constraint_indexing:constraint_classification(job_mobility_suppression, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, human capital is structurally immobile relative to physical capital: workers cannot instantly acquire new skills, cannot freely relocate without family/community costs, cannot instantaneously learn new organizational cultures. Some lag between job change and productivity is immutable. However, the degree of suppression (0.72) far exceeds what mobility friction alone would produce. This perspective risks naturalizing contingent institutional mechanisms (non-competes, vesting, credential gatekeeping, geographic fragmentation) as inherent to labor economics. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(job_mobility_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(job_mobility_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(job_mobility_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_mobility_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(job_mobility_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(job_mobility_suppression, TR),
    TR >= 0.70.

:- end_tests(job_mobility_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original assessment of pure productivity loss from immobility would suggest higher values (0.70-0.80), but the constraint provides genuine coordination benefits: firms can invest in worker development without immediate loss to poaching, workers gain skill development and credential certification that has value beyond current employer. The 0.58 value reflects that extractiveness is primarily rent-seeking through immobility (wage suppression) rather than creation of new value loss. The 20-year trend (0.35 → 0.58) shows intensification: non-competes have expanded geographically and sectorally, equity vesting structures have replaced fixed wages, credential gatekeeping has increased, and psychological identification with firms has become more normalized. Suppression (0.72): High. Multiple reinforcing barriers prevent exit: legal (non-competes with 55-75% enforcement depending on jurisdiction), material (lost benefits, relocation costs, geographic lock-in), informational (credential discounting, knowledge transfer barriers), psychological (identity fusion, learned helplessness about alternatives), and structural (firm-specific capital and tacit knowledge). The barriers are not uniformly distributed — powerless workers face all four; skilled workers face mainly informational and psychological barriers. Theater ratio (0.65): Moderate-high. HR 'talent development' discourse, career pathing, retention bonuses, and 'culture fit' hiring are substantially performative: they serve legitimation and compliance functions while the actual mobility restriction operates through contractual and structural mechanisms. The theater has increased from 0.45 as firms have developed more sophisticated narratives around 'employee engagement' and 'development ecosystems' while mobility suppression has hardened through legal and financial mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The original research assumption was that this constraint has a single 'correct' classification based on objective metrics. The perspectival gaps reveal why this is false. The employer firm's 'retention problem' (Rope) is the worker's 'trap' (Snare). The reform coalition's 'temporary policy failure' (Scaffold) is the trapped worker's 'permanent structure' (Snare from biographical perspective, Mountain from immediate perspective). The HR apparatus' 'talent development' (Piton theater) is the worker's 'career path constraint' (Snare). The skilled mobile worker's 'constrained mobility with benefits' (Tangled Rope) is the powerless worker's 'impossibility of exit' (Snare). The elite professional's 'executive recruitment market' (Tangled Rope with power and mobile exit) is the trapped worker's 'impossibility of aspiration.' No single type is 'correct' — the presheaf over observation positions IS the structure. The false summit detector flags the mountain classification because the civilizational 'inherent friction' narrative naturalizes what is actually a contingent institutional arrangement that varies radically across jurisdictions and time periods.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the agent's structural position: who benefits, who bears costs, and what exit capacity they have. Employers and capital concentration are beneficiaries with arbitrage capacity — they can replace workers, relocate operations, or change strategy — producing low d (0.05-0.20) and negative χ (they experience the constraint as beneficial). Trapped workers are victims with no exit, producing high d (0.95) and maximum χ (1.42 × ε). Skilled mobile workers are victims with constrained exit (high cost but possible), producing moderate-high d (0.75-0.85) and elevated χ (1.15 × ε). Organized reform agents have power and exit options (they can lobby, litigate, organize), producing moderate d (0.40-0.50) and moderate χ (0.60 × ε). The perspectival gap emerges from these divergent d values: the employer experiences Rope because their d is low and they genuinely coordinate; the trapped worker experiences Snare because their d is high and they bear full extraction; the organized coalition experiences Scaffold because they have power to alter the constraint within their time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES MANDATROPHY by showing that the classification spectrum emerges legitimately from structural position differences. The mandatrophy question is 'Is job mobility suppression coordination or extraction?' The answer is 'both, from different positions.' Employers coordinate retention and development (Rope from their position). Workers experience extraction through immobility (Snare from theirs). The constraint cannot be classified as 'pure extraction' (Snare) or 'pure coordination' (Rope) at the aggregate level because the aggregate does not exist — the structure is a presheaf over distinct agent positions. The Tangled Rope classification at institutional/constrained and powerful/mobile reflects that institutional actors in the suppression system (large firms, recruiters, HR systems) experience mixed coordination and extraction even among the beneficiary class, and powerful workers experience extraction even with high exit options (through talent market mechanisms). This resolves the classic mandatrophy: the constraint is simultaneously coordination, extraction, and both-mixed, depending on observer position. The policy implication is that 'reform' cannot eliminate the constraint by choosing the 'right' classification — it must target the extraction mechanisms while preserving genuine coordination function, which is precisely what non-compete bans + portable benefits reform aims to do: remove legal and financial lock-in while preserving skill development and credential systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    non_compete_enforceability_variance,
    'What fraction of suppression derives from legal enforceability of non-competes versus voluntary lock-in through psychological and financial mechanisms?',
    'Comparative analysis across jurisdictions with different non-compete regimes (California ban vs Texas enforcement) controlling for labor market structure, firm size, and worker skill level',
    'If legal enforceability > 60%: suppression drops significantly under regulatory reform, classification remains Snare/Tangled Rope but chi decreases. If psychological lock-in > 60%: legal reform has limited impact, suppression is internalized (identity-locked mechanism), classification deepens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_compete_enforceability_variance, empirical, 'Contribution of legal enforceability vs psychological lock-in to suppression').

omega_variable(
    skill_transferability_asymmetry,
    'Do firm-specific skills genuinely transfer at lower rates than claimed, or does the transferability discount reflect employer-side information asymmetry and credential gatekeeping?',
    'Wage trajectory analysis for workers changing firms with similar skill profiles; comparison of hiring discount for internal transfers vs external hires with identical certifications',
    'If genuine skill gap: suppression is coordination problem (higher Rope/Scaffold proportion). If information asymmetry: suppression is extraction mechanism (higher Snare/Tangled Rope), employers suppress mobility to maintain wage-setting power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transferability_asymmetry, empirical, 'Whether firm-specific skill discount is genuine or information asymmetry').

omega_variable(
    regulatory_sunset_credibility,
    'Are non-compete bans and portable benefits reforms actually reducing mobility suppression, or are employers adapting with new mechanisms (trade secret liability, garden leave, equity acceleration cliffs)?',
    'Longitudinal tracking of actual quit rates and wage growth for mobile workers in pre- vs post-ban jurisdictions; analysis of emerging contractual mechanisms designed to replace non-competes',
    'If bans effective: scaffold perspective confirmed, extractiveness decreases toward 0.35-0.40. If employers adapt: suppression persists through alternate mechanisms, extractiveness stays high (0.55-0.65), scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_credibility, empirical, 'Effectiveness of non-compete ban reform in reducing mobility suppression').

omega_variable(
    identity_lock_in_professional_identity,
    'To what extent is worker immobility driven by internalized professional identity (self-concept fused with current firm/role) versus material barriers to exit?',
    'Post-exit trajectory analysis: do workers who forced into mobility (layoff, firm closure) show rapid re-attachment to new identity/firm or prolonged identity crisis? Do workers express identity loss vs purely financial anxiety about job change?',
    'If identity-locked: suppression is cognitive rather than structural, workers perceive immutability from within their frame but could exercise mobility if identity frame shifted. Classification remains Snare from powerless perspective but becomes Rope from identity-unlocked perspective. If material barriers dominant: suppression is structural, identity lock is secondary effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_professional_identity, empirical, 'Role of professional identity fusion in perceived immobility').

omega_variable(
    gig_economy_substitution_effectiveness,
    'Does gig work actually provide genuine mobility escape or does it represent precarity that reinforces traditional employment lock-in (workers forced to cycle between gig and traditional employment)?',
    'Comparative wage and benefit trajectories between workers with gig-only, traditional-only, and hybrid gig-traditional careers; analysis of whether gig work provides bridge to new traditional employment or trap in lower-paying precarity',
    'If genuine escape: alternative constraint structure emerging (gig economy suppression mechanisms are different ε values, decompose to separate constraint family). If trap: gig economy creates secondary constraint that reinforces traditional employment lock-in, tangled rope classification deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gig_economy_substitution_effectiveness, empirical, 'Whether gig work provides mobility escape or reinforces traditional employment lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(job_mobility_suppression, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jms_tr_t0, job_mobility_suppression, theater_ratio, 0, 0.45).
narrative_ontology:measurement(jms_tr_t10, job_mobility_suppression, theater_ratio, 10, 0.58).
narrative_ontology:measurement(jms_tr_t20, job_mobility_suppression, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(jms_be_t0, job_mobility_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jms_be_t10, job_mobility_suppression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(jms_be_t20, job_mobility_suppression, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(job_mobility_suppression, resource_allocation).
narrative_ontology:affects_constraint(job_mobility_suppression, wage_suppression_through_labor_immobility).
narrative_ontology:affects_constraint(job_mobility_suppression, skills_credential_gatekeeping).
narrative_ontology:affects_constraint(job_mobility_suppression, geographic_lock_in_housing_costs).

% DUAL FORMULATION NOTE:
% Job mobility suppression decomposes into three structurally distinct constraints with different ε values: (1) wage suppression through immobility (ε≈0.55, how immobility enables wage rent-seeking), (2) credential gatekeeping (ε≈0.48, how licensing/certification requirements restrict entry and transfer), and (3) geographic lock-in through housing/family costs (ε≈0.62, how structural immobility compounds job mobility constraints). Each has different beneficiaries, different suppression mechanisms, and different policy solutions. The aggregate 'job mobility suppression' story represents the coordinated effect of these three upstream constraints. Links indicate that policy intervention in one (e.g., non-compete bans) partially addresses the aggregate constraint but leaves others intact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(job_mobility_suppression, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
