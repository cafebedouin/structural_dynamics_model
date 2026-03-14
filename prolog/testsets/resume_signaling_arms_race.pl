% ============================================================================
% CONSTRAINT STORY: resume_signaling_arms_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_resume_signaling_arms_race, []).

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
 *   constraint_id: resume_signaling_arms_race
 *   human_readable: Resume Signaling Arms Race
 *   domain: labor_market/education/credentialing
 *
 * SUMMARY:
 *   The resume signaling arms race is a structural constraint in labor
 *   markets where job seekers must continuously escalate educational
 *   credentials, certifications, internships, and resume optimization to
 *   remain competitive for positions that nominally require lower
 *   qualification levels. What began as efficient screening (employers using
 *   education as a proxy for capability) has degraded into pure positional
 *   competition — individuals escalate signals not because it increases their
 *   absolute productivity, but because it maintains their relative position
 *   as others escalate. This creates a prisoner's dilemma: each job seeker
 *   benefits from signaling to stand out, but the collective effect is
 *   credential inflation with minimal productivity gain. The constraint
 *   exhibits genuine coordination function (reducing information asymmetry
 *   between employers and candidates) alongside asymmetric extraction (time
 *   and money costs concentrated on job seekers, benefits concentrated on
 *   employers and credential issuers). The theater ratio has increased
 *   substantially (0.42 to 0.68) reflecting that credential value
 *   increasingly derives from positional status rather than skill
 *   verification.
 *
 * KEY AGENTS:
 *   - Job Seekers Without Capital: Primary victims (powerless/trapped) — must escalate credentials and signals to remain competitive; bear time, money, and opportunity costs with no alternative pathways
 *   - Resourced Job Seekers: Secondary victims/minor beneficiaries (moderate/constrained) — experience coordination benefit (signals help them distinguish themselves) alongside extraction (must still participate in arms race); have some exit optionality through networks
 *   - Elite Employers: Primary beneficiaries (institutional/arbitrage) — benefit from efficient screening via resume signals; low cost of participation; can always exit to alternative hiring methods but have no incentive
 *   - University Administrators: Secondary beneficiaries (powerful/mobile) — benefit from credential demand (enrollment, prestige, revenue); also manage costs of maintaining credential relevance
 *   - Credentialing Bureaucracy: Tertiary beneficiary (institutional/arbitrage) — maintains institutional inertia in credential requirements through accreditation, regulatory entrenchment, and professional standards
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — reveals the constraint as equilibrium outcome of individual rationality producing collective irrationality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(resume_signaling_arms_race, 0.58).
domain_priors:suppression_score(resume_signaling_arms_race, 0.65).
domain_priors:theater_ratio(resume_signaling_arms_race, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(resume_signaling_arms_race, extractiveness, 0.58).
narrative_ontology:constraint_metric(resume_signaling_arms_race, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(resume_signaling_arms_race, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(resume_signaling_arms_race, tangled_rope).
narrative_ontology:human_readable(resume_signaling_arms_race, "Resume Signaling Arms Race").
narrative_ontology:topic_domain(resume_signaling_arms_race, "labor_market/education/credentialing").

domain_priors:requires_active_enforcement(resume_signaling_arms_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(resume_signaling_arms_race, credential_issuers).
narrative_ontology:constraint_beneficiary(resume_signaling_arms_race, high_barrier_entry_employers).
narrative_ontology:constraint_victim(resume_signaling_arms_race, job_seekers).
narrative_ontology:constraint_victim(resume_signaling_arms_race, low_income_credential_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JOB SEEKER WITHOUT CAPITAL (SNARE) — Faces escalating credential and signal requirements with no ability to exit. Must accumulate degrees, certifications, and resume padding (internships, volunteer work, networks) to remain competitive. The system extracts time, money, and opportunity cost with no coordination benefit — the signaling is purely competitive, not cooperative.
constraint_indexing:constraint_classification(resume_signaling_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESOURCED JOB SEEKER (TANGLED ROPE) — Has access to internship networks, test prep, resume coaches, and family connections. Experiences genuine coordination (signals reduce information asymmetry for employers) alongside extraction (must still participate in arms race to maintain advantage over competitors). Constrained exit — can refuse to optimize resume but faces career penalty.
constraint_indexing:constraint_classification(resume_signaling_arms_race, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE EMPLOYER (ROPE) — Benefits from efficient screening via resume signals. Low cost of participation in the signaling system (just reading resumes) and high benefit (access to pre-filtered talent pool). Arbitrage available — can always switch to alternative hiring methods but has no incentive to do so.
constraint_indexing:constraint_classification(resume_signaling_arms_race, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: UNIVERSITY ADMINISTRATOR (TANGLED ROPE) — Benefits from escalating credential demand (enrollment pressure, credential inflation increases market value of degrees). Also bears costs — must manage credential inflation, maintain reputation, invest in continuous program updates. Mobile exit available (could reduce credential requirements) but mobile in both directions — market pressure forces continued arms race participation.
constraint_indexing:constraint_classification(resume_signaling_arms_race, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDENTIALING BUREAUCRACY (PITON) — The formal credential system (diplomas, certifications, transcripts) is largely performative. Employers report that resume signals (degree from brand-name school, GPA, prestigious internship) predict job performance weakly; the real coordination benefit has decayed. Yet the credential ritual persists through institutional inertia, accreditation standards, and regulatory entrenchment. Theater ratio high because credentials are maintained for status signaling rather than skill verification.
constraint_indexing:constraint_classification(resume_signaling_arms_race, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (LABOR MARKET EQUILIBRIUM) — From the perspective of labor market structure, the resume arms race is a prisoner's dilemma with no exit. Individual employers benefit from filters (resume signals); individuals benefit from signaling (staying competitive). But the collective equilibrium extracts from job seekers without producing information — signals are positional goods, not productive. The analytical view shows this as a Snare at scale.
constraint_indexing:constraint_classification(resume_signaling_arms_race, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(resume_signaling_arms_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(resume_signaling_arms_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(resume_signaling_arms_race, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(resume_signaling_arms_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(resume_signaling_arms_race, TR),
    TR >= 0.70.

:- end_tests(resume_signaling_arms_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The arms race extracts substantial time, money, and opportunity cost from job seekers. Entry costs include degrees (4 years, $20k-$200k+), certifications ($500-$5k each), internships (unpaid or low-wage, 3-12 months), resume coaching, networking events, test prep. The extraction is not maximal (0.66+) because employers do genuinely benefit from reduced information asymmetry, and some credential investment does improve job match quality. Suppression (0.65): Moderate-high. Job seekers face strong barriers to exit: not signaling means remaining invisible to employers; competing on skills alone without credentials triggers automatic resume filters; geographic and family-network barriers increase suppression for low-capital seekers. Alternative pathways (apprenticeships, skills-based hiring) exist but are minority channels. Theater ratio (0.68): High and increasing. Credentials increasingly function as positional signals rather than skill verification. Employers report weak correlation between hiring signals and job performance; degree inflation has reduced GPA and institution predictivity; the ritual of credential accumulation persists despite declining information value. The rise reflects Goodhart's Law: as credentials became targets for optimization, they ceased to measure what they intended to measure.
 *
 * PERSPECTIVAL GAP:
 *   The gap between trapped job seekers (who see pure Snare) and elite employers (who see beneficial Rope) is structural and unbridgeable from within the system. Job seekers see escalation with no exit and no benefit. Employers see efficient filtering with minimal cost. Both are correct from their positions. The Tangled Rope classification at the moderate agent level captures the real mixed function: genuine coordination (reduced information asymmetry) alongside genuine extraction (positional competition). The Piton at the bureaucratic level reveals institutional degradation — the credentialing system sees its own theater but lacks the power to exit (exit would collapse its institutional legitimacy). The perspectival gaps are driven by exit optionality: agents with arbitrage (can leave the system) experience low extraction; agents with no exit (trapped) experience maximum extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Job seekers without capital bear maximum costs with no exit optionality (d ≈ 0.95, trapped × victim). Resourced seekers experience lower extraction because they can exit via networks and have some benefit from signals (d ≈ 0.55, constrained × moderate-benefit). Elite employers experience negative extraction — they benefit from the signaling mechanism (d ≈ 0.15, arbitrage × beneficiary). Universities experience moderate extraction because they benefit from credential demand but also bear costs of maintaining credential relevance (d ≈ 0.50, mobile × mixed-benefit). The credentialing bureaucracy experiences low extraction — it benefits from institutional entrenchment (d ≈ 0.20, arbitrage × beneficiary). The analytical observer sees the full structure with no position-specific bias (d ≈ 0.72, analytical × neutral).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope is the correct classification at the analytical/generational level: the arms race has BOTH genuine coordination function (employers do gain signal value, job seekers do get matched to better-fit roles) AND asymmetric extraction (escalation benefits high-capital seekers and credential issuers disproportionately, while imposing costs on low-capital seekers). The false summit detector would catch the 'natural law' reading ('credentialing is inherent to labor markets') as naturalization of a contingent institutional arrangement. The Snare reading from the trapped perspective is correct from that position but incomplete — it omits the genuine coordination function that makes the constraint persistent. The Rope reading from the employer perspective is correct from that position but incomplete — it omits the extraction function that constrains job seeker choices. The full mandatrophy resolution: this is a Tangled Rope where the coordination function (information reduction) is real but has decayed relative to the extraction function (positional competition). The theater ratio increase (0.42 → 0.68) reflects this decay — the performative component of the credential system is rising relative to its functional component.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_productivity_gap,
    'Do resume signals (education credentials, internship experience, certifications) actually predict job performance, or are they purely positional/competitive goods with no productivity link?',
    'Longitudinal employer data: correlation between resume signals and actual performance reviews, retention rates, promotion trajectories; comparison between hired candidates with vs without elite credentials',
    'If productive: the arms race has genuine coordination function (Rope from more perspectives). If purely positional: the arms race is pure extraction (Snare confirmed). If weak productivity: Tangled Rope correctly classifies the mixed reality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(signal_productivity_gap, empirical, 'Whether resume signals predict actual job performance').

omega_variable(
    credential_inflation_mechanism,
    'What sustains credential inflation? Is it employer signaling demand, institutional rent-seeking, regulatory credentialism, or status competition among job seekers?',
    'Analysis of credential requirement trends by industry; employer surveys on hiring decision factors; comparison of role requirements vs credential requirements; policy analysis of credentialing regulatory drivers',
    'If employer demand: constraint is responsive to labor market (exit paths exist via direct hiring). If institutional rent-seeking: credentialing actors actively maintain extraction. If status competition: job seekers voluntarily escalate (constrained exit rather than trapped exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_mechanism, empirical, 'Root driver of credential inflation dynamics').

omega_variable(
    low_income_exit_pathway_existence,
    'Are there structured exit pathways for low-income job seekers to bypass the resume signaling arms race (apprenticeships, skills-based hiring, network-independent hiring)?',
    'Prevalence analysis of alternative hiring mechanisms; long-term outcome comparison between signals-based hiring vs skills-based hiring; scalability assessment of alternatives',
    'If pathways exist at scale: powerless agent''s exit_options should be upgraded from ''trapped'' to ''constrained'' — representation improves. If pathways minimal: Snare classification confirmed for this group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(low_income_exit_pathway_existence, empirical, 'Availability of alternative hiring pathways bypassing resume signals').

omega_variable(
    family_network_advantage_decay,
    'Does the resume arms race actually reduce the advantage of family/social network hiring, or does it amplify it (high-capital families can better optimize signals)?',
    'Analysis of hiring outcomes by family income, network access, geographic location; trend analysis of diversity metrics in signal-gated vs signals-free hiring',
    'If network advantage decays: arms race has transparency benefit (weak Rope function). If network advantage amplifies: arms race is extraction mechanism that concentrates advantage (Snare confirmed for low-capital seekers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_network_advantage_decay, empirical, 'Whether resume signals reduce or amplify family network hiring advantages').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(resume_signaling_arms_race, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resume_sig_tr_t0, resume_signaling_arms_race, theater_ratio, 0, 0.42).
narrative_ontology:measurement(resume_sig_tr_t5, resume_signaling_arms_race, theater_ratio, 5, 0.55).
narrative_ontology:measurement(resume_sig_tr_t10, resume_signaling_arms_race, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(resume_sig_be_t0, resume_signaling_arms_race, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(resume_sig_be_t5, resume_signaling_arms_race, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(resume_sig_be_t10, resume_signaling_arms_race, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(resume_signaling_arms_race, information_standard).
narrative_ontology:affects_constraint(resume_signaling_arms_race, credential_inflation).
narrative_ontology:affects_constraint(resume_signaling_arms_race, educational_debt_accumulation).
narrative_ontology:affects_constraint(resume_signaling_arms_race, labor_market_sorting).

% DUAL FORMULATION NOTE:
% Resume signaling is upstream of credential inflation (the signaling demand drives credential proliferation) and labor market sorting (signals determine matching quality). Educational debt accumulation is a downstream cost consequence of signaling participation. All three constraints share the structural feature of high theater ratio and positional-good dynamics; decomposition by observable (what aspect of signaling) produces distinct stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(resume_signaling_arms_race, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
