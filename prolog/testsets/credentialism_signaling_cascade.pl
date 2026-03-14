% ============================================================================
% CONSTRAINT STORY: credentialism_signaling_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credentialism_signaling_cascade, []).

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
 *   constraint_id: credentialism_signaling_cascade
 *   human_readable: Credentialism Signaling Cascade
 *   domain: education/economics/labor_markets
 *
 * SUMMARY:
 *   Credentialism signaling cascade is a self-reinforcing institutional
 *   dynamic where credential requirements escalate beyond functional
 *   necessity as employers respond to credential inflation among applicants,
 *   which in turn drives workers to pursue ever-higher credentials to
 *   maintain labor market competitiveness. The constraint exhibits genuine
 *   coordination benefits — credentials do provide real signals of competence
 *   — alongside substantial extraction mechanisms — credential inflation
 *   traps workers in costly signaling arms races. The system is actively
 *   enforced through hiring practices, licensing requirements, and
 *   educational institution incentive structures. The extractiveness value
 *   (0.58) has increased markedly over the measurement interval, reflecting
 *   acceleration of credential inflation. Theater ratio (0.68) indicates that
 *   a significant portion of credential value is ritualistic signaling
 *   unrelated to actual job competence. This constraint produces radically
 *   different classifications from different perspectives: entry-level
 *   workers experience a snare; mid-career holders experience extraction
 *   layered on genuine coordination; educational institutions and employers
 *   experience pure coordination; professional guilds actively extract rents
 *   while claiming to coordinate quality; and the civilizational natural law
 *   view risks falsely naturalizing contingent institutional arrangements.
 *
 * KEY AGENTS:
 *   - Entry-level workers and credential seekers: Primary victims (powerless/trapped) — face credential inflation as a structural trap with no exit options
 *   - Mid-career professionals: Secondary victims (moderate/constrained) — must continuously pursue new credentials to maintain position; experience extraction layered on coordination
 *   - Educational institutions (universities, training providers): Primary beneficiaries (institutional/arbitrage) — capture revenue from credential demand; can shift program offerings and pricing models
 *   - Employers: Institutional beneficiaries (institutional/arbitrage) — use credentials as hiring signals; can adjust requirements and develop alternatives
 *   - Professional guilds and licensing bodies: Organized beneficiaries (organized/constrained) — maintain credential standards; extract rents through barrier maintenance while claiming quality coordination
 *   - Credential credential ritualists: System enforcers — perpetuate theater through institutional inertia; divorced from functional necessity
 *   - Analytical observer: Civilizational perspective risking false summit — naturalizes contingent institutional arrangements as immutable information-asymmetry response
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credentialism_signaling_cascade, 0.58).
domain_priors:suppression_score(credentialism_signaling_cascade, 0.65).
domain_priors:theater_ratio(credentialism_signaling_cascade, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credentialism_signaling_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(credentialism_signaling_cascade, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credentialism_signaling_cascade, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credentialism_signaling_cascade, tangled_rope).
narrative_ontology:human_readable(credentialism_signaling_cascade, "Credentialism Signaling Cascade").
narrative_ontology:topic_domain(credentialism_signaling_cascade, "education/economics/labor_markets").

domain_priors:requires_active_enforcement(credentialism_signaling_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credentialism_signaling_cascade, credential_gatekeepers).
narrative_ontology:constraint_beneficiary(credentialism_signaling_cascade, incumbent_professionals).
narrative_ontology:constraint_victim(credentialism_signaling_cascade, credential_seekers).
narrative_ontology:constraint_victim(credentialism_signaling_cascade, job_market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL WORKER (SNARE) — Faces credential inflation as a structural trap. Cannot access jobs without credentials; cannot obtain credentials without financial resources and time commitment; cannot repay credential costs without job access. The cycle is self-reinforcing. Exit options are severely constrained: geographic relocation requires capital; career pivots require retraining; credential abandonment means permanent labor market exclusion. Maximum suppression and experienced extraction.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER CREDENTIAL HOLDER (TANGLED ROPE) — Possesses credentials obtained when signaling was less inflated. Experiences genuine coordination benefit: credentials enable access to professional networks, specialized knowledge, and income. But also experiences extraction: must continuously pursue additional credentials (certifications, advanced degrees) to maintain labor market position as credential inflation accelerates. Costs are rising faster than benefits.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTIONS (ROPE) — Experience the credential system as pure coordination: they aggregate knowledge, certify competence, and match workers to opportunities. Revenue streams align with credential demand. Can arbitrage between institutions and markets. The constraint functions as intended from this perspective — it solves the real information problem (employers need signals of worker capability). Institutional exit: can diversify program offerings, adjust tuition, or shift credentialing models.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYERS (ROPE) — Use credentials as hiring signals. Experience coordination benefit: credentials reduce hiring uncertainty and reduce training costs by filtering for baseline competence and cultural fit. Employers can arbitrage: shift credential requirements up/down based on labor supply, develop internal certifications, or abandon credentials entirely (though rare). The system works from their perspective — it solves their information problem.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROFESSIONAL GUILDS (TANGLED ROPE) — Maintain credential standards (licensing exams, continuing education, professional certifications). Experience genuine coordination: licensing protects consumers (medical board certifications, engineering standards). But also experience extraction: guilds use credential inflation as a rent-extraction mechanism — raising barriers to entry maintains high member wages and reduces labor supply competition. Active enforcement of credential requirements sustains asymmetric benefit.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY CREDENTIAL RITUALISM (PITON) — Many credentials have degraded into pure signaling theater detached from demonstrated competence. College degrees are evaluated primarily on prestige brand, not learning outcomes. Professional certifications have proliferated (80k+ credential types in US labor market) creating noise rather than signal. The ritual persists through institutional inertia — employers still demand degrees because 'that's how hiring works,' institutions still require degrees because 'that's what employers want.' High theater ratio reflects the self-perpetuating cycle with minimal functional basis.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW (MOUNTAIN — FALSE SUMMIT) — Framing credentialism as an immutable response to unavoidable information asymmetry between employers and workers. This perspective naturalizes contingent institutional arrangements (degree requirements, credential inflation, licensing barriers) as inherent to labor market function. The analytical observer risks false summit classification: the information asymmetry is real, but the credential cascade solution is one of many possible mechanisms. Alternative mechanisms (apprenticeship, work-based testing, portfolio evaluation, reputation networks) solve the same information problem with lower extraction. The mountain classification reveals naturalization.
constraint_indexing:constraint_classification(credentialism_signaling_cascade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credentialism_signaling_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credentialism_signaling_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credentialism_signaling_cascade, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credentialism_signaling_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credentialism_signaling_cascade, TR),
    TR >= 0.70.

:- end_tests(credentialism_signaling_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The baseline measurement shows 0.28 at t=0 (when credentials had higher functional content relative to signaling requirements). Current value of 0.58 reflects accelerating credential inflation — workers invest 4-6 years of education and $50k-$200k in costs to access entry-level positions that functionally require 2 years of training and 0 years of education. The growth trajectory shows the constraint is degrading toward higher extraction (0.28→0.42→0.58 over 20-year interval). Suppression (0.65): High. Barriers to non-credential pathways include: legal barriers (professional licensing requires specific degrees), institutional barriers (employers use credentials as primary filter despite weak correlation with job performance), financial barriers (educational costs have increased 3x faster than wages), social barriers (credentials signal cultural fitness and class membership beyond technical competence), and informational barriers (workers lack reliable signals of credential necessity vs. signaling arms race). Alternative pathways exist but are actively suppressed by credential-gatekeeping institutions. Theater ratio (0.68): High and increasing (0.38→0.54→0.68). Reflects the growing disconnect between credential content and actual job requirements. Many credentials have become pure signaling: liberal arts degrees evaluated on prestige brand rather than learning outcomes; professional certifications that test memorization rather than competence; MBA programs marketing prestige rather than business knowledge. The proliferation of credential types (80k+ distinct credentials in US labor market) creates noise rather than signal, yet institutional inertia maintains degree requirements.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and diagnostic. Entry-level workers experience credentialism as a snare: the constraint has trapped them in a costly signaling cycle with no functional exit. Educational institutions experience it as rope: they genuinely coordinate between knowledge, capability, and labor market opportunity. Employers use credentials as coordination — they solve real hiring uncertainty problems. Professional guilds experience rent extraction opportunities cloaked in quality assurance language (tangled rope: genuine coordination benefit + deliberate barrier maintenance). The legacy credential ritualists see a degraded system maintained by institutional inertia (piton). The analytical observer risks the false summit: framing credential requirements as natural responses to unavoidable information asymmetry, when in fact the credential cascade is one of many possible signaling mechanisms, and not the most efficient one. The perspectival gap between the entry-level worker's snare and the employer's rope reveals the asymmetric benefit distribution: the constraint solves employers' information problems while creating workers' financial and temporal problems.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. Entry-level workers are pure victims (d→1.0): they bear credential costs, face suppression, and cannot exit. Educational institutions are net beneficiaries (d→0.1): they capture enrollment revenue and have arbitrage options (tuition, program mix, alternative models). Employers are moderate beneficiaries (d→0.3): credentials solve their hiring problem, but they could adopt alternatives; they retain flexibility. Professional guilds are hybrid (d→0.4): they coordinate genuine quality standards (doctor licenses reduce patient harm) but actively extract rents through barrier maintenance. The beneficiary/victim split reflects that credential inflation benefits those downstream (institutions, employers, incumbent professionals) and harms those upstream (credential seekers, workers entering the labor market). Mid-career professionals occupy an ambiguous position (d→0.55): they benefit from credentials they already hold but are harmed by inflation that requires continuous new credentials.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY DIAGNOSIS: The credentialism cascade resolves the mandatrophy by revealing that the constraint is a genuine hybrid — coordination + extraction are not mutually exclusive. Credentials DO coordinate real information between workers and employers (rope function). Simultaneously, the system extracts significant surplus from credential seekers through inflation (snare function). The tangled_rope classification is the resolution: the constraint possesses BOTH a genuine coordination function (employers need signals of baseline competence, education does provide some skill) AND active asymmetric extraction (credential inflation exceeds functional requirements, rents accrue to gatekeepers). The classification prevents two errors: (1) falsely treating credentialism as pure coordination because it solves a real information problem, and (2) falsely treating it as pure extraction because workers do gain skills. The mandatrophy is resolved by showing that tangled_rope is the accurate structural reading — both functions are real, and both must be analyzed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_functional_content,
    'What proportion of credential value derives from actual skill certification vs. pure labor market signaling?',
    'Comparison of employer-assessed worker competence for credentialed vs. non-credentialed cohorts in identical roles; longitudinal tracking of credential necessity for job performance across fields',
    'If functional content > 70%: system is Rope-dominated coordination with some extraction overlay. If < 40%: system is primarily Snare with coordination justification as cover story. Determines whether credential inflation is market failure or deliberate rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_functional_content, empirical, 'Functional signal content vs. pure signaling value in credentials').

omega_variable(
    inflation_acceleration_driver,
    'Is credential inflation driven primarily by supply-side competition (employers raising requirements) or demand-side dynamics (workers over-investing in credentials)?',
    'Historical analysis of credential requirement changes in job postings; survey data on employer credential necessity beliefs vs. actual hiring practices; tracking of unmet credential demand in labor markets',
    'If supply-driven: the constraint is employer-imposed extraction (snare from worker perspective). If demand-driven: workers are voluntarily over-investing based on imperfect information (coordination failure). Classification implications differ — supply-driven points to organizing against employers; demand-driven points to information reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_acceleration_driver, empirical, 'Primary driver of credential inflation escalation').

omega_variable(
    alternative_signaling_adoption,
    'Why haven''t alternative skill-signaling mechanisms (work portfolios, apprenticeships, skills assessments, reputation networks) replaced formal credentials despite lower cost?',
    'Market analysis of alternative signaling adoption rates; network effects analysis of credential vs. alternative networks; institutional barriers to alternative adoption (legal, regulatory, cultural)',
    'If barriers are technical/informational: interventions (better portfolio platforms, industry skills standards) could disrupt the cascade. If barriers are structural incentive-alignment: the constraint will persist despite efficiency losses — points to snare rather than rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_signaling_adoption, empirical, 'Barriers preventing alternative signaling mechanism adoption').

omega_variable(
    suppression_structural_vs_internalized,
    'Is suppression experienced by credential seekers primarily structural (external barriers: cost, time, access) or internalized (belief that credentials are necessary, self-imposed urgency)?',
    'Post-entry tracking: how many credential-seeking career-changers abandon pursuit at each barrier stage; psychological research on internalized credential necessity belief; post-barrier-removal engagement (e.g., loan forgiveness impact on new credential pursuit)',
    'If structural: removing cost/access barriers (loan forgiveness, free community colleges) should reduce suppression. If internalized: barrier removal insufficient — beliefs about credential necessity persist even after barriers fall. Suggests deeper identity-lock and cultural narrative reinforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in credential pursuit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credentialism_signaling_cascade, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credentialism_signaling_cascade, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cred_tr_t10, credentialism_signaling_cascade, theater_ratio, 10, 0.54).
narrative_ontology:measurement(cred_tr_t20, credentialism_signaling_cascade, theater_ratio, 20, 0.68).
narrative_ontology:measurement(cred_tr_t5, credentialism_signaling_cascade, theater_ratio, 5, 0.46).
narrative_ontology:measurement(cred_tr_t15, credentialism_signaling_cascade, theater_ratio, 15, 0.61).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credentialism_signaling_cascade, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cred_be_t10, credentialism_signaling_cascade, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cred_be_t20, credentialism_signaling_cascade, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cred_be_t5, credentialism_signaling_cascade, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(cred_be_t15, credentialism_signaling_cascade, base_extractiveness, 15, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credentialism_signaling_cascade, information_standard).
narrative_ontology:affects_constraint(credentialism_signaling_cascade, social_mobility_lock).
narrative_ontology:affects_constraint(credentialism_signaling_cascade, professional_licensing_capture).
narrative_ontology:affects_constraint(credentialism_signaling_cascade, education_financing_extraction).

% DUAL FORMULATION NOTE:
% Credentialism signaling cascade is upstream of professional licensing capture (which uses credential requirements as barrier maintenance) and education financing extraction (which extracts surplus from credential seekers through loan costs). All three constraints form a family: signaling cascade drives credentialism, which drives licensing barriers, which drives financing extraction. The upstream constraint (this one) has moderate extractiveness; downstream constraints have higher extractiveness as extraction mechanisms compound.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credentialism_signaling_cascade, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
