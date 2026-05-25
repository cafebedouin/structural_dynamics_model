% ============================================================================
% CONSTRAINT STORY: work_life_boundary_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_work_life_boundary_erosion, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: work_life_boundary_erosion
 *   human_readable: Work-Life Boundary Erosion and Temporal Extraction
 *   domain: labor/organizational/interpersonal
 *
 * SUMMARY:
 *   Work-life boundary erosion is the structural mechanism by which temporal
 *   extraction occurs in knowledge work and professional labor markets. The
 *   constraint operates through the normalization of always-on connectivity
 *   (email, messaging, availability for calls across time zones) justified as
 *   a coordination solution for global operations, while simultaneously
 *   functioning as an extraction mechanism that captures unpaid labor time
 *   and degradation of autonomy and family relationships. The mechanism is
 *   particularly effective because it operates on two structural registers
 *   simultaneously: the employer sees genuine coordination benefits
 *   (asynchronous global operations, fast decision-making, competitive
 *   efficiency), while workers experience it as identity-fused professional
 *   expectation combined with material penalties for non-compliance. The
 *   constraint exhibits all six classification types from different
 *   perspectives, revealing a hybrid snare-and-tangled-rope structure that
 *   shifts based on organizational power, worker market position, and
 *   identity-lock capacity.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victims (powerless/identity_locked, moderate/constrained, powerful/mobile) — experience extraction across three distinct power contexts with different escape paths
 *   - Employers and Managers: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and unpaid temporal surplus while experiencing boundary erosion as pure coordination benefit
 *   - Parents Negotiating Care: Secondary victim (moderate/constrained) — face double-bind of expanding work hours and non-negotiable childcare time blocks; experience suppression as both structural (economic) and cultural (guilt narratives)
 *   - Labor Movement: Organized victim-beneficiary (organized/constrained) — has legitimate interest in protecting work-time but also extracts membership value and institutional power; faces internal contradictions
 *   - Legal/Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains performative work-hour protections that are weakly enforced and widely exempted; theater ratio indicates regulatory degradation
 *   - Elite Market-Positioned Workers: Bifurcated (powerful/mobile) — experience tangled rope with negotiating power; boundary erosion falls asymmetrically on those with less market leverage
 *   - Analytical Observer: Civilizational view (analytical/analytical) — reveals work-life boundary erosion as systematic temporal extraction mechanism embedded in capital accumulation logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(work_life_boundary_erosion, 0.58).
domain_priors:suppression_score(work_life_boundary_erosion, 0.68).
domain_priors:theater_ratio(work_life_boundary_erosion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(work_life_boundary_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(work_life_boundary_erosion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(work_life_boundary_erosion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(work_life_boundary_erosion, snare).
narrative_ontology:human_readable(work_life_boundary_erosion, "Work-Life Boundary Erosion and Temporal Extraction").
narrative_ontology:topic_domain(work_life_boundary_erosion, "labor/organizational/interpersonal").

domain_priors:requires_active_enforcement(work_life_boundary_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(work_life_boundary_erosion, employers_and_managers).
narrative_ontology:constraint_beneficiary(work_life_boundary_erosion, capital_accumulation).
narrative_ontology:constraint_victim(work_life_boundary_erosion, knowledge_workers).
narrative_ontology:constraint_victim(work_life_boundary_erosion, family_relationships).
narrative_ontology:constraint_victim(work_life_boundary_erosion, personal_autonomy).
narrative_ontology:constraint_victim(work_life_boundary_erosion, collective_labor_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE WORKER (SNARE) — Structurally mobile (can technically leave the job) but identity_locked through professional identity fusion. The worker has internalized 'responsiveness' and 'dedication' as core identity markers. Email and Slack availability become inseparable from professional self-concept. Exit would require abandoning the identity frame that constitutes their career trajectory and social standing. High experienced extraction — suppression mechanism is internalized aspiration rather than external legal barrier.
constraint_indexing:constraint_classification(work_life_boundary_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PARENT (SNARE) — Faces constrained exit: leaving the workforce for childcare carries substantial financial and career penalties; part-time work degrades earning power and advancement prospects; outsourcing care is expensive and emotionally fraught. The work-life boundary erosion creates a double bind: work expands into evening/weekend hours while childcare remains a discrete non-negotiable time block. Suppression is structural (economic dependency, housing costs, healthcare coupling to employment) combined with cultural suppression (guilt narratives around 'having it all'). High extraction with limited exit options.
constraint_indexing:constraint_classification(work_life_boundary_erosion, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Experiences boundary erosion as pure coordination benefit. Asynchronous global operations require 24/7 responsiveness; always-on culture enables operational efficiency, faster decision-making, and competitive advantage in time-sensitive markets. From this perspective, the 'constraint' is a solution to real coordination problems. Net beneficiary with arbitrage options (can switch to competitors with similar norms or relocate operations). Low experienced extraction because the mechanism solves their genuine coordination need.
constraint_indexing:constraint_classification(work_life_boundary_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR ORGANIZING (TANGLED ROPE) — Has genuine interest in work-time coordination (protecting hours, enforcing breaks, preventing burnout-driven attrition) alongside organizational extraction (union membership, dues, institutional power). Recognizes that boundary erosion undermines collective bargaining position but also faces internal dynamics where higher-earning members resist strict work-hour limits that might reduce pay. Constrained exit: unions are embedded in specific industries/regions and face legal/economic barriers to cross-sector organizing. Mixed function with enforcement requirements.
constraint_indexing:constraint_classification(work_life_boundary_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL FRAMEWORK (PITON) — Work-hour regulations (EU Working Time Directive, labor codes mandating break periods) are substantially performative. Enforcement is weak, white-collar exemptions are broad, and 'salaried professional' categories often fall outside protections. The regulations persist as institutional theater — demonstrating concern for worker welfare — while the primary enforcement mechanism (private lawsuit by worker) is effectively blocked by employment contract clauses and power asymmetry. Theater ratio reflects the gap between regulatory intent and enforcement reality. Degraded constraint maintained by institutional inertia rather than functional effectiveness.
constraint_indexing:constraint_classification(work_life_boundary_erosion, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ELITE WORKER (TANGLED ROPE) — High-earning specialists (executives, top engineers, finance professionals) have mobile exit options — can demand boundary protections, negotiate remote flexibility, or move between competitors. Experiences mixed function: genuine coordination problems around global operations and time-sensitive decisions, but also enjoys status signaling and exclusivity (constant availability marks in-group membership among elite). Moderate extraction because they retain negotiating power within the constraint structure. Asymmetric because boundary erosion falls harder on those with less market power.
constraint_indexing:constraint_classification(work_life_boundary_erosion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the boundary erosion mechanism is a structural feature of capital accumulation: extracting unpaid temporal surplus (work performed outside official hours without compensation) enables margin expansion without raising nominal wages. The apparatus (always-on connectivity, professional identity fusion, global arbitrage in labor markets) is systematic rather than aberrant. Suppression is normalized through cultural narratives ('passion for work,' 'startup culture,' 'global competitiveness'). High extraction mechanism designed to be invisible — framed as coordination success rather than temporal expropriation.
constraint_indexing:constraint_classification(work_life_boundary_erosion, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(work_life_boundary_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(work_life_boundary_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(work_life_boundary_erosion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(work_life_boundary_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(work_life_boundary_erosion, TR),
    TR >= 0.70.

:- end_tests(work_life_boundary_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts unpaid temporal surplus (work performed outside contracted hours without additional compensation), degrades autonomy, and reorganizes life structure around employer needs. Empirical estimates suggest 5-15 hours/week of unpaid work among knowledge workers — equivalent to 12-30% wage theft if valued at hourly rate. However, extraction is not total — the constraint offers genuine coordination benefits to employers and some workers retain negotiating power through market scarcity. Theater ratio (0.55): Moderate. The always-on norm is partly functionally necessary (true coordination problems in distributed teams) and partly performative theater. The gap between stated necessity and actual operational requirements appears significant — comparative case studies suggest strict work-hour boundaries achieve equivalent efficiency outcomes without constant availability culture. Theater has increased over the interval as digital connectivity has normalized always-on expectations beyond operational necessity. Suppression (0.68): High. Suppression mechanisms include: material (employment-dependent healthcare, housing, income); structural (career penalties for boundary-setting, promotion freezing for those declining constant availability); internalized (identity fusion making boundary-setting feel like professional failure); and cultural (narratives naturalizing always-on as 'passion' or 'market requirement'). The combination produces substantial barrier to exit without requiring explicit coercion.
 *
 * PERSPECTIVAL GAP:
 *   The identity-locked knowledge worker sees boundary erosion as an internalized professional standard — they cannot imagine themselves outside constant availability culture without experiencing identity dissolution. Exit appears impossible not because they are materially trapped but because 'who I am' is constituted through responsiveness and dedication. The parent sees material suppression (economic dependency) layered with identity conflict (guilt around both work and care). The employer sees pure coordination benefit — a solution to real problems. The union sees genuine work-protection function alongside extractive institutional interest. The legal framework appears to protect but is substantially theater. The elite worker sees negotiation space and selective boundary-setting. The analytical observer sees systematic temporal extraction. The perspectival gap is not disagreement about facts but different visibility of the mechanism based on structural position: those benefiting see coordination, those bearing costs experience extraction, those with institutional power see their regulation as effective while workers experience it as performative.
 *
 * DIRECTIONALITY LOGIC:
 *   The extractiveness metric (0.58) reflects the baseline temporal extraction rate — unpaid work hours captured through boundary erosion, valued as proportion of contracted labor. The suppression metric (0.68) reflects combined barriers to refusing the always-on norm: material job-loss risk for most workers, internalized professional identity for knowledge workers, and cultural suppression for parents. No single directionality value applies to 'the constraint' — different agents experience dramatically different d values. Knowledge workers with identity_lock: d ≈ 0.88 (near-maximum target, trapped by cognitive frame). Parents with constrained exit: d ≈ 0.75 (victim with material barriers). Elite workers with mobile exit: d ≈ 0.45 (partial target with negotiating power). Employers: d ≈ 0.08 (beneficiary, arbitrage-capable). The engine derives d from beneficiary/victim declarations and exit_options, applying the sigmoid f(d) to compute experienced extractiveness chi for each perspective. The perspectival gap (snare from powerless/identity_locked, rope from institutional/arbitrage, tangled_rope from organized/constrained) emerges from these divergent d values and exit capacities.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through perspectival plurality: no single type is 'correct' because the constraint genuinely functions as six different structural mechanisms depending on position. The employer's rope (pure coordination benefit) is their truthful experience but invisible to the powerless worker. The worker's snare (high extraction with suppressed exit) is their truthful experience but invisible to the beneficiary. The union's tangled_rope (mixed coordination and extraction) reflects their hybrid position. The legal framework's piton (performative theater) reflects the gap between regulatory intent and enforcement. The analytical observer's snare (systematic temporal extraction embedded in capital logic) reveals the generative mechanism. The mandatrophy resolution lies in recognizing that the perspectival plurality IS the structural truth: the constraint genuinely coordinates some functions while extracting temporal surplus, and the distribution of benefit/cost follows from power position and exit capacity. The false natural law risk exists in the management framing: 'always-on is required for global coordination' naturalizes a contingent institutional arrangement as technical necessity, suppressing visibility of the extraction mechanism and the available alternatives (async protocols, on-call rotation, scheduled overlap windows).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_material_constraint,
    'For the knowledge worker perspective, how much of the boundary erosion suppression is internalized identity commitment versus material job-loss risk?',
    'Controlled observation: measure worker behavior when job security is decoupled from responsiveness (e.g., union contract with explicit work-hour limits, severance protection, or sabbatical norms that devalue always-on availability). If availability metrics drop sharply, suppression was primarily internalized. If unchanged, material constraints are dominant.',
    'If primarily internalized: the constraint''s extractive mechanism depends on cognitive capture — breaking the identity frame could enable mass exit. If primarily material: exit requires structural change (higher wages, housing decoupling from employment, social insurance). Classification remains snare either way, but the exit pathway differs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_constraint, empirical, 'Relative weight of identity lock vs material job-loss risk in boundary erosion suppression').

omega_variable(
    coordination_function_necessity,
    'Are the actual operational coordination problems that justify always-on culture genuinely unsolvable through other means (scheduled overlap windows, async protocols, on-call rotation), or has the always-on norm become self-perpetuating theater masquerading as necessity?',
    'Comparative case study: organizations that implement strict work-hour boundaries (e.g., ''right to disconnect'' laws in France, German/Spanish labor codes) and measure operational metrics (decision-making speed, error rates, customer satisfaction, retention). If metrics degrade, coordination is necessity. If stable or improve, always-on was theater.',
    'If necessity: rope classification is justified; extraction is partial function of real coordination need. If theater: snare classification becomes even stronger; boundary erosion is pure rent-seeking disguised as coordination efficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_necessity, empirical, 'Whether always-on availability is operationally necessary or performative theater').

omega_variable(
    temporal_extraction_quantification,
    'How much unpaid work time is being extracted through boundary erosion, and does the magnitude constitute a measurable shift in effective hourly compensation?',
    'Time diary studies tracking actual work hours (inclusive of email/Slack/Slack after hours) versus contracted hours, stratified by role and industry. Cross-reference with wage stagnation data for high-extraction industries.',
    'If extraction is > 5 hours/week on average: constitutes ~12% wage theft. If > 10 hours/week: ~25% wage theft. Quantified extraction strengthens snare classification and establishes empirical foundation for mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_extraction_quantification, empirical, 'Magnitude of unpaid work hours extracted through boundary erosion').

omega_variable(
    collective_action_threshold,
    'Can knowledge workers in the same organization develop shared understanding of boundary erosion as extraction rather than personal inadequacy, and if so, can that understanding coordinate collective refusal without triggering retaliation or firing?',
    'Observation of organizing efforts (union drives, work-hour pledges, public commitments to set boundaries) and measurement of management response (retaliation, promotion-freezing, constructive dismissal) and persistence (do coordinators maintain the boundary or revert under pressure).',
    'If coordinated boundary-setting persists: foundation for collective power and potential scaffold/rope transition. If management successfully breaks coordination: snare classification confirmed and suppression mechanism (retaliation threat) becomes explicit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Whether knowledge workers can sustain collective boundary-setting against management pressure').

omega_variable(
    generational_identity_shift,
    'Are younger workers (Gen Z entering labor market) accepting the identity-locked fusion of professional self with constant availability, or are they resisting it as a non-negotiable constraint?',
    'Longitudinal tracking of work-expectation surveys, job-switching patterns, and candidate demands in hiring processes. Measurement of which work norms are treated as negotiable (remote work, flexible hours) versus non-negotiable (responsiveness, always-on culture) in early-career decisions.',
    'If younger cohorts resist identity fusion: identity_locked mechanism may degrade over time as cohort replacement occurs — constraint could shift toward explicit material suppression (pay discrimination for boundary-setters). If accepted: identity-lock mechanism may actually strengthen among new cohorts normalized to always-on culture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_identity_shift, empirical, 'Generational shift in acceptance of work-life boundary erosion and identity fusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(work_life_boundary_erosion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wlbe_tr_t0, work_life_boundary_erosion, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wlbe_tr_t10, work_life_boundary_erosion, theater_ratio, 10, 0.42).
narrative_ontology:measurement(wlbe_tr_t20, work_life_boundary_erosion, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(wlbe_be_t0, work_life_boundary_erosion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wlbe_be_t10, work_life_boundary_erosion, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wlbe_be_t20, work_life_boundary_erosion, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(work_life_boundary_erosion, resource_allocation).
narrative_ontology:boltzmann_floor_override(work_life_boundary_erosion, 0.18).
narrative_ontology:affects_constraint(work_life_boundary_erosion, burnout_epidemic_knowledge_work).
narrative_ontology:affects_constraint(work_life_boundary_erosion, gender_care_work_penalty).
narrative_ontology:affects_constraint(work_life_boundary_erosion, parental_leave_career_damage).
narrative_ontology:affects_constraint(work_life_boundary_erosion, temporal_coupling_housing_markets).

% DUAL FORMULATION NOTE:
% Work-life boundary erosion is the upstream extraction mechanism that enables multiple downstream constraints. Burnout is the pathological outcome of sustained boundary erosion. Gender penalties accumulate because boundary erosion falls asymmetrically on workers with care responsibilities, predominantly women. Temporal coupling to housing markets is enabled by the fact that always-on work normalizes geographic mobility and reliance on employer-provided housing benefits. Each downstream constraint has its own epsilon and perspectives but is causally dependent on the boundary erosion mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(work_life_boundary_erosion, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
