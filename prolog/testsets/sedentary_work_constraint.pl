% ============================================================================
% CONSTRAINT STORY: sedentary_work_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sedentary_work_constraint, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sedentary_work_constraint
 *   human_readable: Sedentary Work Constraint: Health Extraction Within Coordination
 *   domain: labor/occupational_health/economic_structure
 *
 * SUMMARY:
 *   The sedentary work constraint binds knowledge workers into desk-based
 *   labor through a hybrid mechanism: genuine coordination needs
 *   (synchronization, supervision, capital concentration) layered over
 *   asymmetric health extraction. Employers capture productivity and
 *   efficiency gains while workers bear musculoskeletal, metabolic, and
 *   cardiovascular costs. The constraint manifests as mandatory office
 *   presence despite technological obsolescence of physical proximity. It
 *   demonstrates how coordination mechanisms can be captured by extractive
 *   power structures. The theatrical dimension (presenteeism, status
 *   signaling, visibility management) has increased over the interval as
 *   technology has eliminated functional necessity for co-location, revealing
 *   the piton component: office persistence is increasingly institutional
 *   inertia rather than functional coordination. Remote work infrastructure
 *   and ergonomic standards represent real alternative pathways with genuine
 *   sunset potential, making the scaffold perspective structurally plausible
 *   for organized actors, while trapped workers remain locked through
 *   economic dependency.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victims (powerless/trapped) — experience health extraction through sustained postural stress, metabolic constraint, and sedentary-related disease risk. Economic dependency prevents exit.
 *   - Worker Collective/Unions: Secondary actor (moderate/constrained) — can mobilize advocacy and negotiate working conditions but face high organizing barriers in knowledge sectors. Generational timeframe for structural change.
 *   - Employers/Capital: Primary beneficiary (institutional/arbitrage) — capture productivity and efficiency gains from coordination while externalizing health costs. High arbitrage optionality to shift constraints.
 *   - Occupational Health Authorities: Organized reform coalition (organized/mobile) — implement ergonomic standards, accessibility requirements, wellness programs with explicit sunset to distributed work models.
 *   - Real Estate and Facility Management: Institutional actor (institutional/arbitrage) — maintains office constraint through sunk-cost commitment and real estate investment cycles. High arbitrage optionality but structural lock-in to office model.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing office-based coordination as inherent to knowledge work rather than recognizing it as contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sedentary_work_constraint, 0.52).
domain_priors:suppression_score(sedentary_work_constraint, 0.65).
domain_priors:theater_ratio(sedentary_work_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sedentary_work_constraint, extractiveness, 0.52).
narrative_ontology:constraint_metric(sedentary_work_constraint, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sedentary_work_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sedentary_work_constraint, tangled_rope).
narrative_ontology:human_readable(sedentary_work_constraint, "Sedentary Work Constraint: Health Extraction Within Coordination").
narrative_ontology:topic_domain(sedentary_work_constraint, "labor/occupational_health/economic_structure").

domain_priors:requires_active_enforcement(sedentary_work_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sedentary_work_constraint, employers_capital_efficiency).
narrative_ontology:constraint_beneficiary(sedentary_work_constraint, productivity_metrics_optimization).
narrative_ontology:constraint_victim(sedentary_work_constraint, worker_musculoskeletal_health).
narrative_ontology:constraint_victim(sedentary_work_constraint, worker_metabolic_function).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SEDENTARY WORKER (SNARE) — Trapped by economic dependency and labor market structure. Cannot exit desk work without sacrificing income stability. Trapped exit means the constraint is unchangeable from this position. Extraction is maximal: health costs are borne entirely by the worker while productivity gains flow to employer.
constraint_indexing:constraint_classification(sedentary_work_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE WORKER COLLECTIVE (TANGLED ROPE) — Faces high costs to exit sedentary labor (career retraining, wage penalties, geographic relocation) but can mobilize collectively through unions and advocacy. Generational timeframe reveals genuine coordination function: sedentary work enables complex knowledge work and distributed labor processes. Asymmetric extraction: coordinated productivity gains concentrate benefits to capital while health costs diffuse to workers. Active enforcement through workplace norms, productivity expectations, and scheduling requirements.
constraint_indexing:constraint_classification(sedentary_work_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EMPLOYER/CAPITAL (ROPE) — Experiences sedentary work as pure coordination: concentrating workers in fixed locations enables supervision, synchronization, and efficient capital deployment. High arbitrage optionality — can shift to remote work, alternate locations, or automation. Net beneficiary. The constraint solves a coordination problem from this perspective; the extraction is invisible because benefits flow toward this agent.
constraint_indexing:constraint_classification(sedentary_work_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE WELLNESS AND REMOTE WORK COALITION (SCAFFOLD) — Organized agents (occupational health authorities, ergonomics standards bodies, remote-work advocates) see sedentary constraint as a temporary coordination failure being solved through technology and policy. Standing desks, ergonomic mandates, and remote-work infrastructure represent sunset mechanisms. Has explicit temporal horizon: 15-25 years for knowledge work to become location-independent and sedentary work to become optional rather than mandatory.
constraint_indexing:constraint_classification(sedentary_work_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE INDUSTRIAL OFFICE PARADIGM (PITON) — The office building itself has become largely ceremonial. Real-time communication technology has eliminated the need for physical proximity, yet office attendance persists through institutional inertia and supervisory theater. Theater ratio (0.58) reflects that presenteeism and visibility measures maintain the constraint despite functional redundancy. The paradigm knows its own function has atrophied but persists because organizational identity remains fused with office real estate and management culture.
constraint_indexing:constraint_classification(sedentary_work_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some coordination through physical proximity may be inherent to knowledge transfer, apprenticeship, and cultural reproduction. In-person interaction may have irreducible value that no technology can fully replace. This perspective risks naturalizing the office constraint as an immutable feature of human work. However, the structural data contradicts this — the 0.52 extractiveness and 0.65 suppression reveal that the constraint is extractive in ways that genuine coordination mechanisms are not.
constraint_indexing:constraint_classification(sedentary_work_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sedentary_work_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sedentary_work_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sedentary_work_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sedentary_work_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sedentary_work_constraint, TR),
    TR >= 0.70.

:- end_tests(sedentary_work_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant health costs from workers while providing genuine coordination benefits to employers. The value reflects that the coordination function is real (synchronization, supervision, knowledge transfer) but captures benefits asymmetrically — productivity gains concentrate to capital while health externalities diffuse to workers. Rising trajectory (0.38→0.52 over interval) reflects increasing theater ratio and decreasing functional necessity for physical presence, indicating extraction is becoming less coordinated and more purely extractive. Suppression (0.65): High. Economic dependency traps workers in sedentary positions despite knowledge of health risks. Labor market structure, benefits lock-in, geographic immobility, and career risk of job-switching create near-total barriers to exit. However, suppression is not absolute — some workers have mobile options through remote work transitions and sector switching. Theater ratio (0.58): Moderate-high. Office presence increasingly performs status and discipline functions (presenteeism, visibility, supervision) that technology has made functionally unnecessary. Rising trajectory (0.35→0.58) reflects that as remote work has proven viable, office persistence becomes more clearly theatrical. The constraint operates through performative requirements (showing up, being seen, clock time) rather than actual coordination necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a genuine coordination mechanism (office co-location for synchronization and knowledge transfer) becomes captured by asymmetric extraction (health costs borne by workers, productivity gains captured by capital). The trapped worker cannot perceive the coordination function — they experience only extraction (Snare). The worker collective perceives both the coordination and the extraction asymmetry (Tangled Rope). The employer perceives only the coordination benefit, not the extraction (Rope). The scaffold coalition sees the constraint as temporary and solvable through technology (remote infrastructure, asynchronous communication). The piton observer recognizes that office persistence is increasingly theatrical — the function has atrophied but organizational inertia maintains the constraint. The civilizational analytical observer risks assuming office-based knowledge work is inherent to human cognition and learning, naturalizing what is actually a 20th-century industrial artifact that post-industrial technology has rendered optional.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim relationship is clear: employers benefit from capital concentration, synchronization, and supervision; workers bear health extraction costs. Suppression flows through economic dependency (trapped exit) rather than direct coercion. The theater ratio's rise reveals that as technology has eliminated functional necessity for proximity, the constraint persists through performative requirements (presenteeism, visibility) rather than genuine coordination needs. Directionality overrides are not needed — the structural relationship is transparent. However, the intra-agent heterogeneity (some workers are trapped, some are constrained, some are mobile) suggests that perspectives should differentiate exit_options within the worker group. The trapped worker (powerless/trapped) sees snare; the unionized or mobile worker (moderate/constrained or mobile) sees tangled_rope; the professional with flexible options (powerful/mobile) might see rope or scaffold. This heterogeneity is captured in the perspectives above through differential power levels and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: The sedentary work constraint resolves the mandatrophy by showing how a genuine coordination mechanism (office co-location) becomes structurally indistinguishable from pure extraction when asymmetric capture occurs. The tangled_rope classification avoids false positive (incorrectly labeling extraction as pure coordination) by requiring BOTH a real beneficiary group AND a real victim group PLUS active enforcement. The constraint has all three: employers benefit from capital concentration, workers bear health costs, and office presence is actively enforced through workplace norms and productivity expectations. The rising theater ratio (0.35→0.58) indicates the coordination function is atrophying while extraction persists, which would eventually reclassify this as a snare if theaters exceed 0.70 and extractiveness increases further. The scaffold perspective shows a genuine alternative pathway (remote work infrastructure) with explicit sunset mechanisms, preventing misclassification as immutable snare. The piton perspective reveals the institutional inertia component: office persistence is increasingly theatrical, maintained by sunk-cost real estate commitments and organizational identity rather than coordination necessity. The false-summit mountain perspective identifies the risk of naturalizing office work as inherent to knowledge work rather than recognizing it as a contingent institutional choice that technology has made optional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    health_cost_attribution,
    'Are the measured health costs (cardiovascular disease, metabolic syndrome, musculoskeletal disorder) causally attributable to sedentary work or to correlated factors (diet, stress, lifestyle selection)?',
    'Longitudinal studies controlling for diet, stress, exercise behavior, and socioeconomic status; natural experiments comparing health trajectories before and after work-from-home transitions',
    'If sedentary work is causal: suppression value is justified; victims group is accurately identified. If confounded: some measured suppression reflects agent choices rather than constraint imposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(health_cost_attribution, empirical, 'Causal attribution of health costs to sedentary work vs confounded lifestyle factors').

omega_variable(
    remote_work_productivity_parity,
    'Does remote knowledge work produce equivalent or superior productivity compared to office-based work, controlling for task type and worker experience?',
    'Meta-analysis of productivity studies; comparison of output metrics (code commits, project completion, error rates) for identical teams before and after remote transition; industry-specific case studies',
    'If parity or superior: the coordination function claimed by the employer perspective is largely theater; extractiveness should increase toward 0.66+ (pure snare). If office produces measurably higher coordination: the tangled_rope classification is justified; coordination benefit is real but captured asymmetrically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remote_work_productivity_parity, empirical, 'Whether remote work produces equivalent productivity outcomes').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is worker acceptance of sedentary work constraint driven by material economic necessity (structural suppression) or by internalized beliefs about work discipline, professional identity, and organizational loyalty (identity-locked suppression)?',
    'Survey-based measurement of exit costs (income penalty, benefits loss, relocation burden) vs exit beliefs (perceived damage to career trajectory, internalized sense of professional obligation); comparison of stated vs revealed preferences when exit costs are experimentally reduced',
    'If structural: suppression value (0.65) is accurate; exit_options remain trapped/constrained. If partially internalized: some workers are identity_locked rather than trapped; classify with (moderate, identity_locked) perspective showing different perceived mutability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural economic necessity or internalized professional identity').

omega_variable(
    office_real_estate_sunk_cost_lock,
    'Does employer commitment to sedentary office work reflect genuine coordination necessity or primarily reflect sunk cost fallacy and real estate investment lock-in?',
    'Analysis of office real estate commitments relative to productivity data; comparison of firms with long-term leases vs flexible space arrangements; qualitative interviews with facility and HR leadership about remote work resistance drivers',
    'If sunk cost dominates: the piton classification is correct; extractiveness represents institutional inertia rather than functional necessity. If coordination is genuine: scaffold classification may be overstated; sunset timeline may be longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(office_real_estate_sunk_cost_lock, empirical, 'Whether office constraint reflects sunk cost lock-in vs genuine coordination necessity').

omega_variable(
    knowledge_work_tacit_transfer_necessity,
    'Is in-person proximity genuinely necessary for transfer of tacit knowledge, cultural reproduction, and mentorship in knowledge work, or can these functions be adequately supported through distributed means?',
    'Longitudinal measurement of knowledge transfer outcomes (mentee skill development, innovation diffusion) for distributed vs co-located teams; analysis of tacit knowledge sharing mechanisms in high-performing remote organizations; cross-sector comparison of apprenticeship success rates',
    'If in-person is necessary: mountain or rope classifications gain legitimacy; the constraint reflects genuine structural limits. If distribution is adequate: the coordination function is overstated; extractiveness should increase (constraint becomes more purely extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_work_tacit_transfer_necessity, empirical, 'Whether in-person proximity is necessary for tacit knowledge transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sedentary_work_constraint, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sedwork_tr_t0, sedentary_work_constraint, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sedwork_tr_t10, sedentary_work_constraint, theater_ratio, 10, 0.48).
narrative_ontology:measurement(sedwork_tr_t20, sedentary_work_constraint, theater_ratio, 20, 0.58).
narrative_ontology:measurement(sedwork_tr_t25, sedentary_work_constraint, theater_ratio, 25, 0.62).

% Extraction over time
narrative_ontology:measurement(sedwork_be_t0, sedentary_work_constraint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sedwork_be_t10, sedentary_work_constraint, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(sedwork_be_t20, sedentary_work_constraint, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(sedwork_be_t25, sedentary_work_constraint, base_extractiveness, 25, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sedentary_work_constraint, resource_allocation).
narrative_ontology:affects_constraint(sedentary_work_constraint, occupational_health_externalities).
narrative_ontology:affects_constraint(sedentary_work_constraint, real_estate_asset_lock_in).
narrative_ontology:affects_constraint(sedentary_work_constraint, workplace_visibility_surveillance).

% DUAL FORMULATION NOTE:
% Sedentary work constraint decomposes into distinct structural stories: (1) labor coordination through physical concentration (genuine coordination function), (2) health extraction through postural demand (victim extraction), (3) real estate investment lock-in (institutional inertia), (4) presenteeism theater (performative supervision). This story treats them as one constraint because they operate through the same enforcement mechanism (office presence requirement). Decomposition would separate the coordination story (office-based knowledge work) from the extraction story (health externalities) if their ε values diverged significantly. Current modeling treats them as integrated: the constraint's extractiveness reflects both coordination benefit (real) and health cost asymmetry (extractive).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
