% ============================================================================
% CONSTRAINT STORY: cognitive_labor_precarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cognitive_labor_precarity, []).

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
 *   constraint_id: cognitive_labor_precarity
 *   human_readable: Cognitive Labor Precarity: Flexibility-as-Extraction in Knowledge Work
 *   domain: labor_economics/knowledge_work/institutional_dynamics
 *
 * SUMMARY:
 *   Cognitive labor precarity — the structural arrangement where knowledge
 *   workers lack employment stability, benefits, income predictability, and
 *   collective bargaining power — represents a hybrid coordination-extraction
 *   constraint that has intensified over the last two decades through
 *   platform scaling, labor market segmentation, and institutional erosion of
 *   mid-career professional stability. The constraint exhibits genuine
 *   coordination functions (matching specialized skills to variable demand,
 *   enabling geographic flexibility, reducing hiring friction) alongside
 *   severe extraction mechanisms (worker-borne income volatility, benefits
 *   cascade to individual responsibility, algorithmic control replacing
 *   managerial discretion, erosion of professional autonomy through task
 *   commodification). The constraint manifests differently across the
 *   cognitive labor hierarchy: elite consultants experience it as pure
 *   coordination enabling autonomy and premium compensation; mid-career
 *   professionals experience mixed coordination and extraction; precarious
 *   knowledge workers experience near-pure extraction through income
 *   instability and benefits erosion. The extractiveness value (0.58)
 *   reflects moderate-to-high asymmetric extraction, rising over the
 *   measurement interval as platform concentration increased and traditional
 *   employment paths degraded. Theater ratio (0.55) captures the substantial
 *   performative component: corporate narratives about 'empowerment' and
 *   'entrepreneurial freedom' persist despite contradictory wage and benefit
 *   data, and policy responses (UBI pilots, algorithmic transparency
 *   mandates) function partly as theater (visible reform activity) while
 *   structural extraction mechanisms persist. The constraint's
 *   identity-locking dimension is particularly pronounced — many cognitive
 *   workers have internalized the narrative that traditional employment is
 *   oppressive and precarity is liberation, making identity-fusion a
 *   significant suppression mechanism on top of structural economic barriers.
 *
 * KEY AGENTS:
 *   - Precarious Cognitive Workers: Primary victims (powerless/trapped) — subject to algorithmic task allocation, income volatility, no benefits, indefinite availability demands. Trapped by income need and skill-specificity of knowledge work.
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — capture surplus value from labor supply matching; extract through algorithmic allocation, low effective wages, and reduction of employer obligation to benefits/stability.
 *   - Elite Consultants: Secondary beneficiaries (powerful/arbitrage) — high market power, strong exit options, experience constraint as pure coordination enabling autonomy and premium compensation.
 *   - Mid-Career Professionals: Secondary victims (moderate/constrained) — can negotiate some contracts and project selection, but constrained by credential depreciation risk and family obligations. Experience mixed coordination and extraction.
 *   - Labor Unions: Organized agents (organized/constrained) — attempting to extend labor protections to cognitive precariat; see constraint as historically contingent and negotiable with multi-generational time horizon.
 *   - Policy Interventions: Temporary solutions (organized/constrained) — UBI pilots, algorithmic transparency mandates, collective bargaining rights expansion represent scaffold-class interventions with sunset expectations.
 *   - Corporate Stability Narrative: Institutional framing (institutional/arbitrage) — 'flexibility is empowerment' narrative persists through corporate communications despite contradictory labor data; functions as piton through institutional inertia.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing precarity as inherent to post-industrial capitalism rather than contingent institutional choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cognitive_labor_precarity, 0.58).
domain_priors:suppression_score(cognitive_labor_precarity, 0.65).
domain_priors:theater_ratio(cognitive_labor_precarity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cognitive_labor_precarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(cognitive_labor_precarity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cognitive_labor_precarity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cognitive_labor_precarity, tangled_rope).
narrative_ontology:human_readable(cognitive_labor_precarity, "Cognitive Labor Precarity: Flexibility-as-Extraction in Knowledge Work").
narrative_ontology:topic_domain(cognitive_labor_precarity, "labor_economics/knowledge_work/institutional_dynamics").

domain_priors:requires_active_enforcement(cognitive_labor_precarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cognitive_labor_precarity, platform_corporations).
narrative_ontology:constraint_beneficiary(cognitive_labor_precarity, capital_accumulation).
narrative_ontology:constraint_beneficiary(cognitive_labor_precarity, global_labor_arbitrage).
narrative_ontology:constraint_victim(cognitive_labor_precarity, cognitive_workers).
narrative_ontology:constraint_victim(cognitive_labor_precarity, professional_autonomy).
narrative_ontology:constraint_victim(cognitive_labor_precarity, wage_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRECARIOUS COGNITIVE WORKER (SNARE) — No stable employment status, subject to algorithmic task allocation, income volatility, no benefits, indefinite availability demands. Trapped by need for income and skill-specificity of knowledge work. Cannot exit without abandoning professional identity and economic viability. Maximum suppression and extraction experienced — the worker bears full cost of flexibility through income instability, benefits erosion, and identity fragmentation.
constraint_indexing:constraint_classification(cognitive_labor_precarity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Some ability to negotiate contracts and select projects; benefits from autonomy and flexibility in work scheduling. Also constrained by credential depreciation, career continuity anxiety, and family financial obligations. Experiences genuine coordination (flexible scheduling enables deeper focus on complex problems) alongside extraction (inadequate benefits, unpredictable income, responsibility shifted to individual risk management).
constraint_indexing:constraint_classification(cognitive_labor_precarity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ELITE CONSULTANT (ROPE) — High-status knowledge worker with strong exit options and market power. Experiences the flexibility constraint as pure coordination: can select premium clients, set rates, work on intellectually engaging problems. Extraction is minimal or negative (arbitrage privilege). The flexibility that oppresses precarious workers enables this agent's professional autonomy and premium compensation.
constraint_indexing:constraint_classification(cognitive_labor_precarity, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM CORPORATION (ROPE) — Core function is coordinating supply and demand: matching workers to tasks, aggregating specialized labor, enabling rapid scaling. Net beneficiary through extraction of surplus value from the gap between worker compensation and task rates. Experiences the constraint as coordination mechanism that solves real matching problems while capturing differential access to labor supply. Active enforcement consists of algorithmic allocation rules and performance metrics.
constraint_indexing:constraint_classification(cognitive_labor_precarity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR UNION (TANGLED ROPE) — Organized agents attempting to extend traditional labor protections to cognitive precariat. Genuine coordination problem: gig work genuinely requires different governance structures than factory work. But also extraction: union must organize and enforce new standards against institutional resistance. Sees the constraint as historically contingent and negotiable, with multi-generational time horizon for norm change. Constrained by fragmentation of workforce and capital's ability to offshore cognitive labor.
constraint_indexing:constraint_classification(cognitive_labor_precarity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: POLICY INTERVENTION (SCAFFOLD) — Minimum income guarantees, portable benefits, algorithmic transparency mandates, and collective bargaining rights represent temporary policy structures with explicit sunset clauses (e.g., UBI pilots with defined end dates, platform labor reclassification pending legal challenge). These see cognitive labor precarity as a coordination problem requiring active governance intervention, with expectation that the constraint will be resolved or transformed as new labor institutions mature. Suppression declining as norms shift.
constraint_indexing:constraint_classification(cognitive_labor_precarity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CORPORATE STABILITY NARRATIVE (PITON) — Institutional framing that 'flexible work is empowerment,' 'the gig economy enables entrepreneurship,' and 'traditional employment is the constraint we've liberated workers from.' This narrative persists through corporate communications, management training, and startup mythology despite contradictory labor data. Performative function: the narrative justifies erosion of labor protections as liberation. Actual function has degraded — the narrative no longer persuades most precarious workers, but persists through institutional inertia and media cycles. Theater ratio high because much corporate communication on this topic is ritual performance rather than functional governance.
constraint_indexing:constraint_classification(cognitive_labor_precarity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk of naturalizing cognitive labor precarity as inherent to knowledge work: 'information can be copied infinitely, so compensation must be precarious,' 'cognitive work requires creative instability,' or 'the precariat is the natural endpoint of post-industrial capitalism.' These framings treat contingent institutional arrangements (platform ownership, asymmetric information access, capital's coordination power) as natural laws. The engine's false summit detector will flag this as naturalization of what is actually a negotiated institutional constraint.
constraint_indexing:constraint_classification(cognitive_labor_precarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_labor_precarity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cognitive_labor_precarity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_labor_precarity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cognitive_labor_precarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cognitive_labor_precarity, TR),
    TR >= 0.70.

:- end_tests(cognitive_labor_precarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Base value captures asymmetric extraction through wage suppression, benefits erosion, and income volatility. The rise from 0.35 to 0.58 over the measurement interval reflects platform market consolidation, normalization of precarity expectations, and erosion of traditional employment paths. Suppression (0.65): Moderate-high. Multiple suppression mechanisms operate: economic (income dependency), psychological (identity-fusion with 'autonomous work'), institutional (credential depreciation making exit costly), and structural (limited alternative pathways). Suppression is not total — some workers do exit, and policy alternatives exist, but barriers are substantial. Theater ratio (0.55): Moderate. Corporate narratives about empowerment and flexibility are substantially performative (contradicted by wage data, benefits erosion, algorithmic control), but the performative component is not as dominant as in piton-class constraints (where theater ≥ 0.70). Policy responses also have theater component (visible reform activity without structural change). Claimed type (Tangled Rope): Required for classification because the constraint has both genuine coordination functions (flexible matching of specialized labor to variable demand) AND severe extraction mechanisms (benefits erosion, income instability, algorithmic control, wage suppression). Cannot be classified as pure Rope (extraction is too severe) or pure Snare (coordination function is real for some agents). The Tangled Rope classification captures the asymmetry: coordination benefits flow primarily to elite/powerful agents while extraction costs fall on precarious workers.
 *
 * PERSPECTIVAL GAP:
 *   Cognitive labor precarity produces one of the sharpest perspectival gaps in the corpus because the same institutional arrangement creates both genuine coordination (flexible matching of specialized labor) and severe extraction (income instability, benefits erosion, algorithmic control) — and the distribution of benefits and costs is asymmetric across the power hierarchy. For the precarious worker, the coordination function is invisible — they experience only the extraction. For the platform corporation, the extraction is invisible — they experience only the coordination benefit. For the elite consultant, both the coordination and extraction are present but extraction is near-zero because of high market power and arbitrage options. For the analytical observer, the perspectival gap is a diagnostic signal: if two agents perceive the same constraint as (Rope / Rope) or (Snare / Snare), they have symmetric structural positions; if they perceive (Rope / Snare), they have asymmetric positions relative to the extraction flow. The gap here is largest at (Rope / Snare), which indicates the constraint is extractive as well as coordinative. The gap is analytically productive — it reveals the constraint's hybrid nature rather than obscuring it.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious workers experience high effective extraction (χ high) because they are: (1) trapped (no exit → high d), (2) victims of income instability and benefits erosion (victim status → high d), (3) powerless individually (coordination without power → must bear costs). Platform corporations experience low/negative effective extraction (χ low/negative) because they are: (1) arbitrage-capable (can set task rates and walk away → low d), (2) beneficiaries of wage suppression and benefits avoidance (beneficiary status → low d), (3) institutional power (can absorb coordination costs and distribute them to workers). The asymmetry in experienced extraction is not because the chi formula is wrong but because the structural positions are genuinely asymmetric. Precarious workers and platform corporations occupy opposite positions relative to the extraction flow — one must bear the costs, the other captures the benefits. The Tangled Rope classification captures this asymmetry: genuine coordination function exists (χ formula acknowledges f(d) modulators for different positions), but the extraction is severe and asymmetric (suppression ≥ 0.40, base extraction ≥ 0.30, active enforcement required). This is exactly what Tangled Rope is designed to represent: hybrid constraints where coordination and extraction coexist and are asymmetrically distributed across agent positions.
 *
 * MANDATROPHY ANALYSIS:
 *   Cognitive labor precarity resolves the mandatrophy by demonstrating that a constraint can be simultaneously a Rope (pure coordination) from one agent's perspective, a Snare (pure extraction) from another's perspective, and a Tangled Rope (hybrid) from the analytical view — and all three classifications are correct. The mandate is not 'which type is correct?' but 'what is the structural asymmetry?' The constraint solves a real coordination problem (matching specialized skills to variable demand) — this is the Rope component. It also extracts asymmetrically from precarious workers (income instability, benefits erosion, algorithmic control) — this is the Snare component. The Tangled Rope classification captures the coexistence of both, which single-type classifications would obscure. From the precarious worker's perspective, the coordination function is not visible or accessible — they see only extraction. From the platform corporation's perspective, the extraction is not visible — they see only coordination. From the analytical perspective, both are visible: the constraint genuinely coordinates matching while genuinely extracting from workers with low power and trapped exit options. The mandatrophy is resolved by accepting that the same constraint is different things to different agents, and that the perspectival differences reveal real structural asymmetry rather than measurement error or observational bias. The Tangled Rope type is the correct classification because it is the only type that accounts for both the coordination function and the extraction asymmetry simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_trap,
    'Is cognitive worker precarity driven primarily by identity-fusion with professional autonomy (cannot imagine themselves outside freelance/project work) or by structural economic barriers (cannot actually exit to stable employment)?',
    'Longitudinal study of workers'' stated reasons for precarious status vs actual job market barriers; analysis of career transitions post-exit vs career trajectory before exit; comparison of identity-locked vs structurally-trapped worker cohorts',
    'If primarily identity-locked: classification from powerless perspective shifts from ''trapped'' to ''identity_locked'' → snare from identity-lock perspective becomes rope from shifting frame perspective. Constraint is more psychologically than structurally binding. If primarily structural: the trap is real; identity fusion is secondary psychological response. Determines whether escape requires identity transformation or material barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_trap, empirical, 'Whether precarity is identity-fusion or structural entrapment').

omega_variable(
    beneficial_coordination_vs_cover_story,
    'Does cognitive labor precarity genuinely coordinate flexible matching of specialized skills to variable demand (legitimate Rope function) or is flexibility coordination a cover story for extraction?',
    'Comparison of platform efficiency metrics (task completion rates, worker utilization) under precarious vs stable employment models; analysis of whether flexibility enables higher-value problem-solving or merely increases platform extractiveness; study of worker preference when income security is held constant',
    'If genuine coordination: constraint legitimately has Rope-class function for some agents; Tangled Rope classification is correct (coordination + extraction). If cover story: the ''flexibility'' is purely extractive mechanism dressed in coordination language; should be classified as Snare from all perspectives. Determines whether negotiation is possible (Tangled Rope → labor standard improvements) or whether only full exit stops extraction (Snare → abolition required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficial_coordination_vs_cover_story, empirical, 'Whether flexibility is genuine coordination function or extraction cover story').

omega_variable(
    suppression_mechanism_source,
    'Is suppression primarily economic (literal inability to exit due to income need) or psychological (internalized precarity narrative about autonomous work being superior to employment)?',
    'Post-policy intervention analysis: if income floors reduce precarity acceptance despite loss of flexibility, suppression was partly psychological. If precarity persists even with income security, suppression is primarily structural. Comparison of worker satisfaction across policy regimes (guaranteed income, union bargaining, algorithmic transparency mandates).',
    'If primarily economic: policy can reduce suppression through income guarantees and benefits portability. If primarily psychological: policy requires narrative intervention (reframing stable employment as non-oppressive) alongside material protection. Suppression value (0.65) may underestimate if psychological component persists across policy regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Economic vs psychological sources of suppression').

omega_variable(
    global_arbitrage_ceiling,
    'Is offshore cognitive labor (India, Philippines, Eastern Europe) a limiting factor on precarity, or does it feed back to reinforce precarity by creating global wage arbitrage?',
    'Wage trend analysis: do cognitive worker wages in developed economies decline or stabilize as offshore supply increases? Labor substitution studies: what fraction of precarious workers are in competition with offshore labor? Exit transition analysis: do precarious workers in arbitrage-exposed roles face fewer exit options?',
    'If limiting: offshore labor creates competitive pressure that enforces precarity as equilibrium. Constraint becomes nearly immutable at the global scale. If not limiting: precarity is primarily institutional choice; negotiable at national scale. Determines whether constraint remains high-extractiveness (Snare/Tangled Rope at global scope) or becomes negotiable (Scaffold at national scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_arbitrage_ceiling, empirical, 'Whether global labor arbitrage locks in precarity').

omega_variable(
    platform_dependency_lock,
    'Do platforms function as genuine matching infrastructure (low switching costs, many-to-many relationships, worker optionality) or as extractive gatekeepers (high switching costs, monopsony power, algorithmic control)?',
    'Measurement of platform concentration in cognitive labor markets; analysis of worker switching costs (reputation transfer, skill certification portability, alternative platform availability); correlation between platform market share and wage suppression; longitudinal study of wage equality across competing platforms in the same skill domain',
    'If genuine matching: platform role is Rope-class coordination; precarity is negotiable through better contracting. If extractive gatekeeping: platform concentration locks in precarity regardless of worker preference; becomes Snare feature. Determines whether policy should focus on platform regulation (monopsony control) vs universal labor protection vs platform replacement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_dependency_lock, empirical, 'Platform role as matcher vs monopsony gatekeeper').

omega_variable(
    credential_degradation_irreversibility,
    'Can cognitive workers who exit precarity back into stable employment reenter at equivalent skill/compensation levels, or does precarious period create permanent credential/experience damage?',
    'Comparative career trajectory analysis: workers with gaps in traditional employment (due to precarity interruptions) vs workers with continuous employment; hiring discrimination studies on CV signaling of precarious work history; wage recovery analysis for workers transitioning from precarity to stable roles',
    'If reversible: precarity is constraining but not identity-destructive; exit is real option (constrained rather than trapped). If irreversible: precarity damages career trajectory permanently; workers become trapped or identity-locked through credential depreciation. Affects classification of worker''s exit options from ''trapped'' or ''constrained'' vs actual optionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_degradation_irreversibility, empirical, 'Whether precarity creates permanent credential damage').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cognitive_labor_precarity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cognlabor_tr_t0, cognitive_labor_precarity, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cognlabor_tr_t5, cognitive_labor_precarity, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cognlabor_tr_t10, cognitive_labor_precarity, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(cognlabor_be_t0, cognitive_labor_precarity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cognlabor_be_t5, cognitive_labor_precarity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cognlabor_be_t10, cognitive_labor_precarity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cognitive_labor_precarity, resource_allocation).
narrative_ontology:affects_constraint(cognitive_labor_precarity, skill_credential_depreciation).
narrative_ontology:affects_constraint(cognitive_labor_precarity, platform_monopsony_concentration).
narrative_ontology:affects_constraint(cognitive_labor_precarity, welfare_state_erosion).
narrative_ontology:affects_constraint(cognitive_labor_precarity, professional_identity_fragmentation).

% DUAL FORMULATION NOTE:
% Cognitive labor precarity is upstream of multiple structural constraints in knowledge work and post-industrial labor markets. The constraint's extractiveness feeds into credential depreciation (workers accumulate less stable employment history, damaging future job market position), platform concentration (monopsony power enables extraction), welfare erosion (individual risk-bearing replaces collective provision), and identity fragmentation (precarity prevents coherent professional identity formation). Each downstream constraint has its own extractiveness value; cognitive labor precarity acts as a causal mechanism intensifying all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cognitive_labor_precarity, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
