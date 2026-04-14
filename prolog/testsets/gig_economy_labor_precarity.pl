% ============================================================================
% CONSTRAINT STORY: gig_economy_labor_precarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_labor_precarity, []).

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
 *   constraint_id: gig_economy_labor_precarity
 *   human_readable: Gig Economy Labor Precarity
 *   domain: labor/economic/social
 *
 * SUMMARY:
 *   The gig economy labor precarity constraint represents a hybrid
 *   extraction-coordination mechanism that has evolved over the past 15 years
 *   as ride-sharing, delivery, and task platforms have expanded. The
 *   constraint exhibits tension between genuine labor market coordination
 *   (matching workers to tasks with minimal friction) and systematic worker
 *   benefit externalization (shifting costs for benefits, safety, training,
 *   and stability to workers themselves). The structural gap between platform
 *   claims (efficient coordination, worker flexibility) and worker experience
 *   (wage suppression, arbitrary deactivation, precarity) generates multiple
 *   classification types from different perspectives. Theater ratio (0.55)
 *   reflects that algorithmic management systems present themselves as
 *   neutral, efficient tools while actually functioning as behavioral control
 *   mechanisms disguised as performance metrics. The measurement trajectory
 *   shows increasing extractiveness and theater ratio over the interval,
 *   indicating that platforms have progressively layered extraction
 *   mechanisms onto their coordinate functions while making these mechanisms
 *   less visible (higher theater).
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victims (powerless/trapped) — face wage suppression, benefit exclusion, algorithmic control, arbitrary deactivation, economic dependency
 *   - Organized Worker Coalitions: Secondary actors (moderate/constrained) — building collective power through unions, regulatory advocacy, mutual aid networks; face barriers but have exit pathways
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — capture value through labor supply atomization, wage suppression, benefit externalization, regulatory arbitrage; high exit optionality
 *   - Consumer Base: Secondary beneficiaries (powerful/arbitrage) — benefit from suppressed prices enabled by externalized worker costs; perceive efficient coordination
 *   - Labor Market Stability: Systemic victim (powerful/constrained) — faces degradation through precariat expansion, reduced intergenerational mobility, reduced consumer demand, reduced human capital investment
 *   - Regulatory Reform Coalition: Organized agents (organized/mobile) — labor advocates, progressive regulators, political movements pursuing reclassification and portable benefits as exit pathways
 *   - Algorithmic Dispatch System: Institutional actor (institutional/constrained) — maintains performative management ritual; theater serves suppression function
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent policy choices as immutable technological/economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_labor_precarity, 0.58).
domain_priors:suppression_score(gig_economy_labor_precarity, 0.68).
domain_priors:theater_ratio(gig_economy_labor_precarity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_labor_precarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(gig_economy_labor_precarity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gig_economy_labor_precarity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_labor_precarity, tangled_rope).
narrative_ontology:human_readable(gig_economy_labor_precarity, "Gig Economy Labor Precarity").
narrative_ontology:topic_domain(gig_economy_labor_precarity, "labor/economic/social").

domain_priors:requires_active_enforcement(gig_economy_labor_precarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_labor_precarity, platform_corporations).
narrative_ontology:constraint_beneficiary(gig_economy_labor_precarity, consumer_base).
narrative_ontology:constraint_victim(gig_economy_labor_precarity, gig_workers).
narrative_ontology:constraint_victim(gig_economy_labor_precarity, labor_market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Faces minimal ability to exit: economic dependency, lack of alternative employment options in their region, no access to benefits if they leave. The constraint extracts through wage suppression, forced acceptance of arbitrary deactivation, and externalization of all business risk. Bears maximum extraction with no genuine coordination benefit.
constraint_indexing:constraint_classification(gig_economy_labor_precarity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED WORKER COALITION (TANGLED ROPE) — Faces high but surmountable exit costs (organizing campaigns, union drives, regulatory advocacy). Experiences both coordination benefits (collective bargaining potential, mutual aid networks) and significant extraction (platform wage suppression, algorithmic management, risk externalization). Significant agency through collective action, though constrained by legal and political barriers.
constraint_indexing:constraint_classification(gig_economy_labor_precarity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM CORPORATION (ROPE) — Experiences the constraint as pure coordination: efficiently matching supply and demand, reducing friction in labor allocation, creating network benefits for consumers. Extraction flows toward this agent; they perceive only coordination functions. High exit optionality (capital mobility, multi-market presence) and benefits from wage suppression through labor supply atomization.
constraint_indexing:constraint_classification(gig_economy_labor_precarity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSUMER (ROPE) — Benefits from low prices and on-demand access enabled by suppressed worker compensation. Perceives the constraint as efficient coordination between supply and demand. Extraction is hidden in reduced consumer prices; genuine coordination function exists (efficient matching). Exit optionality is high (can use competing platforms or traditional services).
constraint_indexing:constraint_classification(gig_economy_labor_precarity, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR MARKET STABILITY (TANGLED ROPE) — Gig economy expands labor supply while degrading protections and benefits. The constraint coordinates matching of workers to tasks while extracting through wage suppression and benefit externalization. Long-term labor market stability is compromised — birth of a precariat class with reduced human capital investment, reduced consumer demand, reduced intergenerational mobility. Coordinating function exists (task matching) alongside asymmetric extraction (cost socialization).
constraint_indexing:constraint_classification(gig_economy_labor_precarity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY REFORM COALITION (SCAFFOLD) — Organized labor advocates, regulators, and political movements see the gig economy constraint as a temporary institutional failure with a sunset clause. Potential exit paths include: reclassification of workers as employees (EU model), portable benefits systems (sectoral bargaining), algorithmic transparency requirements. Organizing power exists, and a transition pathway is visible. The constraint is temporary if policy intervention succeeds — hence scaffold classification. Suppression is high during the interval, but the coalition perceives it as declining.
constraint_indexing:constraint_classification(gig_economy_labor_precarity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ALGORITHMIC DISPATCH SYSTEM (PITON) — The algorithmic management system (job assignment, performance ratings, deactivation) is largely performative. The system's stated purpose is efficient matching and quality control, but its actual function is behavioral control and wage suppression through opaque metrics. Workers experience it as degraded — ritual rather than genuine quality feedback. The system persists through institutional inertia (regulatory capture, technical opacity, difficulty of alternatives) rather than functional necessity. Theater ratio high; genuine coordination function low.
constraint_indexing:constraint_classification(gig_economy_labor_precarity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From a civilizational/universal perspective, the gig economy risk is that observers naturalize precarity as an immutable feature of labor market evolution: 'Technology has made traditional employment obsolete' or 'Worker flexibility requires accepting precarity.' This perspective risks treating a contingent institutional arrangement (regulatory capture, platform dominance, deliberate benefit externalization) as a law of nature. The engine's false summit detector should flag this: the structural data reveals precarity is engineered through policy choice and platform design, not natural law.
constraint_indexing:constraint_classification(gig_economy_labor_precarity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_labor_precarity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_labor_precarity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_labor_precarity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_labor_precarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_economy_labor_precarity, TR),
    TR >= 0.70.

:- end_tests(gig_economy_labor_precarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms suppress wages systematically (20-40% below comparable employment when benefits are included), externalize all risk and costs, and maintain atomized labor supply preventing collective bargaining. However, genuine efficiency gains exist (reduced matching friction, lower transaction costs) that provide some non-extractive value. The intermediate value reflects hybrid coordination-extraction structure. Suppression (0.68): High. Workers face multiple barriers: no alternative employment in regions without platform ubiquity, economic dependency (minimum income requirements), regulatory exclusion from worker protections, algorithmic control systems that constrain behavior, arbitrary deactivation risks. Barriers are enforced through platform policies, regulatory capture, and labor market concentration. Theater ratio (0.55): Moderate-high. Algorithmic management systems claim to be neutral performance metrics but function as behavioral control and wage suppression tools. Algorithmic assignment opacity is justified as efficiency but serves suppression. The ritual of transparent metrics (ratings, acceptance rates) provides the appearance of meritocratic feedback while actual criteria remain hidden. The trajectory shows increasing theater over the interval as platforms have invested in opacity and mystification of algorithmic systems.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Platform corporations and consumers see rope (efficient coordination, matching, reduced friction). Organized coalitions see tangled rope (mixed coordination benefit with high extraction). Individual workers see snare (pure extraction with no exit). Labor market sees systemic degradation (tangled rope at civilizational scale). The regulatory coalition sees a temporary problem with a sunset (scaffold). The algorithmic system sees its own ritual as performative (piton). The civilizational analytical observer risks seeing immutable natural law (false mountain) — that 'gig economy is inevitable' or 'flexibility requires precarity.' The gap reveals that classification depends entirely on structural position: beneficiaries perceive coordination; targets perceive extraction; observers perceive law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position within the extraction flow. Workers (trapped exit) + victim status derive d ≈ 0.95, producing high f(d) and experienced χ. Organized coalitions (constrained exit) + mixed beneficiary/victim derive d ≈ 0.55, moderate χ. Platform corporations (arbitrage exit) + beneficiary status derive d ≈ 0.10, low/negative χ. Consumers (arbitrage exit) + beneficiary status derive d ≈ 0.15, low χ. Labor market (constrained exit) + victim status derive d ≈ 0.70, high χ. The directed flow is clearly upward toward platforms and consumers, outward to workers. Exit optionality is the critical differentiator: trapped workers cannot escape the extraction; organized coalitions can build power; platforms have capital mobility and multi-market options.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint shows that tangled rope is the correct classificatory anchor. The base properties show genuine coordination function (labor matching efficiency, reduced friction) layered with systematic extraction (wage suppression, benefit externalization, algorithmic control). The mandatrophy is resolved by showing that: (1) the coordination function is real but not the primary extraction driver; (2) the extraction mechanism is not accidental but designed (platform investment in algorithmic opacity, benefit exclusion, labor supply atomization); (3) perspectives diverge based on position in the extraction flow, not perception disagreement. The false mountain perspective (analytical observer naturalizing precarity) is correctly flagged by the structural data — extractiveness (0.58) exceeds mountain threshold, beneficiary/victim structure exists, and active enforcement is required. The organizational coalition's scaffold perspective is conditional on policy implementation (regulatory reclassification, portable benefits) — if sunset mechanisms fail, the constraint becomes long-term snare. The piton perspective reveals that algorithmic management's performative function serves the extraction goal, not genuine coordination. CRITICAL: the mandatrophy is NOT resolved by choosing one type. Instead, it is resolved by showing that the perspectival structure itself IS the constraint — the ability of beneficiaries to perceive rope while victims perceive snare, while observers naturalize the arrangement as law, is what sustains the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    worker_exit_optionality_ambiguity,
    'Are gig workers genuinely trapped (no alternatives) or identity_locked (internalized precarity as normal/inevitable)?',
    'Longitudinal surveys tracking workers who exit gig platforms: do they report reduced suppression post-exit? Can they name alternative employment? Has internalized precarity persisted after structural exit?',
    'If trapped: maximum f(d) value, snare classification stable. If identity_locked: perspectival gap between trapped and analytical observers; workers perceive immutability from identity frame rather than structural barriers. Diagnostic for whether suppression is structural or internalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_exit_optionality_ambiguity, empirical, 'Whether gig workers are structurally trapped or identity-locked in precarity').

omega_variable(
    coordination_function_genuineness,
    'Does the gig economy coordinate labor allocation with minimal friction, or is the ''efficiency'' primarily achieved through wage suppression and benefit externalization?',
    'Comparative cost analysis: gig economy matching efficiency vs traditional employment/temp agencies when full worker costs (benefits, safety, training) are included. Counterfactual: what would matching efficiency be if all costs were internalized?',
    'If genuine coordination: tangled rope classification confirmed — extraction layered onto real efficiency gains. If false efficiency: snare classification strengthens — perceived coordination benefit is primarily artifact of cost externalization. Impacts classification across all non-mountain perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_genuineness, empirical, 'Whether gig economy efficiency is genuine or externality-driven').

omega_variable(
    platform_revenue_dependence_distribution,
    'How much of gig platform revenue comes from wage suppression vs genuine technological efficiency? What proportion of consumer surplus comes from suppressed worker compensation vs eliminated intermediaries?',
    'Platform financial analysis; counterfactual modeling with platform-absorbed worker costs; regional comparison of wage levels in gig vs traditional sectors controlling for task complexity.',
    'High suppression proportion: strengthens snare. Low proportion: tangled rope model holds. Affects interpretation of whether constraint is primarily extractive or has genuine coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_revenue_dependence_distribution, empirical, 'Revenue attribution between suppression and efficiency').

omega_variable(
    regulatory_sunset_viability,
    'Is the scaffold perspective''s sunset (regulatory reclassification, portable benefits, algorithmic transparency) actually achievable or aspirational/contingent on political conditions?',
    'Policy analysis of existing regulatory attempts (EU worker reclassification, sectoral bargaining proposals); barriers to implementation; political economy of platform opposition.',
    'If achievable: scaffold classification robust, temporal horizon for extraction decline is measurable. If aspirational: scaffold is perceptual rather than structural; constraint may be long-term snare/tangled rope. Affects generational and civilizational time horizon classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_viability, empirical, 'Whether regulatory sunset for gig economy precarity is structurally viable').

omega_variable(
    algorithmic_opacity_as_suppression_mechanism,
    'Is the opacity of algorithmic job assignment and performance metrics a necessary feature of efficient dispatch or a deliberate mechanism for wage suppression and behavioral control?',
    'Comparative analysis of platform algorithms that use transparent vs opaque metrics; worker productivity/satisfaction under transparent algorithms; platform reluctance to adopt transparency; hidden algorithm experiments (A/B tests designed to suppress wages).',
    'If necessary: piton classification holds (performative but necessary). If deliberate: piton classification strengthens (theater serving suppression function); snare perspective strengthened. Diagnostic for whether algorithmic management coordinates or extracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity_as_suppression_mechanism, empirical, 'Whether algorithmic opacity is functional necessity or suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_labor_precarity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig_tr_t0, gig_economy_labor_precarity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gig_tr_t5, gig_economy_labor_precarity, theater_ratio, 5, 0.47).
narrative_ontology:measurement(gig_tr_t10, gig_economy_labor_precarity, theater_ratio, 10, 0.55).
narrative_ontology:measurement(gig_tr_t15, gig_economy_labor_precarity, theater_ratio, 15, 0.61).

% Extraction over time
narrative_ontology:measurement(gig_be_t0, gig_economy_labor_precarity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gig_be_t5, gig_economy_labor_precarity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(gig_be_t10, gig_economy_labor_precarity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(gig_be_t15, gig_economy_labor_precarity, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_labor_precarity, resource_allocation).
narrative_ontology:affects_constraint(gig_economy_labor_precarity, wage_suppression_labor_markets).
narrative_ontology:affects_constraint(gig_economy_labor_precarity, benefit_externalization_welfare_systems).
narrative_ontology:affects_constraint(gig_economy_labor_precarity, algorithmic_management_behavioral_control).

% DUAL FORMULATION NOTE:
% Gig economy labor precarity decomposes into three structurally distinct constraints: (1) wage suppression in task-based labor markets (ε ≈ 0.50, snare from worker perspective), (2) benefit externalization shifting costs to public welfare systems (ε ≈ 0.45, tangled rope), (3) algorithmic management as behavioral control (ε ≈ 0.40, piton with suppression function). Each has different measurement trajectories and different policy exit pathways. This story represents the aggregate constraint; decomposed stories should track each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gig_economy_labor_precarity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
