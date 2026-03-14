% ============================================================================
% CONSTRAINT STORY: algorithmic_labor_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_labor_control, []).

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
 *   constraint_id: algorithmic_labor_control
 *   human_readable: Algorithmic Labor Control in Platform-Mediated Work
 *   domain: labor_economics/platform_governance
 *
 * SUMMARY:
 *   Algorithmic labor control in platform-mediated work (ride-sharing,
 *   delivery, task-based gig markets) creates a structural extraction
 *   mechanism that combines genuine coordination functions with asymmetric
 *   control and opacity. The constraint classifies as Tangled Rope at the
 *   system level — platforms solve real coordination problems (matching
 *   workers to tasks, dynamic pricing, reputation feedback) while
 *   simultaneously extracting value through algorithmic opacity, unilateral
 *   termination, and suppressed wage competition. The measurement data shows
 *   extractiveness increasing from 0.38 to 0.62 over the interval, with
 *   rising opacity (theater ratio 0.35 to 0.55) suggesting that as markets
 *   mature, platforms intensify algorithmic control rather than relax it.
 *   Theater increase indicates that regulatory theater is growing (worker
 *   protections at legal/rhetorical level) while actual control intensifies
 *   (algorithmic opacity, deactivation threat). The constraint exhibits all
 *   eight indexed perspectives, revealing how the same structural phenomenon
 *   appears as immutable natural law to some observers, a degraded
 *   institutional fiction to others, a temporary coordination problem with
 *   visible sunset mechanisms to still others, and pure extraction to trapped
 *   workers.
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victims (powerless/trapped) — bear extraction through wage suppression, schedule volatility, deactivation risk; no appeal mechanism for algorithmic decisions
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture first-mover rents, technology arbitrage, regulatory arbitrage across jurisdictions; experience system as coordination
 *   - Organized Labor Groups: Secondary victims (moderate/constrained) — increasingly capable of coordination despite fragmentation; face suppression but building organizing capacity
 *   - Regulatory Agencies: Captured institutional actors (institutional/constrained) — face genuine coordination problems but constrained by platform mobility threat and industry lobbying
 *   - Regulatory Reform Coalitions: Organized reformers (organized/constrained) — unions, NGOs, sympathetic legislators building sunset mechanisms (employment classification, algorithmic transparency, portable reputation)
 *   - Labor Market Epistemic Commons: Victim abstract good (powerless/trapped) — cannot organize; bears cost of algorithmic opacity and wage-discovery degradation
 *   - Independent Contractor Fiction: Institutional performance (institutional/arbitrage) — maintains control through legal classification despite structural contradictions; degraded piton maintained by lobbying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_labor_control, 0.62).
domain_priors:suppression_score(algorithmic_labor_control, 0.68).
domain_priors:theater_ratio(algorithmic_labor_control, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_labor_control, extractiveness, 0.62).
narrative_ontology:constraint_metric(algorithmic_labor_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_labor_control, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_labor_control, tangled_rope).
narrative_ontology:human_readable(algorithmic_labor_control, "Algorithmic Labor Control in Platform-Mediated Work").
narrative_ontology:topic_domain(algorithmic_labor_control, "labor_economics/platform_governance").

domain_priors:requires_active_enforcement(algorithmic_labor_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_labor_control, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_labor_control, capital_holders).
narrative_ontology:constraint_victim(algorithmic_labor_control, gig_workers).
narrative_ontology:constraint_victim(algorithmic_labor_control, labor_market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER UNDER ALGORITHMIC CONTROL (SNARE) — Trapped by economic dependency and algorithmic opacity. Cannot exit without abandoning primary income source. Algorithm determines work availability, compensation, and termination with no transparent rules or appeal mechanism. Suppression is structural: no collective bargaining, no employment law protection, deactivation as threat. Maximum extraction from powerless, trapped position.
constraint_indexing:constraint_classification(algorithmic_labor_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED LABOR GROUPS (TANGLED ROPE) — Constrained by platform switching costs and alternative employment scarcity, but increasingly organized through social media and union organizing. Experience both genuine coordination benefits (task matching, flexible scheduling, income access) and asymmetric extraction (surge pricing volatility, commission structures, deactivation risk). Active enforcement of algorithmic control mechanisms coexists with coordination functions. Medium effective extraction from constrained, moderate-power position.
constraint_indexing:constraint_classification(algorithmic_labor_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the algorithmic control system as coordination mechanism: matching workers to tasks, pricing equilibrium, reputation systems. Net beneficiary with multiple exit options (market diversification, technology transitions, regulatory arbitrage across jurisdictions). Sees their system as solving coordination problems, not extracting. Low or negative effective extraction from institutional, arbitrage position.
constraint_indexing:constraint_classification(algorithmic_labor_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REFORM COALITIONS (SCAFFOLD) — Organized actors (labor unions, NGOs, sympathetic legislators) see algorithmic control as a temporary institutional failure with a documented sunset: minimum employment classification, algorithmic transparency mandates, collective bargaining rights, portable reputation systems. These reforms are actively being implemented (EU Platform Work Directive, California Prop 22 debates, NYC algorithmic impact assessments). Theater is moderate because much regulation remains performative, but substantive protections are accumulating. Exit path is real and visible.
constraint_indexing:constraint_classification(algorithmic_labor_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INDEPENDENT CONTRACTOR STATUS (PITON) — The legal classification of gig workers as independent contractors is substantially performative. The classification persists through institutional inertia and regulatory arbitrage despite contradicting structural realities: algorithmic control, termination at will, acceptance of task-specific terms that negate genuine independence. Courts are increasingly finding this fiction degraded (UK Supreme Court Uber cases, French court worker reclassifications). The classification maintains itself through lobbying and jurisdiction shopping, not through genuine legal coherence. Theater ratio high because the fiction requires continuous performance.
constraint_indexing:constraint_classification(algorithmic_labor_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AGENCIES (TANGLED ROPE) — Regulators face genuine coordination problems (labor standards development, cross-border enforcement, avoiding capital flight) but are also captured by industry lobbying and asymmetric information. Constrained by the threat of industry relocation and the complexity of regulating global platforms. Both enforce algorithmic control protection (through non-interference) and develop worker protections (through slowly advancing standards). Extraction runs toward the industry; regulation is the coordination function that persists in hybrid form.
constraint_indexing:constraint_classification(algorithmic_labor_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LABOR MARKET TRANSPARENCY (SNARE) — Abstract collective good (price discovery, wage benchmarking, skill-demand signals) that cannot organize or exit. Algorithmic opacity systematically degrades market information. No wage transparency, no algorithmic decision criteria visibility, no aggregated productivity metrics. This prevents both workers and external analysts from understanding true compensation and labor conditions. The epistemic commons bears full cost of information asymmetry; no self-correction mechanism.
constraint_indexing:constraint_classification(algorithmic_labor_control, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: NATURALIZATION AS INEVITABLE (MOUNTAIN) — From a naive civilizational/universal view, algorithmic labor control appears to be an inevitable feature of scale: matching many workers to many tasks requires automation and algorithmic coordination. This perspective risks treating contingent institutional choices (opaque algorithms, unilateral termination, wage-setting opacity) as inherent to any large-scale labor matching system. The engine will identify this as a false summit — the structural data reveals that transparency and worker protections are compatible with efficient task matching (see Perspective 4 regulatory implementations).
constraint_indexing:constraint_classification(algorithmic_labor_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_labor_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_labor_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_labor_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_labor_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_labor_control, TR),
    TR >= 0.70.

:- end_tests(algorithmic_labor_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint combines genuine coordination value (task matching saves time and search costs; dynamic pricing reflects supply-demand; reputation feedback incentivizes quality) with substantial extraction (wage suppression through opacity, schedule volatility through algorithmic control, termination threat through at-will deactivation). The increase from 0.38 to 0.62 over the interval reflects that as platforms mature and market share consolidates, the coordination value per worker decreases (fewer alternative platforms, more dependent workers) while extraction intensity increases (algorithmic sophistication, data moats). Suppression (0.68): High. Multiple suppression mechanisms: no collective bargaining, independent contractor status denies employment law protections, algorithmic opacity prevents wage benchmarking, deactivation threat suppresses organizing, switching costs trap workers on platform. No singular barrier but multiple reinforcing mechanisms. Theater ratio (0.55): Moderate. Platforms perform substantial coordination function (genuine task matching, real reputation feedback) but increasingly hide extraction mechanisms behind algorithmic opacity. Regulatory theater is rising (worker protection laws on books) but compliance is performative. Theater increase suggests constraint shifting from Rope (genuine coordination, low theater) toward Piton (degraded fake classification) or intensified Snare (theater masking extraction).
 *
 * PERSPECTIVAL GAP:
 *   The perspectives show maximum divergence. Platform operators (Rope) see genuine coordination that makes workers better off on net — true from the perspective of a worker with no alternatives (gig work provides income they wouldn't have otherwise). Trapped workers (Snare) see pure extraction through algorithmic control and termination threat — true from the perspective of those with highest dependency. Regulatory agencies (Tangled Rope) simultaneously enforce the extraction (non-interference) and build worker protection (slowly advancing standards) — structurally accurate. Reform coalitions (Scaffold) see real sunset mechanisms emerging through regulation and organizing — real empirically but faces captured regulator and piton fiction obstruction. The independent contractor fiction (Piton) is performative theater — legally maintained through lobbying despite structural contradictions courts are increasingly finding (UK Uber ruling). Labor market transparency (Snare) cannot organize or exit — systematic epistemic degradation with no self-correction. The naturalization perspective (Mountain) risks treating contingent design choices (opacity, unilateral termination) as inherent to scale — false because comparable platforms exist with higher transparency and would-be worker protections.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural relationship to extraction flow. Platform operators as beneficiaries with arbitrage exit (multiple markets, technology switching, regulatory arbitrage) derive low d ≈ 0.10 → negative χ. Gig workers as victims with trapped exit (economic dependency, no employment alternatives) derive high d ≈ 0.92 → high f(d) ≈ 1.38 → high χ. Organized labor groups as victims with constrained exit (switching costs, fragmentation) derive d ≈ 0.70 → f(d) ≈ 0.95 → moderate χ. Regulatory agencies as captured institutional actors constrained by platform mobility threat derive d ≈ 0.55 → f(d) ≈ 0.75. The captured regulator perspective shows that institutions not inherently benign — their structural position can make them enforcer of extraction even when their nominal role is protection. Reform coalitions as organized agents with exit paths (regulatory change, organizing growth) derive d ≈ 0.35 → f(d) ≈ 0.35 → low-moderate χ. The epistemic commons cannot exit or organize, deriving d ≈ 0.95 → high f(d), yet is an abstract good not an agent, so directionality framework applies structurally but the victim cannot exercise agency to reduce extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL RESOLUTION: The mandatrophy (coordinate extraction vs pure extraction) is resolved by decomposing by agent perspective and time horizon. At the platform operator level, the constraint genuinely coordinates matching and pricing — objectively true, high-value function. At the worker level at immediate time horizon, the constraint extracts through algorithmic opacity and termination threat — also objectively true. At the regulatory level, the constraint both enables (non-interference) and constrains (slow standard-setting) — simultaneously both. The Tangled Rope classification at system level is appropriate because the constraint simultaneously solves a genuine problem (task matching) and extracts asymmetrically (wage suppression). The mandatrophy does not disappear but becomes perspectival: does the coordination value justify the extraction cost? This is a value judgment (preference type), not a factual ambiguity. Empirically: at immediate/biographical time horizons and from worker perspective, extraction dominates; at institutional/generational horizons and from platform perspective, coordination dominates. The measurement trajectory (extractiveness rising, theater rising) suggests extraction is intensifying relative to coordination value — the constraint is drifting toward Snare (Goodhart drift where optimization of the extraction mechanism degrades the coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_necessity,
    'Is algorithmic opacity necessary for efficient task matching or a chosen design to prevent worker coordination?',
    'Comparative analysis of platforms with varying transparency: proprietary vs open-source matching algorithms, platforms with vs without algorithm explainability. Measurement of task-match efficiency under different transparency regimes.',
    'If opacity necessary: efficiency-extraction tradeoff is real (Tangled Rope justified). If opacity chosen: constraint shifts toward pure extraction (Snare dominant), and transparency requirement becomes feasible sunset mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_necessity, empirical, 'Whether algorithmic opacity is technically necessary or strategically chosen').

omega_variable(
    exit_option_ambiguity,
    'Are gig workers genuinely trapped or constrained by high but surmountable costs? Does this distinction matter for classification?',
    'Worker survey data on income replacement alternatives, switching costs, employment options outside platform work. Longitudinal tracking of worker persistence rates and exit trajectories.',
    'If genuinely trapped: classification as Mountain from worker perspective (permanent constraint). If constrained: classification as Snare or Tangled Rope (high cost but theoretically escapable). If identity_locked: psychological identification with gig work status prevents exit despite mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_ambiguity, empirical, 'Whether worker exit options are trapped or identity_locked vs constrained').

omega_variable(
    coordination_function_authenticity,
    'Does algorithmic labor control perform genuine coordination (task matching, price discovery) or is coordination a cover story for extraction?',
    'Comparison of coordination efficiency under algorithms vs human dispatching vs auction mechanisms. Analysis of whether algorithmic rules serve matching quality or extraction optimization.',
    'If coordination authentic: Rope or Tangled Rope (beneficiary perspective justified). If coordination is cover: Snare dominant (pure extraction posing as matching). Determines whether constraint can credibly move to Scaffold form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_authenticity, empirical, 'Whether algorithmic control serves coordination or masks extraction').

omega_variable(
    regulatory_capture_depth,
    'How deeply captured are labor regulatory agencies by platform capital? Is reform trajectory real or performative?',
    'Analysis of regulatory outcomes: actual enforcement action, industry compliance rates, worker protection implementation. Tracking of lobbying expenditure vs regulatory change.',
    'If deeply captured: Regulatory perspective is Piton (performative theater) not Scaffold. Sunset clause is aspirational not structural. Reform trajectory is less real than Perspective 4 suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Depth of regulatory capture and credibility of reform trajectory').

omega_variable(
    platform_switching_costs,
    'Can workers credibly use multiple platforms simultaneously or do switching costs force de facto single-platform binding?',
    'Worker income data: percentage earning across 2+ platforms, platform concentration by worker, switching frequency, transaction costs of multi-platform work.',
    'If switching feasible: exit options are constrained (not trapped) or mobile. If switching prohibitively costly: exit is genuinely trapped, strengthening Snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_switching_costs, empirical, 'Whether workers can use multiple platforms or face de facto single-platform binding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_labor_control, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alc_tr_t0, algorithmic_labor_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(alc_tr_t5, algorithmic_labor_control, theater_ratio, 5, 0.48).
narrative_ontology:measurement(alc_tr_t10, algorithmic_labor_control, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(alc_be_t0, algorithmic_labor_control, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(alc_be_t5, algorithmic_labor_control, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(alc_be_t10, algorithmic_labor_control, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_labor_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(algorithmic_labor_control, 0.12).
narrative_ontology:affects_constraint(algorithmic_labor_control, wage_discovery_opacity).
narrative_ontology:affects_constraint(algorithmic_labor_control, employment_classification_fiction).
narrative_ontology:affects_constraint(algorithmic_labor_control, platform_regulatory_arbitrage).

% DUAL FORMULATION NOTE:
% Algorithmic labor control decomposes into three downstream constraints with different ε values: wage_discovery_opacity (ε=0.51, Snare) focuses on epistemic degradation; employment_classification_fiction (ε=0.48, Piton) focuses on legal theater; platform_regulatory_arbitrage (ε=0.55, Tangled Rope) focuses on multi-jurisdiction extraction. Each is a distinct constraint with its own beneficiary/victim declarations and temporal trajectory. Linked by network affects structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_labor_control, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
