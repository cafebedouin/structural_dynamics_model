% ============================================================================
% CONSTRAINT STORY: gig_economy_algorithmic_managment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_algorithmic_managment, []).

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
 *   constraint_id: gig_economy_algorithmic_managment
 *   human_readable: Algorithmic Management in the Gig Economy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Algorithmic management in the gig economy creates a structural extraction
 *   mechanism where platforms control task assignment, pricing, performance
 *   metrics, and worker access through opaque computational systems. Workers
 *   cannot inspect, contest, or predict the algorithms that govern their
 *   livelihoods. This constraint exhibits snare characteristics from multiple
 *   perspectives: workers are trapped by economic dependency and information
 *   asymmetry; consumers are unknowingly subsidizing platform extraction
 *   through hidden markups; regulatory agencies lack technical capacity to
 *   audit algorithms; and traditional labor law categories (independent
 *   contractor) have degraded to theater masking coercive control. The
 *   extractiveness score (0.58) reflects that platforms extract both from
 *   workers (suppressed wages, unpredictable earnings) and from consumers
 *   (algorithmic price discrimination). Theater ratio (0.68) reflects that
 *   the platform framing (convenience, flexibility, market pricing) obscures
 *   the true mechanism (centralized algorithmic control, information
 *   asymmetry, absence of meaningful exit). The constraint has intensified
 *   over the interval as platforms accumulated data, refined pricing
 *   algorithms, and normalized surveillance of worker behavior.
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victim (powerless/trapped) — economically dependent, no visibility into task/pricing algorithms, subject to algorithmic deactivation, no collective bargaining capacity
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — control information flows, data, algorithm design, pricing power; capture network effects and scale benefits
 *   - Consumers: Secondary victim (powerless/trapped or unaware/mobile) — pay hidden markups through algorithmic price discrimination, lack transparency on true cost structure
 *   - Labor Organizing Movements: Secondary actor (organized/constrained) — have agency to contest constraint but face technical and legal barriers to algorithm disclosure and worker organizing
 *   - Regulatory Agencies: Institutional actor (institutional/constrained) — mandated to protect labor/consumer welfare but lack technical capacity and political power to audit algorithms, risk regulatory capture
 *   - Traditional Labor Law System: Institutional artifact (institutional/constrained) — independent contractor classification persists through inertia despite degraded functionality in algorithmic context
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_algorithmic_managment, 0.58).
domain_priors:suppression_score(gig_economy_algorithmic_managment, 0.72).
domain_priors:theater_ratio(gig_economy_algorithmic_managment, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_algorithmic_managment, extractiveness, 0.58).
narrative_ontology:constraint_metric(gig_economy_algorithmic_managment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gig_economy_algorithmic_managment, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_algorithmic_managment, snare).
narrative_ontology:human_readable(gig_economy_algorithmic_managment, "Algorithmic Management in the Gig Economy").
narrative_ontology:topic_domain(gig_economy_algorithmic_managment, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_algorithmic_managment, platform_operators).
narrative_ontology:constraint_victim(gig_economy_algorithmic_managment, gig_workers).
narrative_ontology:constraint_victim(gig_economy_algorithmic_managment, consumer_information_asymmetry).
narrative_ontology:constraint_victim(gig_economy_algorithmic_managment, labor_market_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Worker has no visibility into task assignment algorithms, pricing mechanisms, or performance evaluation criteria. No meaningful exit: dependency on platform wages, no collective bargaining power, algorithmic deactivation prevents retaliation. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNAWARE CONSUMER (SNARE) — Consumer bears true cost of platform extraction through hidden markups, while believing they are receiving competitive pricing. Exit mechanism (switching platforms) is illusory when algorithms collude or coordinate. d≈0.88, f(d)≈1.33, σ=0.9 → χ≈0.85.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: LABOR ORGANIZING MOVEMENTS (TANGLED ROPE) — Organizations see both coordination (portable ratings, skill matching) and extraction (wage suppression, algorithmic surveillance). Have agency to contest, build counter-institutions, and demand transparency. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATORS (ROPE) — Experience constraint as pure coordination: algorithm efficiency, matching, and scale. Extraction is secondary to their core interest in network effects and control. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCIES (TANGLED ROPE) — Agencies have mandate to protect labor and consumer welfare (coordination function) but face technical limitations in auditing algorithms and institutional capture by platforms (extraction function). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL LABOR LAW (PITON) — Legal categories (independent contractor vs employee) persist through institutional inertia despite degraded functionality. Platform classification as independent contractor enables extraction while theater (arbitration clauses, terms of service) masks the relationship's true coercive character. theater_ratio=0.68 satisfies piton gate. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational view, algorithmic management exhibits all structural properties of pure extraction: high suppression (opaque algorithms), high extractiveness (wage/price manipulation), victims unable to exit or perceive structure, beneficiary in control of all informational levers. No coordination value reaches workers. d≈0.85, f(d)≈1.24, σ=1.2 → χ≈0.85.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_algorithmic_managment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_algorithmic_managment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_economy_algorithmic_managment, TR),
    TR >= 0.70.

:- end_tests(gig_economy_algorithmic_managment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract through multiple channels: wage suppression (algorithmic task allocation favors speed over fair compensation), consumer price discrimination (surge pricing, location-based markups), and control rent (workers cannot arbitrage platform pricing or build reputation independently). The trajectory from 0.28 to 0.58 reflects increasing sophistication of extraction mechanisms as platforms accumulated user data and computational resources. Not as high as pure monopolistic rent (0.70+) because gig work markets remain contestable at the consumer level (workers can use multiple platforms simultaneously) and new entrants can still enter labor supply. Suppression (0.72): High. Structural barriers prevent meaningful exit: economic desperation drives workers into platforms where algorithmic deactivation creates permanent exclusion threat; consumers cannot easily observe true pricing; regulators lack audit capacity. Information asymmetry is weaponized — workers and consumers operate under false information (belief in flexibility, competitive pricing) while platforms have complete data on system state. Theater ratio (0.68): High. Performative elements include: platform narrative of 'independent contractor flexibility' masking algorithmic control; framing of 'market-based pricing' masking algorithmic collusion potential; arbitration clauses and terms of service theater preventing transparency. The theater has increased as platforms invested more in brand/narrative while extractive mechanisms became more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   Gig workers and powerless consumers perceive pure extraction (snare) with no coordination benefit — they gain efficiency and convenience at the cost of permanent information disadvantage and wage/price suppression. Organized labor sees tangled rope — platforms do provide genuine coordination (matching, scale, portability of basic skills) alongside extraction (surveillance, wage suppression). Regulatory agencies see tangled rope — mandate to protect workers and consumers (coordination) but captured by platform power (extraction). Platform operators see rope — their primary experience is coordination: algorithm matching, network effects, scale efficiency. Extraction is incidental to their model. Piton perspective reveals degraded legal categories: independent contractor status was functional in traditional temp labor (low surveillance, portable skills) but has degraded to pure theater in algorithmic context where platforms exercise complete control over worker schedule and output. Analytical observer sees snare: the system exhibits no genuine coordination benefit that couldn't be achieved through transparent pricing and democratic algorithm design. The coordination function is claimed but not delivered to workers.
 *
 * DIRECTIONALITY LOGIC:
 *   Gig workers: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction minus epsilon for platform's claim of providing work opportunity. Consumers (unaware): Victim + trapped → d≈0.88, f(d)≈1.33. High extraction through price discrimination and hidden costs. Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit any single market while maintaining platform value. Labor organizations: Victim + constrained → d≈0.65, f(d)≈0.95. Have agency to contest but face structural barriers (scale, capital, legal complexity). Regulatory agencies: Mixed (victim of capture + constrained) → d≈0.55, f(d)≈0.75. Mandated to protect but insufficient technical/political power. Traditional labor law: Beneficiary status (enables platform business model) but constrained exit from legal category → d≈0.70, f(d)≈1.08. Piton classification emerges from theater ratio, not from chi value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying what counts as coordination: platform narrative claims efficiency and flexibility as coordination benefits, but structural analysis reveals these are delivered asymmetrically. Workers gain flexibility to multiple-task but lose predictability of earnings; consumers gain convenience but lose price transparency; platforms gain complete information and control. The 'coordination' is unidirectional (from workers/consumers to platform) rather than reciprocal. True coordination would require algorithm transparency, worker input on task design, or consumer visibility into pricing. Without these, the snare classification is confirmed: extraction dressed in coordination language. The analytical observer perspective is identical to worker perspective (both see snare) because algorithmic management exhibits no coordination value that reaches the workers themselves — only claims about coordination directed at consumers and regulators. If platform-claimed coordination (matching efficiency, scale benefits) existed and benefited workers proportionally, the worker perspective would shift toward tangled rope. The fact that it remains snare reveals the coordination claim is theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_opacity_necessity,
    'Is algorithmic opacity inherent to efficient matching, or is it contingent protection of platform profit margins?',
    'Comparative analysis of fully-transparent matching systems vs proprietary platforms; economic modeling of transparency cost vs efficiency loss; jurisdictions with mandatory algorithm disclosure (EU AI Act) vs unrestricted markets',
    'If opacity is necessary: constraint may be mountain (technical limit). If contingent: constraint is pure extraction (snare) and transparency mandates would expose extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_opacity_necessity, empirical, 'Whether algorithmic opacity is structural necessity or contingent protection').

omega_variable(
    worker_skill_mobility,
    'Can gig workers build portable, algorithm-independent skill reputation that transfers across platforms?',
    'Tracking workers who exit one platform for another; measuring reputation portability; comparison with unionized or skilled craft markets with portable credentials',
    'If portable: workers have exit option (mobile), classification shifts from snare to tangled_rope. If platform-dependent: lock-in is structural, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_skill_mobility, empirical, 'Portability of worker reputation across platforms').

omega_variable(
    algorithmic_collusion_feasibility,
    'Are market-wide wage and price suppression coordinated via algorithmic collusion, or is it emergent from independent platform incentives?',
    'Economic data on wage convergence across platforms; technical analysis of algorithm similarity; correlation of wage/price changes within and between platforms; antitrust investigation data',
    'If coordinated: snare classification confirmed + antitrust implications. If emergent: multiple snares (one per platform) rather than single systemic snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_collusion_feasibility, empirical, 'Whether wage suppression is coordinated or emergent from platform competition').

omega_variable(
    consumer_price_awareness,
    'What fraction of consumers perceive hidden platform markups vs algorithmic price discrimination?',
    'Consumer surveys on price transparency; comparison of consumer price expectations vs actual cost structures; analysis of information asymmetry feedback loops',
    'If high awareness: consumer is co-beneficiary (victim classification wrong). If low awareness: consumer extraction is coercive (snare confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_price_awareness, empirical, 'Fraction of consumers aware of hidden pricing mechanisms').

omega_variable(
    alternative_governance_models,
    'Do cooperative, worker-owned, or democratic-algorithmic platforms achieve equivalent efficiency with lower extraction?',
    'Performance metrics (speed, reliability, cost) from cooperative platforms vs Uber/DoorDash; worker retention and wage data; long-term viability analysis',
    'If equivalent: snare classification confirmed — extraction is not necessary for coordination. If cooperative models fail: extraction may be payment for efficiency coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_models, empirical, 'Whether alternative governance models achieve equivalent coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_algorithmic_managment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig_algo_tr_t0, gig_economy_algorithmic_managment, theater_ratio, 0, 0.45).
narrative_ontology:measurement(gig_algo_tr_t5, gig_economy_algorithmic_managment, theater_ratio, 5, 0.58).
narrative_ontology:measurement(gig_algo_tr_t10, gig_economy_algorithmic_managment, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(gig_algo_be_t0, gig_economy_algorithmic_managment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gig_algo_be_t5, gig_economy_algorithmic_managment, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gig_algo_be_t10, gig_economy_algorithmic_managment, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_algorithmic_managment, resource_allocation).
narrative_ontology:affects_constraint(gig_economy_algorithmic_managment, wage_fragmentation_gig_sectors).
narrative_ontology:affects_constraint(gig_economy_algorithmic_managment, platform_regulatory_capture).
narrative_ontology:affects_constraint(gig_economy_algorithmic_managment, algorithmic_unemployment).

% DUAL FORMULATION NOTE:
% Algorithmic management is downstream of platform business model incentives but represents a distinct structural constraint. Upstream constraints include: basic platform concentration (which creates information asymmetry), venture capital ROI expectations (which drive extraction maximization), and legal classification gaps (which allow circumvention of labor law). This story focuses on the algorithmic mechanism itself; upstream stories address business incentives and regulatory failures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gig_economy_algorithmic_managment, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
