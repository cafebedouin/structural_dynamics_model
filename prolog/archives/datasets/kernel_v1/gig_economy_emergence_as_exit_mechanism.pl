% ============================================================================
% CONSTRAINT STORY: gig_economy_emergence_as_exit_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_emergence_as_exit_mechanism, []).

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
 *   constraint_id: gig_economy_emergence_as_exit_mechanism
 *   human_readable: Gig Economy Emergence as Exit Mechanism from Traditional Employment
 *   domain: economic_labor_relations
 *
 * SUMMARY:
 *   The gig economy emerged over the past 15 years as a structural response
 *   to labor market rigidities in traditional employment: credential
 *   requirements, geographic lock-in, scheduling inflexibility, and
 *   benefits-tied-to-employer dependency. It functions simultaneously as a
 *   genuine exit mechanism (workers escape trapped circumstances of
 *   credential gatekeeping and employer-dependent benefits) and as an
 *   extraction mechanism (platforms capture surplus through classification
 *   arbitrage, algorithmic management opacity, and regulatory avoidance).
 *   This dual character makes it a canonical tangled_rope constraint — one
 *   that solves a real coordination problem (matching marginal labor supply
 *   to dispersed demand) while maintaining asymmetric extraction. The
 *   constraint exhibits different classifications from different structural
 *   positions: precarious workers experience snare (trapped in
 *   higher-surveillance, lower-protection environment despite appearing
 *   'choice'); credentialed workers experience tangled_rope (genuine
 *   coordination access plus extraction via commission and opacity);
 *   platforms experience tangled_rope (coordination infrastructure plus
 *   extraction machinery); organized labor sees rope (pure coordination
 *   problem amenable to sectoral bargaining); the traditional employment
 *   standard persists as piton (institutional residue maintained by inertia);
 *   the analytical view risks naturalizing the arrangement as inevitable
 *   economic law (false-summit mountain). The temporal measurements reveal
 *   extraction accumulation: base extractiveness rising from 0.32 to 0.58
 *   over 15 years as algorithmic management mechanisms mature and platform
 *   market concentration increases. Theater ratio remained lower than
 *   traditional employment (0.48 vs ~0.65), reflecting that gig work performs
 *   less institutional legitimacy theater but more algorithmic opacity
 *   theater — different form but comparable aggregate.
 *
 * KEY AGENTS:
 *   - Precarious Gig Workers: Primary victims (powerless/trapped) — majority of gig workforce; lack skills/credentials to access alternative opportunities; experience reduced protections and increased algorithmic surveillance
 *   - Credentialed Gig Workers: Secondary beneficiaries (moderate/constrained) — professional freelancers, skilled contractors; use platforms for flexibility and global market access; experience extraction but retain meaningful agency
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract classification arbitrage and algorithmic management surplus; provide genuine coordination infrastructure; operate at scale to avoid direct worker negotiation
 *   - Organized Worker Coalitions: Secondary actors (organized/mobile) — driver unions, sectoral bargaining initiatives; frame gig economy as coordination problem requiring regulatory resolution; increasing leverage through political channels
 *   - Traditional Employment Regime: Institutional residue (institutional/arbitrage) — Fordist employment relationship persists but is displaced; labor law continues to presume employee status as normative; gig economy defined against this standard
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements (classification status, benefit unbundling) as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_emergence_as_exit_mechanism, 0.58).
domain_priors:suppression_score(gig_economy_emergence_as_exit_mechanism, 0.52).
domain_priors:theater_ratio(gig_economy_emergence_as_exit_mechanism, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_emergence_as_exit_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(gig_economy_emergence_as_exit_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gig_economy_emergence_as_exit_mechanism, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_emergence_as_exit_mechanism, tangled_rope).
narrative_ontology:human_readable(gig_economy_emergence_as_exit_mechanism, "Gig Economy Emergence as Exit Mechanism from Traditional Employment").
narrative_ontology:topic_domain(gig_economy_emergence_as_exit_mechanism, "economic_labor_relations").

domain_priors:requires_active_enforcement(gig_economy_emergence_as_exit_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_emergence_as_exit_mechanism, platform_operators).
narrative_ontology:constraint_beneficiary(gig_economy_emergence_as_exit_mechanism, certain_worker_segments).
narrative_ontology:constraint_victim(gig_economy_emergence_as_exit_mechanism, platform_workers_majority).
narrative_ontology:constraint_victim(gig_economy_emergence_as_exit_mechanism, traditional_employment_standards).
narrative_ontology:constraint_victim(gig_economy_emergence_as_exit_mechanism, labor_protections_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS GIG WORKER (SNARE) — No benefits, algorithmic termination without notice, income volatility, algorithmic wage setting with no negotiation. The exit from traditional employment traps the worker in a structure with fewer protections and greater surveillance. The gig economy appears as a voluntary choice from powerless position, but the alternative (unemployment, credential requirements, geographic immobility) makes it extracted choice. No genuine exit available despite appearance of choice.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREDENTIALED GIG WORKER (TANGLED ROPE) — Professional with specialized skills (software contractor, design freelancer, consulting) who uses gig platforms for genuine flexibility and income supplementation while maintaining portable reputation. Experiences both coordination (access to global market, project matching) and extraction (platform commission, algorithmic opacity, reduced scale benefits). Has constrained exit options due to credential leverage and skill portability, but higher agency than precarious workers.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (TANGLED ROPE) — Benefits from classification arbitrage (workers as contractors, not employees) and network effects. Provides genuine coordination function (matching supply and demand, reducing transaction costs, enabling marginal workers to monetize time). Simultaneously extracts through algorithmic management, dynamic pricing, and regulatory arbitrage. For the platform, the gig economy is a genuinely hybrid mechanism: real coordination value that is systematically undercompensated through structural extraction.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZED WORKER COALITION (ROPE) — Union organizing, driver coalitions, and sectoral bargaining initiatives see the gig economy as a coordination problem requiring collective action to rebuild wage floors and benefits regimes. From this perspective, the mechanism is pure coordination: worker power and platform power are both mobile actors capable of negotiating, and the bottleneck is establishing bargaining frameworks (sectoral standards, minimum earnings guarantees). Low suppression against organized actors with media leverage and political access.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL EMPLOYMENT STANDARD (PITON) — The Fordist employment relationship (stable employer-employee, benefits-bundled, collective bargaining) persists as a residual institutional form. The gig economy has partially displaced it but not eliminated it. The traditional standard now functions as degraded institutional reference — fewer workers hold traditional employment, but the standard remains the default against which gig work is defined as 'alternative.' Theater ratio is high: labor law continues to presume employee status as baseline despite gig proliferation. The institutional form is maintained by inertia and by workers still seeking traditional employment as the preferred outcome.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long historical view, labor market flexibility cycles between periods of worker constraint (capital mobility, credential requirements, geographic lock-in) and periods of worker mobility (labor shortage, credential devaluation, geographic dispersion). The gig economy appears as a natural cyclical response to wage rigidity and capital concentration — inevitable technological manifestation of labor supply seeking outlet. However, this naturalizes what is structurally a contestable institutional arrangement (classification status, algorithmic management, benefits unbundling). The engine will detect this as a false summit, revealing that the 'cycle inevitability' framing obscures the political economy of platform design.
constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_emergence_as_exit_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_emergence_as_exit_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_emergence_as_exit_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_economy_emergence_as_exit_mechanism, TR),
    TR >= 0.70.

:- end_tests(gig_economy_emergence_as_exit_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.58): Moderate-high. The gig economy extracts significant surplus from workers through classification arbitrage (contractor status avoids employer mandates for benefits, overtime, payroll taxes) and algorithmic price-setting (dynamic pricing, surge multipliers, algorithmic assignment). However, extraction is not as severe as pure snare (0.70+) because genuine coordination value exists and some worker segments (credentialed professionals) retain negotiating power. The rise from 0.32 to 0.58 over 15 years reflects maturation of extraction mechanisms — early gig platforms had simpler pricing; contemporary platforms employ sophisticated algorithmic management that was technically unavailable in 2010. Suppression (0.52): Moderate. Barriers to worker power include algorithmic opacity (workers cannot see rules governing assignment, pricing, termination), classification avoidance (contractor status prevents collective bargaining frameworks), geographic distribution (workers isolated from each other), and platform control of reputation (ratings system allows summary termination). However, suppression is not maximal (0.65+) because workers retain ability to exit to other platforms or traditional employment (though at cost), some jurisdictions are successfully regulating (California, UK), and organized worker pressure is mounting. Theater Ratio (0.48): Moderate-low. Gig work involves less institutional legitimacy theater than traditional employment (no elaborate onboarding rituals, no HR departments performing organizational culture, no promotional hierarchies requiring narrative legitimation). However, algorithmic opacity performs its own theater — the pretense that assignment allocation is purely algorithmic and value-neutral obscures the economic extraction. The theater ratio increased slightly from 0.35 to 0.48, reflecting growth of algorithmic legitimation narratives (reviews, ratings, 'community standards'). Claimed Type (Tangled Rope): The constraint requires BOTH genuine coordination (matching dispersed supply and demand, reducing transaction costs, enabling participation at scale) AND asymmetric extraction (classification arbitrage, algorithmic surplus capture, suppression of worker organizing). Neither function alone describes the structure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival variance is substantial and diagnostic. The precarious worker sees snare (trapped exit, no genuine choice, extraction masked as opportunity). The credentialed worker sees tangled_rope (real coordination value with extraction that can be managed through skill leverage). The platform sees tangled_rope (genuine infrastructure with profitable extraction). The organized labor coalition sees rope (pure coordination problem — disagree with platforms about how to divide surplus, not whether coordination function exists). The traditional employment regime sees itself as piton (residual institutional form, maintained through inertia but displaced by gig economy). The analytical observer risks seeing mountain (labor market flexibility as inevitable response to capital mobility, credential devaluation as natural outcome of globalization) — the engine's false-summit detector flags this as naturalization of contestable political economy. The largest gap is between precarious and credentialed workers, suggesting the gig economy should decompose into multiple constraints per the ε-invariance principle: precarious gig work (ε~0.70, Snare) vs credentialed freelancing (ε~0.35, Rope or lower Tangled Rope). The current story uses a blended 0.58 that represents the population average but obscures the within-population heterogeneity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the agent's structural relationship to THIS constraint. Precarious workers: d ≈ 0.92 (full victim, trapped exit, powerless) → f(d) ≈ 1.38 (maximum experienced extraction). Credentialed workers: d ≈ 0.45 (mixed victim-beneficiary, constrained exit, moderate power) → f(d) ≈ 0.55 (moderate experienced extraction). Platform operators: d ≈ 0.15 (beneficiary, arbitrage exit, institutional power) → f(d) ≈ -0.02 (negative experienced extraction; they benefit). Organized labor: d ≈ 0.55 (victim of classification arbitrage, mobile exit via organizing, organized power) → f(d) ≈ 0.75 (experiencing substantial extraction but with coalition capacity to counteract). Traditional employment: d ≈ 0.20 (residual beneficiary of contrast, arbitrage exit as institutional form) → f(d) ≈ 0.02. Analytical observer: d ≈ 0.72 (observer position, analytical exit) → f(d) ≈ 1.15 (standard analytical f(d)). Scope modifier σ(S): Global scope (1.2 multiplier) reflects that gig platforms operate globally and their extraction mechanisms are standardized across jurisdictions — the bottleneck is not local but planetary scale. χ = ε × f(d) × σ(S) produces: precarious worker χ ≈ 0.58 × 1.38 × 1.2 ≈ 0.96 (maximum extractiveness); credentialed χ ≈ 0.58 × 0.55 × 1.2 ≈ 0.38; platform χ ≈ 0.58 × (-0.02) × 1.2 ≈ -0.01 (they benefit); organized χ ≈ 0.58 × 0.75 × 1.2 ≈ 0.52; traditional χ ≈ 0.58 × 0.02 × 1.2 ≈ 0.01. The precarious worker perspectival gap is the constraint's most diagnostic feature.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy Resolution (ε = 0.58 > 0.46): This constraint is NOT automatically false. It is correctly classified as tangled_rope because BOTH genuine coordination (matching function) and asymmetric extraction (classification arbitrage, algorithmic surplus capture, organized suppression) are structurally required for the platform model. A platform that provided matching without extraction would be a public good provider (Rope or Scaffold), not a commercial system. A platform that provided extraction without coordination would be pure rent-seeking (Snare). The tangled_rope classification reflects that contemporary gig platforms REQUIRE both functions to operate — the coordination function subsidizes platform legitimacy (workers benefit from matching despite extraction), and the extraction function funds platform operation and profit. The mandatrophy that could arise: 'is this really coordination or just extraction theater?' — answered by examining whether workers would choose to use a platform if extraction were removed (coordination alone). Evidence from non-profit and cooperative platforms (Stocksy, Fairbnb, Platform Coop movement) suggests genuine coordination value exists and would survive extraction removal, confirming the tangled_rope classification. However, commercial platforms do not extract this surplus; they accept lower adoption. The mandatrophy is empirically resolvable: compare platform usage, worker benefits, and market dynamics across profit-extraction vs profit-free models in the same domain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_vs_trap_boundary,
    'At what point does an exit mechanism from traditional employment constraints become a new extraction mechanism?',
    'Longitudinal wage and benefit comparison: gig workers vs traditional workers cohort-controlled; analysis of worker transitions (how many return to traditional employment vs. remain in gig); survey of worker preference when given equal-compensation alternatives',
    'If majority of gig workers have genuine preference and higher net welfare: classification shifts toward Rope (coordination). If majority prefer traditional employment but lack access: classification shifts toward Snare (extraction). Current evidence is mixed — suggests decomposition into distinct worker segments with different exit experiences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_vs_trap_boundary, empirical, 'Whether gig economy is exit mechanism or extraction trap').

omega_variable(
    classification_arbitrage_sustainability,
    'Is the contractor classification economically stable as a regulatory category, or is it contingent on legal/political contestation?',
    'Jurisdiction-by-jurisdiction analysis of regulatory outcomes (California AB5, UK courts, EU Platform Work Directive); correlation between classification outcomes and platform profitability metrics; analysis of what wage/benefit levels are consistent with different classification regimes',
    'If classification is unstable: the platform extraction mechanism is contingent on regulatory capture and will degrade as regulation shifts. The tangled_rope classification is accurate — tension between genuine coordination and extraction surfaces as classification becomes enforceable. If stable: extraction mechanism is entrenched and classification may shift toward Snare for majority workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_arbitrage_sustainability, empirical, 'Whether contractor classification is stable or politically contestable').

omega_variable(
    segmentation_irreducibility,
    'Is the gig workforce a single constraint or multiple structurally distinct constraints decomposed into worker segments?',
    'Variance analysis: within-segment extractiveness vs between-segment extractiveness; cluster analysis of worker experience by skill, income, platform type, and geography; identification of natural segmentation boundaries where classification type changes',
    'If single constraint: current tangled_rope classification is appropriate across perspectives. If decomposable: separate stories for precarious workers (likely Snare), credentialed freelancers (likely Rope or Tangled Rope), professional contractors (likely Rope), and organized sectors (Tangled Rope toward Rope). The ε-invariance principle suggests decomposition is correct — extractiveness varies substantially across segments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(segmentation_irreducibility, empirical, 'Whether gig economy is single constraint or multiple constraints by worker segment').

omega_variable(
    algorithmic_suppression_mechanism,
    'Does algorithmic management (assignment allocation, rating systems, dynamic pricing) constitute suppression in the same structural sense as legal prohibition or economic dependency?',
    'Qualitative analysis of worker agency: can workers see the rules governing algorithmic decisions? Can they organize around algorithmic rules? Comparison to other opaque systems (state surveillance, corporate hierarchy); analysis of whether algorithmic opacity persists if workers collectively demand transparency vs. whether opacity is essential to the platform model',
    'If algorithmic opacity is essential to extraction: suppression is structural and irreplaceable; classification remains Snare/Tangled Rope with high suppression. If algorithmic opacity is contingent (transparency could be achieved with cost): suppression is lower than measured and extraction mechanism is more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_suppression_mechanism, conceptual, 'Whether algorithmic management constitutes structural suppression').

omega_variable(
    natural_law_false_summit_diagnosis,
    'Is labor market flexibility an inevitable economic law, or a contestable institutional arrangement shaped by policy and power?',
    'Historical comparison: how similar are labor cycles across jurisdictions with different regulations? If regulatory differences produce different outcomes, flexibility is not inevitable but contingent. Analysis of platform design choices: what would different algorithmic rules, classification regimes, or benefit structures look like? If alternatives are technically feasible but chosen against, the ''natural'' framing is false.',
    'If labor cycles are inevitable: mountain classification is correct and false-summit detection is a misclassification. If cycles are contingent: the analytical observer is naturalizing a political economy choice, and FSM reclassification is diagnostically appropriate. Current evidence suggests contingency (regulatory experiments show different outcomes), supporting FSM diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_false_summit_diagnosis, conceptual, 'Whether labor flexibility is natural law or political economy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_emergence_as_exit_mechanism, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig_exit_theater_t0, gig_economy_emergence_as_exit_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gig_exit_theater_t7, gig_economy_emergence_as_exit_mechanism, theater_ratio, 7, 0.45).
narrative_ontology:measurement(gig_exit_theater_t15, gig_economy_emergence_as_exit_mechanism, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(gig_exit_extractiveness_t0, gig_economy_emergence_as_exit_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gig_exit_extractiveness_t7, gig_economy_emergence_as_exit_mechanism, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(gig_exit_extractiveness_t15, gig_economy_emergence_as_exit_mechanism, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gig_exit_suppression_t0, gig_economy_emergence_as_exit_mechanism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gig_exit_suppression_t7, gig_economy_emergence_as_exit_mechanism, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(gig_exit_suppression_t15, gig_economy_emergence_as_exit_mechanism, suppression_requirement, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_emergence_as_exit_mechanism, resource_allocation).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, traditional_employment_credential_gatekeeping).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, benefits_bundling_dependency).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, platform_classification_regulatory_arbitrage).
narrative_ontology:affects_constraint(gig_economy_emergence_as_exit_mechanism, algorithmic_management_opacity).

% DUAL FORMULATION NOTE:
% The gig economy constraint should potentially decompose into multiple constraint stories per the ε-invariance principle: (1) Precarious Gig Work — ε ≈ 0.70, Snare-spectrum, extraction mechanism dominant. (2) Credentialed Gig Freelancing — ε ≈ 0.30, Rope-spectrum, coordination mechanism dominant. (3) Platform Classification Arbitrage — ε ≈ 0.65, Snare, pure extraction mechanism. (4) Algorithmic Wage-Setting — ε ≈ 0.55, Tangled Rope, mixed coordination and extraction. The current story uses a blended 0.58 representing the population average, but decomposition would provide finer-grained analysis. The affects_constraints array links this story to its upstream constraints (credential gatekeeping, benefits dependency) that gig economy functions as exit from, and to its downstream constraints (regulatory arbitrage, algorithmic opacity) that gig economy enables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gig_economy_emergence_as_exit_mechanism, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
