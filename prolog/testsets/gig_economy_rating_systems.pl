% ============================================================================
% CONSTRAINT STORY: gig_economy_rating_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_rating_systems, []).

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
 *   constraint_id: gig_economy_rating_systems
 *   human_readable: Gig Economy Rating Systems as Extraction and Coordination Mechanism
 *   domain: economic/labor/platform_governance
 *
 * SUMMARY:
 *   Gig economy rating systems present a structural exemplar of tangled_rope
 *   classification: they coordinate information flows necessary for trust
 *   between anonymous parties (genuine coordination function) while
 *   simultaneously extracting behavioral control, pricing power, and
 *   surveillance data from workers asymmetrically. The rating system is not
 *   neutral infrastructure — it is actively enforced through algorithmic
 *   deactivation, selective work assignment, and opaque penalty systems.
 *   Workers experience this constraint radically differently based on their
 *   position: elite contractors (4.9+ rating) experience rope (pure
 *   coordination with arbitrage options); mid-reputation workers experience
 *   tangled_rope (mixed coordination and extraction); deactivated workers
 *   experience snare (pure extraction with no exit). The constraint's
 *   theater_ratio (0.55) reflects that platforms present the rating system as
 *   meritocratic quality assurance while simultaneously using ratings to
 *   control wages, eliminate competition for premium work, and suppress labor
 *   organizing (algorithmic silence). The increasing extractiveness
 *   trajectory (0.35 → 0.58 over 6 years) shows that as gig platforms mature
 *   and consolidate market power, the extraction function intensifies while
 *   the coordination function remains constant — the system becomes
 *   progressively more about controlling the labor supply than about matching
 *   supply to demand.
 *
 * KEY AGENTS:
 *   - Deactivated Workers (powerless/trapped): Primary victims experiencing snare-level extraction; no algorithmic appeals process with teeth; income revoked through opaque algorithm
 *   - Mid-Reputation Workers (moderate/constrained): Secondary victims experiencing tangled_rope; benefits from platform matching but constrained by wage negotiation pressure and algorithmic invisibility; can switch platforms but face reputation reset
 *   - Elite Contractors (powerful/arbitrage): Primary beneficiaries experiencing rope; high ratings attract premium work and repeat customers; portable reputation enables arbitrage across platforms
 *   - Platform Operators (institutional/arbitrage): Extractors and coordinators; control algorithm design, rating formula, and deactivation criteria; net beneficiary from asymmetric information and behavioral control
 *   - Regulatory Framework (institutional/constrained): Secondary institutional actor; labor/consumer protection rules designed for traditional employment; escape meaningful constraint through contractor classification
 *   - Analytical Observer (analytical/analytical): Full system view revealing tangled_rope structure; sees both genuine coordination and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_rating_systems, 0.58).
domain_priors:suppression_score(gig_economy_rating_systems, 0.65).
domain_priors:theater_ratio(gig_economy_rating_systems, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_rating_systems, extractiveness, 0.58).
narrative_ontology:constraint_metric(gig_economy_rating_systems, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gig_economy_rating_systems, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_rating_systems, tangled_rope).
narrative_ontology:human_readable(gig_economy_rating_systems, "Gig Economy Rating Systems as Extraction and Coordination Mechanism").
narrative_ontology:topic_domain(gig_economy_rating_systems, "economic/labor/platform_governance").

domain_priors:requires_active_enforcement(gig_economy_rating_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_rating_systems, platform_operators).
narrative_ontology:constraint_beneficiary(gig_economy_rating_systems, high_reputation_workers).
narrative_ontology:constraint_victim(gig_economy_rating_systems, low_income_gig_workers).
narrative_ontology:constraint_victim(gig_economy_rating_systems, workers_new_to_platform).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEACTIVATED WORKER (SNARE) — A worker with sub-4.5 rating facing algorithmic deactivation has no exit. Their primary income source is revoked through opaque algorithm. No appeals process with meaningful teeth. Suppression is structural: dependence on platform income, inability to rebuild reputation elsewhere, legal status as independent contractor preventing labor protections. Maximum extraction experienced — worker bears full cost of rating system while platform captures control flow and engagement data.
constraint_indexing:constraint_classification(gig_economy_rating_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-REPUTATION WORKER (TANGLED ROPE) — Worker with 4.6-4.8 rating experiences genuine coordination (matching to appropriate jobs, quality assurance) alongside extraction (ratings weaponized against wage negotiation, cherry-picking of work, algorithmic invisibility). Constrained exit: can move to other platforms but faces reputation reset and income loss during transition. Both coordination function and asymmetric extraction are structural.
constraint_indexing:constraint_classification(gig_economy_rating_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ELITE CONTRACTOR (ROPE) — Worker with 4.9+ rating experiences the system as pure coordination. Ratings attract premium work, repeat customers, and algorithmic priority. Can arbitrage across platforms (their reputation is portable through reviews/portfolio). For this agent, the rating system is low-extraction coordination — it solves matching problems without coercive overhead.
constraint_indexing:constraint_classification(gig_economy_rating_systems, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (TANGLED ROPE) — Platform genuinely coordinates worker-customer matching and quality assurance (coordination function). Simultaneously extracts control over worker autonomy, pricing power, and engagement metrics. Active enforcement required: algorithmic deactivation, rating manipulation, opaque penalty systems. Net beneficiary but structure contains genuine coordination alongside asymmetric control.
constraint_indexing:constraint_classification(gig_economy_rating_systems, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Labor and consumer protection regulations designed for traditional employment or clear merchant relationships. Rating systems escape meaningful regulation through contractor classification (not employees) and platform-as-neutral-marketplace framing. The regulatory structure persists (theater_ratio 0.55) without effectively constraining platform power. Degraded institutional response to structural extraction.
constraint_indexing:constraint_classification(gig_economy_rating_systems, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, rating systems coordinate information flows necessary for trust in anonymous transactions. They also extract behavioral control, pricing power, and surveillance data from workers at asymmetric rates. The constraint is neither pure coordination nor pure extraction but an entangled hybrid. Mandatrophy is resolved by the tangled_rope classification: genuine coordination + asymmetric extraction + active enforcement.
constraint_indexing:constraint_classification(gig_economy_rating_systems, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_rating_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_rating_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_rating_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_rating_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gig_economy_rating_systems, TR),
    TR >= 0.70.

:- end_tests(gig_economy_rating_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract pricing power (by crushing wage negotiation through surplus labor), behavioral control (through algorithmic work assignment), engagement data (building profiles for predictive labor scheduling), and reputational leverage (ratings weaponized against organizing). However, extractiveness is not maximal (not 0.72+) because the coordination function is real — matching workers to jobs is genuinely difficult without information aggregation. The measured value reflects that extraction is layered onto real coordination, not replacement of it. Suppression (0.65): High. Structural barriers include income dependence on platform, inability to rebuild reputation after deactivation, contractor status preventing labor protections, algorithmic opacity in work assignment and rating decisions, and network effects creating switching costs. Psychological suppression is also high — worker internalization of meritocratic framing despite evidence of rating manipulation. Theater ratio (0.55): Moderate. Platforms present ratings as objective quality metrics while simultaneously using them as wage control mechanism. Appeals processes exist (theater) but have low success rates (functional emptiness). Regulatory discourse emphasizes platform neutrality (theater) while platforms actively enforce extraction (reality). The theater is neither dominant nor negligible — it's substantial enough to maintain legitimacy without obscuring the extraction entirely.
 *
 * PERSPECTIVAL GAP:
 *   The range of classifications (snare, tangled_rope, rope, piton) from a single constraint reveals segmented extraction: the platform extracts differentially based on worker tier. Elite contractors see rope because their bargaining power is sufficient to make the constraint coordination-dominant. Deactivated workers see snare because their barriers to exit are absolute. Mid-reputation workers see tangled_rope because they experience both genuine matching and wage suppression. The regulatory piton is aspirational (rules exist) but non-functional (contractor status defeats them). The mandatrophy is resolved: this is definitively tangled_rope from the analytical perspective (genuine coordination + asymmetric extraction + active enforcement), but the experienced constraint varies radically across worker categories. The system's legitimacy depends on elite contractors seeing rope while the machinery of extraction concentrates on those with least exit power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from beneficiary/victim declarations and exit options. Deactivated workers are victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced χ. Mid-reputation workers are victims with constrained exit → d ≈ 0.70 → f(d) ≈ 1.05 → moderate-high χ. Elite contractors are beneficiaries with arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.05 → negative or near-zero χ (they experience coordination, not extraction). Platforms are beneficiaries with arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 → institutional beneficiary position. The directionality divergence (0.95 for trapped victims vs 0.05 for institutional beneficiary) produces the perspectival gap: the same constraint with vastly different effective extraction depending on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint has (1) genuine coordination function: matching workers to jobs, aggregating information, enabling trust; (2) asymmetric extraction: platform captures wage negotiation power, engagement data, behavioral control, reputational leverage; (3) active enforcement: algorithmic deactivation, selective work assignment, appeal theater. All three gates are satisfied. The mandatrophy (Rope vs Snare confusion) is resolved by identifying that the coordination is real but the extraction is asymmetric — platforms would face matching costs without rating systems, but those costs are paid by workers through wage suppression and surveillance, not by platforms. The system could coordinate without extraction (wages reflect full value, ratings are transparent, appeals are effective), so the extraction is not inherent to coordination — it is engineered. Tangled_rope classification is correct because both functions operate simultaneously and neither is residual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reputation_portability,
    'How portable is a worker''s reputation across platforms? Does high rating on one platform transfer to reputation advantage on another?',
    'Empirical measurement of worker platform switching patterns and wage/work-quality outcomes after switching. Compare workers who maintain high reputation across multiple platforms vs those starting fresh on new platforms.',
    'If highly portable: exit options upgrade from constrained to mobile for mid-reputation workers; snare classification shifts toward tangled_rope. If not portable: exit barrier is even higher than measured; snare classification is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputation_portability, empirical, 'Whether worker reputation is portable across platforms').

omega_variable(
    algorithmic_rating_manipulation,
    'Do platforms systematically inflate or deflate ratings for workers based on labor cost, not actual quality? Does the rating metric measure worker behavior or platform preference?',
    'Audit studies comparing rating distributions for identical work quality across price points and worker categories. Analysis of rating formula changes and their correlation with platform profitability and worker costs.',
    'If ratings are manipulated: extractiveness increases to 0.68+, and tangled_rope becomes snare for all but elite workers. If ratings are honest: extractiveness decreases to 0.45, classification shifts toward rope for mid-reputation workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_rating_manipulation, empirical, 'Whether platforms manipulate ratings for labor cost control').

omega_variable(
    appeal_mechanism_effectiveness,
    'How often do workers successfully appeal unfair rating-based deactivations? What is the actual due process available?',
    'Analysis of appeal success rates by worker category. Qualitative review of appeal decisions. Comparison to independent evaluation of disputed work quality.',
    'If appeals are effective (>40% success rate): suppression decreases to 0.50, classification shifts toward constrained/mobile. If appeals are theater (0-10% success): suppression increases to 0.75, classification becomes snare for all workers below elite status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appeal_mechanism_effectiveness, empirical, 'Whether rating-based deactivation appeals are genuinely effective').

omega_variable(
    identity_lock_mechanism,
    'Do workers internalize rating-based behavioral control (identity_locked) or experience it as external constraint? Do workers believe the rating system is meritocratic even when evidence suggests manipulation?',
    'Qualitative interviews with workers about rating perceptions. Analysis of worker advocacy framing: do they blame the system or themselves for low ratings? Measurement of belief persistence after platform-failure disclosure.',
    'If identity_locked: some workers classified as identity_locked rather than trapped, producing rope classification from biographical perspective despite snare-level structural conditions. Reveals cognitive capture mechanism underlying suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether workers experience ratings as internalized (identity-locked) or external (constrained/trapped)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_rating_systems, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig_rating_tr_t0, gig_economy_rating_systems, theater_ratio, 0, 0.4).
narrative_ontology:measurement(gig_rating_tr_t3, gig_economy_rating_systems, theater_ratio, 3, 0.48).
narrative_ontology:measurement(gig_rating_tr_t6, gig_economy_rating_systems, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(gig_rating_be_t0, gig_economy_rating_systems, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gig_rating_be_t3, gig_economy_rating_systems, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(gig_rating_be_t6, gig_economy_rating_systems, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_rating_systems, resource_allocation).
narrative_ontology:boltzmann_floor_override(gig_economy_rating_systems, 0.18).
narrative_ontology:affects_constraint(gig_economy_rating_systems, algorithmic_wage_suppression).
narrative_ontology:affects_constraint(gig_economy_rating_systems, contractor_legal_status).
narrative_ontology:affects_constraint(gig_economy_rating_systems, platform_data_extraction).

% DUAL FORMULATION NOTE:
% Gig economy rating systems decompose into three structurally distinct constraints: (1) Rating Systems as Coordination (ε=0.15, Rope) — information aggregation for matching; (2) Rating Systems as Labor Control (ε=0.72, Snare) — behavioral suppression and wage pressure; (3) Rating Systems as Surveillance (ε=0.65, Tangled Rope) — engagement data extraction with coordination side-effects. The tangled_rope story captures the entanglement. Upstream constraints include contractor classification (which enables rating-based control) and downstream constraints include algorithmic wage suppression (which operationalizes rating leverage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gig_economy_rating_systems, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
