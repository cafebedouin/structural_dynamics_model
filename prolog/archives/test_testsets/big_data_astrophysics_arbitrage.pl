% ============================================================================
% CONSTRAINT STORY: big_data_astrophysics_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_big_data_astrophysics_arbitrage, []).

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
 *   constraint_id: big_data_astrophysics_arbitrage
 *   human_readable: Big Data Arbitrage in Modern Astrophysics
 *   domain: technological/scientific_practice
 *
 * SUMMARY:
 *   The shift to time-domain astronomy has created a data deluge, with
 *   surveys like LSST and SKA generating petabytes of data. This creates a
 *   structural arbitrage opportunity. Groups with the immense computational
 *   resources, proprietary pipelines, and early access can extract
 *   high-impact discoveries before the broader community can effectively
 *   analyze the data. This constraint is not about financial profit, but the
 *   extraction of scientific capital: priority, publications, and funding.
 *
 * KEY AGENTS:
 *   - Large Survey Consortia: Primary beneficiaries (institutional/arbitrage) - Control the data pipelines and benefit from first-look discovery rights.
 *   - Small Research Groups: Primary victims (powerless/trapped) - Lack the computational and financial resources to process raw data streams in a competitive timeframe.
 *   - Astronomy Epistemic Commons: Abstract victim (powerless/trapped) - The ideal of equitable data access is compromised, potentially slowing the overall pace of discovery.
 *   - Open-Source AI/ML Community: Organized agents (organized/mobile) - Attempting to build tools to close the access gap, viewing it as a temporary technical hurdle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, 0.55).
domain_priors:suppression_score(big_data_astrophysics_arbitrage, 0.65).
domain_priors:theater_ratio(big_data_astrophysics_arbitrage, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, extractiveness, 0.55).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(big_data_astrophysics_arbitrage, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(big_data_astrophysics_arbitrage, tangled_rope).
narrative_ontology:human_readable(big_data_astrophysics_arbitrage, "Big Data Arbitrage in Modern Astrophysics").
narrative_ontology:topic_domain(big_data_astrophysics_arbitrage, "technological/scientific_practice").

domain_priors:requires_active_enforcement(big_data_astrophysics_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(big_data_astrophysics_arbitrage, large_survey_consortia).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, small_research_groups).
narrative_ontology:constraint_victim(big_data_astrophysics_arbitrage, astronomy_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL RESEARCH GROUP (SNARE) — Trapped by resource limitations (compute, storage, personnel). Cannot compete in real-time analysis of petabyte-scale raw data streams. The promise of open data is an illusion; they are locked out of the primary discovery space. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LARGE SURVEY CONSORTIUM (ROPE) — Experiences the system as pure coordination. They built the infrastructure and their proprietary access period/first-look advantage is the necessary reward and operational requirement to manage the data deluge for the benefit of all science. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. Negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (producing an unprecedented public good) and the asymmetric extraction (scientific priority and funding captured by consortia with privileged access). The structure is a hybrid. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE AI/ML COMMUNITY (SCAFFOLD) — Views the access bottleneck as a temporary technical problem. By developing and distributing powerful, efficient, open-source analysis tools and platforms, they aim to democratize access. This effort has an implicit sunset clause: once the tools are mature enough, the arbitrage advantage of the consortia will diminish. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: 'PHYSICS OF PROGRESS' OBSERVER (MOUNTAIN) — Argues this structure is an unchangeable law of big science. Petabyte-scale instruments necessitate large, centralized teams. The resulting inequality is not a contingent policy choice but an emergent, natural feature of technological scaling. The engine will flag this as a false summit, as the high base extractiveness (0.55) and suppression (0.65) violate the Mountain classification gates.
constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(big_data_astrophysics_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(big_data_astrophysics_arbitrage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(big_data_astrophysics_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(big_data_astrophysics_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. The scientific advantage conferred by privileged access is substantial and directly translates to career and funding outcomes. Suppression (0.65): High. The barriers to entry for analyzing raw petabyte-scale data are immense, requiring access to supercomputing facilities and highly specialized software, effectively suppressing competition from smaller institutions. Theater Ratio (0.20): Low. While there is a narrative of 'open data,' the core activity is genuine scientific work, not performative ritual. The enforcement is active through data access policies and the sheer technical architecture of the data systems.
 *
 * PERSPECTIVAL GAP:
 *   The core gap is between the Large Survey Consortium (seeing a Rope) and the Small Research Group (seeing a Snare). The consortium views its privileged access as a necessary component of a massive coordination effort. The small group experiences this same structure as a lock-out, a coercive barrier to participation in cutting-edge science. The Analytical observer reconciles these views as a Tangled Rope, acknowledging both the valid coordination function and the severe extractive asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Large Consortia) have arbitrage exit options, leading to a low 'd' value and a Rope classification. Victims (Small Groups) are trapped by resource constraints, leading to a high 'd' value and a Snare classification. The system's structure directly creates these opposing experiences from a single set of facts.
 *
 * MANDATROPHY ANALYSIS:
 *   This case avoids mandatrophy by demonstrating how a system can be simultaneously a legitimate coordination effort and a highly extractive mechanism. Labeling it purely as a Rope (the consortium's view) would ignore the coercive exclusion of most of the field. Labeling it purely as a Snare (the small group's view) would ignore the genuine scientific infrastructure being created. The Tangled Rope classification from the analytical perspective correctly identifies the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_democratization_hypothesis,
    'Will advanced AI/ML analysis tools democratize data access, or will they concentrate power further by requiring immense computational resources for training and inference?',
    'Track adoption of open-source tools by under-resourced institutions and measure their publication output on survey data vs. consortia members over a 5-10 year period.',
    'If democratizing, the ''Scaffold'' perspective is validated and ε will decrease. If concentrating, the ''Snare'' perspective becomes dominant for most actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_democratization_hypothesis, empirical, 'Whether AI/ML tools will democratize or concentrate access to big astronomical data.').

omega_variable(
    proprietary_period_necessity,
    'Is the proprietary data access period for survey consortia a necessary incentive for instrument construction (coordination) or an artificial rent-seeking mechanism (extraction)?',
    'Comparative analysis of projects with different data access policies, controlling for funding models and scientific scope. Economic modeling of consortium funding with and without proprietary periods.',
    'If deemed necessary, the constraint is closer to a Rope/Tangled Rope. If deemed artificial, it is structurally a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proprietary_period_necessity, conceptual, 'Whether proprietary data periods are necessary incentives or artificial extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(big_data_astrophysics_arbitrage, 2010, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(big__tr_t0, big_data_astrophysics_arbitrage, theater_ratio, 0, 0.1).
narrative_ontology:measurement(big__tr_t10, big_data_astrophysics_arbitrage, theater_ratio, 10, 0.15).
narrative_ontology:measurement(big__tr_t20, big_data_astrophysics_arbitrage, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(big__be_t0, big_data_astrophysics_arbitrage, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(big__be_t10, big_data_astrophysics_arbitrage, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(big__be_t20, big_data_astrophysics_arbitrage, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(big_data_astrophysics_arbitrage, global_infrastructure).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, academic_publishing_models).
narrative_ontology:affects_constraint(big_data_astrophysics_arbitrage, scientific_funding_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
