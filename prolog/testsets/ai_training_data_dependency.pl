% ============================================================================
% CONSTRAINT STORY: ai_training_data_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_training_data_dependency, []).

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
 *   constraint_id: ai_training_data_dependency
 *   human_readable: AI Training Data Dependency
 *   domain: technological
 *
 * SUMMARY:
 *   The performance of state-of-the-art AI models is critically dependent on
 *   access to massive, high-quality training datasets. This creates a
 *   structural constraint where entities controlling such data (typically
 *   large, incumbent tech firms) gain a significant and durable competitive
 *   advantage. This 'data moat' functions as both a coordination mechanism
 *   for the field (advancing the SOTA through scaling) and a powerful
 *   extraction mechanism (locking out new entrants and concentrating market
 *   power). The constraint is actively enforced through legal (copyright,
 *   ToS) and economic (cost of acquisition) barriers.
 *
 * KEY AGENTS:
 *   - Incumbent Tech Firms: Primary beneficiary (institutional/arbitrage) — Owns or controls proprietary datasets, creating a competitive 'moat'.
 *   - New Entrants / Startups: Primary victim (powerless/trapped) — Cannot acquire or generate comparable data, creating an insurmountable barrier to entry.
 *   - Open Source Community: Organized victim (organized/constrained) — Attempts to build public alternative datasets but faces resource and legal constraints.
 *   - Analytical Observer: Sees the dual nature of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_training_data_dependency, 0.55).
domain_priors:suppression_score(ai_training_data_dependency, 0.7).
domain_priors:theater_ratio(ai_training_data_dependency, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_training_data_dependency, extractiveness, 0.55).
narrative_ontology:constraint_metric(ai_training_data_dependency, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_training_data_dependency, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_training_data_dependency, tangled_rope).
narrative_ontology:human_readable(ai_training_data_dependency, "AI Training Data Dependency").
narrative_ontology:topic_domain(ai_training_data_dependency, "technological").

domain_priors:requires_active_enforcement(ai_training_data_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_training_data_dependency, incumbent_tech_firms).
narrative_ontology:constraint_beneficiary(ai_training_data_dependency, proprietary_data_aggregators).
narrative_ontology:constraint_victim(ai_training_data_dependency, new_entrants).
narrative_ontology:constraint_victim(ai_training_data_dependency, open_source_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NEW ENTRANT (SNARE) — Lacking access to proprietary petabyte-scale datasets, a new entrant is trapped. The dependency is a pure barrier to entry, extracting the opportunity for innovation and competition. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(ai_training_data_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCUMBENT FIRM (ROPE) — For a firm that owns a massive dataset, the dependency is a powerful coordination mechanism. It structures the market, defines the state-of-the-art, and reinforces their competitive advantage (a 'data moat'). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08. The negative extraction signifies a net subsidy.
constraint_indexing:constraint_classification(ai_training_data_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — This view sees both sides: the dependency coordinates the field around scaling laws, enabling rapid progress (Rope function), but it also creates immense barriers to entry and concentrates power (Snare function). The high suppression and extraction are undeniable. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(ai_training_data_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (SCAFFOLD) — Organized efforts to create public datasets (e.g., LAION, The Pile) or develop data-efficient methods see the dependency as a temporary, surmountable problem. They are building a scaffold (public data infrastructure) to bypass the proprietary data walls, with an implicit sunset clause: once the scaffold is built, the dependency on private data is broken. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.26.
constraint_indexing:constraint_classification(ai_training_data_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SCALING LAW ABSOLUTIST (MOUNTAIN) — This perspective frames the dependency as an immutable law of nature for current AI paradigms ('scaling laws'). It posits that model performance is inextricably linked to data volume, making the dependency an unchangeable feature of the technological landscape. The engine will flag this as a false summit, as the base properties (ε=0.55, suppression=0.70) are inconsistent with a natural law.
constraint_indexing:constraint_classification(ai_training_data_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_training_data_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_training_data_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_training_data_dependency, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_training_data_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_training_data_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High. The constraint extracts competitive opportunity from the market. The value that could be generated by startups is instead captured by incumbents who control the data resource. Suppression (0.70): High. The combination of legal protections (copyright), high cost of data acquisition, and the sheer technical challenge of managing petabyte-scale data strongly suppresses the emergence of alternatives. Theater (0.30): Low-to-moderate. While the 'data moat' narrative can be performative, the underlying technical requirement for data is very real and functional for current AI architectures.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For an incumbent, the data dependency is a beneficial Rope that organizes the market to their advantage. For a startup, it's a deadly Snare that prevents them from competing. For the open-source community, it's a temporary Scaffold they are actively trying to build their way over. An observer focused on scaling laws might mistake this contingent, market-driven structure for a Mountain, a law of nature. The analytical view must hold all these truths at once to see the full Tangled Rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbents) have arbitrage exit options, leading to a low 'd' value and a Rope classification. Victims (startups) are trapped, leading to a high 'd' value and a Snare classification. Organized actors (open source) are constrained but have agency, leading to a moderate 'd' value and a Scaffold classification. The analytical perspective synthesizes these to arrive at Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case avoids mandatrophy by demonstrating how a single technological reality can be correctly classified in multiple ways depending on the observer's structural position. Mistaking the incumbent's 'Rope' view or the absolutist's 'Mountain' view for the complete picture would be a classic mandatrophy error. The framework correctly identifies the analytical view as a Tangled Rope, acknowledging both the genuine coordination function (scaling enables progress) and the severe, asymmetric extraction (market concentration and barriers to entry).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synthetic_data_viability,
    'Can high-quality synthetic data fully substitute for real-world data at scale, thus breaking the dependency?',
    'Empirical testing of models trained on synthetic vs. real data across diverse, complex domains. Analysis of failure modes unique to synthetic data (e.g., mode collapse, lack of long-tail coverage).',
    'If viable, the constraint becomes a Scaffold (a temporary technical problem). If not, it remains a Tangled Rope or Snare (a structural economic barrier).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synthetic_data_viability, empirical, 'Viability of synthetic data as a substitute for real-world training data.').

omega_variable(
    scaling_law_universality,
    'Are the current scaling laws a fundamental property of intelligence, or an artifact of the current transformer-based architecture?',
    'Discovery of novel AI architectures that achieve state-of-the-art performance with significantly less data.',
    'If laws are fundamental, the ''Mountain'' perspective gains credence. If they are an architectural artifact, the dependency is a contingent technical choice, reinforcing the ''Tangled Rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaling_law_universality, conceptual, 'Whether scaling laws are fundamental or an artifact of current architectures.').

omega_variable(
    data_as_public_utility,
    'Should foundational datasets be regulated as a public utility to ensure fair access and promote competition?',
    'Political and legislative debate, followed by policy implementation and observation of market effects.',
    'If regulated, the constraint could transform into a Rope (a managed public resource). If not, it remains a Tangled Rope dominated by private interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_as_public_utility, preference, 'Policy question of regulating foundational datasets as a public utility.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_training_data_dependency, 2017, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_t_tr_t2017, ai_training_data_dependency, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(ai_t_tr_t2022, ai_training_data_dependency, theater_ratio, 2022, 0.25).
narrative_ontology:measurement(ai_t_tr_t2027, ai_training_data_dependency, theater_ratio, 2027, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_t_be_t2017, ai_training_data_dependency, base_extractiveness, 2017, 0.2).
narrative_ontology:measurement(ai_t_be_t2022, ai_training_data_dependency, base_extractiveness, 2022, 0.45).
narrative_ontology:measurement(ai_t_be_t2027, ai_training_data_dependency, base_extractiveness, 2027, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_training_data_dependency, resource_allocation).
narrative_ontology:affects_constraint(ai_training_data_dependency, ai_model_monoculture).
narrative_ontology:affects_constraint(ai_training_data_dependency, computational_hardware_arms_race).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
