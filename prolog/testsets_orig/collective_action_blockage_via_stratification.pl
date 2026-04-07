% ============================================================================
% CONSTRAINT STORY: collective_action_blockage_via_stratification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_blockage_via_stratification, []).

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
 *   constraint_id: collective_action_blockage_via_stratification
 *   human_readable: Collective Action Blockage via Stratification
 *   domain: social_systems/institutional_dynamics/stratification_mechanics
 *
 * SUMMARY:
 *   Stratification mechanisms prevent marginalized actors from recognizing
 *   their shared structural position, blocking the collective action that
 *   would transform their power relationship to institutional constraints
 *   from powerless (π≈1.5) to organized (π≈0.4). This constraint operates as
 *   a meta-level extraction mechanism: it does not directly extract
 *   resources, but rather prevents the recognition and organization that
 *   would enable actors to resist extraction by other constraints. The
 *   stratification apparatus maintains separation through both structural
 *   barriers (residential segregation, educational tracking, occupational
 *   sorting) and internalized identity frames (status anxiety, aspirational
 *   identification with elites, naturalization of hierarchy). The constraint
 *   exhibits increasing extractiveness over the measurement interval
 *   (0.52→0.68) as stratification mechanisms have become more sophisticated
 *   and less visible — shifting from explicit legal barriers to implicit
 *   algorithmic sorting and meritocratic narratives. Theater ratio (0.58)
 *   reflects the performative equality discourse that coexists with
 *   persistent stratification: formal legal equality, diversity initiatives,
 *   and mobility narratives mask the structural blockage of collective
 *   recognition. This constraint is downstream of
 *   structural_position_constraint_divergence (the mountain constraint
 *   establishing that different structural positions produce genuinely
 *   different constraint experiences) but adds an extractive layer:
 *   stratification prevents actors from recognizing when their structural
 *   positions are actually shared, not divergent.
 *
 * KEY AGENTS:
 *   - Marginalized Analytical Actors: Primary victims (powerless/trapped at biographical/national scale) — experience stratification as naturalized hierarchy; cannot recognize shared position with other marginalized groups due to identity fragmentation and status competition
 *   - Potential Coalition Members: Secondary victims (moderate/constrained) — face barriers to cross-group organizing but have some agency; experience stratification as obstacle to coordination rather than immutable law
 *   - Privileged Institutional Actors: Primary beneficiaries (institutional/arbitrage) — benefit from stratification's prevention of collective action that would challenge their structural advantages; experience stratification as natural meritocratic sorting
 *   - Stratification Maintenance Apparatus: Institutional beneficiary (institutional/arbitrage) — educational tracking systems, residential zoning, occupational licensing, algorithmic sorting platforms that actively maintain separation
 *   - Collective Epistemic Capacity: Abstract victim (powerless/trapped at civilizational/global scale) — the capacity for society to recognize and address shared structural problems is itself degraded by stratification-induced fragmentation
 *   - Emerging Coalition (Counterfactual): Organized perspective (organized/mobile at generational/national scale) — represents the power configuration that would exist if stratification barriers were overcome; sees constraint as tangled_rope rather than snare because coalition formation creates exit options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_blockage_via_stratification, 0.68).
domain_priors:suppression_score(collective_action_blockage_via_stratification, 0.75).
domain_priors:theater_ratio(collective_action_blockage_via_stratification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_blockage_via_stratification, extractiveness, 0.68).
narrative_ontology:constraint_metric(collective_action_blockage_via_stratification, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(collective_action_blockage_via_stratification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_blockage_via_stratification, snare).
narrative_ontology:human_readable(collective_action_blockage_via_stratification, "Collective Action Blockage via Stratification").
narrative_ontology:topic_domain(collective_action_blockage_via_stratification, "social_systems/institutional_dynamics/stratification_mechanics").

domain_priors:requires_active_enforcement(collective_action_blockage_via_stratification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_blockage_via_stratification, privileged_institutional_actors).
narrative_ontology:constraint_beneficiary(collective_action_blockage_via_stratification, stratification_maintenance_apparatus).
narrative_ontology:constraint_victim(collective_action_blockage_via_stratification, marginalized_analytical_actors).
narrative_ontology:constraint_victim(collective_action_blockage_via_stratification, potential_coalition_members).
narrative_ontology:constraint_victim(collective_action_blockage_via_stratification, collective_epistemic_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_blockage_via_stratification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_blockage_via_stratification, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(collective_action_blockage_via_stratification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(collective_action_blockage_via_stratification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Stratification blocks transformation that would reduce experienced extraction across multiple downstream constraints. The π value shift potential (1.5→0.4) represents a 73% reduction in experienced extraction that stratification prevents. However, extractiveness is not maximal (not 0.85+) because some cross-group organizing does occur, and stratification's blockage is not total. The value reflects that most potential coalitions fail to form, but not all. Suppression (0.75): High. Barriers to recognition of shared structural position include residential segregation (limiting cross-group contact), educational tracking (creating divergent socialization), occupational sorting (separating workers by credential and status), media fragmentation (preventing shared narrative), and internalized status hierarchies (making cross-class solidarity psychologically costly). Suppression is not maximal because some actors do achieve cross-group consciousness, and some institutional spaces (unions, social movements, crisis periods) temporarily reduce barriers. Theater ratio (0.58): Moderate-high. Stratification maintenance increasingly operates through performative equality mechanisms: formal legal equality coexists with algorithmic discrimination, diversity initiatives coexist with persistent segregation, mobility narratives coexist with declining intergenerational mobility. The theater has increased over the interval (0.35→0.58) as explicit barriers have been replaced by implicit sorting mechanisms that are harder to recognize and challenge.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence based on structural position and time horizon. Marginalized actors at immediate/biographical time horizons see either naturalized hierarchy (mountain — 'this is just how society works') or inescapable extraction (snare — 'the system is rigged against us'). The difference between these perspectives is primarily time horizon and scope: immediate/local yields mountain (cannot imagine change), biographical/national yields snare (can imagine change but cannot achieve it). Privileged institutional actors see coordination (rope) — stratification is solving the legitimate problem of sorting talent and maintaining social order. The counterfactual organized coalition sees tangled_rope — stratification has some coordination function (preventing unstable coalitions, managing scarce positional goods) but also extracts by blocking beneficial collective action. The analytical observer sees tangled_rope from a civilizational perspective — stratification coordinates some genuine social functions but also extracts by preventing recognition of shared interests. The perspectival gap is not just about different experiences of the same constraint, but about whether the constraint is even visible: the mountain perspective has naturalized stratification to the point where it is not perceived as a constraint at all, but as background reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the stratification mechanism. Marginalized actors are victims with trapped exit options, yielding high d values (d≈0.95 for powerless/trapped, d≈0.85 for moderate/constrained). Privileged institutional actors are beneficiaries with arbitrage exit options, yielding low d values (d≈0.05). The counterfactual organized coalition perspective uses mobile exit options (d≈0.55) because coalition formation creates exit capacity that isolated actors lack. The analytical observer uses the canonical analytical d (d≈0.72) because they can see the structure but are not directly subject to it. The mountain perspective (naturalized hierarchy) uses trapped exit at powerless power, but the classification derives from the immediate time horizon and local scope — the agent cannot imagine the hierarchy changing within their lifetime or local context. This is a false summit: the analytical observer recognizes that stratification is contingent institutional arrangement, not natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED: This constraint presents a genuine mandatrophy that requires empirical resolution. The core ambiguity: is stratification primarily an extraction mechanism (blocking beneficial collective action) or primarily a coordination mechanism (preventing unstable coalitions and managing positional goods scarcity)? The snare classification from marginalized perspectives is structurally sound — they experience high extraction and high suppression with no exit. The rope classification from privileged perspectives is also structurally sound — they experience coordination benefits with minimal extraction. The analytical tangled_rope classification acknowledges both functions but cannot definitively weight them without empirical data on: (1) What transformation would actually occur if stratification barriers were removed? Would cross-class coalitions form and reduce extraction, or would other barriers persist? (2) What coordination functions does stratification actually serve? Does it prevent genuinely unstable coalitions, or does it prevent coalitions that would be stable and beneficial? (3) What is the counterfactual baseline? Compared to what alternative institutional arrangement is stratification extractive or coordinative? The omega variables identify the empirical questions that would resolve the mandatrophy. Until those questions are answered, the constraint legitimately appears as both snare (from below) and rope (from above), with the analytical perspective unable to adjudicate between them from structural data alone. This is not a failure of the framework — it is the framework correctly identifying an irreducible empirical uncertainty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_threshold_ambiguity,
    'What threshold of shared structural position recognition is sufficient to trigger coalition formation?',
    'Historical analysis of successful collective action emergence; identification of critical consciousness thresholds in labor organizing, civil rights movements, and cross-class coalitions',
    'If threshold is low (20-30% recognition): stratification is highly extractive, blocking easily achievable transformation. If threshold is high (70-80%): stratification may be coordination mechanism preventing premature unstable coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_threshold_ambiguity, empirical, 'Recognition threshold for coalition formation').

omega_variable(
    transformation_rule_counterfactual,
    'Would recognition of shared structural position actually enable transformation from Snare (π=1.5) to Rope (π=0.4), or would other barriers persist?',
    'Comparative analysis of cases where stratification barriers were overcome: did power transformation follow recognition, or did recognition prove insufficient without material resource redistribution?',
    'If recognition sufficient: stratification is pure extraction blocking available transformation. If recognition insufficient: stratification may be symptom rather than cause of power asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transformation_rule_counterfactual, conceptual, 'Sufficiency of recognition for power transformation').

omega_variable(
    stratification_maintenance_mechanism,
    'Is stratification maintained primarily through active enforcement (institutional barriers, legal segregation, resource hoarding) or through internalized identity frames (status anxiety, aspirational identification with elites)?',
    'Decomposition of suppression into structural vs internalized components; measurement of stratification persistence after removal of formal barriers',
    'If primarily structural: suppression is accurately measured at 0.75. If primarily internalized: effective suppression may be higher (identity_locked dynamics) and persist after formal barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_maintenance_mechanism, empirical, 'Structural vs internalized stratification maintenance').

omega_variable(
    elite_defection_dynamics,
    'Under what conditions do privileged institutional actors defect from stratification maintenance to join cross-class coalitions?',
    'Historical analysis of elite defection patterns in revolutionary periods, reform movements, and cross-class alliances; identification of material vs ideological defection triggers',
    'If elite defection is common: stratification is fragile, and the constraint may have lower effective extraction than measured. If elite defection is rare: stratification is robust extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_defection_dynamics, empirical, 'Elite defection frequency and triggers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_blockage_via_stratification, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strat_block_tr_t0, collective_action_blockage_via_stratification, theater_ratio, 0, 0.35).
narrative_ontology:measurement(strat_block_tr_t25, collective_action_blockage_via_stratification, theater_ratio, 25, 0.45).
narrative_ontology:measurement(strat_block_tr_t50, collective_action_blockage_via_stratification, theater_ratio, 50, 0.52).
narrative_ontology:measurement(strat_block_tr_t75, collective_action_blockage_via_stratification, theater_ratio, 75, 0.56).
narrative_ontology:measurement(strat_block_tr_t100, collective_action_blockage_via_stratification, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(strat_block_be_t0, collective_action_blockage_via_stratification, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(strat_block_be_t25, collective_action_blockage_via_stratification, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(strat_block_be_t50, collective_action_blockage_via_stratification, base_extractiveness, 50, 0.63).
narrative_ontology:measurement(strat_block_be_t75, collective_action_blockage_via_stratification, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(strat_block_be_t100, collective_action_blockage_via_stratification, base_extractiveness, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_blockage_via_stratification, identity_coordination).
narrative_ontology:affects_constraint(collective_action_blockage_via_stratification, labor_organizing_barriers).
narrative_ontology:affects_constraint(collective_action_blockage_via_stratification, cross_class_coalition_formation).
narrative_ontology:affects_constraint(collective_action_blockage_via_stratification, epistemic_commons_fragmentation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural_position_constraint_divergence (the mountain constraint establishing that different structural positions produce different constraint experiences). The upstream constraint is a genuine natural law: agents at different structural positions DO experience constraints differently. This constraint adds an extractive layer: stratification prevents agents from recognizing when their structural positions are actually SHARED (not divergent), blocking collective action that would transform their power relationship to other constraints. The distinction: structural_position_constraint_divergence explains why a CEO and a janitor experience labor law differently (genuine divergence). collective_action_blockage_via_stratification explains why two janitors in different identity groups fail to recognize their shared position and organize together (extractive blockage of recognition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
