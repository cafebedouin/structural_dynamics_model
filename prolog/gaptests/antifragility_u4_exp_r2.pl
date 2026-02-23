% ============================================================================
% CONSTRAINT STORY: antifragility_u4_exp_r2
% ============================================================================
% Version: 4.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2027-08-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u4_exp_r2, []).

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
 *   constraint_id: antifragility_u4_exp_r2
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes the property of systems that increase in
 *   capability, resilience, or robustness as a result of stressors, shocks,
 *   volatility, and randomness. While it can be framed as a desirable
 *   systemic property, its implementation often creates a stark asymmetry:
 *   some agents benefit from volatility ('harvesting convexity') while others
 *   are made more fragile, bearing the uncompensated costs. This creates a
 *   significant perspectival gap.
 *
 * KEY AGENTS:
 *   - Optimized Serfs: Primary targets (powerless/trapped) whose predictable fragility provides the upside for others.
 *   - Barbell Practitioners: Primary beneficiaries (moderate/arbitrage) who structure their exposure to profit from volatility.
 *   - Fragilista Institutions: Enforcers/victims (institutional/constrained) that attempt to eliminate volatility, thereby creating hidden and more catastrophic risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u4_exp_r2, 0.75).
domain_priors:suppression_score(antifragility_u4_exp_r2, 0.65).
domain_priors:theater_ratio(antifragility_u4_exp_r2, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u4_exp_r2, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u4_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u4_exp_r2, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u4_exp_r2, tangled_rope).
narrative_ontology:human_readable(antifragility_u4_exp_r2, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u4_exp_r2, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u4_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u4_exp_r2, barbell_practitioners).
narrative_ontology:constraint_victim(antifragility_u4_exp_r2, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u4_exp_r2, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the agent whose stability is sacrificed to provide upside for others, the system is a pure Snare. They bear the costs of volatility without access to the gains.
constraint_indexing:constraint_classification(antifragility_u4_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the agent who can structure their affairs to gain from volatility (e.g., via a barbell strategy), the principle is a coordination tool (Rope) for harvesting upside.
constraint_indexing:constraint_classification(antifragility_u4_exp_r2, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% For the institution attempting to suppress volatility, the constraint is a Tangled Rope. It coordinates short-term stability but generates long-term, hidden fragility and extraction.
constraint_indexing:constraint_classification(antifragility_u4_exp_r2, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational or biological timescale, antifragility is a fundamental property of complex adaptive systems. It is an unchangeable law of evolution (Mountain).
constraint_indexing:constraint_classification(antifragility_u4_exp_r2, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u4_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u4_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u4_exp_r2, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u4_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u4_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (ε=0.75) represents the 'convexity transfer' where the upside from volatility is captured by a few, while the downside (tail risk) is socialized or pushed onto fragile agents. Suppression (0.65) reflects how systems (e.g., financial markets, corporate structures) are designed to make stable, low-volatility paths non-viable, forcing participation in the antifragile game. The system requires active enforcement through contracts, regulations, and market structures that facilitate this risk transfer.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the practitioner with arbitrage, it's a Rope for navigating reality. For the trapped serf, it's a Snare that drains their resources and stability. For the long-term analyst, it's a Mountain, a law of nature. The institutional bureaucrat, caught in the middle, experiences it as a Tangled Rope—a tool for control that ultimately extracts from the very system it's meant to protect.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from an agent's ability to manage their exposure to volatility. Beneficiaries ('barbell_practitioners') have the knowledge and capital to create asymmetric payoffs (limited downside, unlimited upside). Victims ('optimized_serfs', 'fragile_institutions') have linear or concave exposures; they are harmed by volatility and their fragility is the 'food' for the antifragile agents.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the analytical perspective. This avoids mislabeling it as a pure Mountain (ignoring the engineered extraction) or a pure Snare (ignoring its genuine function in evolutionary systems). The Tangled Rope classification correctly identifies that a natural law is being leveraged via an enforced coordination system to produce asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_intent,
    'Is the high extraction from fragile agents a necessary, functional byproduct of systemic evolution, or is it an intentionally designed predatory feature of specific implementations?',
    'Audit of 'skin in the game' metrics for beneficiaries. High skin-in-the-game across the system suggests functional necessity; concentrated, low skin-in-the-game beneficiaries suggest predation.',
    'If a functional necessity, the analytical classification shifts from Tangled Rope to Mountain. If predatory, the system is a pure Snare at the institutional scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intent, empirical, 'Distinguishing between antifragility as a natural law versus a predatory economic strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u4_exp_r2, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antifragility_tr_t0, antifragility_u4_exp_r2, theater_ratio, 0, 0.15).
narrative_ontology:measurement(antifragility_tr_t50, antifragility_u4_exp_r2, theater_ratio, 50, 0.35).
narrative_ontology:measurement(antifragility_tr_t100, antifragility_u4_exp_r2, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(antifragility_be_t0, antifragility_u4_exp_r2, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(antifragility_be_t50, antifragility_u4_exp_r2, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(antifragility_be_t100, antifragility_u4_exp_r2, base_extractiveness, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u4_exp_r2, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
