% ============================================================================
% CONSTRAINT STORY: indian_import_tariffs_eu
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_indian_import_tariffs_eu, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: indian_import_tariffs_eu
 *   human_readable: "Indian Protective Tariffs on European Union Imports (Autos & Spirits)"
 *   domain: economic/political
 *
 * SUMMARY:
 *   India imposes high tariffs (up to 150%) on certain European imports,
 *   notably cars and alcoholic beverages. These tariffs are a major sticking
 *   point in ongoing EU-India free trade agreement (FTA) negotiations. While
 *   the tariffs protect domestic Indian industries from foreign competition
 *   and generate government revenue, they make European goods prohibitively
 *   expensive, effectively blocking market access and extracting value from
 *   both EU exporters and Indian consumers. The constraint's structure
 *   combines a coordination function for domestic producers with asymmetric
 *   extraction from external and internal actors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Indian Consumers: Primary target (powerless/trapped) — bear extraction via higher prices and reduced choice.
 *   - European Exporters: Primary target (organized/constrained) — bear extraction via uncompetitive pricing and lost market share.
 *   - Indian Domestic Producers (Auto/Spirits): Primary beneficiary (institutional/arbitrage) — benefit from protection against competition.
 *   - Government of India: Institutional beneficiary & enforcer (institutional/arbitrage) — benefits from tariff revenue and political support from protected industries.
 *   - European Union Commission: Institutional negotiator (institutional/constrained) — represents the targets, trying to dismantle the constraint.
 *   - Analytical Observer: Sees the full structure as a Tangled Rope.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Tariffs up to 150% represent a very high rate of direct extraction.
domain_priors:base_extractiveness(indian_import_tariffs_eu, 0.55).
% The tariffs are legally enforced, leaving no alternative for market access.
domain_priors:suppression_score(indian_import_tariffs_eu, 0.75).   % Structural property (raw, unscaled).
% The tariffs are purely functional, not performative.
domain_priors:theater_ratio(indian_import_tariffs_eu, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_import_tariffs_eu, extractiveness, 0.55).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(indian_import_tariffs_eu, tangled_rope).

% --- Binary flags ---
% The constraint is a state-enforced economic policy.
domain_priors:requires_active_enforcement(indian_import_tariffs_eu). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_domestic_producers).
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, government_of_india).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(indian_import_tariffs_eu, european_exporters).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, indian_consumers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE INDIAN CONSUMER (PRIMARY TARGET - SNARE)
% Victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42.
% χ = 0.55 * 1.42 * σ(national=1.0) = 0.78 (Snare)
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE EUROPEAN EXPORTER (PRIMARY TARGET - SNARE)
% Victim membership + constrained exit -> d ≈ 0.90 -> f(d) ≈ 1.35.
% χ = 0.55 * 1.35 * σ(national=1.0) = 0.74 (Snare)
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE INDIAN DOMESTIC PRODUCER (PRIMARY BENEFICIARY - ROPE)
% Beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12.
% χ = 0.55 * -0.12 * σ(national=1.0) = -0.066 (Rope)
constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the coordination function
% for domestic industry and the high asymmetric extraction.
% d ≈ 0.72 -> f(d) ≈ 1.15.
% χ = 0.55 * 1.15 * σ(global=1.2) = 0.76 (Tangled Rope)
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 5A: GOVERNMENT OF INDIA (BENEFICIARY & ENFORCER - ROPE)
% As a beneficiary with full control over the policy (arbitrage exit), it
% experiences the constraint as a tool for coordination and revenue.
% Beneficiary + arbitrage -> d ≈ 0.05 -> f(d) ≈ -0.12 -> χ < 0 (Rope).
constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5B: EUROPEAN UNION COMMISSION (NEGOTIATOR - TANGLED ROPE)
% As an institutional actor representing victims but lacking unilateral power
% to change the rule (constrained exit), it perceives the extraction. The
% engine derives a higher d value than for the Indian govt.
% Represents victims + constrained exit -> d ≈ 0.7 -> f(d) ≈ 1.1.
% χ = 0.55 * 1.1 * σ(continental=1.1) = 0.66 (Tangled Rope).
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_import_tariffs_eu_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    % Verify that the primary victim (consumer) sees a Snare and the primary
    % beneficiary (producer) sees a Rope.
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify the gap between the two negotiating institutions.
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(tangled_rope_conditions_met) :-
    % Verify that the structural requirements for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, _),
    narrative_ontology:constraint_victim(indian_import_tariffs_eu, _),
    domain_priors:requires_active_enforcement(indian_import_tariffs_eu).

:- end_tests(indian_import_tariffs_eu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set high to reflect tariffs of 100-150%, which represent a direct and significant extraction of value.
 *   - Suppression (0.75): The tariffs are a legal barrier to market entry, strongly suppressing the alternative of free trade.
 *   - Theater (0.10): The tariffs are fully functional economic instruments, not performative acts.
 *   The combination of high extraction, high suppression, and the presence of both beneficiaries and victims makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For Indian domestic producers, the tariffs are a 'Rope' coordinating the market to their benefit, shielding them from competition. For EU exporters and Indian consumers, they are a 'Snare' that extracts value and limits options. This disagreement is the core of the trade dispute. The EU sees an unfair barrier (Tangled Rope), India's protected industries see a necessary support structure (Rope), and consumers are trapped in a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'indian_domestic_producers' and 'government_of_india' directly benefit via protection and revenue. Their 'arbitrage' exit status (they can lobby for/control the policy) gives them a very low directionality `d`, leading to a Rope classification.
 *   - Victims: 'european_exporters' and 'indian_consumers' bear the costs. Their 'constrained' or 'trapped' exit options give them a high `d`, scaling up the effective extraction χ and leading to a Snare classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model captures the asymmetry between the negotiating parties. The Government of India, having full control (arbitrage exit), perceives the constraint as a beneficial Rope. The EU Commission, representing the victims and having to negotiate (constrained exit), perceives the extractive nature of the constraint, classifying it as a Tangled Rope. Their different structural positions and exit options, despite both being 'institutional' actors, create a measurable difference in classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two common errors. It does not label the tariffs as pure extraction (a Snare from all views), because that would ignore their genuine (if protectionist) coordination function for the domestic market. It also does not label them as pure coordination (a Rope), which would ignore the immense extraction imposed on exporters and consumers. The Tangled Rope classification correctly identifies the hybrid nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_indian_import_tariffs_eu,
    'Are these tariffs a temporary "Scaffold" for nascent domestic industries, or a permanent "Piton" of entrenched rent-seeking?',
    'Analysis of historical changes in domestic industry productivity and competitiveness. If productivity grows, it suggests a Scaffold. If it stagnates, it suggests a Piton.',
    'If Scaffold, a gradual phase-out is a viable policy. If a rent-seeking Piton/Tangled Rope, removal will face intense political resistance from beneficiaries regardless of economic efficiency.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(indian_import_tariffs_eu, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has existed for a long time, rooted in India's post-
% independence economic policies. The extraction has been consistently high.
% Required because base_extractiveness (0.55) > 0.46.

% Theater ratio over time (consistently low):
narrative_ontology:measurement(iiteu_tr_t0, indian_import_tariffs_eu, theater_ratio, 0, 0.10).
narrative_ontology:measurement(iiteu_tr_t5, indian_import_tariffs_eu, theater_ratio, 5, 0.10).
narrative_ontology:measurement(iiteu_tr_t10, indian_import_tariffs_eu, theater_ratio, 10, 0.10).

% Extraction over time (consistently high):
narrative_ontology:measurement(iiteu_ex_t0, indian_import_tariffs_eu, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(iiteu_ex_t5, indian_import_tariffs_eu, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(iiteu_ex_t10, indian_import_tariffs_eu, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The tariffs function to allocate market share away from imports and towards
% domestic companies.
narrative_ontology:coordination_type(indian_import_tariffs_eu, resource_allocation).

% These tariffs are structurally linked to the competitiveness of India's
% domestic auto market and the EU's overall trade strategy.
narrative_ontology:affects_constraint(indian_import_tariffs_eu, indian_auto_market_competitiveness).
narrative_ontology:affects_constraint(eu_global_trade_strategy, indian_import_tariffs_eu).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain,
% using the declared beneficiary/victim groups and their associated exit
% options, accurately computes the directionality `d` for each agent and
% captures the essential dynamics of the conflict.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */