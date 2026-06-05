% ============================================================================
% CONSTRAINT STORY: msgs_asset_bundling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_msgs_asset_bundling, []).

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
 *   constraint_id: msgs_asset_bundling
 *   human_readable: Bundled Ownership of Knicks and Rangers under MSG Sports
 *   domain: economic
 *
 * SUMMARY:
 *   The corporate structure of Madison Square Garden Sports (MSGS) combines
 *   two distinct, high-value assets—the New York Knicks (NBA) and New York
 *   Rangers (NHL)—into a single publicly traded stock. This bundling creates
 *   a situation where investors cannot directly invest in either team
 *   individually, potentially extracting value from investors with specific
 *   preferences.
 *
 * KEY AGENTS:
 *   - Potential Knicks Investors: Primary target (powerless/trapped) - forced to buy into bundled asset.
 *   - Potential Rangers Investors: Primary target (powerless/trapped) - forced to buy into bundled asset.
 *   - MSG Sports Shareholders: Primary beneficiary (institutional/arbitrage) - benefit from diversified risk and potentially increased valuation.
 *   - MSG Management: Secondary beneficiary (institutional/constrained) - benefits from operational efficiencies but constrained by balancing team interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(msgs_asset_bundling, 0.55).
domain_priors:suppression_score(msgs_asset_bundling, 0.4).
domain_priors:theater_ratio(msgs_asset_bundling, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(msgs_asset_bundling, extractiveness, 0.55).
narrative_ontology:constraint_metric(msgs_asset_bundling, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(msgs_asset_bundling, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(msgs_asset_bundling, tangled_rope).
narrative_ontology:human_readable(msgs_asset_bundling, "Bundled Ownership of Knicks and Rangers under MSG Sports").
narrative_ontology:topic_domain(msgs_asset_bundling, "economic").

domain_priors:requires_active_enforcement(msgs_asset_bundling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, msg_sports_shareholders).
narrative_ontology:constraint_beneficiary(msgs_asset_bundling, msg_management).
narrative_ontology:constraint_victim(msgs_asset_bundling, potential_knicks_investors).
narrative_ontology:constraint_victim(msgs_asset_bundling, potential_rangers_investors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Potential investors specifically interested in owning a stake in the New York Knicks find their options severely limited, as they are forced to buy into a bundled asset that includes the New York Rangers. Their preferences are suppressed, and they lack alternatives for investing in just the Knicks. This represents a high degree of extraction, as they are forced to allocate capital to an unwanted asset.
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Similarly, potential investors specifically interested in the New York Rangers are forced to buy into the bundled MSGS asset, including the Knicks, which they may not desire. This also represents a high degree of extraction, as they are forced to allocate capital to an unwanted asset.
constraint_indexing:constraint_classification(msgs_asset_bundling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Current shareholders of MSG Sports benefit from the bundling, as it diversifies risk and potentially increases the overall valuation of the company. They experience the constraint as a coordination mechanism that allows for efficient management and cross-promotion between the two assets. They have the arbitrage option of selling their shares if they disapprove of the bundled structure, though this requires selling both assets.
constraint_indexing:constraint_classification(msgs_asset_bundling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% MSG Management benefits from the control and operational efficiencies afforded by managing both the Knicks and Rangers under a single corporate umbrella. However, they are also constrained by the need to balance the interests of both teams and their respective fan bases, potentially leading to suboptimal decisions for one or both teams. The 'constrained' exit option reflects the difficulty in fully extracting themselves from managing one team without affecting their position with the other.
constraint_indexing:constraint_classification(msgs_asset_bundling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the bundled ownership represents a tangled rope. It provides a coordination benefit by allowing for synergies between the two teams, but also results in extraction by limiting investment choices. The active enforcement comes from the corporate structure and regulatory approvals required for such a bundled ownership.
constraint_indexing:constraint_classification(msgs_asset_bundling, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(msgs_asset_bundling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(msgs_asset_bundling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(msgs_asset_bundling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(msgs_asset_bundling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(msgs_asset_bundling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) because while it suppresses investor choice, it also provides diversification benefits to shareholders. Suppression is also moderate (0.40) as investors are not entirely trapped - they can invest in MSGS, though it's not their ideal scenario. The theater ratio is low (0.30) as the corporate structure has functional economic implications beyond pure performative branding.
 *
 * PERSPECTIVAL GAP:
 *   The potential Knicks/Rangers-only investors see a snare because they are forced to invest in an unwanted asset. MSG Sports shareholders see a rope because they benefit from diversification. MSG Management experiences the constraint as a tangled rope due to control efficiencies versus balancing conflicting interests. The analytical observer also classifies it as a tangled rope, recognizing both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The potential investors are considered victims because they have reduced investment choices. MSG Sports shareholders are beneficiaries because they gain diversification. MSG Management benefits from operational efficiencies and control. Exit options determine the intensity of the experience for the involved parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling as pure extraction by acknowledging the coordination benefit of diversified risk for shareholders. However, the extraction experienced by potential investors with specific team preferences is significant enough to classify this as a tangled rope rather than a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synergy_vs_distraction,
    'To what extent do synergies between the Knicks and Rangers outweigh potential management distractions or suboptimal resource allocation for either team?',
    'Comparative analysis of team performance, financial metrics, and resource allocation strategies under bundled vs. independent ownership structures.',
    'If synergies dominate: bundling is efficient coordination. If distractions dominate: bundling is primarily extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(synergy_vs_distraction, empirical, 'Quantifying the balance between synergies and distractions in bundled ownership.').

omega_variable(
    investor_preference_elasticity,
    'How sensitive are potential investors to the bundling requirement? Would separate ownership attract a significantly larger pool of dedicated capital?',
    'Surveys of potential investors, analysis of market demand for sports team ownership, and hypothetical market valuations under separate ownership.',
    'High elasticity: bundling significantly suppresses investor demand. Low elasticity: bundling has minimal impact on investor interest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investor_preference_elasticity, empirical, 'Measuring investor sensitivity to the bundling of Knicks and Rangers ownership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(msgs_asset_bundling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(msgs_tr_t0, msgs_asset_bundling, theater_ratio, 0, 0.2).
narrative_ontology:measurement(msgs_tr_t5, msgs_asset_bundling, theater_ratio, 5, 0.25).
narrative_ontology:measurement(msgs_tr_t10, msgs_asset_bundling, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(msgs_be_t0, msgs_asset_bundling, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(msgs_be_t5, msgs_asset_bundling, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(msgs_be_t10, msgs_asset_bundling, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(msgs_asset_bundling, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
