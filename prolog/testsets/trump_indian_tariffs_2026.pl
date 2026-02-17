% ============================================================================
% CONSTRAINT STORY: trump_indian_tariffs_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_trump_indian_tariffs_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: trump_indian_tariffs_2026
 * human_readable: Trump's Tariff Reduction on India (2026)
 * domain: economic
 * * SUMMARY:
 * In a hypothetical 2026, a Trump administration reduces tariffs on Indian goods to 18% after the Modi government agrees to reduce oil purchases from Russia. This is a quid-pro-quo arrangement where economic concessions are exchanged for political alignment, seemingly benefiting both countries at the expense of Russia.
 * * KEY AGENTS:
 * - Indian Exporters: Subject (Powerless initially, potentially Organized)
 * - US Consumers/Importers: Beneficiary (Institutional)
 * - Russian State: Victim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(trump_indian_tariffs_2026, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(trump_indian_tariffs_2026, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(trump_indian_tariffs_2026, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(trump_indian_tariffs_2026, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(trump_indian_tariffs_2026, tangled_rope).
narrative_ontology:human_readable(trump_indian_tariffs_2026, "Trump's Tariff Reduction on India (2026)").

% Binary flags
domain_priors:requires_active_enforcement(trump_indian_tariffs_2026). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(trump_indian_tariffs_2026, us_consumers_importers).
narrative_ontology:constraint_victim(trump_indian_tariffs_2026, russian_state).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For Indian exporters, the tariffs remain a significant barrier, and their reduction is conditional on geopolitical actions outside their control.
% χ = 0.55 * π(powerless:1.5) * σ(national:1.0) = 0.825. This is a clear Snare.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For US importers and consumers, the deal is pure coordination, lowering prices and securing supply chains in exchange for geopolitical alignment they benefit from.
% χ = 0.55 * π(institutional:-0.2) * σ(national:1.0) = -0.11. This is a clear Rope.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination function (US-India trade) and the asymmetric extraction (coercing India, punishing Russia).
% χ = 0.55 * π(analytical:1.15) * σ(global:1.2) = 0.759. High extraction.
% With beneficiaries, victims, and active enforcement, this is a canonical Tangled Rope.
constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_indian_tariffs_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_conditions) :-
    % Verify the analytical observer sees a Tangled Rope.
    constraint_indexing:constraint_classification(trump_indian_tariffs_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(trump_indian_tariffs_2026, ExtMetricName, E),
    E >= 0.46. % Ensures it's a high-extraction Snare/Tangled Rope.

:- end_tests(trump_indian_tariffs_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a geopolitical trade deal that functions as both coordination and coercion.
 *  - extractiveness: 0.55. High. The tariff structure itself is extractive, and its conditional reduction is a tool of coercive power.
 *  - suppression: 0.70. High. India's sovereign economic policy (sourcing energy) is suppressed, and Russia's market access is directly targeted.
 *  - theater_ratio: 0.20. Low. The deal has tangible economic and political consequences, it is not merely performative.
 *
 * The perspectival gap is stark. For US institutional actors, it's a beneficial coordination mechanism (Rope). For Indian exporters subject to the policy, it's a coercive trap (Snare). The analytical view, which must account for both the coordination benefits and the coercive extraction, correctly identifies it as a Tangled Rope.
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification is critical here. A simpler model might classify this as a Snare, focusing only on the coercion against Russia and India. However, that would ignore the genuine coordination function benefiting US importers and consumers. By requiring the declaration of beneficiaries, victims, and active enforcement, the system correctly identifies the hybrid nature of the constraint, preventing the misclassification of complex geopolitical agreements as simple extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_trump_indian_tariffs_2026,
    'Is India''s compliance with the oil purchase reduction sustainable against domestic pressure and its relationship with other BRICS nations?',
    'Monitor Indian oil import data from Russia, and track diplomatic statements from India, Russia, and China.',
    'If compliance is sustainable, the Tangled Rope holds. If not, the constraint may collapse or escalate into a more overt Snare (e.g., sanctions).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(trump_indian_tariffs_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. The narrative implies a shift from a
% more extractive/suppressive state (higher tariffs) to the current deal.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(trump_indian_tariffs_2026_tr_t0, trump_indian_tariffs_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(trump_indian_tariffs_2026_tr_t5, trump_indian_tariffs_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(trump_indian_tariffs_2026_tr_t10, trump_indian_tariffs_2026, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
% T=0 represents the pre-deal higher tariff regime.
narrative_ontology:measurement(trump_indian_tariffs_2026_ex_t0, trump_indian_tariffs_2026, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(trump_indian_tariffs_2026_ex_t5, trump_indian_tariffs_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(trump_indian_tariffs_2026_ex_t10, trump_indian_tariffs_2026, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This is a geopolitical deal enforced through economic levers.
narrative_ontology:coordination_type(trump_indian_tariffs_2026, enforcement_mechanism).

% Network relationships (structural influence edges)
% This deal would directly impact India's strategic alignment within economic blocs like BRICS.
narrative_ontology:affects_constraint(trump_indian_tariffs_2026, india_brics_alignment_2027).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */