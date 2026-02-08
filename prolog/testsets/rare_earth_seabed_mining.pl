% ============================================================================
% CONSTRAINT STORY: rare_earth_seabed_mining
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_rare_earth_seabed_mining, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: rare_earth_seabed_mining
 * human_readable: Deep-Sea Rare Earth Mining
 * domain: economic/technological/environmental
 * * SUMMARY:
 * Japan retrieves rare-earth-bearing seabed sediment in a deep-sea test near Minamitorishima Island. This represents a constraint due to the potential extraction of resources, environmental impact, and the international legal and geopolitical implications of deep-sea mining. The constraint involves balancing resource acquisition with environmental protection and international norms.
 * * KEY AGENTS:
 * - Marine Ecosystem: Subject (Powerless)
 * - Japan/Mining Companies: Beneficiary (Institutional)
 * - International Community/Scientists: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rare_earth_seabed_mining, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(rare_earth_seabed_mining, 0.45).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(rare_earth_seabed_mining, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(rare_earth_seabed_mining, extractiveness, 0.65).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(rare_earth_seabed_mining, theater_ratio, 0.2).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(rare_earth_seabed_mining, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(rare_earth_seabed_mining).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(rare_earth_seabed_mining). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, japan_and_mining_consortia).
narrative_ontology:constraint_victim(rare_earth_seabed_mining, marine_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
% The marine ecosystem is powerless and trapped, experiencing the mining as a pure Snare.
% χ = 0.65 * 1.5 (powerless) * 1.2 (global) = 1.17 (Snare)
constraint_indexing:constraint_classification(rare_earth_seabed_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
% For the institutional beneficiary, the extraction feels minimal and the coordination function is primary.
% χ = 0.65 * -0.2 (institutional) * 1.0 (national) = -0.13 (Rope)
constraint_indexing:constraint_classification(rare_earth_seabed_mining, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective reveals the hybrid nature: a coordination function (resource security)
% with high asymmetric extraction and required enforcement.
% χ = 0.65 * 1.15 (analytical) * 1.2 (global) = 0.897 (Tangled Rope)
constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% This perspective is not applicable as the constraint has no sunset clause.
% constraint_indexing:constraint_classification(rare_earth_seabed_mining, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(rare_earth_seabed_mining).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_seabed_mining_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    narrative_ontology:constraint_metric(rare_earth_seabed_mining, extractiveness, E),
    (E =< 0.15 ; E >= 0.46). % Ensures it's either low-extraction or high-extraction.

test(tangled_rope_conditions) :-
    % Verify that the conditions for a tangled_rope classification are met.
    constraint_indexing:constraint_classification(rare_earth_seabed_mining, tangled_rope, _),
    narrative_ontology:constraint_beneficiary(rare_earth_seabed_mining, _),
    narrative_ontology:constraint_victim(rare_earth_seabed_mining, _),
    domain_priors:requires_active_enforcement(rare_earth_seabed_mining).

:- end_tests(rare_earth_seabed_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness is high (0.65) due to the direct removal of valuable resources from a common-pool environment. The suppression score (0.45) reflects the technological and capital barriers that suppress alternative actors and the silencing of the primary victim (the ecosystem). The theater ratio is low (0.2) as the activity is functionally focused on resource extraction, not performance.
 * The perspectival gap is stark: the marine ecosystem (powerless) experiences the mining as a Snare, a destructive and inescapable trap. The state and corporate actors (institutional) view it as a Rope, a necessary coordination mechanism for securing strategic resources vital for technology. The analytical observer, weighing both the coordination function and the asymmetric extraction, classifies it as a Tangled Rope.
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical for preventing Mandatrophy. A simpler model might classify this as a pure Snare, ignoring the legitimate coordination goal of securing a national supply of critical materials. By acknowledging the coordination function (via `constraint_beneficiary`) alongside the asymmetric extraction (`constraint_victim`) and enforcement requirement, the system correctly identifies this as a hybrid constraint. This nuance is essential for policy analysis, which must balance resource needs against environmental costs, rather than simply condemning the act as pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rare_earth_seabed_mining,
    'What is the true, long-term ecological cost and recovery potential of the deep-sea ecosystem post-mining?',
    'Decades-long ecological monitoring of mined and control sites, coupled with advances in deep-sea biology.',
    'If cost is catastrophic and irreversible, the base extractiveness approaches 1.0 and it becomes a pure Snare from all but the institutional view. If recovery is faster than expected, extractiveness could be revised downwards.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rare_earth_seabed_mining, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% This models the shift from exploratory phases to full-scale industrial extraction.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rare_earth_seabed_mining_tr_t0, rare_earth_seabed_mining, theater_ratio, 0, 0.1).
narrative_ontology:measurement(rare_earth_seabed_mining_tr_t5, rare_earth_seabed_mining, theater_ratio, 5, 0.15).
narrative_ontology:measurement(rare_earth_seabed_mining_tr_t10, rare_earth_seabed_mining, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rare_earth_seabed_mining_ex_t0, rare_earth_seabed_mining, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(rare_earth_seabed_mining_ex_t5, rare_earth_seabed_mining, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(rare_earth_seabed_mining_ex_t10, rare_earth_seabed_mining, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The coordination is around enforcing national resource claims and managing
% the complex logistics of extraction.
narrative_ontology:coordination_type(rare_earth_seabed_mining, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(rare_earth_seabed_mining, 0.1).

% Network relationships (structural influence edges)
% The availability of rare earths from this source directly impacts the
% stability and structure of the global technology supply chain.
narrative_ontology:affects_constraint(rare_earth_seabed_mining, global_tech_supply_chain).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */