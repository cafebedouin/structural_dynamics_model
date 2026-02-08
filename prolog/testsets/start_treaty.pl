% ============================================================================
% CONSTRAINT STORY: start_treaty
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_start_treaty, []).

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
 * * constraint_id: start_treaty
 * human_readable: START Treaty Expiration
 * domain: political
 * * SUMMARY:
 * The expiration of the New START treaty between the US and Russia removes legally binding limits on their nuclear arsenals for the first time in half a century. This creates uncertainty and increases the risk of a nuclear arms race. The constraint is the new, unregulated environment that emerges post-expiration.
 * * KEY AGENTS:
 * - Global Community: Subject (Powerless) / Victim
 * - Arms Manufacturers: Beneficiary (Institutional)
 * - Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(start_treaty, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(start_treaty, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(start_treaty, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(start_treaty, extractiveness, 0.55).
narrative_ontology:constraint_metric(start_treaty, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(start_treaty, theater_ratio, 0.2).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(start_treaty, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(start_treaty).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(start_treaty). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(start_treaty, arms_manufacturers).
narrative_ontology:constraint_victim(start_treaty, global_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% The global community is trapped in a more dangerous world with no alternative.
constraint_indexing:constraint_classification(start_treaty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Arms manufacturers see the lack of a treaty as a pure coordination mechanism
% for increased defense spending and production.
constraint_indexing:constraint_classification(start_treaty, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The situation has both a coordination function (residual norms, strategic
% posturing) and high asymmetric extraction (risk borne by populace).
constraint_indexing:constraint_classification(start_treaty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(start_treaty_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(start_treaty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(start_treaty, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint as intended.
    narrative_ontology:constraint_metric(start_treaty, extractiveness, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify that the conditions for a tangled_rope are met for the analytical perspective.
    constraint_indexing:constraint_classification(start_treaty, tangled_rope, context(agent_power(analytical), _, _, _)),
    domain_priors:requires_active_enforcement(start_treaty),
    narrative_ontology:constraint_beneficiary(start_treaty, _),
    narrative_ontology:constraint_victim(start_treaty, _).

:- end_tests(start_treaty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The expiration of the START treaty is modeled as a Tangled Rope from an analytical view, with significant perspectival gaps. The constraint is the new, unregulated strategic environment.
 *
 * *Subject (Snare)*: For the global community (powerless, trapped), the absence of the treaty is a Snare. It imposes the high cost and risk of a renewed arms race without their consent and with no viable alternative, suppressing global security for the benefit of a few.
 * *Beneficiary (Rope)*: From the perspective of an institutional beneficiary like an arms manufacturer, the *absence* of the treaty is a perfect `Rope`. It removes regulatory friction and coordinates national security policy and government spending towards armament, creating a predictable and profitable market. For this agent, the extraction is negligible and the system is purely enabling.
 * *Analytical (Tangled Rope)*: An analytical observer sees a system with both a coordination function (the remaining strategic doctrines and communication channels that prevent immediate catastrophe) and a severe asymmetric extraction function (the global populace bears the risk and cost, while arms manufacturers and hawkish political factions benefit). It requires active enforcement (military posturing, budget allocation) to sustain. This meets all requirements for a Tangled Rope.
 *
 * *PERSPECTIVAL GAP*: The gap is stark: what is a Snare of existential risk for the powerless is a Rope of economic opportunity for the institutional beneficiary. The Tangled Rope classification captures this duality.
 *
 * *MANDATROPHY ANALYSIS*: [RESOLVED MANDATROPHY] The Tangled Rope classification correctly identifies that this is not pure, functionless extraction (Snare). It acknowledges the residual coordination functions (strategic stability doctrines) that coexist with the extractive arms race, preventing a misclassification that would ignore the system's complex internal logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_start_treaty,
    'Will the expiration of the treaty trigger a new, unrestrained arms race, or will informal norms suffice to maintain stability?',
    'Monitoring of strategic weapons development, deployment, and national security doctrines by key state actors over the next decade.',
    'If a race begins, the extraction score is accurate. If norms hold, the extraction is lower and the constraint is closer to a pure Rope (for states) or Piton (the ghost of the treaty).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(start_treaty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the unregulated environment after
% the treaty expires. Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (remains low, indicating function over performance):
narrative_ontology:measurement(start_treaty_tr_t0, start_treaty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(start_treaty_tr_t5, start_treaty, theater_ratio, 5, 0.15).
narrative_ontology:measurement(start_treaty_tr_t10, start_treaty, theater_ratio, 10, 0.2).

% Extraction over time (models the gradual buildup of arsenals and risk):
narrative_ontology:measurement(start_treaty_ex_t0, start_treaty, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(start_treaty_ex_t5, start_treaty, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(start_treaty_ex_t10, start_treaty, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The new environment coordinates resource allocation towards armaments.
narrative_ontology:coordination_type(start_treaty, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */