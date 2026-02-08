% ============================================================================
% CONSTRAINT STORY: mutual_defection_equilibrium
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_mutual_defection_equilibrium, []).

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
 * * constraint_id: mutual_defection_equilibrium
 * human_readable: The Infinite Prisoner's Dilemma Stalemate
 * domain: economic/social
 * * SUMMARY:
 * This constraint represents a stable social state where all agents defect
 * because no single agent can gain by switching to cooperation alone.
 * While the state is optimal for no one, it acts as a 'Mountain' of
 * inescapable logic for the individual and a 'Tangled Rope' for the collective,
 * mixing a coordination function (predictability) with massive extraction
 * (lost cooperative surplus).
 * * KEY AGENTS:
 * - Individual Actor: Subject (Powerless), Victim
 * - Systemic Incentive Structure: Beneficiary (Institutional)
 * - Game Theory Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is extreme (0.82) because the equilibrium wastes the vast majority
% of potential surplus that cooperation would generate.
domain_priors:base_extractiveness(mutual_defection_equilibrium, 0.82).
domain_priors:suppression_score(mutual_defection_equilibrium, 0.70).
domain_priors:theater_ratio(mutual_defection_equilibrium, 0.15). % Low theater; it is a raw logical limit.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(mutual_defection_equilibrium, extractiveness, 0.82).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(mutual_defection_equilibrium, theater_ratio, 0.15).

% The constraint claims to be a form of coordination, providing stability.
narrative_ontology:constraint_claim(mutual_defection_equilibrium, tangled_rope).

% The logic of the prisoner's dilemma itself enforces the equilibrium.
% Any agent attempting to cooperate is immediately punished. This is required for Tangled Rope.
domain_priors:requires_active_enforcement(mutual_defection_equilibrium).

% Structural property derivation hooks for Tangled Rope:
% has_coordination_function/1 is derived from constraint_beneficiary/2
% has_asymmetric_extraction/1 is derived from constraint_victim/2
narrative_ontology:constraint_beneficiary(mutual_defection_equilibrium, systemic_incentive_structure).
narrative_ontology:constraint_victim(mutual_defection_equilibrium, individual_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To the powerless individual, the need to defect is an unchangeable law of survival.
% Cooperating alone leads to certain exploitation.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institutional structure 'views' this state as a successful coordination
% mechanism that ensures predictable (if low-value) behavior and prevents shocks.
constraint_indexing:constraint_classification(mutual_defection_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% From a systems perspective, the equilibrium is a Tangled Rope. It has a genuine
% coordination function (creating a stable, predictable state) but this function
% is coupled with extreme asymmetric extraction (the loss of all potential
% cooperative surplus, borne by the individual actors).
constraint_indexing:constraint_classification(mutual_defection_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mutual_defection_equilibrium_tests).

test(perspectival_gap) :-
    % Verify the 'Individual' sees a Mountain (necessity) while the 'Auditor' sees a Tangled Rope (trap).
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, mountain,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mutual_defection_equilibrium, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all necessary conditions for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(mutual_defection_equilibrium),
    narrative_ontology:constraint_beneficiary(mutual_defection_equilibrium, _),
    narrative_ontology:constraint_victim(mutual_defection_equilibrium, _).

:- end_tests(mutual_defection_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This file was regenerated to fix multiple structural lint errors.
 * 1.  Tangled Rope Requirements: Added `requires_active_enforcement/1`,
 *     `constraint_beneficiary/2`, and `constraint_victim/2` facts. These are
 *     mandatory for a `tangled_rope` classification, as it must possess both a
 *     coordination function (derived from beneficiary) and asymmetric extraction
 *     (derived from victim), backed by enforcement.
 * 2.  Temporal Data: Added `measurement/5` facts for `base_extractiveness` and
 *     `theater_ratio` because the base extraction (0.82) is > 0.46. This data
 *     models the equilibrium becoming more entrenched over time.
 * 3.  Boltzmann Data: Added `constraint_claim/2` and `coordination_type/2` to
 *     enable deeper system analysis. The claim is `coordination`, which the
 *     system can test against the high extraction value.
 *
 * PERSPECTIVAL GAP:
 * The Individual Actor feels a Mountain because they have no degrees of freedom;
 * to cooperate is to be exploited. The Analytical Auditor identifies a Tangled
 * Rope because the system is a stable equilibrium (coordination) that is
 * simultaneously designed (intentionally or not) to extract all potential
 * cooperative surplus from its actors.
 *
 * [RESOLVED MANDATROPHY]:
 * Mandatrophy is resolved by the Tangled Rope classification. It correctly
 * identifies that 'mutual defection' is a form of coordination—it creates a
 * stable, predictable environment—but that this coordination is coupled with
 * extreme, structurally enforced extraction. It avoids misclassifying the
 * constraint as a pure Snare (ignoring the stability function) or a Rope
 * (ignoring the massive lost surplus).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for extraction (0.82) > 0.46.
omega_variable(
    omega_mde_cooperation_barrier,
    'Is the defection equilibrium due to a fundamental lack of trust (Snare) or an insurmountable communication/verification cost (Mountain)?',
    'Implementation of a zero-cost, un-hackable, high-bandwidth reputation ledger.',
    'If cooperation emerges: Snare. If defection remains: Mountain of computational/social complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mutual_defection_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% Models the equilibrium becoming more entrenched as trust erodes over time.
%
% Theater ratio over time (minor increase in performative justification):
narrative_ontology:measurement(mde_tr_t0, mutual_defection_equilibrium, theater_ratio, 0, 0.10).
narrative_ontology:measurement(mde_tr_t5, mutual_defection_equilibrium, theater_ratio, 5, 0.12).
narrative_ontology:measurement(mde_tr_t10, mutual_defection_equilibrium, theater_ratio, 10, 0.15).

% Extraction over time (lost surplus grows as cooperative pathways atrophy):
narrative_ontology:measurement(mde_ex_t0, mutual_defection_equilibrium, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(mde_ex_t5, mutual_defection_equilibrium, base_extractiveness, 5, 0.79).
narrative_ontology:measurement(mde_ex_t10, mutual_defection_equilibrium, base_extractiveness, 10, 0.82).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The equilibrium is a behavioral enforcement mechanism.
narrative_ontology:coordination_type(mutual_defection_equilibrium, enforcement_mechanism).

% Network relationship: This game-theoretic trap is a foundational model for
% many other constraints, such as arms races or market failures.
narrative_ontology:affects_constraint(mutual_defection_equilibrium, international_arms_races).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */