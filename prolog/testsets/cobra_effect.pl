% ============================================================================
% CONSTRAINT STORY: cobra_effect
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_cobra_effect, []).

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
 * * constraint_id: cobra_effect
 * human_readable: The Cobra Effect (Perverse Bounty Incentive)
 * domain: economic/political
 * * SUMMARY:
 * The Cobra Effect occurs when an incentive has an unintended and undesirable
 * result contrary to its design. Named after a colonial-era bounty on cobras
 * in Delhi, the policy inadvertently encouraged citizens to breed cobras to
 * claim the bounty, ultimately worsening the snake problem when the program
 * was cancelled and the now-worthless snakes were released.
 * * KEY AGENTS:
 * - Bitten Citizen: Subject (Powerless), suffers from the increased snake population.
 * - Colonial Administrator: Beneficiary (Institutional), sees the policy as a tool.
 * - Cobra Breeder: Beneficiary (Moderate), exploits the incentive for profit.
 * - Systems Analyst: Auditor (Analytical), observes the structural flaws.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cobra_effect, 0.60). % Snare extraction >= 0.46. The policy extracts capital from the state and safety from the public.
domain_priors:suppression_score(cobra_effect, 0.40).   % Structural property (raw, unscaled). Alternatives (professional pest control) were suppressed by the perceived simplicity of the bounty.
domain_priors:theater_ratio(cobra_effect, 0.10).       % Piton detection (>= 0.70). Low theater; the policy was functional, just perversely so.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cobra_effect, extractiveness, 0.60).
narrative_ontology:constraint_metric(cobra_effect, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(cobra_effect, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The bounty program was presented as a coordination mechanism to solve a public problem.
narrative_ontology:constraint_claim(cobra_effect, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(cobra_effect). % Required for Tangled Rope. The bounty must be actively paid out.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(cobra_effect, colonial_administrators).
narrative_ontology:constraint_beneficiary(cobra_effect, cobra_breeders).
narrative_ontology:constraint_victim(cobra_effect, public_safety_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE BITTEN CITIZEN (SNARE)
% Experiences the policy's failure as a direct, inescapable threat to safety.
% χ = 0.60 * 1.5 (powerless) * 0.8 (local) = 0.72
constraint_indexing:constraint_classification(cobra_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE COLONIAL ADMINISTRATOR (ROPE)
% Views the bounty as a simple, effective coordination tool to achieve a public good.
% χ = 0.60 * -0.2 (institutional) * 0.9 (regional) = -0.108
constraint_indexing:constraint_classification(cobra_effect, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes both the coordination function (paying bounties) and the severe
% asymmetric extraction (public harm, wasted funds).
% χ = 0.60 * 1.15 (analytical) * 1.2 (global) = 0.828
constraint_indexing:constraint_classification(cobra_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cobra_effect_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cobra_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cobra_effect, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(cobra_effect, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction is in the high-extraction band for Snare/Tangled Rope.
    narrative_ontology:constraint_metric(cobra_effect, extractiveness, E),
    E >= 0.46.

:- end_tests(cobra_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Cobra Effect is a canonical example of a policy that is a Rope from the
 * perspective of its architect (institutional power) but a Snare for those
 * who suffer its consequences (powerless). The base extractiveness of 0.60
 * reflects the significant waste of public funds and the severe negative
 * externality of increased public danger. The suppression score of 0.40
 * acknowledges that simpler, more direct solutions (like hiring professional
 * snake catchers) were overlooked in favor of a market-based incentive scheme.
 *
 * The analytical perspective resolves this as a Tangled Rope. It correctly
 * identifies that the policy has a genuine coordination function (mobilizing
 * people via payment) but that this function is inseparable from its severe
 * asymmetric extraction. This classification is only possible because all three
 * structural requirements are met: it has beneficiaries (coordination), victims
 * (asymmetric extraction), and requires active enforcement (paying the bounty).
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] Classifying this as a Tangled Rope prevents the system
 * from making a simplistic judgment. It is not a pure Snare, because it did
 * have a (flawed) coordination goal. It is not a Rope, because the extraction
 * is undeniable. The Tangled Rope classification captures the nuance that a
 * well-intentioned coordination mechanism became pathologically extractive
 * due to a failure to anticipate second-order effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cobra_effect,
    'At what bounty price point does opportunistic collection of wild snakes transition to industrial-scale breeding for profit?',
    'Historical economic analysis of bounty claims vs. estimated wild population, or a modern behavioral economics experiment simulating the incentive structure.',
    'If the threshold is very low, almost any such bounty is a latent Snare. If high, small-scale bounties can remain effective Ropes.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cobra_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy degraded over time. It began as a low-extraction attempt at
% coordination and became highly extractive as citizens learned to game the system.
% This progression is required data since base_extractiveness > 0.46.

% Theater ratio over time (remains low, the policy was always functional):
narrative_ontology:measurement(cobra_effect_tr_t0, cobra_effect, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cobra_effect_tr_t5, cobra_effect, theater_ratio, 5, 0.08).
narrative_ontology:measurement(cobra_effect_tr_t10, cobra_effect, theater_ratio, 10, 0.10).

% Extraction over time (starts low, ends high as gaming becomes rampant):
narrative_ontology:measurement(cobra_effect_ex_t0, cobra_effect, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cobra_effect_ex_t5, cobra_effect, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cobra_effect_ex_t10, cobra_effect, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The bounty system is a classic resource allocation mechanism.
narrative_ontology:coordination_type(cobra_effect, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */