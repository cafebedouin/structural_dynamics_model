% ============================================================================
% CONSTRAINT STORY: reputational_cascade_failure
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_reputational_cascade_failure, []).

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
 *   constraint_id: reputational_cascade_failure
 *   human_readable: The Social Liquidity Trap
 *   domain: social/economic/informational
 *
 * SUMMARY:
 *   A scenario where a single piece of unverified but high-fitness negative
 *   information triggers a self-reinforcing loop of social disinvestment.
 *   This "Rope" for rapid group coordination and accountability becomes a
 *   "Snare" for the individual, whose entire life-surplus (economic access,
 *   social ties) is liquidated in seconds by an automated network reaction,
 *   trapping them in a state of permanent exclusion with no path for
 *   informational recovery or due process.
 *
 * KEY AGENTS (by structural relationship):
 *   - Targeted Individual: Primary target (powerless/trapped) — bears extraction
 *   - Social Platform Operators: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Network Dynamics Analyst: Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reputational_cascade_failure, 0.91).
domain_priors:suppression_score(reputational_cascade_failure, 0.82).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(reputational_cascade_failure, 0.87).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reputational_cascade_failure, extractiveness, 0.91).
narrative_ontology:constraint_metric(reputational_cascade_failure, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(reputational_cascade_failure, theater_ratio, 0.87).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(reputational_cascade_failure, tangled_rope).
narrative_ontology:human_readable(reputational_cascade_failure, "The Social Liquidity Trap").

% --- Binary flags ---
domain_priors:requires_active_enforcement(reputational_cascade_failure). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(reputational_cascade_failure, social_platform_operators).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(reputational_cascade_failure, targeted_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE TARGETED INDIVIDUAL (SNARE)
% The individual is trapped: the cascade moves faster than legal or
% logical intervention, liquidating their ability to exist within the system.
% Engine derives d from victim status + trapped exit -> d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PLATFORM OPERATOR (ROPE)
% The platform/institution views the cascade as a Rope—the ultimate
% coordination tool for enforcing "consensus" and maintaining
% high-velocity social order.
% Engine derives d from beneficiary status + arbitrage exit -> d ≈ 0.05 -> low/negative χ.
constraint_indexing:constraint_classification(reputational_cascade_failure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical context detects extreme extraction (0.91) masking as
% coordination (Rope), classifying it as a Tangled Rope. This is the basis
% for the constraint_claim.
constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% A secondary analytical view focused on the performative "Appeals Process".
% The high theater ratio (0.87 > 0.70) triggers a Piton classification,
% identifying the process as an inertial spike that lacks functional authority.
constraint_indexing:constraint_classification(reputational_cascade_failure, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reputational_cascade_failure_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless subject vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.87) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(reputational_cascade_failure, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(reputational_cascade_failure, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(reputational_cascade_failure, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(reputational_cascade_failure).

:- end_tests(reputational_cascade_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction score (0.91) reflects a state where the "coordination"
 *   benefit of social accountability is achieved by liquidating the subject's
 *   entire life-territory. The suppression score (0.82) represents the
 *   algorithmic suppression of counter-narratives, which cannot propagate
 *   as quickly as the initial high-fitness negative signal. The high theater
 *   ratio (0.87) points to performative "appeals processes" that have no
 *   causal power over the automated cascade, classifying the system as a Piton
 *   from one analytical view.
 *
 * PERSPECTIVAL GAP:
 *   The Targeted Individual experiences a Snare because the network consumes them
 *   based on a "hallucination" of consensus. The Platform Operator sees a Rope because
 *   the cascade coordinates massive behavioral alignment at near-zero marginal cost.
 *   The Analytical Observer sees a Tangled Rope, recognizing both the coordination
 *   function and the asymmetric, life-liquidating extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the clear beneficiary/victim structure.
 *   - Beneficiary: `social_platform_operators`. They benefit from the engagement
 *     and perceived order-enforcement function of the cascade. Their institutional
 *     power and arbitrage exit options lead to a low derived `d` value (~0.05),
 *     producing a negative effective extraction (χ) and a Rope classification.
 *   - Victim: `targeted_individual`. They bear the full, unrecoverable cost.
 *     Their powerless status and trapped exit options lead to a high derived `d`
 *     value (~0.95), maximizing effective extraction (χ) and producing a Snare.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The extreme extraction (0.91) creates a Mandatrophy risk: the system might
 *   misclassify this as a pure Snare, ignoring its coordination function, or
 *   as a pure Rope, ignoring the devastating extraction. The Tangled Rope
 *   classification resolves this by acknowledging both properties simultaneously.
 *   It correctly identifies a mechanism that provides a genuine coordination
 *   benefit to one group (the platform) while imposing asymmetric, life-liquidating
 *   extraction on another (the individual), maintained by active (algorithmic)
 *   enforcement. This prevents the system from collapsing the analysis into a
 *   simplistic good/bad binary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cascade_reversal,
    'Can a reputation be "un-liquidated" once the network commits, or is the Snare terminal (Snare vs Mountain)?',
    'Tracking the success rate of public exoneration in restoring economic access scores.',
    'If recovery fails: Mountain of Permanent Stigma. If recovery holds: Snare of current system design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(reputational_cascade_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system evolved from a genuine community tool to an automated,
% extractive enforcement mechanism.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(rcf_tr_t0, reputational_cascade_failure, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rcf_tr_t5, reputational_cascade_failure, theater_ratio, 5, 0.65).
narrative_ontology:measurement(rcf_tr_t10, reputational_cascade_failure, theater_ratio, 10, 0.87).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(rcf_ex_t0, reputational_cascade_failure, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(rcf_ex_t5, reputational_cascade_failure, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(rcf_ex_t10, reputational_cascade_failure, base_extractiveness, 10, 0.91).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(reputational_cascade_failure, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */