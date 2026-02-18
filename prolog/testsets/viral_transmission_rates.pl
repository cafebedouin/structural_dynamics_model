% ============================================================================
% CONSTRAINT STORY: viral_transmission_rates
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_viral_transmission_rates, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: viral_transmission_rates
 *   human_readable: Socio-Political Response to Viral Transmission
 *   domain: political/technological
 *
 * SUMMARY:
 *   This constraint models the socio-political system of mandates, lockdowns,
 *   and behavioral controls enacted in response to a biological reality (viral
 *   transmission). While ostensibly a coordination mechanism for public health,
 *   this system imposes severe, asymmetric costs on specific populations,
 *   extracting economic agency, mobility, and labor opportunities. The constraint
 *   is the policy layer, not the virus itself.
 *
 * KEY AGENTS (by structural relationship):
 *   - Precarious Laborers: Primary target (powerless/trapped) — bears the dual extraction of economic shutdown and infection risk.
 *   - Public Health Governance: Primary beneficiary (institutional/arbitrage) — gains coordination capacity and authority.
 *   - Small Business Owners: Secondary target (moderate/constrained) — experiences both the coordination benefits and extractive costs.
 *   - The Epidemiologist: Analytical observer — sees the full structure of the biological drivers and the policy response.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The policy layer, not the virus, is the source of extraction.
% Lockdowns, mandates, and business closures extract immense economic value and
% agency from the population. ε=0.70 reflects this severe economic cost.
domain_priors:base_extractiveness(viral_transmission_rates, 0.70).
% Suppression is high because alternative strategies (e.g., focused protection)
% were actively suppressed, leaving compliance as the only viable option.
domain_priors:suppression_score(viral_transmission_rates, 0.80).
domain_priors:theater_ratio(viral_transmission_rates, 0.40).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_transmission_rates, extractiveness, 0.70).
narrative_ontology:constraint_metric(viral_transmission_rates, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(viral_transmission_rates, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
% The analytical view sees the entire system (policy + biology) which has a
% coordination function but also massive, asymmetric extraction. This is a Tangled Rope.
narrative_ontology:constraint_claim(viral_transmission_rates, tangled_rope).
narrative_ontology:human_readable(viral_transmission_rates, "Socio-Political Response to Viral Transmission").
narrative_ontology:topic_domain(viral_transmission_rates, "political/technological").

% --- Binary flags ---
% The policy mandates require constant state enforcement (e.g., fines, closures).
domain_priors:requires_active_enforcement(viral_transmission_rates).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(viral_transmission_rates, public_health_governance).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(viral_transmission_rates, precarious_laborers).
narrative_ontology:constraint_victim(viral_transmission_rates, small_business_owners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRECARIOUS LABORER (SNARE)
% Agent who bears the most extraction. Trapped between infection risk and
% starvation, with policy suppressing their ability to earn a living.
constraint_indexing:constraint_classification(viral_transmission_rates, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PUBLIC HEALTH GOVERNANCE (ROPE)
% Agent who benefits most. For the institution, the effective reproduction
% number (Rt) is a tool for population coordination, justifying collective
% action to prevent systemic collapse.
constraint_indexing:constraint_classification(viral_transmission_rates, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The epidemiologist sees the full picture: a necessary coordination function
% (the Rope part) entangled with severe, asymmetrically applied extraction
% (the Snare part). The high ε and suppression are undeniable.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SMALL BUSINESS OWNER (TANGLED ROPE)
% The business owner needs the system to coordinate safety so customers return
% (Rope), but the mandates asymmetrically extract their revenue and viability
% (Snare). They experience the hybrid nature directly.
constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_transmission_rates_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(viral_transmission_rates, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify that the base metrics are in the high-extraction range.
    domain_priors:base_extractiveness(viral_transmission_rates, E),
    domain_priors:suppression_score(viral_transmission_rates, S),
    E >= 0.46,
    S >= 0.60.

test(hybrid_tangled_rope_detection) :-
    % Ensure moderate power detects the coordination/extraction hybrid.
    constraint_indexing:constraint_classification(viral_transmission_rates, tangled_rope, context(agent_power(moderate), _, _, _)).

:- end_tests(viral_transmission_rates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (0.70) and suppression (0.80) reflect the
 *   socio-economic impact of the policy response, not the biological properties
 *   of the virus. The constraint being modeled is the governance system. The
 *   `constraint_claim` is `tangled_rope` because the analytical perspective
 *   must account for both the genuine public health coordination function
 *   (beneficiary exists) and the severe, asymmetric extraction (victim exists,
 *   high ε and suppression). This resolves the original file's MOUNTAIN_METRIC_CONFLICT
 *   by correctly identifying the object of analysis as the policy layer.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For public health institutions, the system is a Rope—a
 *   tool for managing a crisis. For the precarious worker, it is a Snare—a
 *   trap that removes all viable options. For the analyst and the small
 *   business owner, it is a Tangled Rope—a hybrid of both realities.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `public_health_governance` benefits from increased authority and a mechanism for societal coordination.
 *   - Victims: `precarious_laborers` and `small_business_owners` bear the direct economic costs of shutdowns and mandates.
 *   This clear structural division drives the perspectival differences in classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids mislabeling the system as a pure Snare
 *   (which would ignore the coordination function) or a pure Rope (which would
 *   ignore the massive extraction). The Tangled Rope classification acknowledges
 *   the dual nature of the policy, preventing the "it's for the greater good"
 *   justification from obscuring the severe costs imposed on the powerless.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viral_extraction_intent,
    "Is the extraction of economic agency a functional necessity for biosecurity or a predatory centralization of power?",
    "Audit of state emergency power rollbacks vs. virus prevalence normalization",
    "If necessity: Tangled Rope (unavoidable extraction). If predatory: Snare (Mandatrophy).",
    confidence_without_resolution(medium)
).

omega_variable(
    herd_immunity_mechanism,
    "Is the threshold for endemic stability reached via natural infection (Snare) or vaccination (Rope)?",
    "Seroprevalence study comparison with vaccine uptake metrics",
    "If natural: high victim extraction. If vaccine: coordination success.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_transmission_rates, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time:
narrative_ontology:measurement(viral_tr_t0, viral_transmission_rates, theater_ratio, 0, 0.10).
narrative_ontology:measurement(viral_tr_t5, viral_transmission_rates, theater_ratio, 5, 0.45).
narrative_ontology:measurement(viral_tr_t10, viral_transmission_rates, theater_ratio, 10, 0.40).

% Extraction over time, tracking the intensification of biosecurity extraction:
narrative_ontology:measurement(viral_ex_t0, viral_transmission_rates, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(viral_ex_t5, viral_transmission_rates, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(viral_ex_t10, viral_transmission_rates, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The policy layer acts as an enforcement mechanism for public health goals.
narrative_ontology:coordination_type(viral_transmission_rates, enforcement_mechanism).

% --- Network Decomposition (Constraint Families) ---
%
% DUAL FORMULATION NOTE:
% This constraint models the socio-political RESPONSE to viral spread. It is
% one of two stories decomposed from the colloquial label "the pandemic".
% Decomposed because ε differs across observables (ε-invariance principle).
% The biological reality is a separate, upstream constraint.
% Related stories:
%   - viral_kinetics (ε=0.05, Mountain)
%
narrative_ontology:affects_constraint(viral_kinetics, viral_transmission_rates).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural derivation from beneficiary/victim
% declarations accurately models the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */