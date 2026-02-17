% ============================================================================
% CONSTRAINT STORY: fiscal_equalization_friction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-15
% ============================================================================

:- module(constraint_fiscal_equalization_friction, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fiscal_equalization_friction
 *   human_readable: The Equalization Conflict (Net Transfer Friction)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Models the coordination mechanism of fiscal federalism in Canada, specifically
 *   the equalization program. This system transfers funds from wealthier
 *   ("have") provinces to less wealthy ("have-not") provinces to ensure
 *   comparable levels of public services. From the perspective of net
 *   contributor provinces like Alberta, this constitutional coordination tool
 *   is often perceived as a "siphoning" of resource wealth.
 *
 * KEY AGENTS (by structural relationship):
 *   - individual_taxpayer_in_alberta: Primary target (powerless/trapped) — Experiences tax outflow with no direct control or perceived return.
 *   - provincial_nationalist_bloc: Secondary target (moderate/constrained) — Politically frames the net fiscal outflow as extraction to argue for sovereignty.
 *   - federal_government: Primary beneficiary (institutional/arbitrage) — Maintains national standards and political stability through redistribution.
 *   - recipient_provinces: Secondary beneficiary (institutional/constrained) — Relies on transfers for service parity.
 *   - fiscal_analyst: Analytical observer — Evaluates net contribution formulas and their economic impact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fiscal_equalization_friction, 0.32).
domain_priors:suppression_score(fiscal_equalization_friction, 0.15).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(fiscal_equalization_friction, 0.45).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fiscal_equalization_friction, extractiveness, 0.32).
narrative_ontology:constraint_metric(fiscal_equalization_friction, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fiscal_equalization_friction, theater_ratio, 0.45).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(fiscal_equalization_friction, rope).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(fiscal_equalization_friction, federal_government_and_recipient_provinces).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(fiscal_equalization_friction, net_contributor_provinces).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% An individual taxpayer in a net contributor province. They are a victim of the
% extraction and trapped within the federal system. The engine derives a high
% d value, leading to high effective extraction (χ) and a Snare classification.
constraint_indexing:constraint_classification(fiscal_equalization_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The federal government, which uses the system to maintain national standards.
% As a beneficiary with arbitrage exit (policy control), the engine derives a
% low/negative d value, resulting in low/negative χ and a Rope classification.
constraint_indexing:constraint_classification(fiscal_equalization_friction, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PROVINCIAL NATIONALIST (SNARE)
% A political actor in a contributor province. From this perspective, the lack
% of an exit option from federal transfers makes the coordination function
% appear as pure extraction, a tool to justify sovereignty claims.
constraint_indexing:constraint_classification(fiscal_equalization_friction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% Recognizes a Rope with high friction due to asymmetric wealth distribution.
% The coordination function is real, but the large-scale transfers create
% significant political and economic tension.
constraint_indexing:constraint_classification(fiscal_equalization_friction, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fiscal_equalization_friction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(fiscal_equalization_friction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fiscal_equalization_friction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_claim_matches_rope) :-
    constraint_indexing:constraint_classification(fiscal_equalization_friction, rope, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(fiscal_equalization_friction, rope).

:- end_tests(fiscal_equalization_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is analytically a Rope because the transfer system serves a
 *   genuine, constitutional coordination purpose (parity of services across
 *   the federation). However, the base extractiveness (ε=0.32) is significant,
 *   reflecting the large scale of the fiscal outflow from Alberta's
 *   petro-economy. This high ε value is what creates the political friction
 *   and enables the Snare perception from those who bear the costs.
 *
 * PERSPECTIVAL GAP:
 *   The gap is driven by directionality and exit options. For the federal
 *   government (beneficiary, arbitrage exit), it's a tool for stability (Rope).
 *   For an individual taxpayer in Alberta (victim, trapped exit), it's an
 *   inescapable outflow of wealth (Snare). The provincial nationalist bloc
 *   (moderate power, trapped) weaponizes this Snare perception for political gain.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary is the federal system itself, which gains stability, and
 *   the recipient provinces, which gain funding. The victim group is the
 *   population of the net contributor provinces, whose provincial tax base is
 *   effectively reduced before it can be used for local priorities.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Rope from the analytical perspective is crucial. It
 *   prevents the total erasure of the coordination function (service parity)
 *   that would occur if it were labeled a Snare based solely on the provincial
 *   perspective. The framework correctly identifies that a single mechanism can be
 *   both a coordination tool and a source of asymmetric extraction, but its
 *   fundamental nature here is coordination, albeit a highly contentious one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_fair_deal_metric,
    'What constitutes a "fair" net contribution in a resource-rich federation?',
    'Comparative analysis of Australian and German fiscal federalism models.',
    'Determines if separatist rhetoric is based on empirical Snare-like extraction or ideological drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_fair_deal_metric, conceptual, 'Definitional threshold of fiscal fairness in a federal system').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fiscal_equalization_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction is not high enough to require this, but data is included
% to model the rising political tension over the interval.

% Theater ratio over time (political signaling vs. actual formula change):
narrative_ontology:measurement(fef_tr_t0, fiscal_equalization_friction, theater_ratio, 0, 0.20).
narrative_ontology:measurement(fef_tr_t5, fiscal_equalization_friction, theater_ratio, 5, 0.35).
narrative_ontology:measurement(fef_tr_t10, fiscal_equalization_friction, theater_ratio, 10, 0.45).

% Extraction over time (reflects changes in resource prices and formula):
narrative_ontology:measurement(fef_ex_t0, fiscal_equalization_friction, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fef_ex_t5, fiscal_equalization_friction, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(fef_ex_t10, fiscal_equalization_friction, base_extractiveness, 10, 0.32).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(fiscal_equalization_friction, resource_allocation).

% Network relationships (structural influence edges)
% Equalization friction provides the narrative fuel for separatist arbitrage.
narrative_ontology:affects_constraint(fiscal_equalization_friction, sovereignty_as_arbitrage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */