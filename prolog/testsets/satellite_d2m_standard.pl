% ============================================================================
% CONSTRAINT STORY: satellite_d2m_standard
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-12-14
% ============================================================================

:- module(constraint_satellite_d2m_standard, []).

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
    narrative_ontology:coordination_type/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: satellite_d2m_standard
 *   human_readable: Direct-to-Mobile (D2M) Satellite Communication Standard
 *   domain: technological
 *
 * SUMMARY:
 *   The constraint represents the emerging technological standard and
 *   infrastructure for direct-to-mobile (D2M) satellite broadband, as
 *   exemplified by the 'Bluebird' satellite deployed by ISRO for Omnispace.
 *   This standard enables unmodified smartphones to connect directly to
 *   satellites, providing connectivity in areas without terrestrial coverage.
 *   The system creates a new market for connectivity but also establishes
 *   a dependency on a small number of global satellite operators.
 *
 * KEY AGENTS (by structural relationship):
 *   - Remote Area Residents: Primary target/user (powerless/trapped) — gains connectivity but is a price-taker with few alternatives.
 *   - Satellite Operators (e.g., Omnispace): Primary commercial beneficiary (institutional/arbitrage) — captures revenue from a new, captive market.
 *   - National Space Agencies (e.g., ISRO): Primary public beneficiary (institutional/constrained) — achieves strategic goals and national mandates.
 *   - Legacy Satellite Providers: Incumbent victim (organized/constrained) — business model is disrupted by lower-cost, hardware-agnostic competition.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(satellite_d2m_standard, 0.45). % Significant value capture from users, balanced by high infrastructure costs.
domain_priors:suppression_score(satellite_d2m_standard, 0.48).  % Suppresses lack of an alternative, but also suppresses competition from legacy/terrestrial solutions in marginal areas. Unscaled structural property.
domain_priors:theater_ratio(satellite_d2m_standard, 0.15).      % Highly functional; theater is limited to marketing and national pride announcements.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(satellite_d2m_standard, extractiveness, 0.45).
narrative_ontology:constraint_metric(satellite_d2m_standard, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(satellite_d2m_standard, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(satellite_d2m_standard, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(satellite_d2m_standard). % Requires constant satellite maintenance, spectrum licensing, and billing systems.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(satellite_d2m_standard, satellite_operators).
narrative_ontology:constraint_beneficiary(satellite_d2m_standard, national_space_agencies).
narrative_ontology:constraint_beneficiary(satellite_d2m_standard, remote_area_residents). % Beneficiary of the service itself.

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(satellite_d2m_standard, remote_area_residents). % Victim of the pricing structure and lack of alternatives.
narrative_ontology:constraint_victim(satellite_d2m_standard, legacy_satellite_providers). % Victim of market disruption.

% Gate requirements met:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement -> YES.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (REMOTE AREA RESIDENT)
% Is both a beneficiary (gains service) and victim (pays monopoly price).
% The 'trapped' exit option dominates the derivation, yielding a high d.
% Engine derives d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.45 * 1.42 * 1.0 (national) = 0.639.
% This χ is in [0.40, 0.90] with ε >= 0.30 and suppression >= 0.40,
% classifying as a Tangled Rope, not a Snare. It's highly extractive but
% provides a genuine coordination function (connectivity).
constraint_indexing:constraint_classification(satellite_d2m_standard, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY COMMERCIAL BENEFICIARY (SATELLITE OPERATOR)
% Agent who captures revenue.
% Engine derives d from: beneficiary + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.45 * -0.12 * 1.2 (global) = -0.0648.
% Negative χ indicates a pure coordination function from this view.
constraint_indexing:constraint_classification(satellite_d2m_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.45 * 1.15 * 1.2 (global) = 0.621.
% This χ value is squarely in the Tangled Rope range [0.40, 0.90].
constraint_indexing:constraint_classification(satellite_d2m_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: INTER-INSTITUTIONAL (NATIONAL SPACE AGENCY)
% A public institution achieving a strategic mandate.
% Engine derives d from: beneficiary + constrained exit → d ≈ 0.25 → f(d) ≈ 0.15.
% The 'constrained' exit reflects being bound by national policy.
% χ = 0.45 * 0.15 * 1.0 (national) = 0.0675.
% Low χ classifies it as a pure coordination mechanism (Rope) for this actor.
constraint_indexing:constraint_classification(satellite_d2m_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(satellite_d2m_standard_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the gap between the end-user (target) and operator (beneficiary).
    constraint_indexing:constraint_classification(satellite_d2m_standard, tangled_rope,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(satellite_d2m_standard, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(perspectival_gap_inter_institutional) :-
    % Verify the gap between the commercial and public institutional actors.
    % Both classify as Rope, but the engine calculates a different χ due to
    % different exit options (arbitrage vs. constrained).
    constraint_indexing:get_chi(satellite_d2m_standard, context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global)), ChiCommercial),
    constraint_indexing:get_chi(satellite_d2m_standard, context(agent_power(institutional), time_horizon(generational), exit_options(constrained), spatial_scope(national)), ChiPublic),
    ChiCommercial < 0,
    ChiPublic > 0,
    ChiCommercial \= ChiPublic.

test(analytical_claim_matches) :-
    % The analytical observer's classification must match the declared claim.
    narrative_ontology:constraint_claim(satellite_d2m_standard, Claim),
    constraint_indexing:constraint_classification(satellite_d2m_standard, Claim,
        context(agent_power(analytical), _, _, _)).

:- end_tests(satellite_d2m_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.45): Set high to reflect the potential for monopoly
 *     pricing in captive markets, but below the Snare threshold (0.46) because
 *     the service provides genuine, high-cost infrastructure value.
 *   - Suppression (0.48): The standard primarily suppresses the absence of
 *     service but also raises barriers to entry for competing technologies.
 *   - The combination of a clear coordination function (a global standard) and
 *     significant asymmetric extraction makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the satellite operator (institutional/arbitrage), it is a
 *   perfect Rope: a coordination tool that generates revenue (negative effective
 *   extraction). For the remote resident (powerless/trapped), it is a Tangled Rope
 *   bordering on a Snare: an essential service for which they must pay a high
 *   price due to a lack of alternatives. This difference is driven entirely by
 *   their structural positions, which the directionality 'd' parameter captures.
 *
 * DIRECTIONALITY LOGIC:
 *   - The beneficiaries are the operators who extract revenue and the space
 *     agencies who fulfill strategic mandates.
 *   - The primary victims are the same residents who benefit from the service.
 *     They are beneficiaries of the technology but victims of the business model.
 *     This dual membership is a key indicator of a Tangled Rope. The engine
 *     correctly weights their `trapped` exit status to derive a high `d`,
 *     reflecting their position as price-takers.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The constraint is experienced differently by the commercial operator
 *   (Omnispace) and the public agency (ISRO). Omnispace has `arbitrage` exit;
 *   it can sell assets or the company itself. ISRO has a `constrained` exit;
 *   it is bound by national mandate. This structural difference leads to a
 *   different derived directionality `d` and thus a different perceived
 *   effective extraction (χ), even though both are `institutional` actors.
 *   The system is a tool for profit for one, and a tool for policy for the other.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two errors. 1) It does not label the
 *   system a pure Snare, which would ignore its vital coordination function of
 *   providing connectivity where none existed. 2) It does not label it a pure
 *   Rope, which would ignore the significant, asymmetric extraction from a
 *   captive user base. The Tangled Rope classification captures this essential
 *   duality of "coordination for a price."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_satellite_d2m_standard,
    'Will competition or regulation emerge to limit monopoly pricing power?',
    'Analysis of market consolidation trends and regulatory actions (e.g., spectrum auctions with price caps) over the next 5-10 years.',
    'If competition/regulation is effective, ε will effectively decrease, shifting classifications toward Rope. If not, ε will increase, shifting them toward Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(satellite_d2m_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a new technology. The data models a potential lifecycle where initial
% focus on function gives way to increasing extraction as the market matures and
% consolidates. This is not high-extraction yet, but modeling drift is prudent.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(satellite_d2m_standard_tr_t0, satellite_d2m_standard, theater_ratio, 0, 0.10).
narrative_ontology:measurement(satellite_d2m_standard_tr_t5, satellite_d2m_standard, theater_ratio, 5, 0.15).
narrative_ontology:measurement(satellite_d2m_standard_tr_t10, satellite_d2m_standard, theater_ratio, 10, 0.20).

% Extraction over time (potential for accumulation):
narrative_ontology:measurement(satellite_d2m_standard_ex_t0, satellite_d2m_standard, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(satellite_d2m_standard_ex_t5, satellite_d2m_standard, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(satellite_d2m_standard_ex_t10, satellite_d2m_standard, base_extractiveness, 10, 0.50).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(satellite_d2m_standard, global_infrastructure).

% Network relationships (structural influence edges)
% This standard competes with and influences terrestrial network buildout.
narrative_ontology:affects_constraint(satellite_d2m_standard, terrestrial_5g_rollout).
% It also creates new demands on spectrum allocation policy.
narrative_ontology:affects_constraint(satellite_d2m_standard, spectrum_allocation_policy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are used. The structural derivation from beneficiary/victim
% declarations and the distinct exit options for institutional actors
% (arbitrage vs. constrained) are sufficient to capture the dynamics of this scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */