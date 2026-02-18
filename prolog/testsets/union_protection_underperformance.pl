% ============================================================================
% CONSTRAINT STORY: union_protection_underperformance
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_union_protection_underperformance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: union_protection_underperformance
 * human_readable: "Just Cause" Protection for Underperforming Union Employees
 * domain: economic
 * * SUMMARY:
 * This constraint models the "Just Cause" and due process provisions in
 * collective bargaining agreements that make it difficult to terminate
 * employees for sub-par performance without exhaustive documentation and
 * remediation attempts. While designed to prevent arbitrary firing (a
 * coordination function), it often results in the retention of chronic
 * underperformers, extracting productivity from the organization and morale
 * from high-performing peers.
 * * KEY AGENTS:
 * - Front-line Supervisor: Subject (Powerless); must navigate a high
 *   bureaucratic burden of proof to manage performance.
 * - The Union: Beneficiary (Institutional); enforces the rules to ensure
 *   procedural fidelity and protect all members.
 * - High-Performing Coworkers: Victims; must often cover productivity gaps.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(union_protection_underperformance, 0.60). % Extraction from the organization (wages for low output) and peers (covering slack).
domain_priors:suppression_score(union_protection_underperformance, 0.50).   % Alternatives (at-will employment) are suppressed by the legal force of the contract.
domain_priors:theater_ratio(union_protection_underperformance, 0.15).       % The system is highly functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(union_protection_underperformance, extractiveness, 0.60).
narrative_ontology:constraint_metric(union_protection_underperformance, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(union_protection_underperformance, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(union_protection_underperformance, tangled_rope).
narrative_ontology:topic_domain(union_protection_underperformance, "economic").
narrative_ontology:human_readable(union_protection_underperformance, "\"Just Cause\" Protection for Underperforming Union Employees").

% Binary flags
domain_priors:requires_active_enforcement(union_protection_underperformance). % Requires grievance procedures, arbitration, etc.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(union_protection_underperformance, union_members_seeking_security).
narrative_ontology:constraint_victim(union_protection_underperformance, organizational_efficiency).
narrative_ontology:constraint_victim(union_protection_underperformance, high_performing_coworkers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (FRONT-LINE SUPERVISOR)
% The supervisor is powerless, trapped by rules that create a high coercive
% burden (document everything) and extract productivity. This feels like a Snare.
constraint_indexing:constraint_classification(union_protection_underperformance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (THE UNION)
% The union, as an institutional actor, sees this as a pure coordination
% mechanism (Rope) that provides stability and prevents arbitrary management action.
constraint_indexing:constraint_classification(union_protection_underperformance, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination function (beneficiaries exist) and the
% asymmetric extraction (victims exist), enforced actively. This is a canonical
% Tangled Rope.
constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_protection_underperformance_tests).

test(perspectival_gap) :-
    % Verify the gap between the supervisor (Snare) and the union (Rope).
    constraint_indexing:constraint_classification(union_protection_underperformance, TypePowerless,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_protection_underperformance, TypeInstitutional,
        context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(union_protection_underperformance, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three conditions for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(union_protection_underperformance, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(union_protection_underperformance, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(union_protection_underperformance).

:- end_tests(union_protection_underperformance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.60 reflects the significant cost imposed on the
 * organization and high-performing peers, who subsidize the wages and
 * productivity gaps of underperformers. The suppression score of 0.50 reflects
 * that while merit-based alternatives exist, they are legally and culturally
 * suppressed within the unionized context.
 *
 * The key Perspectival Gap is between the supervisor and the union. The
 * supervisor, despite their title, is powerless against the contract's rules,
 * experiencing the system as a Snare that extracts their time and their team's
 * productivity. The union, an institutional power, sees the same rules as a
 * Rope, a necessary tool for coordinating fair labor practices and protecting
 * all members from arbitrary dismissal.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification is critical here. A naive analysis might
 * label this a pure Snare (focusing only on the victims) or a pure Rope
 * (focusing only on the beneficiaries). Tangled Rope correctly identifies that
 * the constraint has a genuine, non-trivial coordination function (preventing
 * arbitrary firings) that is structurally coupled with significant, asymmetric
 * extraction (protecting underperformers at others' expense).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_union_protection,
    "Is 'underperformance' an objective, measurable deficit, or a subjective management label used to undermine worker protections?",
    "Analysis of grievance arbitration outcomes, comparing cases with quantitative performance data vs. those with qualitative assessments.",
    "If primarily objective, the system is a flawed but functional Tangled Rope. If primarily subjective, it's a necessary Rope protecting against a management Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_protection_underperformance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (0.60 > 0.46), requiring temporal data.
% We model a slow increase in extraction over time as contracts become more
% rigid and procedural burdens accumulate. Theater ratio remains low.

% Theater ratio over time:
narrative_ontology:measurement(up_tr_t0, union_protection_underperformance, theater_ratio, 0, 0.10).
narrative_ontology:measurement(up_tr_t5, union_protection_underperformance, theater_ratio, 5, 0.12).
narrative_ontology:measurement(up_tr_t10, union_protection_underperformance, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(up_ex_t0, union_protection_underperformance, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(up_ex_t5, union_protection_underperformance, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(up_ex_t10, union_protection_underperformance, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's function is based on enforcing a contract via grievance
% procedures, making it an enforcement mechanism.
narrative_ontology:coordination_type(union_protection_underperformance, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */